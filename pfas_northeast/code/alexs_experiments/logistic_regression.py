#!/usr/bin/env python3
"""Train and evaluate a Northeast PFAS logistic-regression baseline.

PFAS detection is a binary outcome, so logistic regressionâ€”not ordinary
least-squares linear regressionâ€”is the appropriate linear model.

The script keeps every row from a public water system (PWS) in the same
cross-validation fold. This prevents the same PWS from appearing in both
training and validation data. Missing-value imputation and feature scaling
are fitted separately inside each training fold through a scikit-learn
Pipeline.

Default input:
    NE_data_ready.csv (next to this script)

Default outputs:
    logistic_regression_output/
        cv_metrics.csv
        facility_oof_predictions.csv
        pws_oof_predictions.csv
        summary.json
        coefficients.csv
        logistic_regression_model.joblib

Run:
    python 01_logistic_regression_baseline.py

Optional:
    python 01_logistic_regression_baseline.py --folds 5 --threshold 0.5
"""

from __future__ import annotations

import argparse
import json
import logging
from pathlib import Path
from typing import Any

import joblib
import numpy as np
import pandas as pd
import sklearn
from sklearn.base import clone
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    average_precision_score,
    confusion_matrix,
    f1_score,
    precision_score,
    recall_score,
    roc_auc_score,
)
from sklearn.model_selection import StratifiedGroupKFold
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler


HERE = Path(__file__).resolve().parent
DEFAULT_DATA_PATH = HERE / "NE_data_ready.csv"
DEFAULT_OUTPUT_DIR = HERE / "logistic_regression_output"

TARGET_COLUMN = "pfas_detected"
GROUP_COLUMN = "PWSID"
RANDOM_SEED = 42

# Match the land-use transformation used by the shared national pipeline.
LAND_USE_COLUMNS = [
    "source_other_pct",
    "source_urban_pct",
    "source_barren_pct",
    "source_forest_pct",
    "source_shrub_pct",
    "source_grassland_pct",
    "source_agriculture_pct",
    "source_wetland_pct",
    "source_water_pct",
]
CLR_PSEUDOCOUNT = 0.00005


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Train a grouped-CV logistic-regression model for Northeast PFAS detection."
    )
    parser.add_argument(
        "--data",
        type=Path,
        default=DEFAULT_DATA_PATH,
        help=f"Training CSV (default: {DEFAULT_DATA_PATH.name}).",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help="Directory for model and evaluation outputs.",
    )
    parser.add_argument(
        "--folds",
        type=int,
        default=5,
        help="Number of StratifiedGroupKFold splits (default: 5).",
    )
    parser.add_argument(
        "--threshold",
        type=float,
        default=0.5,
        help="Probability threshold for binary predictions (default: 0.5).",
    )
    parser.add_argument(
        "--c",
        type=float,
        default=1.0,
        help="Inverse L2 regularization strength for LogisticRegression (default: 1.0).",
    )
    return parser.parse_args()


def configure_logging() -> logging.Logger:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return logging.getLogger("northeast_logistic_regression")


def clr_transform_land_use(features: pd.DataFrame) -> pd.DataFrame:
    """Replace available land-use percentages with row-wise CLR features.

    This transformation is deterministic for each row and therefore can be
    performed before cross-validation without learning information from the
    validation folds.
    """
    transformed = features.copy()
    present = [column for column in LAND_USE_COLUMNS if column in transformed.columns]
    if not present:
        return transformed

    land_use = transformed[present].apply(pd.to_numeric, errors="coerce").clip(lower=0)
    land_use = land_use.where(land_use > 0, CLR_PSEUDOCOUNT)
    logged = np.log(land_use)
    row_mean_log = logged.mean(axis=1, skipna=False)

    for column in present:
        clr_name = f"{column.removesuffix('_pct')}_clr"
        transformed[clr_name] = logged[column] - row_mean_log

    return transformed.drop(columns=present)


def load_training_data(
    data_path: Path, logger: logging.Logger
) -> tuple[pd.DataFrame, np.ndarray, np.ndarray, pd.Index, list[str]]:
    if not data_path.exists():
        raise FileNotFoundError(
            f"Training data not found: {data_path}. "
            "Place NE_data_ready.csv next to this script or use --data."
        )

    data = pd.read_csv(data_path, low_memory=False)
    required = {TARGET_COLUMN, GROUP_COLUMN}
    missing_required = sorted(required - set(data.columns))
    if missing_required:
        raise ValueError(f"Training data is missing required columns: {missing_required}")

    labels = pd.to_numeric(data[TARGET_COLUMN], errors="coerce")
    labeled_mask = labels.notna()
    if not labeled_mask.all():
        logger.warning("Dropping %d rows with missing target labels.", int((~labeled_mask).sum()))
        data = data.loc[labeled_mask].copy()
        labels = labels.loc[labeled_mask]

    invalid_labels = sorted(set(labels.unique()) - {0, 1, 0.0, 1.0})
    if invalid_labels:
        raise ValueError(f"{TARGET_COLUMN} must contain only 0/1 labels; found {invalid_labels}")

    row_ids = data.index.copy()
    groups = data[GROUP_COLUMN].astype(str).to_numpy()
    y = labels.astype(int).to_numpy()

    X = data.drop(columns=[TARGET_COLUMN, GROUP_COLUMN])
    X = X.apply(pd.to_numeric, errors="coerce")
    X = clr_transform_land_use(X)

    all_missing = X.columns[X.isna().all()].tolist()
    if all_missing:
        logger.warning("Dropping all-missing predictors: %s", all_missing)
        X = X.drop(columns=all_missing)

    constant = X.columns[X.nunique(dropna=True) <= 1].tolist()
    if constant:
        logger.info("Dropping %d constant predictors: %s", len(constant), constant)
        X = X.drop(columns=constant)

    if X.empty:
        raise ValueError("No usable predictors remain after data preparation.")

    if len(np.unique(groups)) < 2:
        raise ValueError("Grouped cross-validation requires at least two unique PWSIDs.")

    logger.info(
        "Loaded %d rows, %d PWSs, %d predictors; positive rate %.1f%%.",
        len(X),
        len(np.unique(groups)),
        X.shape[1],
        100 * y.mean(),
    )
    return X, y, groups, row_ids, all_missing + constant


def build_model(c_value: float) -> Pipeline:
    """Create a fold-safe imputation, scaling, and LR pipeline."""
    return Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler()),
            (
                "logistic_regression",
                LogisticRegression(
                    C=c_value,
                    class_weight="balanced",
                    solver="lbfgs",
                    max_iter=2000,
                    random_state=RANDOM_SEED,
                ),
            ),
        ]
    )


def classification_metrics(
    y_true: np.ndarray, y_probability: np.ndarray, threshold: float
) -> dict[str, float | int]:
    y_pred = (y_probability >= threshold).astype(int)
    tn, fp, fn, tp = confusion_matrix(y_true, y_pred, labels=[0, 1]).ravel()

    return {
        "n": int(len(y_true)),
        "n_positive": int(y_true.sum()),
        "positive_rate": float(y_true.mean()),
        "roc_auc": float(roc_auc_score(y_true, y_probability)),
        "pr_auc": float(average_precision_score(y_true, y_probability)),
        "accuracy": float(accuracy_score(y_true, y_pred)),
        "precision": float(precision_score(y_true, y_pred, zero_division=0)),
        "recall": float(recall_score(y_true, y_pred, zero_division=0)),
        "f1": float(f1_score(y_true, y_pred, zero_division=0)),
        "threshold": float(threshold),
        "true_negative": int(tn),
        "false_positive": int(fp),
        "false_negative": int(fn),
        "true_positive": int(tp),
    }


def grouped_cross_validation(
    model: Pipeline,
    X: pd.DataFrame,
    y: np.ndarray,
    groups: np.ndarray,
    row_ids: pd.Index,
    n_splits: int,
    threshold: float,
    logger: logging.Logger,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    if n_splits < 2:
        raise ValueError("--folds must be at least 2.")
    if n_splits > len(np.unique(groups)):
        raise ValueError("--folds cannot exceed the number of unique PWSIDs.")

    cv = StratifiedGroupKFold(
        n_splits=n_splits,
        shuffle=True,
        random_state=RANDOM_SEED,
    )

    oof_probability = np.full(len(y), np.nan, dtype=float)
    oof_fold = np.zeros(len(y), dtype=int)
    fold_records: list[dict[str, Any]] = []

    for fold, (train_index, test_index) in enumerate(cv.split(X, y, groups), start=1):
        fold_model = clone(model)
        fold_model.fit(X.iloc[train_index], y[train_index])
        probabilities = fold_model.predict_proba(X.iloc[test_index])[:, 1]
        oof_probability[test_index] = probabilities
        oof_fold[test_index] = fold

        metrics = classification_metrics(y[test_index], probabilities, threshold)
        metrics.update(
            {
                "fold": fold,
                "n_train": int(len(train_index)),
                "n_test_pws": int(len(np.unique(groups[test_index]))),
            }
        )
        fold_records.append(metrics)
        logger.info(
            "Fold %d: ROC-AUC=%.3f, PR-AUC=%.3f, recall=%.3f, precision=%.3f.",
            fold,
            metrics["roc_auc"],
            metrics["pr_auc"],
            metrics["recall"],
            metrics["precision"],
        )

    if np.isnan(oof_probability).any():
        raise RuntimeError("Some rows did not receive an out-of-fold prediction.")

    oof_predictions = pd.DataFrame(
        {
            "source_row_index": row_ids,
            GROUP_COLUMN: groups,
            "y_true": y,
            "y_probability": oof_probability,
            "y_predicted": (oof_probability >= threshold).astype(int),
            "fold": oof_fold,
        }
    )
    return pd.DataFrame(fold_records).sort_values("fold"), oof_predictions


def aggregate_pws_predictions(
    facility_predictions: pd.DataFrame, threshold: float
) -> pd.DataFrame:
    """Use the repository's any-positive/max-probability PWS rule."""
    pws = (
        facility_predictions.groupby(GROUP_COLUMN, as_index=False)
        .agg(
            y_true=("y_true", "max"),
            y_probability=("y_probability", "max"),
            n_rows=("y_true", "size"),
            fold=("fold", "first"),
        )
        .sort_values(GROUP_COLUMN)
        .reset_index(drop=True)
    )
    pws["y_predicted"] = (pws["y_probability"] >= threshold).astype(int)
    return pws


def mean_fold_metrics(cv_metrics: pd.DataFrame) -> dict[str, float]:
    metric_columns = ["roc_auc", "pr_auc", "accuracy", "precision", "recall", "f1"]
    summary: dict[str, float] = {}
    for column in metric_columns:
        summary[f"{column}_mean"] = float(cv_metrics[column].mean())
        summary[f"{column}_std"] = float(cv_metrics[column].std(ddof=1))
    return summary


def main() -> None:
    args = parse_args()
    logger = configure_logging()

    if not 0 < args.threshold < 1:
        raise ValueError("--threshold must be between 0 and 1.")
    if args.c <= 0:
        raise ValueError("--c must be greater than 0.")

    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    X, y, groups, row_ids, dropped_predictors = load_training_data(
        args.data.resolve(), logger
    )
    model = build_model(args.c)

    logger.info("Running %d-fold stratified grouped cross-validation...", args.folds)
    cv_metrics, facility_predictions = grouped_cross_validation(
        model=model,
        X=X,
        y=y,
        groups=groups,
        row_ids=row_ids,
        n_splits=args.folds,
        threshold=args.threshold,
        logger=logger,
    )
    pws_predictions = aggregate_pws_predictions(facility_predictions, args.threshold)

    facility_metrics = classification_metrics(
        facility_predictions["y_true"].to_numpy(),
        facility_predictions["y_probability"].to_numpy(),
        args.threshold,
    )
    pws_metrics = classification_metrics(
        pws_predictions["y_true"].to_numpy(),
        pws_predictions["y_probability"].to_numpy(),
        args.threshold,
    )

    logger.info(
        "OOF facility: ROC-AUC=%.3f, PR-AUC=%.3f. PWS: ROC-AUC=%.3f, PR-AUC=%.3f.",
        facility_metrics["roc_auc"],
        facility_metrics["pr_auc"],
        pws_metrics["roc_auc"],
        pws_metrics["pr_auc"],
    )

    cv_metrics.to_csv(output_dir / "cv_metrics.csv", index=False)
    facility_predictions.to_csv(output_dir / "facility_oof_predictions.csv", index=False)
    pws_predictions.to_csv(output_dir / "pws_oof_predictions.csv", index=False)

    logger.info("Fitting the final baseline model on all labeled Northeast rows...")
    model.fit(X, y)
    fitted_lr = model.named_steps["logistic_regression"]
    coefficients = pd.DataFrame(
        {
            "feature": X.columns,
            "coefficient": fitted_lr.coef_[0],
            "odds_ratio": np.exp(np.clip(fitted_lr.coef_[0], -700, 700)),
        }
    )
    coefficients["absolute_coefficient"] = coefficients["coefficient"].abs()
    coefficients = coefficients.sort_values("absolute_coefficient", ascending=False)
    coefficients.to_csv(output_dir / "coefficients.csv", index=False)

    model_bundle = {
        "model": model,
        "model_type": "logistic_regression",
        "target_column": TARGET_COLUMN,
        "group_column": GROUP_COLUMN,
        "raw_land_use_columns": LAND_USE_COLUMNS,
        "clr_pseudocount": CLR_PSEUDOCOUNT,
        "prepared_feature_columns": X.columns.tolist(),
        "dropped_predictors": dropped_predictors,
        "threshold": args.threshold,
        "random_seed": RANDOM_SEED,
    }
    joblib.dump(model_bundle, output_dir / "logistic_regression_model.joblib", compress=3)

    summary = {
        "data_path": str(args.data.resolve()),
        "n_rows": int(len(X)),
        "n_pws": int(len(np.unique(groups))),
        "n_features": int(X.shape[1]),
        "positive_rows": int(y.sum()),
        "positive_rate": float(y.mean()),
        "folds": int(args.folds),
        "class_weight": "balanced",
        "regularization_c": float(args.c),
        "decision_threshold": float(args.threshold),
        "fold_metrics": mean_fold_metrics(cv_metrics),
        "facility_oof_metrics": facility_metrics,
        "pws_oof_metrics": pws_metrics,
        "versions": {
            "pandas": pd.__version__,
            "numpy": np.__version__,
            "scikit_learn": sklearn.__version__,
        },
    }
    with (output_dir / "summary.json").open("w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)

    logger.info("Saved model and evaluation outputs to %s", output_dir)


if __name__ == "__main__":
    main()
