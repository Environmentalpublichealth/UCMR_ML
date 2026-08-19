#!/usr/bin/env python3
"""Train and evaluate an XGBoost classifier for Northeast PFAS detection.

This script follows the same workflow as the other models in
alexs_experiments:

1. Load and prepare NE_data_ready.csv with logistic_regression.py.
2. Keep every row from a PWS in the same five-fold validation split.
3. Generate out-of-fold predictions at row and PWS levels.
4. Fit one final XGBoost model on all labeled Northeast rows.
5. Save metrics, predictions, feature importances, and the fitted model.

The defaults are reasonable starting values, not hyperparameters selected
using Pennsylvania data. Keep PA data outside model fitting and tuning so it
can remain an independent external-validation dataset.

Run from the repository root:

    python pfas_northeast/code/alexs_experiments/xgboost_classifier.py
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
import xgboost
from sklearn.base import clone
from sklearn.impute import SimpleImputer
from sklearn.model_selection import StratifiedGroupKFold
from sklearn.pipeline import Pipeline
from sklearn.utils.class_weight import compute_sample_weight
from xgboost import XGBClassifier

import logistic_regression as base


HERE = Path(__file__).resolve().parent
DEFAULT_DATA_PATH = HERE / "NE_data_ready.csv"
DEFAULT_OUTPUT_DIR = HERE / "xgboost_output"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Train an XGBoost PFAS classifier with PWS-grouped five-fold "
            "out-of-fold evaluation."
        )
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
        "--n-estimators",
        type=int,
        default=300,
        help="Number of boosted trees (default: 300).",
    )
    parser.add_argument(
        "--learning-rate",
        type=float,
        default=0.03,
        help="Contribution of each new tree (default: 0.03).",
    )
    parser.add_argument(
        "--max-depth",
        type=int,
        default=3,
        help="Maximum depth of each tree (default: 3).",
    )
    parser.add_argument(
        "--min-child-weight",
        type=float,
        default=3.0,
        help="Minimum child-node weight used to limit overfitting (default: 3).",
    )
    parser.add_argument(
        "--subsample",
        type=float,
        default=0.8,
        help="Fraction of training rows used by each tree (default: 0.8).",
    )
    parser.add_argument(
        "--colsample-bytree",
        type=float,
        default=0.8,
        help="Fraction of predictors considered by each tree (default: 0.8).",
    )
    parser.add_argument(
        "--reg-alpha",
        type=float,
        default=0.05,
        help="L1 regularization strength (default: 0.05).",
    )
    parser.add_argument(
        "--reg-lambda",
        type=float,
        default=1.0,
        help="L2 regularization strength (default: 1.0).",
    )
    parser.add_argument(
        "--gamma",
        type=float,
        default=0.0,
        help="Minimum loss reduction required for another split (default: 0).",
    )
    return parser.parse_args()


def configure_logging() -> logging.Logger:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return logging.getLogger("northeast_xgboost")


def build_model(args: argparse.Namespace) -> Pipeline:
    """Create a fold-safe median-imputation and XGBoost pipeline."""
    return Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            (
                "xgboost",
                XGBClassifier(
                    objective="binary:logistic",
                    eval_metric="logloss",
                    n_estimators=args.n_estimators,
                    learning_rate=args.learning_rate,
                    max_depth=args.max_depth,
                    min_child_weight=args.min_child_weight,
                    subsample=args.subsample,
                    colsample_bytree=args.colsample_bytree,
                    reg_alpha=args.reg_alpha,
                    reg_lambda=args.reg_lambda,
                    gamma=args.gamma,
                    tree_method="hist",
                    importance_type="gain",
                    random_state=base.RANDOM_SEED,
                    n_jobs=-1,
                    verbosity=0,
                ),
            ),
        ]
    )


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
    """Generate OOF predictions while preventing PWS leakage."""
    if n_splits < 2:
        raise ValueError("--folds must be at least 2.")
    if n_splits > len(np.unique(groups)):
        raise ValueError("--folds cannot exceed the number of unique PWSIDs.")

    splitter = StratifiedGroupKFold(
        n_splits=n_splits,
        shuffle=True,
        random_state=base.RANDOM_SEED,
    )
    oof_probability = np.full(len(y), np.nan, dtype=float)
    oof_fold = np.zeros(len(y), dtype=int)
    fold_records: list[dict[str, Any]] = []

    for fold, (train_index, test_index) in enumerate(
        splitter.split(X, y, groups), start=1
    ):
        train_pws = set(groups[train_index])
        test_pws = set(groups[test_index])
        if train_pws.intersection(test_pws):
            raise RuntimeError(f"PWS leakage detected in fold {fold}.")

        fold_model = clone(model)
        train_weights = compute_sample_weight(
            class_weight="balanced",
            y=y[train_index],
        )
        fold_model.fit(
            X.iloc[train_index],
            y[train_index],
            xgboost__sample_weight=train_weights,
        )
        probabilities = fold_model.predict_proba(X.iloc[test_index])[:, 1]
        oof_probability[test_index] = probabilities
        oof_fold[test_index] = fold

        metrics = base.classification_metrics(
            y[test_index], probabilities, threshold
        )
        metrics.update(
            {
                "fold": fold,
                "n_train": int(len(train_index)),
                "n_test": int(len(test_index)),
                "n_train_pws": int(len(train_pws)),
                "n_test_pws": int(len(test_pws)),
            }
        )
        fold_records.append(metrics)
        logger.info(
            "Fold %d: ROC-AUC=%.3f, PR-AUC=%.3f, recall=%.3f, "
            "precision=%.3f.",
            fold,
            metrics["roc_auc"],
            metrics["pr_auc"],
            metrics["recall"],
            metrics["precision"],
        )

    if np.isnan(oof_probability).any() or (oof_fold == 0).any():
        raise RuntimeError("Some rows did not receive an out-of-fold prediction.")

    predictions = pd.DataFrame(
        {
            "source_row_index": row_ids,
            base.GROUP_COLUMN: groups,
            "y_true": y,
            "y_probability": oof_probability,
            "y_predicted": (oof_probability >= threshold).astype(int),
            "fold": oof_fold,
        }
    )
    return pd.DataFrame(fold_records).sort_values("fold"), predictions


def validate_arguments(args: argparse.Namespace) -> None:
    if not 0.0 < args.threshold < 1.0:
        raise ValueError("--threshold must be between 0 and 1.")
    if args.n_estimators < 1:
        raise ValueError("--n-estimators must be at least 1.")
    if args.learning_rate <= 0:
        raise ValueError("--learning-rate must be positive.")
    if args.max_depth < 1:
        raise ValueError("--max-depth must be at least 1.")
    if args.min_child_weight < 0:
        raise ValueError("--min-child-weight cannot be negative.")
    if not 0.0 < args.subsample <= 1.0:
        raise ValueError("--subsample must be greater than 0 and at most 1.")
    if not 0.0 < args.colsample_bytree <= 1.0:
        raise ValueError("--colsample-bytree must be greater than 0 and at most 1.")
    if args.reg_alpha < 0 or args.reg_lambda < 0 or args.gamma < 0:
        raise ValueError("Regularization values and --gamma cannot be negative.")


def main() -> None:
    args = parse_args()
    validate_arguments(args)
    logger = configure_logging()

    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    X, y, groups, row_ids, dropped_predictors = base.load_training_data(
        args.data.resolve(), logger
    )
    model = build_model(args)

    logger.info(
        "Running %d-fold grouped CV with %d boosted trees...",
        args.folds,
        args.n_estimators,
    )
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
    pws_predictions = base.aggregate_pws_predictions(
        facility_predictions, args.threshold
    )

    facility_metrics = base.classification_metrics(
        facility_predictions["y_true"].to_numpy(),
        facility_predictions["y_probability"].to_numpy(),
        args.threshold,
    )
    pws_metrics = base.classification_metrics(
        pws_predictions["y_true"].to_numpy(),
        pws_predictions["y_probability"].to_numpy(),
        args.threshold,
    )

    cv_metrics.to_csv(output_dir / "cv_metrics.csv", index=False)
    facility_predictions.to_csv(
        output_dir / "facility_oof_predictions.csv", index=False
    )
    pws_predictions.to_csv(
        output_dir / "pws_oof_predictions.csv", index=False
    )

    logger.info("Fitting the final XGBoost model on all labeled rows...")
    final_weights = compute_sample_weight(class_weight="balanced", y=y)
    model.fit(X, y, xgboost__sample_weight=final_weights)

    fitted_xgb = model.named_steps["xgboost"]
    feature_names = model.named_steps["imputer"].get_feature_names_out(X.columns)
    feature_importances = pd.DataFrame(
        {
            "feature": feature_names,
            "importance": fitted_xgb.feature_importances_,
        }
    ).sort_values("importance", ascending=False, ignore_index=True)
    feature_importances.insert(
        0, "rank", np.arange(1, len(feature_importances) + 1)
    )
    feature_importances.to_csv(
        output_dir / "feature_importances.csv", index=False
    )

    hyperparameters = {
        "n_estimators": args.n_estimators,
        "learning_rate": args.learning_rate,
        "max_depth": args.max_depth,
        "min_child_weight": args.min_child_weight,
        "subsample": args.subsample,
        "colsample_bytree": args.colsample_bytree,
        "reg_alpha": args.reg_alpha,
        "reg_lambda": args.reg_lambda,
        "gamma": args.gamma,
        "tree_method": "hist",
        "importance_type": "gain",
        "class_balance": "balanced sample weights",
    }
    model_bundle = {
        "model": model,
        "model_type": "xgboost_classifier",
        "target_column": base.TARGET_COLUMN,
        "group_column": base.GROUP_COLUMN,
        "raw_land_use_columns": base.LAND_USE_COLUMNS,
        "clr_pseudocount": base.CLR_PSEUDOCOUNT,
        "prepared_feature_columns": X.columns.tolist(),
        "dropped_predictors": dropped_predictors,
        "threshold": args.threshold,
        "random_seed": base.RANDOM_SEED,
        "hyperparameters": hyperparameters,
    }
    joblib.dump(
        model_bundle,
        output_dir / "xgboost_model.joblib",
        compress=3,
    )

    summary = {
        "data_path": str(args.data.resolve()),
        "model_type": "xgboost_classifier",
        "n_rows": int(len(X)),
        "n_pws": int(len(np.unique(groups))),
        "n_features": int(X.shape[1]),
        "positive_rows": int(y.sum()),
        "positive_rate": float(y.mean()),
        "folds": int(args.folds),
        "decision_threshold": float(args.threshold),
        "grouped_by": base.GROUP_COLUMN,
        "hyperparameters": hyperparameters,
        "fold_metrics": base.mean_fold_metrics(cv_metrics),
        "facility_oof_metrics": facility_metrics,
        "pws_oof_metrics": pws_metrics,
        "dropped_predictors": dropped_predictors,
        "versions": {
            "pandas": pd.__version__,
            "numpy": np.__version__,
            "scikit_learn": sklearn.__version__,
            "xgboost": xgboost.__version__,
            "joblib": joblib.__version__,
        },
    }
    with (output_dir / "summary.json").open("w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)

    logger.info("Saved model and evaluation outputs to %s", output_dir)
    logger.info(
        "Facility OOF: accuracy=%.3f, precision=%.3f, recall=%.3f, "
        "F1=%.3f, ROC-AUC=%.3f.",
        facility_metrics["accuracy"],
        facility_metrics["precision"],
        facility_metrics["recall"],
        facility_metrics["f1"],
        facility_metrics["roc_auc"],
    )
    logger.info(
        "PWS OOF: accuracy=%.3f, precision=%.3f, recall=%.3f, "
        "F1=%.3f, ROC-AUC=%.3f.",
        pws_metrics["accuracy"],
        pws_metrics["precision"],
        pws_metrics["recall"],
        pws_metrics["f1"],
        pws_metrics["roc_auc"],
    )


if __name__ == "__main__":
    main()
