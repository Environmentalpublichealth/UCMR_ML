#!/usr/bin/env python3
"""Train and evaluate a boosted tree model for Northeast PFAS detection.

Although this is commonly called a boosted regression tree, the outcome in
NE_data_ready.csv is binary (PFAS detected or not detected), so this script
uses scikit-learn's GradientBoostingClassifier.

The evaluation uses five-fold out-of-fold (OOF) predictions. Entire public
water systems (PWSIDs) stay together in either training or validation within
each fold, preventing rows from the same PWS from leaking across the split.
After OOF evaluation, one final model is trained on all available Northeast
rows and saved for later prediction or validation against Pennsylvania data.
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
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.impute import SimpleImputer
from sklearn.model_selection import StratifiedGroupKFold
from sklearn.pipeline import Pipeline
from sklearn.utils.class_weight import compute_sample_weight

import logistic_regression as base


HERE = Path(__file__).resolve().parent
DEFAULT_DATA_PATH = HERE / "NE_data_ready.csv"
DEFAULT_OUTPUT_DIR = HERE / "boosted_regression_tree_output"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Train a gradient-boosted tree classifier with PWS-grouped "
            "five-fold out-of-fold evaluation."
        )
    )
    parser.add_argument("--data", type=Path, default=DEFAULT_DATA_PATH)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--folds", type=int, default=5)
    parser.add_argument("--threshold", type=float, default=0.5)
    parser.add_argument("--n-estimators", type=int, default=200)
    parser.add_argument("--learning-rate", type=float, default=0.05)
    parser.add_argument("--max-depth", type=int, default=3)
    parser.add_argument("--min-samples-leaf", type=int, default=2)
    parser.add_argument("--subsample", type=float, default=0.8)
    return parser.parse_args()


def build_model(args: argparse.Namespace) -> Pipeline:
    return Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            (
                "gradient_boosting",
                GradientBoostingClassifier(
                    n_estimators=args.n_estimators,
                    learning_rate=args.learning_rate,
                    max_depth=args.max_depth,
                    min_samples_leaf=args.min_samples_leaf,
                    subsample=args.subsample,
                    random_state=base.RANDOM_SEED,
                ),
            ),
        ]
    )


def grouped_cross_validation(
    model: Pipeline,
    X: pd.DataFrame,
    y: np.ndarray,
    groups: np.ndarray,
    row_ids: np.ndarray,
    n_splits: int,
    threshold: float,
    logger: logging.Logger,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Create OOF predictions while keeping every PWS inside one fold."""
    if n_splits > len(np.unique(groups)):
        raise ValueError("--folds cannot exceed the number of unique PWSIDs.")

    splitter = StratifiedGroupKFold(
        n_splits=n_splits,
        shuffle=True,
        random_state=base.RANDOM_SEED,
    )

    probabilities = np.full(len(y), np.nan, dtype=float)
    fold_numbers = np.zeros(len(y), dtype=int)
    fold_records: list[dict[str, Any]] = []

    for fold, (train_index, validation_index) in enumerate(
        splitter.split(X, y, groups), start=1
    ):
        train_groups = set(groups[train_index])
        validation_groups = set(groups[validation_index])
        if train_groups.intersection(validation_groups):
            raise RuntimeError(f"PWS leakage detected in fold {fold}.")

        fold_model = clone(model)
        train_weights = compute_sample_weight(
            class_weight="balanced", y=y[train_index]
        )
        fold_model.fit(
            X.iloc[train_index],
            y[train_index],
            gradient_boosting__sample_weight=train_weights,
        )

        fold_probabilities = fold_model.predict_proba(
            X.iloc[validation_index]
        )[:, 1]
        probabilities[validation_index] = fold_probabilities
        fold_numbers[validation_index] = fold

        metrics = base.classification_metrics(
            y[validation_index], fold_probabilities, threshold
        )
        metrics.update(
            {
                "fold": fold,
                "n_train_rows": int(len(train_index)),
                "n_validation_rows": int(len(validation_index)),
                "n_train_pws": int(len(train_groups)),
                "n_validation_pws": int(len(validation_groups)),
            }
        )
        fold_records.append(metrics)
        logger.info(
            "Fold %d: %d validation rows, %d validation PWS, "
            "ROC AUC %.3f, recall %.3f",
            fold,
            len(validation_index),
            len(validation_groups),
            metrics["roc_auc"],
            metrics["recall"],
        )

    if np.isnan(probabilities).any() or (fold_numbers == 0).any():
        raise RuntimeError("Some rows did not receive an OOF prediction.")

    facility_predictions = pd.DataFrame(
        {
            "source_row_index": row_ids,
            base.GROUP_COLUMN: groups,
            "y_true": y,
            "y_probability": probabilities,
            "y_predicted": (probabilities >= threshold).astype(int),
            "fold": fold_numbers,
        }
    )
    return pd.DataFrame(fold_records), facility_predictions


def main() -> None:
    args = parse_args()
    logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")
    logger = logging.getLogger(__name__)

    if args.folds < 2:
        raise ValueError("--folds must be at least 2.")
    if not 0.0 < args.threshold < 1.0:
        raise ValueError("--threshold must be between 0 and 1.")
    if args.n_estimators < 1:
        raise ValueError("--n-estimators must be at least 1.")
    if args.learning_rate <= 0:
        raise ValueError("--learning-rate must be positive.")
    if args.max_depth < 1:
        raise ValueError("--max-depth must be at least 1.")
    if args.min_samples_leaf < 1:
        raise ValueError("--min-samples-leaf must be at least 1.")
    if not 0.0 < args.subsample <= 1.0:
        raise ValueError("--subsample must be greater than 0 and at most 1.")

    args.output_dir.mkdir(parents=True, exist_ok=True)
    X, y, groups, row_ids, dropped_predictors = base.load_training_data(
        args.data, logger
    )
    model = build_model(args)

    logger.info("Running %d-fold PWS-grouped OOF evaluation.", args.folds)
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

    logger.info("Fitting the final model on all %d Northeast rows.", len(X))
    final_weights = compute_sample_weight(class_weight="balanced", y=y)
    model.fit(X, y, gradient_boosting__sample_weight=final_weights)

    imputer = model.named_steps["imputer"]
    boosted_tree = model.named_steps["gradient_boosting"]
    feature_names = imputer.get_feature_names_out(X.columns)
    importances = pd.DataFrame(
        {
            "feature": feature_names,
            "importance": boosted_tree.feature_importances_,
        }
    ).sort_values("importance", ascending=False, ignore_index=True)
    importances.insert(0, "rank", np.arange(1, len(importances) + 1))

    summary = {
        "model_type": "GradientBoostingClassifier",
        "data_path": str(args.data.resolve()),
        "n_rows": int(len(X)),
        "n_pws": int(pd.Series(groups).nunique()),
        "n_features": int(X.shape[1]),
        "positive_rows": int(np.sum(y == 1)),
        "negative_rows": int(np.sum(y == 0)),
        "positive_rate": float(np.mean(y)),
        "n_folds": args.folds,
        "decision_threshold": args.threshold,
        "grouped_by": base.GROUP_COLUMN,
        "class_balancing": "balanced sample weights within each training fold",
        "hyperparameters": {
            "n_estimators": args.n_estimators,
            "learning_rate": args.learning_rate,
            "max_depth": args.max_depth,
            "min_samples_leaf": args.min_samples_leaf,
            "subsample": args.subsample,
            "random_state": base.RANDOM_SEED,
        },
        "fold_metrics": base.mean_fold_metrics(cv_metrics),
        "facility_oof_metrics": facility_metrics,
        "pws_oof_metrics": pws_metrics,
        "dropped_predictors": dropped_predictors,
        "versions": {
            "python_packages": {
                "numpy": np.__version__,
                "pandas": pd.__version__,
                "scikit_learn": sklearn.__version__,
                "joblib": joblib.__version__,
            }
        },
    }

    model_bundle = {
        "model": model,
        "feature_columns": list(X.columns),
        "target_column": base.TARGET_COLUMN,
        "group_column": base.GROUP_COLUMN,
        "threshold": args.threshold,
        "random_seed": base.RANDOM_SEED,
        "class_balance": "balanced_sample_weight",
        "land_use_columns": base.LAND_USE_COLUMNS,
        "clr_pseudocount": base.CLR_PSEUDOCOUNT,
    }

    cv_metrics.to_csv(args.output_dir / "cv_metrics.csv", index=False)
    facility_predictions.to_csv(
        args.output_dir / "facility_oof_predictions.csv", index=False
    )
    pws_predictions.to_csv(
        args.output_dir / "pws_oof_predictions.csv", index=False
    )
    importances.to_csv(args.output_dir / "feature_importances.csv", index=False)
    with (args.output_dir / "summary.json").open("w", encoding="utf-8") as file:
        json.dump(summary, file, indent=2)
    joblib.dump(
        model_bundle,
        args.output_dir / "boosted_regression_tree_model.joblib",
    )

    logger.info("Saved results to %s", args.output_dir.resolve())
    logger.info(
        "Facility OOF: accuracy %.3f, precision %.3f, recall %.3f, "
        "F1 %.3f, ROC AUC %.3f",
        facility_metrics["accuracy"],
        facility_metrics["precision"],
        facility_metrics["recall"],
        facility_metrics["f1"],
        facility_metrics["roc_auc"],
    )
    logger.info(
        "PWS OOF: accuracy %.3f, precision %.3f, recall %.3f, "
        "F1 %.3f, ROC AUC %.3f",
        pws_metrics["accuracy"],
        pws_metrics["precision"],
        pws_metrics["recall"],
        pws_metrics["f1"],
        pws_metrics["roc_auc"],
    )


if __name__ == "__main__":
    main()
