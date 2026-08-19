#!/usr/bin/env python3
"""Train and evaluate a Northeast PFAS random-forest model.

This script mirrors the grouped five-fold workflow in logistic_regression.py:

1. Load NE_data_ready.csv and prepare the same feature set.
2. Keep every row from a PWS in the same cross-validation fold.
3. Generate out-of-fold predictions at row and PWS levels.
4. Fit a final random forest on all labeled Northeast rows.
5. Save metrics, predictions, feature importances, and the fitted model.

The default random-forest settings use the national PFAS model's documented
hyperparameters as a starting point. They have not been tuned using the PA
external-validation data.

Run from the repository root:

    python pfas_northeast/code/alexs_experiments/random_forest.py

Optional example:

    python pfas_northeast/code/alexs_experiments/random_forest.py \
        --folds 5 --n-estimators 188 --max-features 0.3 \
        --min-samples-leaf 2 --threshold 0.5
"""

from __future__ import annotations

import argparse
import json
import logging
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
import sklearn
from sklearn.ensemble import RandomForestClassifier
from sklearn.impute import SimpleImputer
from sklearn.pipeline import Pipeline

import logistic_regression as base


HERE = Path(__file__).resolve().parent
DEFAULT_DATA_PATH = HERE / "NE_data_ready.csv"
DEFAULT_OUTPUT_DIR = HERE / "random_forest_output"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Train a random forest with grouped five-fold Northeast PWS "
            "cross-validation."
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
        default=188,
        help="Number of trees (default: 188).",
    )
    parser.add_argument(
        "--max-features",
        type=float,
        default=0.3,
        help="Fraction of features considered at each split (default: 0.3).",
    )
    parser.add_argument(
        "--min-samples-leaf",
        type=int,
        default=2,
        help="Minimum training rows in each terminal leaf (default: 2).",
    )
    parser.add_argument(
        "--max-depth",
        type=int,
        default=None,
        help="Maximum tree depth (default: unrestricted).",
    )
    return parser.parse_args()


def configure_logging() -> logging.Logger:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return logging.getLogger("northeast_random_forest")


def build_random_forest(
    n_estimators: int,
    max_features: float,
    min_samples_leaf: int,
    max_depth: int | None,
) -> Pipeline:
    """Create a fold-safe median-imputation and random-forest pipeline."""
    return Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            (
                "random_forest",
                RandomForestClassifier(
                    n_estimators=n_estimators,
                    max_depth=max_depth,
                    max_features=max_features,
                    min_samples_leaf=min_samples_leaf,
                    class_weight="balanced",
                    random_state=base.RANDOM_SEED,
                    n_jobs=-1,
                ),
            ),
        ]
    )


def main() -> None:
    args = parse_args()
    logger = configure_logging()

    if not 0 < args.threshold < 1:
        raise ValueError("--threshold must be between 0 and 1.")
    if args.n_estimators < 1:
        raise ValueError("--n-estimators must be at least 1.")
    if not 0 < args.max_features <= 1:
        raise ValueError("--max-features must be greater than 0 and at most 1.")
    if args.min_samples_leaf < 1:
        raise ValueError("--min-samples-leaf must be at least 1.")
    if args.max_depth is not None and args.max_depth < 1:
        raise ValueError("--max-depth must be at least 1 when provided.")

    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    X, y, groups, row_ids, dropped_predictors = base.load_training_data(
        args.data.resolve(), logger
    )
    model = build_random_forest(
        n_estimators=args.n_estimators,
        max_features=args.max_features,
        min_samples_leaf=args.min_samples_leaf,
        max_depth=args.max_depth,
    )

    logger.info(
        "Running %d-fold grouped CV with %d trees...",
        args.folds,
        args.n_estimators,
    )
    cv_metrics, facility_predictions = base.grouped_cross_validation(
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

    logger.info("Fitting the final random forest on all labeled rows...")
    model.fit(X, y)

    fitted_rf = model.named_steps["random_forest"]
    feature_names = model.named_steps["imputer"].get_feature_names_out(X.columns)
    feature_importances = pd.DataFrame(
        {
            "feature": feature_names,
            "importance": fitted_rf.feature_importances_,
        }
    ).sort_values("importance", ascending=False)
    feature_importances["rank"] = np.arange(1, len(feature_importances) + 1)
    feature_importances.to_csv(
        output_dir / "feature_importances.csv", index=False
    )

    model_bundle = {
        "model": model,
        "model_type": "random_forest",
        "target_column": base.TARGET_COLUMN,
        "group_column": base.GROUP_COLUMN,
        "raw_land_use_columns": base.LAND_USE_COLUMNS,
        "clr_pseudocount": base.CLR_PSEUDOCOUNT,
        "prepared_feature_columns": X.columns.tolist(),
        "dropped_predictors": dropped_predictors,
        "threshold": args.threshold,
        "random_seed": base.RANDOM_SEED,
        "hyperparameters": {
            "n_estimators": args.n_estimators,
            "max_depth": args.max_depth,
            "max_features": args.max_features,
            "min_samples_leaf": args.min_samples_leaf,
            "class_weight": "balanced",
        },
    }
    joblib.dump(
        model_bundle,
        output_dir / "random_forest_model.joblib",
        compress=3,
    )

    summary = {
        "data_path": str(args.data.resolve()),
        "model_type": "random_forest",
        "n_rows": int(len(X)),
        "n_pws": int(len(np.unique(groups))),
        "n_features": int(X.shape[1]),
        "positive_rows": int(y.sum()),
        "positive_rate": float(y.mean()),
        "folds": int(args.folds),
        "decision_threshold": float(args.threshold),
        "hyperparameters": model_bundle["hyperparameters"],
        "fold_metrics": base.mean_fold_metrics(cv_metrics),
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
