#!/usr/bin/env python3
"""Train and evaluate an elastic-net logistic-regression PFAS model.

The workflow matches logistic_regression.py so results can be compared
directly:

1. Load NE_data_ready.csv and prepare the same feature set.
2. Keep every row from a PWS in the same cross-validation fold.
3. Generate five-fold out-of-fold predictions at row and PWS levels.
4. Fit a final elastic-net model on all labeled Northeast rows.
5. Save metrics, predictions, coefficients, and the fitted model bundle.

Elastic net combines L1 and L2 regularization. The default l1_ratio of 0.5
gives both penalties equal influence. Use --l1-ratio 0 for L2 only or 1 for
L1 only.

Run from the repository root:

    python pfas_northeast/code/alexs_experiments/elastic_net_logistic_regression.py

Optional example:

    python pfas_northeast/code/alexs_experiments/elastic_net_logistic_regression.py \
        --folds 5 --c 1.0 --l1-ratio 0.5 --threshold 0.5
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
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

import logistic_regression as base


HERE = Path(__file__).resolve().parent
DEFAULT_DATA_PATH = HERE / "NE_data_ready.csv"
DEFAULT_OUTPUT_DIR = HERE / "elastic_net_logistic_regression_output"
DEFAULT_MAX_ITER = 10_000
NONZERO_TOLERANCE = 1e-10


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Train an elastic-net logistic-regression model with grouped "
            "five-fold Northeast PWS cross-validation."
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
        "--c",
        type=float,
        default=1.0,
        help=(
            "Inverse regularization strength. Smaller values apply stronger "
            "regularization (default: 1.0)."
        ),
    )
    parser.add_argument(
        "--l1-ratio",
        type=float,
        default=0.5,
        help=(
            "Elastic-net mixture: 0 is L2 only, 1 is L1 only, and values "
            "between 0 and 1 combine both (default: 0.5)."
        ),
    )
    parser.add_argument(
        "--max-iter",
        type=int,
        default=DEFAULT_MAX_ITER,
        help=f"Maximum solver iterations (default: {DEFAULT_MAX_ITER}).",
    )
    return parser.parse_args()


def configure_logging() -> logging.Logger:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return logging.getLogger("northeast_elastic_net_logistic_regression")


def build_elastic_net_model(
    c_value: float,
    l1_ratio: float,
    max_iter: int,
) -> Pipeline:
    """Create a fold-safe preprocessing and elastic-net LR pipeline."""
    return Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
            ("scaler", StandardScaler()),
            (
                "logistic_regression",
                LogisticRegression(
                    penalty="elasticnet",
                    C=c_value,
                    l1_ratio=l1_ratio,
                    class_weight="balanced",
                    solver="saga",
                    max_iter=max_iter,
                    random_state=base.RANDOM_SEED,
                ),
            ),
        ]
    )


def main() -> None:
    args = parse_args()
    logger = configure_logging()

    if not 0 < args.threshold < 1:
        raise ValueError("--threshold must be between 0 and 1.")
    if args.c <= 0:
        raise ValueError("--c must be greater than 0.")
    if not 0 <= args.l1_ratio <= 1:
        raise ValueError("--l1-ratio must be between 0 and 1.")
    if args.max_iter < 1:
        raise ValueError("--max-iter must be at least 1.")

    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    X, y, groups, row_ids, dropped_predictors = base.load_training_data(
        args.data.resolve(), logger
    )
    model = build_elastic_net_model(
        c_value=args.c,
        l1_ratio=args.l1_ratio,
        max_iter=args.max_iter,
    )

    logger.info(
        "Running %d-fold grouped CV with C=%g and l1_ratio=%g...",
        args.folds,
        args.c,
        args.l1_ratio,
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

    logger.info("Fitting the final elastic-net model on all labeled rows...")
    model.fit(X, y)

    fitted_lr = model.named_steps["logistic_regression"]
    coefficient_features = model.named_steps["imputer"].get_feature_names_out(
        X.columns
    )
    coefficients = pd.DataFrame(
        {
            "feature": coefficient_features,
            "coefficient": fitted_lr.coef_[0],
            "odds_ratio": np.exp(np.clip(fitted_lr.coef_[0], -700, 700)),
        }
    )
    coefficients["absolute_coefficient"] = coefficients["coefficient"].abs()
    coefficients["is_selected"] = (
        coefficients["absolute_coefficient"] > NONZERO_TOLERANCE
    )
    coefficients = coefficients.sort_values(
        "absolute_coefficient", ascending=False
    )
    coefficients.to_csv(output_dir / "coefficients.csv", index=False)

    n_selected = int(coefficients["is_selected"].sum())
    logger.info(
        "Elastic net retained %d of %d prepared features.",
        n_selected,
        len(coefficients),
    )

    model_bundle = {
        "model": model,
        "model_type": "elastic_net_logistic_regression",
        "target_column": base.TARGET_COLUMN,
        "group_column": base.GROUP_COLUMN,
        "raw_land_use_columns": base.LAND_USE_COLUMNS,
        "clr_pseudocount": base.CLR_PSEUDOCOUNT,
        "prepared_feature_columns": X.columns.tolist(),
        "dropped_predictors": dropped_predictors,
        "threshold": args.threshold,
        "regularization_c": args.c,
        "l1_ratio": args.l1_ratio,
        "class_weight": "balanced",
        "random_seed": base.RANDOM_SEED,
    }
    joblib.dump(
        model_bundle,
        output_dir / "elastic_net_logistic_regression_model.joblib",
        compress=3,
    )

    summary = {
        "data_path": str(args.data.resolve()),
        "model_type": "elastic_net_logistic_regression",
        "n_rows": int(len(X)),
        "n_pws": int(len(np.unique(groups))),
        "n_features": int(X.shape[1]),
        "n_selected_features": n_selected,
        "positive_rows": int(y.sum()),
        "positive_rate": float(y.mean()),
        "folds": int(args.folds),
        "class_weight": "balanced",
        "regularization_c": float(args.c),
        "l1_ratio": float(args.l1_ratio),
        "solver": "saga",
        "max_iter": int(args.max_iter),
        "decision_threshold": float(args.threshold),
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
