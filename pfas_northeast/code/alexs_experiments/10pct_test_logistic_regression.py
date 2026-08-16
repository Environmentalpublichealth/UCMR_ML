#!/usr/bin/env python3
"""Test the Northeast PFAS logistic model on a random 10% PWS holdout.

This is an internal holdout test. Public water systems, rather than individual
rows, are randomly assigned to the test set. All rows belonging to a selected
PWS stay together, preventing the same PWS from leaking into both training and
testing data.

The split is stratified using a PWS-level label: a PWS is positive when any of
its facility rows has pfas_detected == 1. The model is trained on the remaining
90% of PWSs, and preprocessing is learned only from those training rows.

Run from the repository root:
    python pfas_northeast/code/alexs_experiments/logistic_regression_10pct_test.py
"""

from __future__ import annotations

import argparse
import json
import logging
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split

import logistic_regression as base


HERE = Path(__file__).resolve().parent
DEFAULT_DATA_PATH = HERE / "NE_data_ready.csv"
DEFAULT_OUTPUT_DIR = HERE / "logistic_regression_10pct_output"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Train on 90% of Northeast PWSs and test on a random 10% holdout."
    )
    parser.add_argument(
        "--data",
        type=Path,
        default=DEFAULT_DATA_PATH,
        help="Northeast model-ready CSV (default: NE_data_ready.csv).",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=DEFAULT_OUTPUT_DIR,
        help="Directory for holdout results and the 90%-trained model.",
    )
    parser.add_argument(
        "--test-size",
        type=float,
        default=0.10,
        help="Fraction of PWSs assigned to the holdout set (default: 0.10).",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed used to select holdout PWSs (default: 42).",
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
        help="Inverse L2 regularization strength (default: 1.0).",
    )
    return parser.parse_args()


def configure_logging() -> logging.Logger:
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S",
    )
    return logging.getLogger("northeast_logistic_10pct")


def stratified_pws_split(
    groups: np.ndarray,
    labels: np.ndarray,
    test_size: float,
    seed: int,
) -> tuple[np.ndarray, np.ndarray, pd.DataFrame]:
    """Return non-overlapping train/test PWS IDs and the PWS label table."""
    pws_labels = (
        pd.DataFrame({base.GROUP_COLUMN: groups, "pws_label": labels})
        .groupby(base.GROUP_COLUMN, as_index=False)["pws_label"]
        .max()
        .sort_values(base.GROUP_COLUMN)
        .reset_index(drop=True)
    )

    train_pws, test_pws = train_test_split(
        pws_labels[base.GROUP_COLUMN].to_numpy(),
        test_size=test_size,
        random_state=seed,
        shuffle=True,
        stratify=pws_labels["pws_label"].to_numpy(),
    )

    overlap = set(train_pws) & set(test_pws)
    if overlap:
        raise RuntimeError(f"PWS leakage detected between train and test: {overlap}")
    return np.asarray(train_pws), np.asarray(test_pws), pws_labels


def metrics_for_model(
    model,
    X: pd.DataFrame,
    y: np.ndarray,
    threshold: float,
) -> tuple[np.ndarray, dict[str, float | int]]:
    probabilities = model.predict_proba(X)[:, 1]
    metrics = base.classification_metrics(y, probabilities, threshold)
    return probabilities, metrics


def main() -> None:
    args = parse_args()
    logger = configure_logging()

    if not 0 < args.test_size < 1:
        raise ValueError("--test-size must be between 0 and 1.")
    if not 0 < args.threshold < 1:
        raise ValueError("--threshold must be between 0 and 1.")
    if args.c <= 0:
        raise ValueError("--c must be greater than 0.")

    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)

    X, y, groups, row_ids, dropped_predictors = base.load_training_data(
        args.data.resolve(), logger
    )
    train_pws, test_pws, pws_labels = stratified_pws_split(
        groups=groups,
        labels=y,
        test_size=args.test_size,
        seed=args.seed,
    )

    train_mask = np.isin(groups, train_pws)
    test_mask = np.isin(groups, test_pws)
    if np.any(train_mask & test_mask):
        raise RuntimeError("At least one row was assigned to both train and test sets.")
    if not np.all(train_mask | test_mask):
        raise RuntimeError("At least one row was not assigned to either split.")

    X_train = X.loc[train_mask]
    X_test = X.loc[test_mask]
    y_train = y[train_mask]
    y_test = y[test_mask]

    logger.info(
        "Split: train=%d PWSs/%d rows; test=%d PWSs/%d rows (%.1f%% of rows).",
        len(train_pws),
        len(X_train),
        len(test_pws),
        len(X_test),
        100 * len(X_test) / len(X),
    )
    logger.info(
        "Positive rates: train rows=%.1f%%; test rows=%.1f%%.",
        100 * y_train.mean(),
        100 * y_test.mean(),
    )

    model = base.build_model(args.c)
    logger.info("Training logistic regression using only the 90%% training PWSs...")
    model.fit(X_train, y_train)

    _, train_metrics = metrics_for_model(
        model, X_train, y_train, args.threshold
    )
    test_probabilities, facility_test_metrics = metrics_for_model(
        model, X_test, y_test, args.threshold
    )

    facility_predictions = pd.DataFrame(
        {
            "source_row_index": row_ids[test_mask],
            base.GROUP_COLUMN: groups[test_mask],
            "y_true": y_test,
            "y_probability": test_probabilities,
            "y_predicted": (test_probabilities >= args.threshold).astype(int),
            "fold": 0,
        }
    )
    pws_predictions = base.aggregate_pws_predictions(
        facility_predictions, args.threshold
    )
    pws_test_metrics = base.classification_metrics(
        pws_predictions["y_true"].to_numpy(),
        pws_predictions["y_probability"].to_numpy(),
        args.threshold,
    )

    logger.info(
        "10%% holdout facility metrics: ROC-AUC=%.3f, PR-AUC=%.3f, "
        "recall=%.3f, precision=%.3f.",
        facility_test_metrics["roc_auc"],
        facility_test_metrics["pr_auc"],
        facility_test_metrics["recall"],
        facility_test_metrics["precision"],
    )
    logger.info(
        "10%% holdout PWS metrics: ROC-AUC=%.3f, PR-AUC=%.3f, "
        "recall=%.3f, precision=%.3f.",
        pws_test_metrics["roc_auc"],
        pws_test_metrics["pr_auc"],
        pws_test_metrics["recall"],
        pws_test_metrics["precision"],
    )

    selected_pws = pws_labels[pws_labels[base.GROUP_COLUMN].isin(test_pws)].copy()
    selected_pws["split"] = "test"
    selected_pws.to_csv(output_dir / "test_pwsids.csv", index=False)
    facility_predictions.to_csv(
        output_dir / "holdout_facility_predictions.csv", index=False
    )
    pws_predictions.to_csv(output_dir / "holdout_pws_predictions.csv", index=False)

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
    coefficients.sort_values("absolute_coefficient", ascending=False).to_csv(
        output_dir / "coefficients.csv", index=False
    )

    bundle = {
        "model": model,
        "model_type": "logistic_regression_90pct_train",
        "prepared_feature_columns": X.columns.tolist(),
        "target_column": base.TARGET_COLUMN,
        "group_column": base.GROUP_COLUMN,
        "dropped_predictors": dropped_predictors,
        "test_pwsids": sorted(test_pws.tolist()),
        "test_size_pws": args.test_size,
        "threshold": args.threshold,
        "random_seed": args.seed,
    }
    joblib.dump(
        bundle,
        output_dir / "logistic_regression_90pct_train_model.joblib",
        compress=3,
    )

    summary = {
        "purpose": "Random 10% PWS holdout test",
        "data_path": str(args.data.resolve()),
        "random_seed": args.seed,
        "requested_test_fraction_pws": args.test_size,
        "actual_test_fraction_rows": len(X_test) / len(X),
        "n_total_rows": int(len(X)),
        "n_train_rows": int(len(X_train)),
        "n_test_rows": int(len(X_test)),
        "n_total_pws": int(len(pws_labels)),
        "n_train_pws": int(len(train_pws)),
        "n_test_pws": int(len(test_pws)),
        "n_features": int(X.shape[1]),
        "train_row_positive_rate": float(y_train.mean()),
        "test_row_positive_rate": float(y_test.mean()),
        "train_metrics": train_metrics,
        "facility_holdout_metrics": facility_test_metrics,
        "pws_holdout_metrics": pws_test_metrics,
    }
    with (output_dir / "holdout_metrics.json").open("w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)

    logger.info("Saved 10%% holdout results to %s", output_dir)


if __name__ == "__main__":
    main()
