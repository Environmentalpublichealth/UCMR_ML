#!/usr/bin/env python3
"""
Load the three saved PFAS models (LR, RF, XGB) and score an external
validation feature table (e.g. a state PFAS monitoring dataset, built with
the same feature-engineering pipeline as national_pfas_features_ready.csv --
see ../feature_pipeline/ and the validation-set build scripts referenced in
the top-level README).

Column alignment against the training schema:
  - One-hot dummy columns (PWS Type_*, Primary Source_*, FWT_*, ROCK_*,
    disinfect_*, treat_*) missing from the validation set (a category that
    simply never occurs there) are filled with 0, not imputed -- a missing
    dummy means "not in that category," not "unknown."
  - All other missing columns are median-imputed using the TRAINING set's
    own column medians (not the validation set's) to avoid leaking
    validation-specific distribution info into the fill values.

Usage:
  python3 score_validation.py --features /path/to/validation_features.csv \\
      --labels /path/to/validation_labels.csv --label-col label --id-col PWSID
  (--labels/--label-col/--id-col optional -- omit to just get predictions,
   no metrics, e.g. for scoring PWS with unknown true status)
"""
import argparse
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
from sklearn.metrics import (average_precision_score, confusion_matrix,
                              roc_auc_score)

HERE = Path(__file__).resolve().parent
REPO_ROOT = HERE.parent.parent  # UCMR_ML/pfas
TRAIN_CSV = REPO_ROOT / 'data' / 'national_pfas_features_ready.csv'
MODELS_DIR = REPO_ROOT / 'models'
NAT_ML_PY = HERE.parent / 'feature_pipeline' / '15_ml_pipeline_national.py'


def _clr_transform(df):
    import importlib.util
    spec = importlib.util.spec_from_file_location("base_ml", str(NAT_ML_PY))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)
    return base_ml.clr_transform_land_use(df.copy())


def load_training_reference():
    """Training column schema + per-column medians + dummy-column detection.
    Applies the same on-the-fly land-use CLR transform load_prepared_features()
    applies -- the raw ready CSV stores _pct columns, not _clr."""
    df = pd.read_csv(TRAIN_CSV, low_memory=False)
    df = _clr_transform(df)
    feature_cols = [c for c in df.columns if c not in ('pfas_detected', 'PWSID')]
    X = df[feature_cols]
    medians = X.median()
    is_dummy = X.apply(lambda s: set(s.dropna().unique()) <= {0, 1, 0.0, 1.0})
    return feature_cols, medians, is_dummy[is_dummy].index.tolist()


def align_columns(val_df, feature_cols, medians, dummy_cols):
    out = val_df.copy()
    missing = [c for c in feature_cols if c not in out.columns]
    for c in missing:
        out[c] = 0.0 if c in dummy_cols else np.nan
    for c in feature_cols:
        if c in dummy_cols:
            out[c] = out[c].fillna(0.0)
        else:
            out[c] = pd.to_numeric(out[c], errors='coerce').fillna(medians[c])
    return out[feature_cols]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--features', required=True, help='Validation feature CSV '
                     '(same schema as national_pfas_features_ready.csv)')
    ap.add_argument('--labels', default=None, help='Optional CSV with true labels for metrics')
    ap.add_argument('--label-col', default='label')
    ap.add_argument('--id-col', default='PWSID')
    ap.add_argument('--out', default=None, help='Output CSV path for predictions (default: alongside --features)')
    args = ap.parse_args()

    print("Loading training reference schema (columns, medians, dummy detection)...")
    feature_cols, medians, dummy_cols = load_training_reference()
    print(f"  {len(feature_cols)} training feature columns, {len(dummy_cols)} detected as one-hot dummies")

    val_df = pd.read_csv(args.features, low_memory=False)
    val_df = _clr_transform(val_df)
    ids = val_df[args.id_col].astype(str) if args.id_col in val_df.columns else pd.Series(range(len(val_df)))
    X_imp = align_columns(val_df, feature_cols, medians, dummy_cols)
    X_raw = val_df.reindex(columns=feature_cols)  # NaN preserved, for XGB

    print("Loading models...")
    rf_bundle = joblib.load(MODELS_DIR / 'rf_model_final.joblib')
    xgb_bundle = joblib.load(MODELS_DIR / 'xgb_model_final.joblib')
    lr_bundle = joblib.load(MODELS_DIR / 'lr_model_final.joblib')
    rf_model, rf_features = rf_bundle['model'], rf_bundle['features']
    xgb_model, xgb_features = xgb_bundle['model'], xgb_bundle['features']
    lr_model, lr_scaler = lr_bundle['model'], lr_bundle['scaler']
    lr_scaler_features, lr_features = lr_bundle['scaler_features'], lr_bundle['features']

    print("Scoring...")
    rf_prob = rf_model.predict_proba(X_imp[rf_features].values)[:, 1]
    xgb_prob = xgb_model.predict_proba(X_raw[xgb_features].values)[:, 1]
    # scaler was fit on the full VIF-pruned set -- scale that first, then
    # subselect the final p-value-pruned columns from the scaled output
    lr_scaled_full = pd.DataFrame(lr_scaler.transform(X_imp[lr_scaler_features].values),
                                   columns=lr_scaler_features, index=X_imp.index)
    lr_prob = lr_model.predict_proba(lr_scaled_full[lr_features].values)[:, 1]

    out = pd.DataFrame({args.id_col: ids, 'lr_prob': lr_prob, 'rf_prob': rf_prob, 'xgb_prob': xgb_prob})
    out_path = args.out or str(Path(args.features).with_name(Path(args.features).stem + '_predictions.csv'))
    out.to_csv(out_path, index=False)
    print(f"Saved predictions: {out_path}")

    if args.labels:
        lab_df = pd.read_csv(args.labels, low_memory=False)
        lab_df[args.id_col] = lab_df[args.id_col].astype(str)
        merged = out.merge(lab_df[[args.id_col, args.label_col]], on=args.id_col, how='inner')
        merged = merged.dropna(subset=[args.label_col])
        y_true = merged[args.label_col].astype(int).values
        print(f"\n=== Metrics (n={len(merged):,}, positive rate={y_true.mean()*100:.1f}%) ===")
        for model, col in [('LR', 'lr_prob'), ('RF', 'rf_prob'), ('XGB', 'xgb_prob')]:
            y_prob = merged[col].values
            auc = roc_auc_score(y_true, y_prob)
            pr_auc = average_precision_score(y_true, y_prob)
            y_pred = (y_prob >= 0.5).astype(int)
            tn, fp, fn, tp = confusion_matrix(y_true, y_pred).ravel()
            sens = tp / (tp + fn) if (tp + fn) else float('nan')
            spec = tn / (tn + fp) if (tn + fp) else float('nan')
            print(f"  {model}: ROC-AUC={auc:.3f}  PR-AUC={pr_auc:.3f}  "
                  f"sensitivity={sens:.3f}  specificity={spec:.3f}  "
                  f"(TP={tp} FP={fp} FN={fn} TN={tn})")


if __name__ == '__main__':
    main()
