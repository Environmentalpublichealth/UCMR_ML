#!/usr/bin/env python3
"""
Train and persist the PFAS LR baseline as a proper model artifact.

This exact VIF-prune -> scale -> p-value-prune -> LogisticRegression pipeline
was previously only ever run inline inside plot_roc_curves.py to produce a
comparison ROC curve -- it was never saved to disk. This script reproduces
that identical pipeline (same LR_PRE_DROP list, same VIF threshold=10.0,
same p-value threshold=0.05, same C=1.0/class_weight='balanced'/random_state=42)
and saves the fitted model + scaler + final feature list as a single joblib
bundle, since (unlike RF/XGB, whose feature list is a static CSV) LR's live
feature set is only known after fitting VIF/p-value pruning on the training
data.

Usage:
  python3 save_lr_model.py
Output:
  models/lr_model_final.joblib  -- dict with keys:
    'model'    : fitted sklearn.linear_model.LogisticRegression
    'scaler'   : fitted sklearn.preprocessing.StandardScaler
    'features' : list[str], the final pruned feature names, IN ORDER
                 (apply scaler to X[features] before calling model.predict_proba)
"""
import importlib.util
import logging
from pathlib import Path

import joblib
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler

HERE = Path(__file__).resolve().parent
REPO_ROOT = HERE.parent.parent  # UCMR_ML/pfas
DATA_CSV = REPO_ROOT / 'data' / 'national_pfas_features_ready.csv'
NAT_ML_PY = HERE.parent / 'feature_pipeline' / '15_ml_pipeline_national.py'
OUT_PATH = REPO_ROOT / 'models' / 'lr_model_final.joblib'


def main():
    logging.basicConfig(level=logging.INFO, format='%(asctime)s [INFO] %(message)s', datefmt='%H:%M:%S')
    logger = logging.getLogger('save_lr')

    spec = importlib.util.spec_from_file_location("base_ml", str(NAT_ML_PY))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)

    X_raw, X_imp, y_all, groups_all = base_ml.load_prepared_features(
        DATA_CSV, 'pfas_detected', logger)
    labeled_mask = y_all.notna()
    y = y_all[labeled_mask].values.astype(int)
    X_imp = X_imp.loc[labeled_mask].reset_index(drop=True)

    logger.info("Fitting VIF + p-value pruned LR baseline (same pipeline as plot_roc_curves.py)...")
    pre_drop = [c for c in base_ml.LR_PRE_DROP if c in X_imp.columns]
    X_lr = X_imp.drop(columns=pre_drop)
    zero_var = [c for c in X_lr.columns if X_lr[c].std() == 0]
    if zero_var:
        X_lr = X_lr.drop(columns=zero_var)
    vif_cols = base_ml.vif_prune(X_lr, threshold=10.0, logger=logger)
    X_lr = X_lr[vif_cols]

    scaler = StandardScaler()
    X_scaled = pd.DataFrame(scaler.fit_transform(X_lr), columns=X_lr.columns, index=X_lr.index)
    final_cols = base_ml.pvalue_prune(X_scaled, y, threshold=0.05, logger=logger)
    if len(final_cols) == 0:
        final_cols = vif_cols
    X_final = X_scaled[final_cols].values

    logger.info(f"Training LogisticRegression on {len(final_cols)} final features "
                f"(n={len(y):,}, pos={y.sum():,})...")
    lr = LogisticRegression(class_weight='balanced', solver='lbfgs', max_iter=1000,
                             C=1.0, random_state=42)
    lr.fit(X_final, y)

    # scaler was fit on the VIF-pruned (62-col) set, BEFORE p-value pruning
    # selected the final 48 -- so scoring must scale vif_cols first, THEN
    # subselect final_cols from the scaled output (not scale final_cols alone).
    bundle = {'model': lr, 'scaler': scaler, 'scaler_features': vif_cols, 'features': final_cols}
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    joblib.dump(bundle, OUT_PATH, compress=3)
    logger.info(f"Saved LR model bundle ({len(final_cols)} features) to {OUT_PATH}")


if __name__ == '__main__':
    main()
