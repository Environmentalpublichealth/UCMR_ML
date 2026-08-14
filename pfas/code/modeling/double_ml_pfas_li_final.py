#!/usr/bin/env python3
"""
DML interpretation sweep on the FINAL PFAS/Lithium (national arm) feature
sets — the ones backward elimination + train_final_pfas_li.py actually
produced and deployed (read from the saved models' 'features' list, not
re-derived).

Per this project's established methodology: DML is NOT used here to decide
what's in the model (that was backward elimination's job, already done on
HPC) — it's used only afterward, to characterize which of the surviving
features have an independent, cleanly-separable effect on the target vs.
which are useful-but-entangled with correlated features. Same cross-fitted
method as chromium6/double_ml_cr6_final.py and the original
double_ml_cr6_exceedance.py.

Candidate pool = union of RF's and XGB's final feature sets for that
target, so each feature is controlled for the union.

Usage:
  python3 double_ml_pfas_li_final.py --target pfas
  python3 double_ml_pfas_li_final.py --target li
"""
import argparse
import importlib.util
import logging
import warnings
from pathlib import Path

import joblib
import numpy as np
import pandas as pd
import statsmodels.api as sm
from scipy.stats import pointbiserialr, pearsonr
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.model_selection import GroupKFold
from statsmodels.stats.multitest import multipletests

warnings.filterwarnings('ignore')

HERE = Path(__file__).resolve().parent
N_FOLDS = 5
RANDOM_STATE = 42


def is_binary(s):
    vals = set(pd.unique(s.dropna()))
    return vals <= {0.0, 1.0, 0, 1}


def cross_fitted_residual(X_z, target, groups, binary, n_folds, seed):
    gkf = GroupKFold(n_splits=n_folds)
    oof_pred = np.full(len(target), np.nan)
    for tr_idx, te_idx in gkf.split(X_z, target, groups):
        if binary:
            model = RandomForestClassifier(n_estimators=300, max_depth=8,
                                            min_samples_leaf=5, n_jobs=-1, random_state=seed)
            model.fit(X_z[tr_idx], target[tr_idx])
            oof_pred[te_idx] = model.predict_proba(X_z[te_idx])[:, 1]
        else:
            model = RandomForestRegressor(n_estimators=300, max_depth=8,
                                           min_samples_leaf=5, n_jobs=-1, random_state=seed)
            model.fit(X_z[tr_idx], target[tr_idx])
            oof_pred[te_idx] = model.predict(X_z[te_idx])
    return target - oof_pred


def run_dml(X_i, y, groups, candidate_pool, test_features, logger, tag):
    rows = []
    for j, feat in enumerate(test_features, 1):
        Z_cols = [c for c in candidate_pool if c != feat]
        X_z = X_i[Z_cols].values.astype(float)
        x_j = X_i[feat].values.astype(float)
        feat_binary = is_binary(X_i[feat])

        x_res = cross_fitted_residual(X_z, x_j, groups, feat_binary, N_FOLDS, RANDOM_STATE)
        y_res = cross_fitted_residual(X_z, y.astype(float), groups, True, N_FOLDS, RANDOM_STATE)

        X_design = sm.add_constant(x_res)
        ols = sm.OLS(y_res, X_design).fit(cov_type='HC3')
        coef, se, pval = ols.params[1], ols.bse[1], ols.pvalues[1]

        if feat_binary:
            naive_r, naive_p = pointbiserialr(x_j, y)
        else:
            naive_r, naive_p = pearsonr(x_j, y)

        rows.append({
            'Target': tag, 'Feature': feat,
            'Naive corr with y': round(naive_r, 4), 'Naive p-value': naive_p,
            'DML coef (adj. for other features)': round(coef, 5),
            'DML SE': round(se, 5), 'DML p-value': pval,
        })
        logger.info(f"  {j}/{len(test_features)} {feat}: "
                    f"naive_r={naive_r:.3f}  DML coef={coef:.4f}  p={pval:.4f}")
    return pd.DataFrame(rows)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--target', choices=['pfas', 'li'], required=True)
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format='%(asctime)s [INFO] %(message)s', datefmt='%H:%M:%S')
    logger = logging.getLogger(f'dml_{args.target}_final')

    spec = importlib.util.spec_from_file_location("base_ml", str(HERE / 'national' / '15_ml_pipeline_national.py'))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)

    csv_path = base_ml.PFAS_FEATURES_CSV if args.target == 'pfas' else base_ml.LI_FEATURES_CSV
    label_col = 'pfas_detected' if args.target == 'pfas' else 'li_detected'
    X_raw, X_imp, y_all, groups_all = base_ml.load_prepared_features(csv_path, label_col, logger)

    model_dir = HERE / 'ml_output_final_national' / args.target / 'models'
    rf_feats = set(joblib.load(model_dir / 'rf_model_final.joblib')['features'])
    xgb_feats = set(joblib.load(model_dir / 'xgb_model_final.joblib')['features'])

    labeled_mask = y_all.notna()
    y = y_all[labeled_mask].values.astype(int)
    groups = groups_all[labeled_mask].values
    X_i_full = X_imp.loc[labeled_mask].reset_index(drop=True)

    union_feats = sorted(rf_feats | xgb_feats)
    zero_var = [c for c in union_feats if X_i_full[c].std() == 0]
    if zero_var:
        logger.info(f"  Dropping {len(zero_var)} zero-variance columns (constant in labeled "
                    f"subset, undefined for DML): {zero_var}")
    candidate_pool = [c for c in union_feats if c not in zero_var]
    logger.info(f"Final feature union: RF={len(rf_feats)}, XGB={len(xgb_feats)}, "
                f"union={len(union_feats)}, after zero-var drop={len(candidate_pool)}")

    X_i = X_i_full[candidate_pool]
    logger.info(f"n={len(y):,}")

    # Only DML-test the top-10-by-importance features per model (union of
    # RF permutation importance and XGB SHAP, each already computed by
    # train_final_pfas_li.py) — NOT every feature in the final set. Controls
    # (Z) still use the full candidate_pool either way, so each tested
    # feature is still adjusted for the complete backward-elimination
    # feature set; testing every feature exhaustively was not the point.
    rf_imp = pd.read_csv(model_dir.parent / 'rf_permutation_importance_final.csv')
    xgb_imp = pd.read_csv(model_dir.parent / 'xgb_shap_importance_final.csv')
    top10_union = sorted(set(rf_imp.head(10)['feature']) | set(xgb_imp.head(10)['feature']))
    top10_union = [f for f in top10_union if f in candidate_pool]  # drop if it was a zero-var casualty
    logger.info(f"Testing top-10-by-importance union: {len(top10_union)} features: {top10_union}")

    tag = f'{args.target}_detected_FINAL'
    out_df = run_dml(X_i, y, groups, candidate_pool, top10_union, logger, tag)
    reject, qvals, _, _ = multipletests(out_df['DML p-value'].values, alpha=0.05, method='fdr_bh')
    out_df['DML q-value (FDR-BH)'] = qvals
    out_df['DML significant (FDR q<0.05)'] = np.where(reject, 'Yes', 'No')
    out_df['Interpretation'] = np.where(
        out_df['DML significant (FDR q<0.05)'] == 'Yes',
        'Robust: still significant after controlling for all other features (FDR-corrected)',
        'Likely confounded/redundant, or too weak to distinguish from noise after multiple-testing correction',
    )
    out_df = out_df.sort_values('DML p-value').reset_index(drop=True)

    out_path = HERE / f'double_ml_{args.target}_final_results.csv'
    out_df.to_csv(out_path, index=False)
    logger.info(f"[DONE] Saved {out_path}")
    print(out_df.to_string(index=False))


if __name__ == '__main__':
    main()
