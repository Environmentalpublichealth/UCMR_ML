#!/usr/bin/env python3
"""
ROC curve plots (facility-level and PWS-level) for the FINAL national PFAS
and Li models. No ROC curve was saved by train_final_pfas_li.py — only the
scalar roc_auc metric. This reruns the exact same 5-fold GroupKFold CV
(same hyperparameters, same backward-elimination feature set, same
random_state) to regenerate the out-of-fold probabilities needed to draw
the curve, then aggregates to PWS-level using the same "max facility prob
per PWSID" rule as pws_agg_metrics() in the main pipeline.

Usage:
  python3 plot_roc_curves.py --target pfas
  python3 plot_roc_curves.py --target li
"""
import argparse
import importlib.util
import logging
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score, roc_curve
from sklearn.preprocessing import StandardScaler

HERE = Path(__file__).resolve().parent
COLOR = {'lr': '#7f7f7f', 'rf': '#2ca02c', 'xgb': '#9467bd'}
LABEL = {'lr': 'LR (baseline)', 'rf': 'RF', 'xgb': 'XGB'}

# Same fixed hyperparameters as train_final_pfas_li.py (national arm only),
# kept in sync deliberately.
HYPERPARAMS = {
    ('li', 'rf'): dict(
        class_weight='balanced', max_depth=None, max_features=0.3,
        min_samples_leaf=2, n_estimators=188, random_state=42, n_jobs=-1),
    ('li', 'xgb'): dict(
        colsample_bytree=0.6, gamma=0, learning_rate=0.05, max_depth=7,
        min_child_weight=3, n_estimators=287, reg_alpha=0, reg_lambda=2.0,
        scale_pos_weight=1.912605435801312, subsample=0.6,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
    ('pfas', 'rf'): dict(
        class_weight='balanced', max_depth=None, max_features=0.3,
        min_samples_leaf=2, n_estimators=188, random_state=42, n_jobs=-1),
    ('pfas', 'xgb'): dict(
        colsample_bytree=0.6, gamma=0, learning_rate=0.05, max_depth=7,
        min_child_weight=3, n_estimators=287, reg_alpha=0, reg_lambda=2.0,
        scale_pos_weight=3.2566477955052324, subsample=0.6,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
}


def pws_aggregate(y_true, y_prob, groups):
    df = pd.DataFrame({'pwsid': groups, 'y_true': y_true, 'y_prob': y_prob})
    pws = df.groupby('pwsid').agg(pws_true=('y_true', 'max'), pws_prob=('y_prob', 'max'))
    return pws['pws_true'].values.astype(int), pws['pws_prob'].values


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--target', choices=['pfas', 'li'], required=True)
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format='%(asctime)s [INFO] %(message)s', datefmt='%H:%M:%S')
    logger = logging.getLogger(f'roc_{args.target}')

    spec = importlib.util.spec_from_file_location("base_ml", str(HERE / 'national' / '15_ml_pipeline_national.py'))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)

    csv_path = base_ml.PFAS_FEATURES_CSV if args.target == 'pfas' else base_ml.LI_FEATURES_CSV
    label_col = 'pfas_detected' if args.target == 'pfas' else 'li_detected'
    X_raw, X_imp, y_all, groups_all = base_ml.load_prepared_features(csv_path, label_col, logger)

    labeled_mask = y_all.notna()
    y = y_all[labeled_mask].values.astype(int)
    groups = groups_all[labeled_mask].values
    X_imp = X_imp.loc[labeled_mask].reset_index(drop=True)
    X_raw = X_raw.loc[labeled_mask].reset_index(drop=True)

    recall_target = {'pfas': 0.60, 'li': 0.80}[args.target]

    fig_fac, ax_fac = plt.subplots(figsize=(7, 7))
    fig_pws, ax_pws = plt.subplots(figsize=(7, 7))

    def plot_model(model, oof_y_true, oof_y_prob, oof_groups):
        fac_auc = roc_auc_score(oof_y_true, oof_y_prob)
        fpr, tpr, _ = roc_curve(oof_y_true, oof_y_prob)
        ax_fac.plot(fpr, tpr, color=COLOR[model], linewidth=2,
                    label=f'{LABEL[model]} (AUC={fac_auc:.3f})')
        logger.info(f"[{model}] facility-level ROC-AUC={fac_auc:.4f}")

        pws_true, pws_prob = pws_aggregate(oof_y_true, oof_y_prob, oof_groups)
        pws_auc = roc_auc_score(pws_true, pws_prob)
        fpr_p, tpr_p, _ = roc_curve(pws_true, pws_prob)
        ax_pws.plot(fpr_p, tpr_p, color=COLOR[model], linewidth=2,
                    label=f'{LABEL[model]} (AUC={pws_auc:.3f})')
        logger.info(f"[{model}] PWS-level ROC-AUC={pws_auc:.4f}")

    # ── LR baseline: same VIF-prune -> scale -> p-value-prune pipeline as
    # base_ml.run_lr(), reimplemented here (rather than calling run_lr
    # directly) only to get back the raw oof arrays for PWS aggregation,
    # which run_lr's return dict doesn't expose. Same hyperparameters
    # (C=1.0, class_weight='balanced'), an old/untuned baseline on purpose.
    logger.info("[lr] Fitting VIF + p-value pruned baseline...")
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
    logger.info(f"[lr] Running 5-fold GroupKFold CV ({len(final_cols)} features after pruning)...")
    lr_clf = LogisticRegression(class_weight='balanced', solver='lbfgs', max_iter=1000, C=1.0, random_state=42)
    _, oof_y_true, _, oof_y_prob, oof_groups = base_ml.group_cv(
        lr_clf, X_final, y, groups, n_splits=5, recall_target=recall_target, return_preds=True)
    plot_model('lr', oof_y_true, oof_y_prob, oof_groups)

    for model in ['rf', 'xgb']:
        feat_path = HERE / f'backward_elim_national_{args.target}_{model}_final_features.csv'
        final_features = pd.read_csv(feat_path)['feature'].tolist()
        params = HYPERPARAMS[(args.target, model)]

        if model == 'rf':
            est = RandomForestClassifier(**params)
            X_df = X_imp
        else:
            est = xgb.XGBClassifier(**params)
            X_df = X_raw
        X_arr = X_df[final_features].values.astype(float)

        logger.info(f"[{model}] Running 5-fold GroupKFold CV ({len(final_features)} features)...")
        cv_df, oof_y_true, oof_y_pred, oof_y_prob, oof_groups = base_ml.group_cv(
            est, X_arr, y, groups, n_splits=5, recall_target=recall_target, return_preds=True)
        plot_model(model, oof_y_true, oof_y_prob, oof_groups)

    for ax, level, n in [(ax_fac, 'Facility', len(y)), (ax_pws, 'PWS', None)]:
        ax.plot([0, 1], [0, 1], color='gray', linestyle='--', linewidth=1, label='Random (AUC=0.500)')
        ax.set_xlim(-0.01, 1.01)
        ax.set_ylim(-0.01, 1.01)
        ax.set_xlabel('False Positive Rate', fontsize=15)
        ax.set_ylabel('True Positive Rate', fontsize=15)
        title = f'ROC Curve — {args.target.upper()} ({level}-level)'
        ax.set_title(title, fontsize=17)
        ax.tick_params(axis='both', labelsize=13)
        ax.legend(loc='lower right', fontsize=13)
        ax.set_aspect('equal')

    out_dir = HERE / 'ml_output_final_national' / args.target / 'plots'
    fig_fac.tight_layout()
    fig_fac.savefig(out_dir / 'roc_curve_facility.png', dpi=150, bbox_inches='tight')
    plt.close(fig_fac)
    fig_pws.tight_layout()
    fig_pws.savefig(out_dir / 'roc_curve_pws.png', dpi=150, bbox_inches='tight')
    plt.close(fig_pws)

    logger.info(f"[DONE] Saved roc_curve_facility.png and roc_curve_pws.png in {out_dir}")


if __name__ == '__main__':
    main()
