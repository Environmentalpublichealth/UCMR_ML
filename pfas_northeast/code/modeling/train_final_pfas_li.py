#!/usr/bin/env python3
"""
Train and save the FINAL PFAS/Lithium models using the feature sets
backward elimination selected (backward_elim_pfas_li.py), instead of RFE's
block-truncated selection. No hyperparameter search, no RFE — just fit the
model that was already validated during backward elimination and produce
the same style of outputs (5-fold CV metrics, facility + PWS confusion
matrices, PWS-level metrics, saved model) as the main pipelines
(ml_pipeline.py / national/15_ml_pipeline_national.py) produce for their
RFE-selected models, so results are directly comparable.

Requires backward_elim_{arm}_{target}_{model}_final_features.csv to exist
(produced by backward_elim_pfas_li.py --arm ... --target ... --model ...),
same directory.

Usage (one of the 8 combinations):
  python3 train_final_pfas_li.py --arm midwest8 --target pfas --model rf
"""
import argparse
import importlib.util
import logging
from pathlib import Path

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import shap
from sklearn.ensemble import RandomForestClassifier
from sklearn.inspection import permutation_importance
from sklearn.model_selection import StratifiedGroupKFold
import xgboost as xgb

HERE = Path(__file__).resolve().parent
RECALL_TARGET = {'pfas': 0.60, 'li': 0.80}

# Same fixed hyperparameters used in backward_elim_pfas_li.py, kept in sync
# deliberately — the final model should be exactly what backward
# elimination validated, not a re-tuned variant.
HYPERPARAMS = {
    ('midwest8', 'li', 'rf'): dict(
        class_weight='balanced', max_depth=None, max_features=0.3,
        min_samples_leaf=2, n_estimators=152, random_state=42, n_jobs=-1),
    ('midwest8', 'li', 'xgb'): dict(
        colsample_bytree=0.9, gamma=0.1, learning_rate=0.05, max_depth=5,
        min_child_weight=9, n_estimators=285, reg_alpha=0.1, reg_lambda=1.0,
        scale_pos_weight=2.6482364156339373, subsample=0.7,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
    ('midwest8', 'pfas', 'rf'): dict(
        class_weight='balanced', max_depth=15, max_features=0.3,
        min_samples_leaf=6, n_estimators=357, random_state=42, n_jobs=-1),
    ('midwest8', 'pfas', 'xgb'): dict(
        colsample_bytree=0.9, gamma=0.1, learning_rate=0.05, max_depth=5,
        min_child_weight=9, n_estimators=285, reg_alpha=0.1, reg_lambda=1.0,
        scale_pos_weight=3.661388550548112, subsample=0.7,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
    ('national', 'li', 'rf'): dict(
        class_weight='balanced', max_depth=None, max_features=0.3,
        min_samples_leaf=2, n_estimators=188, random_state=42, n_jobs=-1),
    ('national', 'li', 'xgb'): dict(
        colsample_bytree=0.6, gamma=0, learning_rate=0.05, max_depth=7,
        min_child_weight=3, n_estimators=287, reg_alpha=0, reg_lambda=2.0,
        scale_pos_weight=1.912605435801312, subsample=0.6,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
    ('national', 'pfas', 'rf'): dict(
        class_weight='balanced', max_depth=None, max_features=0.3,
        min_samples_leaf=2, n_estimators=188, random_state=42, n_jobs=-1),
    ('national', 'pfas', 'xgb'): dict(
        colsample_bytree=0.6, gamma=0, learning_rate=0.05, max_depth=7,
        min_child_weight=3, n_estimators=287, reg_alpha=0, reg_lambda=2.0,
        scale_pos_weight=3.2566477955052324, subsample=0.6,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
    # Northeast regional arm: reuses the national PFAS hyperparameters as a
    # documented starting default (no region-specific search has been run yet) --
    # re-tune via a fresh randomized search if you want region-optimized values.
    ('northeast', 'pfas', 'rf'): dict(
        class_weight='balanced', max_depth=None, max_features=0.3,
        min_samples_leaf=2, n_estimators=188, random_state=42, n_jobs=-1),
    ('northeast', 'pfas', 'xgb'): dict(
        colsample_bytree=0.6, gamma=0, learning_rate=0.05, max_depth=7,
        min_child_weight=3, n_estimators=287, reg_alpha=0, reg_lambda=2.0,
        scale_pos_weight=3.2566477955052324, subsample=0.6,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
}


def load_midwest8(target, logger):
    spec = importlib.util.spec_from_file_location("base_ml", str(HERE / 'ml_pipeline.py'))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)

    X_raw, X_imp, y_pfas, y_li, groups, col_medians = base_ml.build_features(base_ml.DATA_PATH, logger)
    y_all = y_pfas if target == 'pfas' else y_li
    extra_drop = base_ml.PFAS_EXTRA_DROP if target == 'pfas' else base_ml.LI_EXTRA_DROP
    drop = [c for c in extra_drop if c in X_raw.columns]
    if drop:
        X_raw = X_raw.drop(columns=drop)
        X_imp = X_imp.drop(columns=drop)
    return base_ml, X_raw, X_imp, y_all, groups


def load_national(target, logger):
    spec = importlib.util.spec_from_file_location("base_ml", str(HERE / 'national' / '15_ml_pipeline_national.py'))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)

    csv_path = base_ml.PFAS_FEATURES_CSV if target == 'pfas' else base_ml.LI_FEATURES_CSV
    label_col = 'pfas_detected' if target == 'pfas' else 'li_detected'
    X_raw, X_imp, y_all, groups = base_ml.load_prepared_features(csv_path, label_col, logger)
    return base_ml, X_raw, X_imp, y_all, groups


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--arm', choices=['midwest8', 'national', 'northeast'], required=True)
    parser.add_argument('--target', choices=['pfas', 'li'], required=True)
    parser.add_argument('--model', choices=['rf', 'xgb'], required=True)
    parser.add_argument('--output-dir', type=str, default=None)
    args = parser.parse_args()

    tag = f"{args.arm}_{args.target}_{args.model}"
    logging.basicConfig(level=logging.INFO, format='%(asctime)s [INFO] %(message)s', datefmt='%H:%M:%S')
    logger = logging.getLogger(f'train_final_{tag}')

    output_dir = Path(args.output_dir) if args.output_dir else HERE / f'ml_output_final_{args.arm}' / args.target
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / 'models').mkdir(exist_ok=True)
    (output_dir / 'plots').mkdir(exist_ok=True)

    loader = load_midwest8 if args.arm == 'midwest8' else load_national
    base_ml, X_raw, X_imp, y_all, groups = loader(args.target, logger)

    labeled_mask = y_all.notna()
    y = y_all[labeled_mask].values.astype(int)
    groups = groups[labeled_mask].values
    X_imp = X_imp.loc[labeled_mask].reset_index(drop=True)
    X_raw = X_raw.loc[labeled_mask].reset_index(drop=True)

    feat_path = HERE / f'backward_elim_{tag}_final_features.csv'
    if not feat_path.exists():
        raise FileNotFoundError(
            f"{feat_path} not found — run backward_elim_pfas_li.py --arm {args.arm} "
            f"--target {args.target} --model {args.model} first, then rerun this script.")
    final_features = pd.read_csv(feat_path)['feature'].tolist()
    logger.info(f"[{tag}] Using {len(final_features)} backward-elimination-selected features: {final_features}")

    recall_target = RECALL_TARGET[args.target]
    params = HYPERPARAMS[(args.arm, args.target, args.model)]

    if args.model == 'rf':
        est = RandomForestClassifier(**params)
        X_df = X_imp
    else:
        est = xgb.XGBClassifier(**params)
        X_df = X_raw

    X_arr = X_df[final_features].values.astype(float)

    logger.info(f"[{tag}] Running 5-fold GroupKFold CV...")
    cv_df, oof_y_true, oof_y_pred, oof_y_prob, oof_groups = base_ml.group_cv(
        est, X_arr, y, groups, n_splits=5, recall_target=recall_target, return_preds=True)
    summary = base_ml.summarize_cv(cv_df)
    logger.info(f"[{tag}] CV: ROC-AUC={summary['roc_auc_mean']:.4f}  PR-AUC={summary['pr_auc_mean']:.4f}  "
                f"Precision={summary['precision_mean']:.4f}  Recall={summary['recall_mean']:.4f}  "
                f"F1={summary['f1_opt_mean']:.4f}")
    cv_df.to_csv(output_dir / f'{args.model}_cv_results.csv', index=False)

    base_ml.plot_cm(oof_y_true, oof_y_pred, output_dir, args.target, args.model, logger)
    pws_metrics = base_ml.pws_agg_metrics(oof_y_true, oof_y_pred, oof_y_prob, oof_groups,
                                           recall_target, output_dir, args.target, args.model, logger)

    est.fit(X_arr, y)
    joblib.dump({'model': est, 'features': final_features},
                output_dir / 'models' / f'{args.model}_model_final.joblib', compress=3)

    # ── Feature importance: permutation importance (RF and XGB) + SHAP (XGB only) ──
    logger.info(f"[{tag}] Computing permutation importance (n_repeats=20)...")
    sgkf = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=42)
    _, te_idx = list(sgkf.split(X_arr, y, groups))[-1]
    perm = permutation_importance(est, X_arr[te_idx], y[te_idx],
                                   n_repeats=20, scoring='roc_auc',
                                   random_state=42, n_jobs=-1)
    imp_df = pd.DataFrame({
        'feature': final_features,
        'importance_mean': perm.importances_mean,
        'importance_std': perm.importances_std,
    }).sort_values('importance_mean', ascending=False).reset_index(drop=True)
    imp_df.to_csv(output_dir / f'{args.model}_permutation_importance_final.csv', index=False)

    perm_color = '#2ca02c' if args.model == 'rf' else '#9467bd'  # green (RF) / purple (XGB) — matches main pipeline
    top = imp_df.head(10).sort_values('importance_mean')
    fig, ax = plt.subplots(figsize=(9, 6))
    ax.barh(top['feature'], top['importance_mean'], xerr=top['importance_std'],
            color=perm_color, alpha=0.8, capsize=4, error_kw={'linewidth': 1.5})
    ax.axvline(0, color='red', linestyle='--', linewidth=0.8, label='No effect')
    ax.set_xlabel('Mean AUC drop when feature shuffled')
    ax.set_title(f'{args.model.upper()} Feature Importance — Permutation (Final Model) — {tag}')
    ax.legend()
    plt.tight_layout()
    fig.savefig(output_dir / 'plots' / f'{args.model}_permutation_importance_final.png', dpi=150, bbox_inches='tight')
    plt.close(fig)
    logger.info(f"[{tag}] Top 5 features (permutation): {imp_df.head(5)['feature'].tolist()}")

    if args.model == 'xgb':
        logger.info(f"[{tag}] Computing SHAP values...")
        explainer = shap.TreeExplainer(est)
        shap_values = explainer.shap_values(X_arr)
        mean_abs_shap = np.abs(shap_values).mean(axis=0)
        shap_df = pd.DataFrame({
            'feature': final_features,
            'mean_abs_shap': mean_abs_shap,
        }).sort_values('mean_abs_shap', ascending=False).reset_index(drop=True)
        shap_df.to_csv(output_dir / f'{args.model}_shap_importance_final.csv', index=False)

        top = shap_df.head(10).sort_values('mean_abs_shap')
        fig, ax = plt.subplots(figsize=(9, 6))
        ax.barh(top['feature'], top['mean_abs_shap'], color='#ff7f0e', alpha=0.85)
        ax.set_xlabel('Mean |SHAP value| (average impact on prediction)')
        ax.set_title(f'XGBoost Feature Importance — SHAP (Final Model) — {tag}')
        plt.tight_layout()
        fig.savefig(output_dir / 'plots' / f'{args.model}_shap_bar_final.png', dpi=150, bbox_inches='tight')
        plt.close(fig)

        top10_feats = shap_df.head(min(10, len(final_features)))['feature'].tolist()
        top10_idx = [final_features.index(f) for f in top10_feats]
        fig, ax = plt.subplots(figsize=(10, 7))
        shap.summary_plot(shap_values[:, top10_idx], X_arr[:, top10_idx],
                           feature_names=top10_feats, plot_type='dot', show=False, max_display=10)
        plt.tight_layout()
        fig = plt.gcf()
        fig.savefig(output_dir / 'plots' / f'{args.model}_shap_beeswarm_final.png', dpi=150, bbox_inches='tight')
        plt.close(fig)
        logger.info(f"[{tag}] Top 5 features (SHAP): {shap_df.head(5)['feature'].tolist()}")

    with open(output_dir / f'{args.model}_final_summary.txt', 'w') as fh:
        fh.write(f"arm={args.arm}\ntarget={args.target}\nmodel={args.model}\n"
                 f"n_features={len(final_features)}\nfeatures={final_features}\n\n"
                 f"5-fold CV (facility level):\n")
        for k, v in summary.items():
            fh.write(f"  {k}={v}\n")
        fh.write(f"\nPWS-level:\n")
        for k, v in pws_metrics.items():
            fh.write(f"  {k}={v}\n")

    logger.info(f"[{tag}] [DONE] Outputs in {output_dir}")


if __name__ == '__main__':
    main()
