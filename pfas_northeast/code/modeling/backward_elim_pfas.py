#!/usr/bin/env python3
"""
True one-at-a-time backward elimination for the PFAS detection model,
northeast regional arm, mirroring chromium6/backward_elim_cr6_exceedance.py
— same methodology, generalized to cover multiple (arm x model)
combinations via CLI args instead of one script per combination.

Why backward elimination instead of RFE (the method currently used to
select each model's final feature set in ml_pipeline.py / national's
15_ml_pipeline_national.py): RFE ranks all features by importance from a
single full-feature fit, then keeps the smallest BLOCK of top-K features
within a tolerance of peak CV score. Two problems: (1) it evaluates
importance from ONE fit and cuts features in blocks, not one at a time, so
correlated features that split importance credit can get bulk-discarded
even when genuinely useful; (2) its tolerance rule explicitly favors fewer
features, trading away real (if small) performance gains for parsimony.
Backward elimination instead re-evaluates the true marginal cost of each
remaining feature at every step, directly answering "does the model need
this," not "can I get away with fewer."

Algorithm (identical structure to the chromium6 version):
  current = full candidate feature pool for this (arm, target), after a
            Spearman dedup pre-filter (see v2 note below)
  loop:
    for each feature f in current: score(current - {f}) via 5-fold CV (PR-AUC)
    best_f = feature whose removal gives the HIGHEST score (least harmful)
    if best_score < peak_score_so_far - tolerance: STOP
    else: remove best_f, continue
  hard floor as a safety net

v2 fixes two real bugs found by inspecting the v1 HPC results (final sets
were only trimmed by a handful of features out of 65-71 — i.e. it barely
removed anything). Same root causes as chromium6/backward_elim_cr6_exceedance.py
v2 (see that file's docstring for the full explanation):

1. REDUNDANT FEATURES: v1 had no correlation pre-filter, so structurally
   near-collinear pairs (one-hot dummy groups encoded with drop_first=False
   — e.g. PWS Type columns at Spearman r=-1.0) could both survive. Fixed
   by running a Spearman dedup pass (|r|>0.6, same function/threshold the
   RFE pipeline already uses) before elimination starts.

2. NOISY STOPPING: v1's stop condition is all-or-nothing per round — one
   noisy 3-fold PR-AUC estimate exceeding TOLERANCE (0.003) halts the
   ENTIRE remaining process, not just that feature. 3-fold search-phase
   noise turned out to be larger than the 0.003 tolerance. Fixed by using
   5-fold (same as the final validation) throughout, removing the
   fast-vs-rigorous two-phase distinction — both phases are 5-fold now.

Reuses each (arm, target, model)'s current saved best hyperparameters
(from ml_output*/*/models/*.joblib .get_params()) so elimination-path
deltas are attributable to the removed feature, not re-tuning noise —
same tradeoff accepted in the chromium6 version.

Usage:
  python3 backward_elim_pfas.py --arm northeast --model rf
  python3 backward_elim_pfas.py --arm northeast --model xgb
"""
import argparse
import importlib.util
import logging
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.inspection import permutation_importance
from sklearn.model_selection import StratifiedGroupKFold
import xgboost as xgb

HERE = Path(__file__).resolve().parent
SEARCH_FOLDS = 5    # was 3 — noise from too few folds was smaller than
                    # TOLERANCE, causing premature stopping (see docstring)
FINAL_FOLDS = 5
TOLERANCE = 0.003   # PR-AUC points; acceptable "free" cost per removal step
FLOOR = 5           # hard stop safety net
DEDUP_THRESHOLD = 0.60   # Spearman |r| pre-filter, matches RFE pipeline's own threshold

# Domain-knowledge overrides for the Spearman dedup step (same pattern the
# main RFE pipeline already uses for well_top_open_ft): any feature that is
# a direct measurement of, or a documented mechanistic source pathway for,
# the target contaminant must never lose to a merely-correlated proxy just
# because a quick importance snapshot happened to rank the proxy slightly
# higher. Verified none of these were actually at risk in the current
# candidate pool (none appeared in any dedup drop list across RF/XGB), but
# protected anyway as cheap insurance against a future rerun where the
# feature set or correlations shift: log_dist_pfas_clustered_km (the
# PFAS-specific source-cluster distance feature — PFAS has no natural
# geochemistry analog to lean on instead) plus
# log_dist_airport_km/military_km/fuds_km — airports, military bases, and
# FUDS are the documented AFFF firefighting-foam PFAS source category,
# distinct from generic industrial/landfill/wwtp proxies also in the pool.
PRIORITY_KEEP = {
    'pfas': {'log_well_top_open_ft', 'log_dist_pfas_clustered_km',
             'log_dist_airport_km', 'log_dist_military_km', 'log_dist_fuds_km'},
}

RECALL_TARGET = {'pfas': 0.60}

# Hyperparameters selected by RandomizedSearchCV for the national PFAS
# model, extracted via joblib.load(...)['model'].get_params(). Fixed here
# so elimination-path deltas reflect only the removed feature.
HYPERPARAMS = {
    # Starting hyperparameters, inherited from the national PFAS model's own
    # randomized search -- no region-specific search has been run yet. Re-tune
    # with a fresh randomized search if you want region-optimized values.
    ('northeast', 'pfas', 'rf'): dict(
        class_weight='balanced', max_depth=None, max_features=0.3,
        min_samples_leaf=2, n_estimators=188, random_state=42, n_jobs=-1),
    ('northeast', 'pfas', 'xgb'): dict(
        colsample_bytree=0.6, gamma=0, learning_rate=0.05, max_depth=7,
        min_child_weight=3, n_estimators=287, reg_alpha=0, reg_lambda=2.0,
        scale_pos_weight=3.2566477955052324, subsample=0.6,
        eval_metric='logloss', random_state=42, n_jobs=-1, tree_method='hist'),
}


def eval_prauc(group_cv_fn, estimator_fn, X_df, y, groups, feat_list, n_folds, recall_target):
    X_arr = X_df[feat_list].values.astype(float)
    cv_df = group_cv_fn(estimator_fn(), X_arr, y, groups, n_splits=n_folds,
                         recall_target=recall_target)
    return cv_df['pr_auc'].mean(), cv_df['f1_opt'].mean(), cv_df['precision'].mean(), cv_df['recall'].mean()


def dedup_candidate_pool(tag, target, base_ml, estimator_fn, X_df, y, groups, feat_list, logger):
    """Spearman dedup pre-filter (fix #1). Ranks features by permutation
    importance from a single full-pool fit (held-out fold), purely to
    decide which member of a correlated pair to keep — this ranking is
    NOT used for anything else (no RFE-style block truncation)."""
    X_arr = X_df[feat_list].values.astype(float)
    sgkf = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=42)
    tr_idx, te_idx = list(sgkf.split(X_arr, y, groups))[-1]

    est = estimator_fn()
    est.fit(X_arr[tr_idx], y[tr_idx])
    perm = permutation_importance(est, X_arr[te_idx], y[te_idx], n_repeats=10,
                                   scoring='average_precision', random_state=42, n_jobs=-1)

    kept = base_ml.spearman_dedup(
        X=X_arr, feat_names=feat_list, importance_arr=perm.importances_mean,
        output_dir=HERE, tname=target, model_tag=f'backward_elim_{tag}',
        threshold=DEDUP_THRESHOLD, priority_keep=PRIORITY_KEEP[target], logger=logger,
    )
    logger.info(f"[{tag}] Spearman dedup: {len(feat_list)} -> {len(kept)} features "
                f"(threshold |r|>{DEDUP_THRESHOLD})")
    return sorted(kept)


def backward_eliminate(tag, group_cv_fn, estimator_fn, X_df, y, groups, feat_list,
                        recall_target, logger):
    current = list(feat_list)
    prauc0, f10, prec0, rec0 = eval_prauc(group_cv_fn, estimator_fn, X_df, y, groups,
                                           current, SEARCH_FOLDS, recall_target)
    peak = prauc0
    path = [{'tag': tag, 'step': 0, 'n_features': len(current), 'removed': None,
             'pr_auc': prauc0, 'f1': f10, 'precision': prec0, 'recall': rec0, 'peak_so_far': peak}]
    logger.info(f"[{tag}] step 0: n={len(current)}  PR-AUC={prauc0:.4f} (search, {SEARCH_FOLDS}-fold)")

    step = 1
    while len(current) > FLOOR:
        scores = {}
        for f in current:
            reduced = [c for c in current if c != f]
            prauc, f1, prec, rec = eval_prauc(group_cv_fn, estimator_fn, X_df, y, groups,
                                               reduced, SEARCH_FOLDS, recall_target)
            scores[f] = (prauc, f1, prec, rec)
        best_f = max(scores, key=lambda f: scores[f][0])
        best_prauc, best_f1, best_prec, best_rec = scores[best_f]

        if best_prauc < peak - TOLERANCE:
            logger.info(f"[{tag}] STOP at n={len(current)}: best available removal "
                        f"({best_f}) would drop PR-AUC to {best_prauc:.4f}, "
                        f"more than {TOLERANCE} below peak {peak:.4f}")
            break

        current.remove(best_f)
        peak = max(peak, best_prauc)
        path.append({'tag': tag, 'step': step, 'n_features': len(current),
                      'removed': best_f, 'pr_auc': best_prauc, 'f1': best_f1,
                      'precision': best_prec, 'recall': best_rec, 'peak_so_far': peak})
        logger.info(f"[{tag}] step {step}: removed '{best_f}' -> n={len(current)}  "
                    f"PR-AUC={best_prauc:.4f}  F1={best_f1:.4f}  (peak so far={peak:.4f})")
        step += 1

    logger.info(f"[{tag}] Elimination path done. Final set: {len(current)} features")
    return current, pd.DataFrame(path)


def load_data(logger):
    spec = importlib.util.spec_from_file_location(
        "base_ml", str(HERE.parent / 'feature_pipeline' / '15_ml_pipeline_national.py'))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)

    X_raw, X_imp, y_all, groups = base_ml.load_prepared_features(
        base_ml.PFAS_FEATURES_CSV, 'pfas_detected', logger)
    return base_ml, X_raw, X_imp, y_all, groups


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--arm', choices=['northeast'], default='northeast')
    parser.add_argument('--target', choices=['pfas'], default='pfas')
    parser.add_argument('--model', choices=['rf', 'xgb'], required=True)
    args = parser.parse_args()

    tag = f"{args.arm}_{args.target}_{args.model}"
    logging.basicConfig(level=logging.INFO, format='%(asctime)s [INFO] %(message)s', datefmt='%H:%M:%S')
    logger = logging.getLogger(tag)

    base_ml, X_raw, X_imp, y_all, groups = load_data(logger)

    labeled_mask = y_all.notna()
    y = y_all[labeled_mask].values.astype(int)
    groups = groups[labeled_mask].values
    X_imp = X_imp.loc[labeled_mask].reset_index(drop=True)
    X_raw = X_raw.loc[labeled_mask].reset_index(drop=True)

    feat_list = sorted(X_imp.columns.tolist())
    recall_target = RECALL_TARGET[args.target]
    params = HYPERPARAMS[(args.arm, args.target, args.model)]
    logger.info(f"[{tag}] n={len(y):,}  full candidate pool={len(feat_list)} features  "
                f"recall_target={recall_target}")

    if args.model == 'rf':
        estimator_fn = lambda: RandomForestClassifier(**params)
        X_df = X_imp
    else:
        estimator_fn = lambda: xgb.XGBClassifier(**params)
        X_df = X_raw

    deduped_list = dedup_candidate_pool(tag, args.target, base_ml, estimator_fn, X_df, y, groups,
                                         feat_list, logger)

    final_set, path_df = backward_eliminate(tag, base_ml.group_cv, estimator_fn, X_df, y, groups,
                                             deduped_list, recall_target, logger)
    path_df.to_csv(HERE / f'backward_elim_{tag}_path.csv', index=False)

    logger.info(f"[{tag}] Final validation (5-fold) on {len(final_set)}-feature set...")
    prauc5, f15, prec5, rec5 = eval_prauc(base_ml.group_cv, estimator_fn, X_df, y, groups,
                                           final_set, FINAL_FOLDS, recall_target)
    logger.info(f"[{tag}] FINAL (5-fold): n={len(final_set)}  PR-AUC={prauc5:.4f}  "
                f"F1={f15:.4f}  Precision={prec5:.4f}  Recall={rec5:.4f}")

    pd.Series(sorted(final_set)).to_csv(HERE / f'backward_elim_{tag}_final_features.csv',
                                         index=False, header=['feature'])
    with open(HERE / f'backward_elim_{tag}_final_metrics.txt', 'w') as fh:
        fh.write(f"n_features={len(final_set)}\npr_auc_5fold={prauc5:.4f}\nf1_5fold={f15:.4f}\n"
                 f"precision_5fold={prec5:.4f}\nrecall_5fold={rec5:.4f}\n")
    logger.info(f"[{tag}] [DONE]")


if __name__ == '__main__':
    main()
