#!/usr/bin/env python3
"""
Re-plot XGBoost SHAP (bar + beeswarm) restricted to features that satisfy
BOTH: (1) in the XGB top-10 by mean |SHAP|, AND (2) DML-significant
(FDR q<0.05) in double_ml_{target}_final_results.csv. Reloads the saved
final XGB model and recomputes SHAP on the same feature matrix it was
trained on, then filters the display to just the passing features.

Usage:
  python3 plot_shap_dml_filtered.py --target pfas
  python3 plot_shap_dml_filtered.py --target li
"""
import argparse
import importlib.util
import inspect
import logging
import warnings
from pathlib import Path

import joblib
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import shap

warnings.filterwarnings('ignore')

HERE = Path(__file__).resolve().parent
COLOR = {'pfas': '#2E86AB', 'li': '#27823A'}
COLLINEARITY_THRESHOLD = 0.60  # same threshold used in backward elimination's Spearman dedup

# Short (<4-word) human-readable labels for plotting. Mechanism, not column name.
FEATURE_LABELS = {
    'FWT_SW': 'Surface water source',
    'FWT_GW': 'Groundwater source',
    'source_geochem_Cr_mean': 'Bedrock Cr level',
    'source_geochem_Li_mean': 'Bedrock Li level',
    'log_well_top_open_ft': 'Well depth',
    'log_well_screen_len_ft': 'Well screen length',
    'precip_mm_ann': 'Annual precipitation',
    'source_forest_clr': 'Forest land use',
    'source_water_clr': 'Water land use',
    'source_wetland_clr': 'Wetland land use',
    'source_urban_clr': 'Urban land use',
    'source_grassland_clr': 'Grassland land use',
    'source_shrub_clr': 'Shrub land use',
    'ROCK_100': 'Sand/gravel aquifer',
    'ROCK_200': 'Semiconsolidated sand aquifer',
    'ROCK_300': 'Sandstone aquifer',
    'ROCK_400': 'Carbonate rock aquifer',
    'ROCK_500': 'Sandstone-carbonate aquifer',
    'ROCK_600': 'Igneous/metamorphic aquifer',
    'ROCK_999': 'Other rock aquifer',
    'ROCK_missing': 'No aquifer data',
    'Service Area_Municipality': 'Municipal service area',
    'Primary Source_Ground water': 'Groundwater primary source',
    '# of Facilities': 'Facility count',
    'Is Source Water Protected': 'Source water protection area',
    'log_dist_coalplant_km': 'Coal plant distance',
    'log_Population Served Count': 'Population served',
    'log_count_wwtp_in_huc12': 'Wastewater plant count',
    'log_count_battery_mfg_in_huc12': 'Battery plant count',
    'log_dist_pfas_clustered_km': 'PFAS industry distance',
    'log_dist_fuds_km': 'Defense site distance',
    'log_dist_military_km': 'Military base distance',
    'log_dist_airport_km': 'Airport distance',
    'log_dist_landfill_km': 'Landfill distance',
    'is_purchased': 'Purchased water',
    'treat_gwd': 'Groundwater treatment',
    'source_geochem_Ca_mean': 'Bedrock Ca level',
    'source_geochem_Na_mean': 'Bedrock Na level',
    'source_geochem_Mg_mean': 'Bedrock Mg level',
    'depth_to_water_m': 'Depth to water table',
    'log_transmissivity_m2day': 'Aquifer transmissivity',
    'log_dist_evaporite_km': 'Dist. to evaporite rock',
}

# Manual, domain-judgment exclusions applied AFTER the DML significance filter
# (per-target — these are NOT caught by DML or the Spearman collinearity
# check, since the reason is structural, not statistical).
MANUAL_EXCLUDE = {
    'pfas': {
        'is_purchased': "consolidate_source_features() already folds a purchasing "
                         "system's seller into its source_* features (geochem/class "
                         "= max(own, seller), land use % = mean(own, seller)) — so "
                         "the source characteristics of a purchased system already "
                         "reflect the seller. is_purchased itself is then just an "
                         "administrative flag from that consolidation step, not an "
                         "independent contamination mechanism.",
    },
    'li': set(),
}


def fmt_stat(dml_stats, feat):
    coef = dml_stats.loc[feat, 'DML coef (adj. for other features)']
    q = dml_stats.loc[feat, 'DML q-value (FDR-BH)']
    q_str = f"{q:.1e}" if q < 0.001 else f"{q:.3f}"
    coef_str = f"{coef:+.1e}" if abs(coef) < 0.0005 else f"{coef:+.3f}"
    return f"β={coef_str}, q={q_str}"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--target', choices=['pfas', 'li'], required=True)
    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format='%(asctime)s [INFO] %(message)s', datefmt='%H:%M:%S')
    logger = logging.getLogger(f'shap_dml_filtered_{args.target}')

    spec = importlib.util.spec_from_file_location("base_ml", str(HERE / 'national' / '15_ml_pipeline_national.py'))
    base_ml = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(base_ml)

    csv_path = base_ml.PFAS_FEATURES_CSV if args.target == 'pfas' else base_ml.LI_FEATURES_CSV
    label_col = 'pfas_detected' if args.target == 'pfas' else 'li_detected'
    X_raw, X_imp, y_all, groups_all = base_ml.load_prepared_features(csv_path, label_col, logger)
    labeled_mask = y_all.notna()
    X_raw = X_raw.loc[labeled_mask].reset_index(drop=True)
    X_imp = X_imp.loc[labeled_mask].reset_index(drop=True)

    out_dir = HERE / 'ml_output_final_national' / args.target / 'plots'

    d = joblib.load(HERE / 'ml_output_final_national' / args.target / 'models' / 'xgb_model_final.joblib')
    model, features = d['model'], d['features']
    X_arr = X_raw[features].values.astype(float)

    logger.info("Recomputing SHAP on final XGB model...")
    explainer = shap.TreeExplainer(model)
    shap_values = explainer.shap_values(X_arr)

    xgb_top10 = pd.read_csv(HERE / 'ml_output_final_national' / args.target /
                             'xgb_shap_importance_final.csv').head(10)['feature'].tolist()
    dml = pd.read_csv(HERE / f'double_ml_{args.target}_final_results.csv')
    dml_sig = set(dml[dml['DML significant (FDR q<0.05)'] == 'Yes']['Feature'])
    dml_kept = [f for f in xgb_top10 if f in dml_sig]
    dml_dropped = [f for f in xgb_top10 if f not in dml_sig]
    manual_exclude = MANUAL_EXCLUDE.get(args.target, set())
    keep = [f for f in dml_kept if f not in manual_exclude]
    manual_dropped = [f for f in dml_kept if f in manual_exclude]
    logger.info(f"XGB top-10: {xgb_top10}")
    logger.info(f"Dropped (top-10 but NOT DML-significant): {dml_dropped}")
    logger.info(f"Dropped (manual, domain judgment): {manual_dropped}")
    logger.info(f"Kept (post-DML, pre-collinearity check): {keep}")

    if not keep:
        logger.warning("No features pass both criteria — nothing to plot.")
        return

    dml_stats = dml.set_index('Feature')[['DML coef (adj. for other features)', 'DML q-value (FDR-BH)']]

    keep_idx = [features.index(f) for f in keep]
    mean_abs = np.abs(shap_values[:, keep_idx]).mean(axis=0)

    # Post-hoc collinearity check on the surviving list. DML already controls
    # each tested feature for the FULL final-model feature set, so a feature
    # here is not just "along for the ride" on some confounder in general —
    # but two specific SURVIVORS can still be pairwise collinear with each
    # other. Apply the same Spearman dedup (and the same 0.60 threshold) used
    # in backward elimination, ranked by SHAP importance so the more
    # predictive member of any correlated pair survives.
    keep_deduped = base_ml.spearman_dedup(
        X_imp[keep].values.astype(float), keep, mean_abs, out_dir, args.target,
        'shap_final', threshold=COLLINEARITY_THRESHOLD, logger=logger)
    collinear_dropped = [f for f in keep if f not in keep_deduped]
    if collinear_dropped:
        logger.info(f"Dropped (collinear with a higher-importance survivor, "
                     f"|r|>{COLLINEARITY_THRESHOLD}): {collinear_dropped}")
    else:
        logger.info(f"Collinearity check: no pair among the {len(keep)} survivors "
                     f"exceeds |r|>{COLLINEARITY_THRESHOLD} — nothing dropped.")
    keep = keep_deduped
    keep_idx = [features.index(f) for f in keep]
    mean_abs = np.abs(shap_values[:, keep_idx]).mean(axis=0)

    order = np.argsort(mean_abs)  # ascending, for horizontal bar (largest at top)
    keep_sorted = [keep[i] for i in order]
    labels_sorted = [FEATURE_LABELS.get(f, f) for f in keep_sorted]
    stats_sorted = [fmt_stat(dml_stats, f) for f in keep_sorted]

    order_desc = order[::-1]  # descending; sort=False beeswarm puts col 0 at top
    keep_idx_sorted = [keep_idx[i] for i in order_desc]
    labels_desc = [FEATURE_LABELS.get(keep[i], keep[i]) for i in order_desc]
    stats_desc = [fmt_stat(dml_stats, keep[i]) for i in order_desc]

    color = COLOR[args.target]
    fig, ax = plt.subplots(figsize=(9, max(3, 0.6 * len(keep) + 1)))
    bars = ax.barh(labels_sorted, mean_abs[order], color=color, alpha=0.85)
    ax.set_xlabel('Mean |SHAP value| (average impact on prediction)')
    ax.set_title(f'XGBoost SHAP — {args.target.upper()}\nTop-10 XGB importance AND DML-significant only')
    x_pad = mean_abs.max() * 0.02
    for bar, label in zip(bars, stats_sorted):
        ax.text(bar.get_width() + x_pad, bar.get_y() + bar.get_height() / 2, label,
                va='center', ha='left', fontsize=9, color='dimgray')
    ax.set_xlim(right=mean_abs.max() * 1.35)
    plt.tight_layout()
    fig.savefig(out_dir / 'xgb_shap_bar_dml_filtered.png', dpi=150, bbox_inches='tight')
    plt.close(fig)

    shap_cmap = inspect.signature(shap.summary_plot).parameters['cmap'].default
    fig, ax = plt.subplots(figsize=(13, max(4, 0.6 * len(keep) + 2)))
    shap.summary_plot(shap_values[:, keep_idx_sorted], X_arr[:, keep_idx_sorted], feature_names=labels_desc,
                       plot_type='dot', show=False, max_display=len(keep), sort=False, color_bar=False)
    plt.title(f'XGBoost SHAP Summary — {args.target.upper()}\nTop-10 XGB importance AND DML-significant only')
    ax = plt.gca()
    plt.tight_layout()
    fig = plt.gcf()

    # Shrink the main axes to free a right-hand margin, then place the
    # stat text and a manually-drawn colorbar (shap's own colorbar is
    # disabled above) in that margin so neither overlaps the other.
    pos = ax.get_position()
    ax.set_position([pos.x0, pos.y0, 0.52 - pos.x0, pos.height])
    label_to_stat = dict(zip(labels_desc, stats_desc))
    trans = ax.get_yaxis_transform()  # x = axes fraction, y = data coords
    for tick, lbl in zip(ax.get_yticks(), [t.get_text() for t in ax.get_yticklabels()]):
        stat = label_to_stat.get(lbl)
        if stat:
            ax.text(1.05, tick, stat, transform=trans, va='center', ha='left', fontsize=9, color='dimgray')

    cax = fig.add_axes([0.87, pos.y0, 0.02, pos.height])
    sm = mpl.cm.ScalarMappable(cmap=shap_cmap, norm=mpl.colors.Normalize(vmin=0, vmax=1))
    cb = fig.colorbar(sm, cax=cax, ticks=[0, 1])
    cb.ax.set_yticklabels(['Low', 'High'])
    cb.set_label('Feature value', labelpad=-8)

    fig.savefig(out_dir / 'xgb_shap_beeswarm_dml_filtered.png', dpi=150, bbox_inches='tight')
    plt.close(fig)

    logger.info(f"[DONE] Saved xgb_shap_bar_dml_filtered.png and xgb_shap_beeswarm_dml_filtered.png in {out_dir}")


if __name__ == '__main__':
    main()
