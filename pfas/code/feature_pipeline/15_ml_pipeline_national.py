#!/usr/bin/env python3
"""
PFAS and Lithium detection prediction for UCMR5 national (all-state) facilities.

Models:
  - Logistic Regression   (baseline; VIF-pruned → p-value-pruned → standardized)
  - Random Forest         (class_weight='balanced'; RandomizedSearchCV)
  - XGBoost               (scale_pos_weight; RandomizedSearchCV; SHAP)

Split: StratifiedGroupKFold (5 folds, grouped by PWSID so all facilities
       from one water system stay in the same fold).
Metrics: ROC-AUC, PR-AUC, F1 at optimal threshold (no accuracy).
State is NOT a model feature (used only to verify CV stratification).
Bootstrap: off by default; enable with --bootstrap.

Usage:
    python ml_pipeline.py --target pfas
    python ml_pipeline.py --target li
    python ml_pipeline.py --target both
    python ml_pipeline.py --target pfas --bootstrap --n-bootstrap 200
"""

import matplotlib
matplotlib.use("Agg")

import argparse
import logging
import sys
import time
import warnings
from pathlib import Path

import joblib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import shap
import statsmodels.api as sm
import xgboost as xgb
from scipy.stats import randint
from sklearn.base import clone
from sklearn.ensemble import RandomForestClassifier
from sklearn.inspection import permutation_importance
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    auc, confusion_matrix, f1_score, precision_recall_curve,
    roc_auc_score, roc_curve,
)
from sklearn.model_selection import RandomizedSearchCV, StratifiedGroupKFold
from sklearn.feature_selection import RFECV
from sklearn.preprocessing import StandardScaler
from statsmodels.stats.outliers_influence import variance_inflation_factor

warnings.filterwarnings("ignore")

# ── Paths ─────────────────────────────────────────────────────────────────────
BASE              = Path(__file__).resolve().parent.parent  # HPC package: self-locating (this file lives in national/, data CSVs are one level up)
PFAS_FEATURES_CSV = BASE / 'national_pfas_features_ready.csv'
LI_FEATURES_CSV   = BASE / 'national_li_features_ready.csv'

# ── Feature lists ─────────────────────────────────────────────────────────────

# Columns to drop entirely from the feature matrix
DROP_ALWAYS = {
    'PWSID', 'FacilityID', 'FacilityName', 'PWSName',
    'huc12', 'huc12_name', 'huc12_key', 'AQ_NAME', 'AQ_CODE', 'ROCK_NAME',
    'geochem_Li_source', 'seller_geochem_Li_source',                  # provenance flags, not features
    'coord_source', 'query_lat', 'query_lon',
    'seller_pwsid', 'seller_pws_name',
    'seller_primary_source_code', 'seller_gw_sw_code',
    'ultimate_seller_pwsid', 'ultimate_seller_source_code', 'ultimate_seller_gw_sw',
    'seller_huc12', 'seller_huc12_name',
    'Is Outstanding Performer',                                      # 100% missing
    'geochem_U_nearest', 'geochem_U_mean', 'geochem_U_max',         # 97%+ missing
    'seller_geochem_U_nearest', 'seller_geochem_U_mean', 'seller_geochem_U_max',  # 99%+ missing
    'total_pixels', 'seller_total_pixels',                           # redundant with pct
    'State',                                                         # excluded: see plan
    'pfas_detected', 'li_detected',                                  # labels
    'FacilityWaterType', 'ROCK_TYPE',                                # encoded below
    # Superseded by dist_pfas_clustered_km (pooled nearest-of-either
    # discharge/manufacturer distance); kept raw in the CSV for reference
    # but must not leak into the model unprocessed (not in LOG_COLS anymore).
    'dist_discharge_km', 'dist_manufacturer_km',
    # battery_mfg distance dropped entirely (both full-segment and the
    # recycling+cell_mfg+battery_grade-restricted version) — restricting to
    # plausible segments fixed the DML sign (closer=higher Li) for 90% of
    # the range, but SHAP dependence still reversed sharply in the farthest
    # decile (>~200km), a shape inconsistent with any known groundwater
    # contaminant-transport mechanism (measured plume lengths rarely exceed
    # ~1-2km even for the most mobile compounds — Rice et al. 1995, EPA/
    # AFCEE BIOCHLOR database) and consistent instead with residual regional
    # confounding (arid, naturally Li-rich states sitting far from all
    # industry). dist_coalplant_km is NOT dropped — its SHAP dependence is
    # smooth and monotonic across the full range with no such reversal.
    'dist_battery_mfg_km', 'dist_battery_mfg_restricted_km',
    # Administrative/business-classification metadata: no mechanistic exposure
    # pathway to PFAS or Li, AND confirmed near-zero permutation importance
    # (checked directly against rf/xgb_permutation_importance_final.csv for
    # both targets before removing — this is not a blind interpretability-only
    # cut). Service Area_*: importance ~0.00001-0.0005 (weak, indirect proxy
    # for land use already captured directly by the source_*_clr features).
    # Primary Source_Ground water purchased: importance ~0.0000 everywhere
    # (the mechanistic source-type signal is carried by FWT_GW/FWT_SW and
    # the non-purchased Primary Source_* flags, which are kept — note
    # Primary Source_Surface water purchased is intentionally KEPT despite
    # looking similar, per explicit user review). seller_chain_depth: low
    # importance (~0.001-0.002), administrative consolidation-depth count.
    # Is Wholesaler: low-moderate importance (0.0009-0.004), business status
    # with no exposure mechanism. is_purchased: real importance for PFAS
    # (0.0033, rank 19/52) but its actual job — triggering
    # consolidate_source_features()'s own/seller blending — already happened
    # upstream into source_*; keeping it as a separate model input just
    # re-exposes the same fact in a non-mechanistic form (explicit user
    # call, accepting the same performance-vs-interpretability tradeoff as
    # '# of Facilities' below).
    # PWS Type_*: previously dropped here too, REVERTED per explicit user
    # request to keep it despite near-zero importance (0.0000-0.0002).
    # Contrast with '# of Facilities' and 'Is Source Water Protected', which
    # ALSO look administrative but were kept — they rank in the top ~15% of
    # permutation importance in every model that has them (imp 0.01-0.05),
    # so removing them would cost real predictive performance ('# of
    # Facilities' removed anyway per explicit user call; see DROP below).
    'Service Area_Industrial/Agricultural', 'Service Area_Institution',
    'Service Area_School', 'Service Area_Municipality', 'Service Area_Residential Area',
    'Primary Source_Ground water purchased',
    'seller_chain_depth', 'Is Wholesaler', 'is_purchased',
    '# of Facilities',
}

# Source-water feature consolidation pairs
# Each tuple is (own_column, seller_column); both get replaced by source_<own_column>
# Geochem & Li class → max(own, seller)  [worst-case contamination exposure for mixed PWSs]
# Land use %         → mean(own, seller) [blended watershed; preserves ~100% sum property]
GEOCHEM_PAIRS = [
    ('geochem_n',           'seller_geochem_n'),
    ('geochem_Li_nearest',  'seller_geochem_Li_nearest'),
    ('geochem_Li_mean',     'seller_geochem_Li_mean'),
    ('geochem_Li_max',      'seller_geochem_Li_max'),
    ('geochem_Li_count',    'seller_geochem_Li_count'),
    ('geochem_Fe_nearest',  'seller_geochem_Fe_nearest'),
    ('geochem_Fe_mean',     'seller_geochem_Fe_mean'),
    ('geochem_Fe_max',      'seller_geochem_Fe_max'),
    ('geochem_Fe_count',    'seller_geochem_Fe_count'),
    ('geochem_Al_nearest',  'seller_geochem_Al_nearest'),
    ('geochem_Al_mean',     'seller_geochem_Al_mean'),
    ('geochem_Al_max',      'seller_geochem_Al_max'),
    ('geochem_Al_count',    'seller_geochem_Al_count'),
    ('geochem_As_nearest',  'seller_geochem_As_nearest'),
    ('geochem_As_mean',     'seller_geochem_As_mean'),
    ('geochem_As_max',      'seller_geochem_As_max'),
    ('geochem_As_count',    'seller_geochem_As_count'),
]
LI_CLASS_PAIRS = [
    ('li_class_pub',  'seller_li_class_pub'),
    ('li_lt4_pub',    'seller_li_lt4_pub'),
    ('li_4_10_pub',   'seller_li_4_10_pub'),
    ('li_10_30_pub',  'seller_li_10_30_pub'),
    ('li_gt30_pub',   'seller_li_gt30_pub'),
]
LAND_USE_PAIRS = [
    ('water_pct',       'seller_water_pct'),
    ('agriculture_pct', 'seller_agriculture_pct'),
    ('urban_pct',       'seller_urban_pct'),
    ('forest_pct',      'seller_forest_pct'),
    ('grassland_pct',   'seller_grassland_pct'),
    ('shrub_pct',       'seller_shrub_pct'),
    ('wetland_pct',     'seller_wetland_pct'),
    ('barren_pct',      'seller_barren_pct'),
    ('other_pct',       'seller_other_pct'),
]

# Land-use percentages are compositional data (sum to ~100% per facility).
# Raw percentages induce spurious negative correlation among categories
# purely from the sum constraint — Pearson's 1897 "spurious correlation,"
# formalized by Aitchison's 1986 compositional data analysis (CoDA)
# framework — which neither naive correlation checks nor DML's RF-based
# nuisance models fully absorb. Centered log-ratio (CLR) transform fixes
# this: clr(x_i) = log(x_i / geometric_mean(x)). Applied in
# load_prepared_features() below (used by both PFAS and Li — the
# pre-prepared CSVs share the same 9 source_*_pct columns). Same
# implementation as chromium6/15_ml_pipeline_cr6.py's build_features().
# source_other_pct excluded entirely: always exactly 0 in this dataset too
# (dead constant column, not usable for a ratio anyway).
LAND_USE_PCT_COLS = [
    'source_water_pct', 'source_urban_pct', 'source_barren_pct', 'source_forest_pct',
    'source_shrub_pct', 'source_grassland_pct', 'source_agriculture_pct', 'source_wetland_pct',
]
CLR_PSEUDOCOUNT = 0.00005   # half the smallest nonzero value observed — standard
                            # CoDA zero-replacement convention (log(0) is undefined)


def clr_transform_land_use(df, logger=None):
    """Replace the 9 raw land-use percentage columns with 8 CLR-transformed
    columns (source_other_pct dropped — always exactly 0). In-place on a copy;
    returns the modified df. Shared helper so midwest8 (ml_pipeline.py) and
    national (this file) apply the identical transform."""
    lu_present = [c for c in LAND_USE_PCT_COLS if c in df.columns]
    if lu_present:
        lu_vals = df[lu_present].apply(pd.to_numeric, errors='coerce').clip(lower=0)
        lu_vals = lu_vals.where(lu_vals > 0, CLR_PSEUDOCOUNT)
        log_vals = np.log(lu_vals)
        row_mean_log = log_vals.mean(axis=1, skipna=False)
        for c in lu_present:
            df[f"{c.replace('_pct', '')}_clr"] = log_vals[c] - row_mean_log
        df.drop(columns=lu_present, inplace=True)
        if logger:
            logger.info(f"  Land-use CLR transform: {len(lu_present)} pct columns -> "
                        f"{len(lu_present)} CLR columns")
    if 'source_other_pct' in df.columns:
        df.drop(columns=['source_other_pct'], inplace=True)
    return df


# Right-skewed non-negative columns → log1p transform
LOG_COLS = [
    'Population Served Count', 'Service Connections Count',
    '# of Violations', '# of Site Visits',
    'dist_industry_km',
    'dist_fuds_km', 'dist_military_km',
    'count_industry_in_huc12', 'count_discharge_in_huc12',
    'count_manufacturer_in_huc12', 'count_fuds_in_huc12', 'count_military_in_huc12',
    # v6 additions (well depth: bottom dropped — collinear with top+len)
    'well_top_open_ft', 'well_screen_len_ft',
    # n_snsv: count of routine sanitary surveys (scheduled every 3-5 yrs; age proxy)
    'n_snsv',
    # v6 Li/WWTP anthropogenic source distances
    'dist_coalplant_km', 'dist_wwtp_km',
    'count_coalplants_in_huc12', 'count_battery_mfg_in_huc12', 'count_wwtp_in_huc12',
    # v10: discharge+manufacturer pooled into a nearest-of-either distance
    # (Tokranov et al. 2024 Science approach: group sparse/clustered source
    # categories instead of modeling each separately). Present in <30 of 51
    # states individually, so raw per-category distance encoded region as
    # much as proximity. DML-checked: both have the same (sensible) sign,
    # so pooling is safe here.
    'dist_pfas_clustered_km',
    # v8: airport distances (FAA Part 139 certified airports)
    'dist_airport_km', 'count_airports_in_huc12',
    # v9: MSW landfill distances (EPA LMOP — Li-ion battery leachate source)
    'dist_landfill_km', 'count_landfills_in_huc12',
]

# Binary 0/1 columns (may be stored as strings)
BINARY_COLS = [
    'Is Wholesaler', 'Is Source Water Protected',
    'PWS Type_Community water system',
    'PWS Type_Non-Transient non-community system',
    'PWS Type_Transient non-community system',
    'Primary Source_Ground water', 'Primary Source_Ground water purchased',
    'Primary Source_Groundwater under influence of surface water',
    'Primary Source_Surface water', 'Primary Source_Surface water purchased',
    'Service Area_Municipality', 'Service Area_Residential Area',
    'Service Area_Institution', 'Service Area_School',
    'Service Area_Industrial/Agricultural',
]

# Features excluded from PFAS models only.
# Applied to all three models (LR, RF, XGB) before any feature selection.
# After source-water consolidation, geochem/li features are named source_* (not own/seller).
PFAS_EXTRA_DROP = [
    # Geochemistry + Li classification: PFAS is anthropogenic, not rock/soil driven
    'source_geochem_n',
    'source_geochem_Li_nearest', 'source_geochem_Li_mean', 'source_geochem_Li_max', 'source_geochem_Li_count',
    'source_geochem_Fe_nearest', 'source_geochem_Fe_mean', 'source_geochem_Fe_max', 'source_geochem_Fe_count',
    'source_geochem_Al_nearest', 'source_geochem_Al_mean', 'source_geochem_Al_max', 'source_geochem_Al_count',
    'source_geochem_As_nearest', 'source_geochem_As_mean', 'source_geochem_As_max', 'source_geochem_As_count',
    'source_li_class_pub', 'source_li_lt4_pub', 'source_li_4_10_pub', 'source_li_10_30_pub', 'source_li_gt30_pub',
    'seller_geochem_U_count',  # U geochem: not consolidated (sparse), drop explicitly
    # PFAS treatment flag: only 47 positives, unclear whether treatment is effective
    'pfas_treat_any',
    # Regulatory compliance history — near-zero r with PFAS, administrative artifact
    'log_# of Violations', 'log_# of Site Visits',
    # Sanitary survey count: administrative age proxy, not a physical PFAS driver
    'log_n_snsv',
    # Li-specific anthropogenic sources: not relevant to PFAS contamination
    'log_dist_coalplant_km',
    'log_count_coalplants_in_huc12', 'log_count_battery_mfg_in_huc12',
]

# Features excluded from the Li model only.
# PFAS-specific source databases (PFAS discharge, AFFF, industry) have no
# documented pathway to Li contamination. Keep WWTP in both (Li from
# pharmaceutical discharge, PFAS from AFFF effluent).
LI_EXTRA_DROP = [
    'log_dist_industry_km',       'log_count_industry_in_huc12',
    'log_dist_pfas_clustered_km',
    'log_count_discharge_in_huc12', 'log_count_manufacturer_in_huc12',
    'log_dist_fuds_km',           'log_count_fuds_in_huc12',
    'log_dist_military_km',       'log_count_military_in_huc12',
    # PFAS treatment indicator: only 47 positives, unclear mechanism for Li
    'pfas_treat_any',
    # Regulatory compliance history — administrative artifact, not a physical Li driver
    'log_# of Violations', 'log_# of Site Visits',
    # Sanitary survey count: administrative age proxy, not a physical Li driver
    'log_n_snsv',
    # Lindsey et al. USGS modeled Li rasters — reserved for validation, not training
    'source_li_class_pub', 'source_li_lt4_pub', 'source_li_4_10_pub',
    'source_li_10_30_pub', 'source_li_gt30_pub',
    # Airport distances — PFAS source (AFFF fire training), not a Li driver
    'log_dist_airport_km', 'log_count_airports_in_huc12',
    # Fe/Al/As geochem: not individually DML-significant on midwest8
    # (redundant with ROCK_TYPE/precip/forest_pct, not spurious) and an
    # ablation test there showed ~0 AUC cost to removing them (RF -0.005,
    # XGB +0.002, LR unchanged) — dropped here too for a simpler,
    # equally-accurate model, consistent with the midwest8 decision.
    'source_geochem_Fe_nearest', 'source_geochem_Fe_mean', 'source_geochem_Fe_max', 'source_geochem_Fe_count',
    'source_geochem_Al_nearest', 'source_geochem_Al_mean', 'source_geochem_Al_max', 'source_geochem_Al_count',
    'source_geochem_As_nearest', 'source_geochem_As_mean', 'source_geochem_As_max', 'source_geochem_As_count',
]

# Features to drop ONLY for LR (pre-VIF, to break structural collinearity)
# After source-water consolidation, land use and geochem are named source_* (no separate seller_*).
LR_PRE_DROP = [
    'source_other_pct',           # land use pcts sum to 100% → drop reference category
    'log_Service Connections Count',  # ~0.95 corr with log Population
    # Within-element geochem: keep _mean only, drop _nearest and _max
    'source_geochem_Li_nearest', 'source_geochem_Li_max',
    'source_geochem_Fe_nearest', 'source_geochem_Fe_max',
    'source_geochem_Al_nearest', 'source_geochem_Al_max',
    'source_geochem_As_nearest', 'source_geochem_As_max',
    # NOTE: well depth (log_well_top_open_ft/log_well_screen_len_ft) is NOT
    # dropped here despite being missing for 100% of SW/MX facilities. FWT_SW/
    # FWT_MX (already in the model) capture almost exactly why it's missing —
    # GW/GU facilities are only missing it ~3-5% of the time (real coverage
    # gaps, not structural) — so those dummies absorb the imputed constant for
    # SW/MX rows while the well-depth coefficient still captures real signal
    # from GW/GU rows. VIF pruning below is the safety net if this causes
    # collinearity trouble in practice.
]


# ── Logging ───────────────────────────────────────────────────────────────────

def setup_logging(output_dir: Path) -> logging.Logger:
    log_dir = output_dir / 'logs'
    log_dir.mkdir(parents=True, exist_ok=True)
    logger = logging.getLogger('ml_pipeline')
    logger.setLevel(logging.DEBUG)
    logger.handlers.clear()
    fmt = logging.Formatter('%(asctime)s [%(levelname)s] %(message)s',
                            datefmt='%H:%M:%S')
    ch = logging.StreamHandler(sys.stdout)
    ch.setLevel(logging.INFO)
    ch.setFormatter(fmt)
    logger.addHandler(ch)
    fh = logging.FileHandler(log_dir / 'pipeline.log', mode='w')
    fh.setLevel(logging.DEBUG)
    fh.setFormatter(fmt)
    logger.addHandler(fh)
    return logger


def save_fig(fig, path: Path, logger):
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path, dpi=150, bbox_inches='tight')
    plt.close(fig)
    logger.info(f"  Saved: {path.name}")


# ── Feature engineering ───────────────────────────────────────────────────────

def consolidate_source_features(df: pd.DataFrame, logger=None) -> pd.DataFrame:
    """
    Collapse own/seller feature pairs into unified source_<own> columns.

    Self-supplied (no seller):  source_X = own_X
    Purchased / mixed (seller present):
      - Geochem & Li class: source_X = max(own_X, seller_X)  -- worst-case exposure
      - Land use %:         source_X = mean(own_X, seller_X) -- blended watershed

    All original own_X and seller_X paired columns are dropped.
    U-geochem columns are excluded from consolidation (too sparse) and handled separately.
    """
    purch = (
        df['seller_pwsid'].astype(str).str.strip().ne('') &
        df['seller_pwsid'].astype(str).str.strip().ne('nan')
    ).values

    for own_col, sel_col in GEOCHEM_PAIRS + LI_CLASS_PAIRS:
        if own_col not in df.columns or sel_col not in df.columns:
            continue
        own_v = pd.to_numeric(df[own_col], errors='coerce').values
        sel_v = pd.to_numeric(df[sel_col], errors='coerce').values
        df['source_' + own_col] = np.where(purch, np.fmax(own_v, sel_v), own_v)

    for own_col, sel_col in LAND_USE_PAIRS:
        if own_col not in df.columns or sel_col not in df.columns:
            continue
        own_v = pd.to_numeric(df[own_col], errors='coerce').values
        sel_v = pd.to_numeric(df[sel_col], errors='coerce').values
        df['source_' + own_col] = np.where(purch, (own_v + sel_v) / 2.0, own_v)

    to_drop = [c for pair in GEOCHEM_PAIRS + LI_CLASS_PAIRS + LAND_USE_PAIRS
               for c in pair if c in df.columns]
    df.drop(columns=to_drop, inplace=True)

    if logger:
        n_pairs = len(GEOCHEM_PAIRS) + len(LI_CLASS_PAIRS) + len(LAND_USE_PAIRS)
        logger.info(f"  Source consolidation: {n_pairs} pairs → source_* features "
                    f"(purchased n={purch.sum()}: geochem/class=max, land_use=avg)")
    return df


def build_features(csv_path: Path, logger) -> tuple:
    """
    Load v5 CSV and build:
      X_raw  : all features, NaN preserved (for XGBoost)
      X_imp  : all features, NaN median-imputed (for RF and LR)
      y_pfas, y_li : labels (float, NaN for 3 unlabeled rows)
      groups : PWSID Series
      col_medians : Series used for imputation
    """
    logger.info(f"Loading {csv_path.name}...")
    df = pd.read_csv(csv_path, low_memory=False)
    logger.info(f"  Raw shape: {df.shape}")

    groups = df['PWSID'].astype(str)
    y_pfas = pd.to_numeric(df['pfas_detected'], errors='coerce')
    y_li   = pd.to_numeric(df['li_detected'],   errors='coerce')

    # ── Binary columns ─────────────────────────────────────────────────────
    for col in BINARY_COLS:
        if col not in df.columns:
            continue
        s = df[col].astype(str).str.strip().str.lower()
        df[col] = s.map({'1': 1.0, '0': 0.0, 'yes': 1.0, 'no': 0.0,
                         'true': 1.0, 'false': 0.0}).astype(float)
        # missing binary → 0
        df[col] = df[col].fillna(0.0)

    # ── seller_chain_depth: impute 0 for non-purchased ─────────────────────
    df['seller_chain_depth'] = pd.to_numeric(
        df['seller_chain_depth'], errors='coerce').fillna(0.0)

    # ── Derived binary: is_purchased ───────────────────────────────────────
    df['is_purchased'] = (
        df['seller_pwsid'].astype(str).str.strip().ne('') &
        df['seller_pwsid'].astype(str).str.strip().ne('nan')
    ).astype(float)

    # ── Source water feature consolidation ─────────────────────────────────
    df = consolidate_source_features(df, logger=logger)

    # ── Log1p transforms ───────────────────────────────────────────────────
    for col in LOG_COLS:
        if col not in df.columns:
            continue
        num = pd.to_numeric(df[col], errors='coerce').clip(lower=0)
        df[f'log_{col}'] = np.log1p(num)
    df.drop(columns=[c for c in LOG_COLS if c in df.columns], inplace=True)

    # ── One-hot encode FacilityWaterType ───────────────────────────────────
    fwt = df['FacilityWaterType'].astype(str).str.strip().replace({'nan': 'Unknown', '': 'Unknown'})
    fwt_dummies = pd.get_dummies(fwt, prefix='FWT', drop_first=False, dtype=float)
    df = pd.concat([df, fwt_dummies], axis=1)

    # ── One-hot encode ROCK_TYPE ───────────────────────────────────────────
    def rock_label(x):
        x = str(x).strip()
        if x in ('', 'nan'):
            return 'ROCK_missing'
        try:
            return f'ROCK_{int(float(x))}'
        except ValueError:
            return f'ROCK_{x}'

    rt = df['ROCK_TYPE'].apply(rock_label)
    rt_dummies = pd.get_dummies(rt, drop_first=False, dtype=float)
    df = pd.concat([df, rt_dummies], axis=1)

    # ── Parse remaining numeric columns ────────────────────────────────────
    non_numeric = DROP_ALWAYS | set(BINARY_COLS) | {
        'seller_chain_depth', 'is_purchased', 'PWSID', 'State',
    }
    for col in df.columns:
        if col in non_numeric:
            continue
        if df[col].dtype == object:
            df[col] = pd.to_numeric(df[col], errors='coerce')

    # ── Drop metadata / label / already-encoded columns ────────────────────
    df.drop(columns=[c for c in DROP_ALWAYS if c in df.columns],
            inplace=True, errors='ignore')

    # ── Build X_raw (NaN preserved) ────────────────────────────────────────
    feature_cols = [c for c in df.columns
                    if c not in ('PWSID', 'State', 'pfas_detected', 'li_detected')]
    X_raw = df[feature_cols].copy()

    # ── Compute per-column medians on full dataset for imputation ──────────
    col_medians = X_raw.median()

    # ── Build X_imp (median-imputed) ───────────────────────────────────────
    X_imp = X_raw.copy()
    for col in X_imp.columns:
        if X_imp[col].isna().any():
            X_imp[col] = X_imp[col].fillna(col_medians[col])

    logger.info(f"  Features built: {X_raw.shape[1]} columns")
    logger.info(f"  NaN in X_raw:   {X_raw.isna().sum().sum()} cells")
    logger.info(f"  NaN in X_imp:   {X_imp.isna().sum().sum()} cells (should be 0)")
    logger.info(f"  y_pfas labeled: {y_pfas.notna().sum()}  "
                f"(+{int(y_pfas.sum())} / -{int((y_pfas==0).sum())})")
    logger.info(f"  y_li   labeled: {y_li.notna().sum()}    "
                f"(+{int(y_li.sum())} / -{int((y_li==0).sum())})")

    return X_raw, X_imp, y_pfas, y_li, groups, col_medians


# ── VIF pruning ───────────────────────────────────────────────────────────────

def vif_prune(X: pd.DataFrame, threshold: float = 10.0, logger=None) -> list:
    """
    Iteratively remove the feature with highest VIF > threshold until all
    remaining features have VIF <= threshold.
    Returns list of remaining feature names.
    """
    cols = list(X.columns)
    removed = []
    iteration = 0
    while True:
        iteration += 1
        X_arr = X[cols].values.astype(float)
        # Add constant for VIF (statsmodels convention)
        X_c = sm.add_constant(X_arr, has_constant='add')
        vifs = [variance_inflation_factor(X_c, i + 1) for i in range(len(cols))]
        max_vif = max(vifs)
        if max_vif <= threshold:
            break
        drop_idx = int(np.argmax(vifs))
        drop_col = cols[drop_idx]
        removed.append((drop_col, round(max_vif, 2)))
        cols.pop(drop_idx)
        if logger:
            logger.debug(f"    VIF iter {iteration}: drop '{drop_col}' (VIF={max_vif:.1f})")

    if logger:
        logger.info(f"  VIF pruning: removed {len(removed)} features, "
                    f"{len(cols)} remaining (threshold={threshold})")
        if removed:
            logger.debug(f"  Removed: {removed[:10]}{'...' if len(removed)>10 else ''}")
    return cols


# ── P-value pruning ───────────────────────────────────────────────────────────

def pvalue_prune(X: pd.DataFrame, y: np.ndarray,
                 threshold: float = 0.05, logger=None) -> list:
    """
    Iteratively fit statsmodels Logit and remove the feature with the highest
    p-value > threshold until all remaining p-values <= threshold.
    Returns list of remaining feature names.
    """
    cols = list(X.columns)
    removed = []
    iteration = 0
    while True:
        iteration += 1
        X_c = sm.add_constant(X[cols].values.astype(float), has_constant='add')
        try:
            result = sm.Logit(y, X_c).fit(method='lbfgs', maxiter=300, disp=False)
            pvals = result.pvalues[1:]  # skip constant
        except Exception as e:
            if logger:
                logger.warning(f"  p-value fit failed at iter {iteration}: {e}")
            break
        max_pval = pvals.max()
        if max_pval <= threshold:
            break
        drop_idx = int(np.argmax(pvals))
        drop_col = cols[drop_idx]
        removed.append((drop_col, round(max_pval, 4)))
        cols.pop(drop_idx)
        if logger:
            logger.debug(f"    p-val iter {iteration}: drop '{drop_col}' (p={max_pval:.4f})")
        if len(cols) == 0:
            break

    if logger:
        logger.info(f"  p-value pruning: removed {len(removed)} features, "
                    f"{len(cols)} remaining (threshold={threshold})")
    return cols


# ── RFE feature selection for tree models ────────────────────────────────────

def rfe_select(estimator, X: np.ndarray, y: np.ndarray,
               groups: np.ndarray, feat_names: list,
               importance_arr: np.ndarray,
               output_dir: Path, tname: str, model_tag: str,
               tolerance: float = 0.005, logger=None,
               metric: str = 'roc_auc') -> list:
    """
    Post-hoc RFE for RF and XGB using importance ranking from the full model.

    Steps:
      1. Rank features by importance_arr (descending).
      2. Evaluate cumulative subsets at checkpoints via 5-fold GroupKFold CV.
      3. Return the smallest subset whose CV score (`metric`) is within
         `tolerance` of the peak score across all subset sizes.
      4. Save score-vs-n-features curve plot and selection CSV.

    tolerance=0.005 means we accept up to a 0.5%-point drop in `metric`
    for a simpler model.

    metric: 'roc_auc' (default, matches original behavior) or 'pr_auc'.
            For strongly imbalanced targets, ROC-AUC barely moves across
            feature-count checkpoints (it's dominated by the easy negatives),
            so tolerance-based selection collapses to a near-minimal feature
            set. 'pr_auc' is directly sensitive to precision/recall on the
            positive class and is the more appropriate selection criterion
            when the goal is to optimize precision/recall rather than
            ranking quality alone.
    """
    n_total = len(feat_names)
    # Checkpoints: fine-grained at the small end, coarser at large end
    checkpoints = sorted(set(
        list(range(5, min(31, n_total + 1), 5)) +
        list(range(30, min(76, n_total + 1), 10)) +
        list(range(75, n_total, 25)) +
        [n_total]
    ))
    checkpoints = [c for c in checkpoints if c <= n_total]

    # Rank features by descending importance
    order = np.argsort(importance_arr)[::-1]
    ranked_names = [feat_names[i] for i in order]

    rows = []
    if logger:
        logger.info(f"  [{model_tag} RFE] Testing {len(checkpoints)} subset sizes: {checkpoints}")

    for n in checkpoints:
        cols = ranked_names[:n]
        col_idx = [feat_names.index(c) for c in cols]
        X_sub = X[:, col_idx]
        cv_df = group_cv(clone(estimator), X_sub, y, groups, n_splits=5)
        mean_auc = cv_df['roc_auc'].mean()
        std_auc  = cv_df['roc_auc'].std()
        mean_pr  = cv_df['pr_auc'].mean()
        std_pr   = cv_df['pr_auc'].std()
        rows.append({'n_features': n, 'cv_auc_mean': mean_auc, 'cv_auc_std': std_auc,
                     'cv_prauc_mean': mean_pr, 'cv_prauc_std': std_pr})
        if logger:
            logger.info(f"    n={n:3d}: CV ROC-AUC={mean_auc:.4f} ± {std_auc:.4f}  "
                        f"PR-AUC={mean_pr:.4f} ± {std_pr:.4f}")

    rfe_df = pd.DataFrame(rows)
    rfe_df.to_csv(output_dir / f'{model_tag}_rfe_curve.csv', index=False)

    score_col = 'cv_auc_mean' if metric == 'roc_auc' else 'cv_prauc_mean'
    score_label = 'ROC-AUC' if metric == 'roc_auc' else 'PR-AUC'

    # Pick smallest subset within tolerance of peak score
    peak_score = rfe_df[score_col].max()
    threshold = peak_score - tolerance
    eligible = rfe_df[rfe_df[score_col] >= threshold]
    best_n = int(eligible['n_features'].min())
    best_score = float(eligible[eligible['n_features'] == best_n][score_col].iloc[0])
    selected_cols = ranked_names[:best_n]

    if logger:
        logger.info(f"  [{model_tag} RFE] Peak {score_label}={peak_score:.4f} | "
                    f"Selected n={best_n} ({score_label}={best_score:.4f}, "
                    f"Δ={best_score-peak_score:+.4f})")

    # Score-vs-n-features curve plot
    fig, ax = plt.subplots(figsize=(9, 5))
    ax.plot(rfe_df['n_features'], rfe_df[score_col],
            marker='o', color='#1f77b4', linewidth=2, label=f'CV {score_label} (5-fold mean)')
    ax.fill_between(
        rfe_df['n_features'],
        rfe_df[score_col] - rfe_df[score_col.replace('mean', 'std')],
        rfe_df[score_col] + rfe_df[score_col.replace('mean', 'std')],
        alpha=0.2, color='#1f77b4'
    )
    ax.axvline(best_n, color='red', linestyle='--', linewidth=1.5,
               label=f'Selected: n={best_n} ({score_label}={best_score:.3f})')
    ax.axhline(threshold, color='gray', linestyle=':', linewidth=1,
               label=f'Threshold (peak − {tolerance})')
    ax.set_xlabel('Number of Features')
    ax.set_ylabel(f'5-Fold CV {score_label}')
    ax.set_title(f'{model_tag.upper()} RFE Feature Selection — {tname.upper()}')
    ax.legend()
    ax.grid(alpha=0.3)
    plt.tight_layout()
    fig.savefig(output_dir / 'plots' / f'{model_tag}_rfe_curve.png',
                dpi=150, bbox_inches='tight')
    plt.close(fig)

    return selected_cols


# ── Spearman correlation deduplication ───────────────────────────────────────

def spearman_dedup(X: np.ndarray, feat_names: list, importance_arr: np.ndarray,
                   output_dir: Path, tname: str, model_tag: str,
                   threshold: float = 0.60,
                   priority_keep: set = None,
                   logger=None) -> list:
    """
    Remove redundant features using pairwise Spearman correlation.
    For each pair with |r| > threshold, drop the less important feature.
    Features are processed in descending importance order so the more
    predictive representative of each correlated cluster always survives.

    priority_keep: set of feature names that always survive over their
                   correlated partners regardless of importance rank.
                   Used to encode domain-knowledge decisions (e.g. always
                   keep well_top_open_ft over well_screen_len_ft).
    """
    corr_mat = pd.DataFrame(X, columns=feat_names).corr(method='spearman').values
    pk = priority_keep or set()

    order = np.argsort(importance_arr)[::-1]  # most important first
    dropped = set()
    pairs_dropped = []

    for i_rank, i in enumerate(order):
        if i in dropped:
            continue
        for j in order[i_rank + 1:]:
            if j in dropped:
                continue
            if abs(corr_mat[i, j]) > threshold:
                # Domain-knowledge override: if j is priority and i is not,
                # drop i instead and stop processing i's remaining pairs.
                if feat_names[j] in pk and feat_names[i] not in pk:
                    dropped.add(i)
                    pairs_dropped.append((feat_names[j], feat_names[i],
                                          round(float(corr_mat[i, j]), 3)))
                    break  # i is dropped; exit inner loop
                else:
                    dropped.add(j)
                    pairs_dropped.append((feat_names[i], feat_names[j],
                                          round(float(corr_mat[i, j]), 3)))

    kept = [feat_names[i] for i in range(len(feat_names)) if i not in dropped]

    if logger:
        logger.info(f"  [{model_tag} Spearman dedup] |r|>{threshold}: "
                    f"{len(feat_names)} → {len(kept)} features "
                    f"({len(dropped)} removed)")
        for kf, df, rv in pairs_dropped:
            logger.info(f"    keep '{kf}' | drop '{df}' (r={rv:+.3f})")

    if pairs_dropped:
        pd.DataFrame(pairs_dropped,
                     columns=['kept_feature', 'dropped_feature', 'spearman_r']
                     ).to_csv(output_dir / f'{model_tag}_spearman_dropped.csv', index=False)

    return kept


# ── Cross-validation helper ───────────────────────────────────────────────────

def group_cv(estimator, X: np.ndarray, y: np.ndarray,
             groups: np.ndarray, n_splits: int = 5,
             recall_target: float = None,
             fit_params: dict = None,
             return_preds: bool = False):
    """
    StratifiedGroupKFold CV. Returns per-fold DataFrame with
    roc_auc, pr_auc, precision, recall, f1, threshold.

    recall_target: if set, picks the highest-precision threshold where
                   recall >= recall_target instead of the F1-optimal threshold.
    return_preds:  if True, also returns
                     (oof_y_true, oof_y_pred_bin, oof_y_prob, oof_groups)
                   arrays of aggregated out-of-fold predictions suitable for
                   a facility-level confusion matrix and PWS-level aggregation.
    """
    sgkf = StratifiedGroupKFold(n_splits=n_splits, shuffle=True, random_state=42)
    fit_params = fit_params or {}
    results = []
    oof_y_true, oof_y_pred, oof_y_prob, oof_groups = [], [], [], []

    for fold, (tr_idx, te_idx) in enumerate(sgkf.split(X, y, groups)):
        X_tr, X_te = X[tr_idx], X[te_idx]
        y_tr, y_te = y[tr_idx], y[te_idx]

        est = clone(estimator)
        est.fit(X_tr, y_tr, **fit_params)

        y_prob = est.predict_proba(X_te)[:, 1]

        roc_auc = roc_auc_score(y_te, y_prob)
        prec_arr, rec_arr, thresh_arr = precision_recall_curve(y_te, y_prob)
        pr_auc = auc(rec_arr, prec_arr)

        if recall_target is not None:
            eligible = rec_arr[:-1] >= recall_target
            if eligible.any():
                best_i = int(np.where(eligible)[0][np.argmax(prec_arr[:-1][eligible])])
            else:
                best_i = int(np.argmax(rec_arr[:-1]))
        else:
            f1s = 2 * prec_arr * rec_arr / (prec_arr + rec_arr + 1e-9)
            best_i = int(np.argmax(f1s[:-1]))

        best_thresh = float(thresh_arr[best_i])
        y_pred = (y_prob >= best_thresh).astype(int)
        f1_opt = f1_score(y_te, y_pred, zero_division=0)

        results.append({
            'fold':          fold + 1,
            'roc_auc':       round(roc_auc, 4),
            'pr_auc':        round(pr_auc, 4),
            'precision':     round(float(prec_arr[best_i]), 4),
            'recall':        round(float(rec_arr[best_i]), 4),
            'f1_opt':        round(f1_opt, 4),
            'opt_threshold': round(best_thresh, 4),
            'n_test':        len(y_te),
            'n_pos_test':    int(y_te.sum()),
        })

        if return_preds:
            oof_y_true.extend(y_te.tolist())
            oof_y_pred.extend(y_pred.tolist())
            oof_y_prob.extend(y_prob.tolist())
            oof_groups.extend(groups[te_idx].tolist())

    cv_df = pd.DataFrame(results)
    if return_preds:
        return (cv_df,
                np.array(oof_y_true), np.array(oof_y_pred),
                np.array(oof_y_prob), np.array(oof_groups))
    return cv_df


def summarize_cv(cv_df: pd.DataFrame) -> dict:
    summary = {}
    for col in ['roc_auc', 'pr_auc', 'precision', 'recall', 'f1_opt']:
        if col in cv_df.columns:
            summary[f'{col}_mean'] = round(cv_df[col].mean(), 4)
            summary[f'{col}_std']  = round(cv_df[col].std(), 4)
    return summary


# ── Confusion matrix plot ─────────────────────────────────────────────────────

def plot_cm(y_true: np.ndarray, y_pred: np.ndarray,
            output_dir: Path, tname: str, model_tag: str, logger,
            level: str = 'Facility'):
    """
    Plot and save a confusion matrix from out-of-fold predictions.
    Shows raw counts and row-normalised rates.
    level: 'Facility' or 'PWS' — shown in title and used for filename suffix.
    """
    cm = confusion_matrix(y_true, y_pred)
    tn, fp, fn, tp = cm.ravel()
    total = len(y_true)
    n_pos = int(y_true.sum())
    n_neg = total - n_pos

    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall    = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1        = 2 * precision * recall / (precision + recall + 1e-9)

    cm_norm = cm.astype(float) / cm.sum(axis=1, keepdims=True)

    fig, axes = plt.subplots(1, 2, figsize=(12, 5))

    labels = ['Not Detected', 'Detected']
    for ax, data, fmt, title in zip(
        axes,
        [cm, cm_norm],
        ['d', '.2%'],
        ['Out-of-Fold Counts', 'Row-Normalised Rates'],
    ):
        sns.heatmap(data, annot=True, fmt=fmt, cmap='Blues',
                    xticklabels=labels, yticklabels=labels,
                    ax=ax, linewidths=0.5, linecolor='gray',
                    cbar=False, annot_kws={'size': 14})
        ax.set_xlabel('Predicted', fontsize=12)
        ax.set_ylabel('Actual', fontsize=12)
        ax.set_title(title, fontsize=12)

    target_label = {'pfas': 'PFAS', 'li': 'Lithium'}.get(tname, tname.upper())
    model_label  = {'lr': 'Logistic Regression',
                    'rf': 'Random Forest',
                    'xgb': 'XGBoost'}.get(model_tag, model_tag.upper())

    unit = 'PWS' if level == 'PWS' else 'facilities'
    fig.suptitle(
        f'{model_label} — {target_label} Detection  [{level} Level]  |  '
        f'Precision={precision:.1%}  Recall={recall:.1%}  F1={f1:.3f}\n'
        f'TP={tp}  FP={fp}  FN={fn}  TN={tn}  '
        f'(n={total} {unit}, pos={n_pos}, neg={n_neg})',
        fontsize=11, y=1.02,
    )
    plt.tight_layout()
    suffix = '_pws' if level == 'PWS' else ''
    save_fig(fig, output_dir / 'plots' / f'{model_tag}_confusion_matrix{suffix}.png', logger)


# ── PWS-level aggregation metrics ────────────────────────────────────────────

def pws_agg_metrics(oof_y_true: np.ndarray, oof_y_pred_bin: np.ndarray,
                    oof_y_prob: np.ndarray, oof_groups: np.ndarray,
                    recall_target: float,
                    output_dir: Path, tname: str, model_tag: str, logger) -> dict:
    """
    Aggregate OOF facility predictions to PWS level and compute metrics.

    Rule: a PWS is positive if ANY facility in it is truly positive.
    PWS predicted probability = max facility probability in the PWS.
    Binary prediction uses the same recall-constrained threshold logic
    applied at PWS level.

    Because GroupKFold splits by PWSID, every PWSID appears in exactly
    one fold's test set, so aggregation is leak-free.
    """
    df = pd.DataFrame({
        'pwsid':    oof_groups,
        'y_true':   oof_y_true,
        'y_prob':   oof_y_prob,
    })
    pws = df.groupby('pwsid').agg(
        pws_true=('y_true', 'max'),   # positive if any facility detected
        pws_prob=('y_prob', 'max'),   # max facility probability
    ).reset_index()

    pws_true = pws['pws_true'].values.astype(int)
    pws_prob = pws['pws_prob'].values

    pws_roc_auc = roc_auc_score(pws_true, pws_prob)
    prec_arr, rec_arr, thresh_arr = precision_recall_curve(pws_true, pws_prob)
    pws_pr_auc = auc(rec_arr, prec_arr)

    if recall_target is not None:
        eligible = rec_arr[:-1] >= recall_target
        if eligible.any():
            best_i = int(np.where(eligible)[0][np.argmax(prec_arr[:-1][eligible])])
        else:
            best_i = int(np.argmax(rec_arr[:-1]))
    else:
        f1s = 2 * prec_arr * rec_arr / (prec_arr + rec_arr + 1e-9)
        best_i = int(np.argmax(f1s[:-1]))

    pws_thresh = float(thresh_arr[best_i])
    pws_pred   = (pws_prob >= pws_thresh).astype(int)

    pws_prec = float(prec_arr[best_i])
    pws_rec  = float(rec_arr[best_i])
    pws_f1   = f1_score(pws_true, pws_pred, zero_division=0)
    n_pws    = len(pws)
    n_pws_pos = int(pws_true.sum())

    logger.info(
        f"  [PWS-level {model_tag.upper()}] "
        f"n_pws={n_pws} (pos={n_pws_pos})  "
        f"ROC-AUC={pws_roc_auc:.4f}  PR-AUC={pws_pr_auc:.4f}  "
        f"Precision={pws_prec:.4f}  Recall={pws_rec:.4f}  F1={pws_f1:.4f}  "
        f"(threshold={pws_thresh:.3f})"
    )

    plot_cm(pws_true, pws_pred, output_dir, tname, model_tag, logger, level='PWS')

    pws_metrics_df = pd.DataFrame([{
        'model':         model_tag,
        'level':         'PWS',
        'n':             n_pws,
        'n_pos':         n_pws_pos,
        'roc_auc':       round(pws_roc_auc, 4),
        'pr_auc':        round(pws_pr_auc, 4),
        'precision':     round(pws_prec, 4),
        'recall':        round(pws_rec, 4),
        'f1':            round(pws_f1, 4),
        'threshold':     round(pws_thresh, 4),
    }])
    out_csv = output_dir / f'{model_tag}_pws_metrics.csv'
    pws_metrics_df.to_csv(out_csv, index=False)

    return {
        'pws_roc_auc':  pws_roc_auc,
        'pws_pr_auc':   pws_pr_auc,
        'pws_precision': pws_prec,
        'pws_recall':   pws_rec,
        'pws_f1':       pws_f1,
        'pws_n':        n_pws,
        'pws_n_pos':    n_pws_pos,
    }


# ── LR model ──────────────────────────────────────────────────────────────────

def run_lr(X_imp: pd.DataFrame, y: np.ndarray, groups: np.ndarray,
           scale_pos_weight: float, output_dir: Path, tname: str,
           logger, recall_target: float = None) -> dict:
    """
    Logistic Regression baseline:
      1. Drop LR_PRE_DROP columns
      2. VIF pruning (threshold=10)
      3. StandardScaler
      4. P-value pruning (statsmodels Logit, threshold=0.05)
      5. GroupKFold CV
      6. Refit on all data → coefficients plot + statsmodels summary
    """
    logger.info(f"\n{'='*60}")
    logger.info(f"[LR] Running Logistic Regression for {tname}")

    # 1. Pre-drop collinear columns
    pre_drop = [c for c in LR_PRE_DROP if c in X_imp.columns]
    X_lr = X_imp.drop(columns=pre_drop)
    logger.info(f"  After pre-drop: {X_lr.shape[1]} features")

    # Drop zero-variance columns (constant features break VIF/Logit)
    zero_var = [c for c in X_lr.columns if X_lr[c].std() == 0]
    if zero_var:
        logger.info(f"  Dropping {len(zero_var)} zero-variance columns before VIF: {zero_var}")
        X_lr = X_lr.drop(columns=zero_var)

    # 2. VIF pruning on raw (unscaled but imputed) data
    logger.info("  VIF pruning...")
    vif_cols = vif_prune(X_lr, threshold=10.0, logger=logger)
    X_lr = X_lr[vif_cols]

    # 3. Scale
    scaler = StandardScaler()
    X_scaled = pd.DataFrame(scaler.fit_transform(X_lr),
                             columns=X_lr.columns, index=X_lr.index)

    # 4. P-value pruning on scaled data
    logger.info("  P-value pruning...")
    final_cols = pvalue_prune(X_scaled, y, threshold=0.05, logger=logger)

    if len(final_cols) == 0:
        logger.warning("  All features pruned by p-value! Keeping VIF-pruned set.")
        final_cols = vif_cols

    X_final = X_scaled[final_cols].values
    logger.info(f"  Final LR features: {len(final_cols)}")

    # 5. GroupKFold CV with sklearn LR
    n_neg = int((y == 0).sum())
    n_pos = int((y == 1).sum())
    cw = {0: 1.0, 1: n_neg / n_pos}  # equivalent to class_weight='balanced'
    lr_clf = LogisticRegression(
        class_weight='balanced', solver='lbfgs', max_iter=1000,
        C=1.0, random_state=42,
    )
    logger.info("  Running 5-fold GroupKFold CV...")
    cv_df, oof_y_true, oof_y_pred, oof_y_prob, oof_groups = group_cv(
        lr_clf, X_final, y, groups, n_splits=5,
        recall_target=recall_target, return_preds=True)
    summary = summarize_cv(cv_df)
    logger.info(f"  CV results: ROC-AUC={summary['roc_auc_mean']:.4f}±{summary['roc_auc_std']:.4f}  "
                f"PR-AUC={summary['pr_auc_mean']:.4f}±{summary['pr_auc_std']:.4f}  "
                f"Precision={summary['precision_mean']:.4f}  Recall={summary['recall_mean']:.4f}  "
                f"F1={summary['f1_opt_mean']:.4f}±{summary['f1_opt_std']:.4f}")
    plot_cm(oof_y_true, oof_y_pred, output_dir, tname, 'lr', logger)
    pws_agg_metrics(oof_y_true, oof_y_pred, oof_y_prob, oof_groups,
                    recall_target, output_dir, tname, 'lr', logger)

    # Save CV results
    cv_df.to_csv(output_dir / 'lr_cv_results.csv', index=False)

    # 6. Refit on all data for coefficient interpretation
    lr_final = LogisticRegression(
        class_weight='balanced', solver='lbfgs', max_iter=1000, C=1.0, random_state=42
    )
    lr_final.fit(X_final, y)

    # Also refit with statsmodels for CIs and p-values
    X_sm = sm.add_constant(X_final, has_constant='add')
    try:
        sm_result = sm.Logit(y, X_sm).fit(method='lbfgs', maxiter=500, disp=False)
        coef_df = pd.DataFrame({
            'feature': ['const'] + final_cols,
            'coef':    sm_result.params,
            'pvalue':  sm_result.pvalues,
            'ci_low':  sm_result.conf_int()[:, 0],
            'ci_high': sm_result.conf_int()[:, 1],
        })
        coef_df.to_csv(output_dir / 'lr_coefficients.csv', index=False)
    except Exception as e:
        logger.warning(f"  statsmodels refit failed: {e}")
        coef_df = pd.DataFrame({
            'feature': final_cols,
            'coef':    lr_final.coef_[0],
            'pvalue':  np.nan,
            'ci_low':  np.nan,
            'ci_high': np.nan,
        })
        coef_df.to_csv(output_dir / 'lr_coefficients.csv', index=False)

    # ── Coefficient plot (top 10 by |coef|, excluding const) ─────────────
    plot_df = coef_df[coef_df['feature'] != 'const'].copy()
    plot_df['abs_coef'] = plot_df['coef'].abs()
    plot_df = plot_df.nlargest(10, 'abs_coef').sort_values('abs_coef')

    fig, ax = plt.subplots(figsize=(9, 6))
    colors = ['#d62728' if c > 0 else '#1f77b4' for c in plot_df['coef']]
    bars = ax.barh(plot_df['feature'], plot_df['coef'], color=colors, alpha=0.8)
    if not plot_df['ci_low'].isna().all():
        ax.errorbar(plot_df['coef'], range(len(plot_df)),
                    xerr=[plot_df['coef'] - plot_df['ci_low'],
                          plot_df['ci_high'] - plot_df['coef']],
                    fmt='none', color='black', capsize=4, linewidth=1.5)
    ax.axvline(0, color='black', linewidth=0.8, linestyle='--')
    ax.set_xlabel('Standardized Coefficient (log-odds)')
    ax.set_title(f'LR Top 10 Features — {tname.upper()} Detection\n'
                 f'(red=increases risk, blue=decreases risk)')
    ax.grid(axis='x', alpha=0.3)
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'lr_coefficients.png', logger)

    # ── Probability distribution plot ─────────────────────────────────────
    y_prob_all = lr_final.predict_proba(X_final)[:, 1]
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.hist(y_prob_all[y == 0], bins=40, alpha=0.6, label='Not detected (0)', color='#1f77b4')
    ax.hist(y_prob_all[y == 1], bins=40, alpha=0.6, label='Detected (1)', color='#d62728')
    ax.set_xlabel('Predicted Probability of Detection')
    ax.set_ylabel('Count')
    ax.set_title(f'LR Predicted Probability Distribution — {tname.upper()}')
    ax.legend()
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'lr_prob_distribution.png', logger)

    # ── Save model ─────────────────────────────────────────────────────────
    (output_dir / 'models').mkdir(exist_ok=True)
    joblib.dump({'model': lr_final, 'scaler': scaler, 'features': final_cols},
                output_dir / 'models' / 'lr_model.joblib')

    # ── ROC/PR from pooled out-of-fold CV predictions (for comparison plot) ─
    # NOT y_prob_all (in-sample refit) — that inflates high-capacity models
    # (esp. RF, with its unbounded tree depth) and makes the plotted curve
    # inconsistent with the CV AUC shown in the legend, which IS out-of-fold.
    fpr, tpr, _ = roc_curve(oof_y_true, oof_y_prob)
    prec, rec, _ = precision_recall_curve(oof_y_true, oof_y_prob)

    top10 = plot_df.sort_values('abs_coef', ascending=False)['feature'].tolist()

    return {
        'name': 'Logistic Regression',
        'cv': summary,
        'cv_df': cv_df,
        'fpr': fpr, 'tpr': tpr,
        'prec': prec, 'rec': rec,
        'roc_auc': roc_auc_score(oof_y_true, oof_y_prob),
        'pr_auc': auc(rec, prec),
        'top10_features': top10,
        'feature_importance': plot_df.set_index('feature')['abs_coef'].to_dict(),
        'n_features': len(final_cols),
    }


# ── Random Forest model ───────────────────────────────────────────────────────

def run_rf(X_imp: pd.DataFrame, y: np.ndarray, groups: np.ndarray,
           scale_pos_weight: float, output_dir: Path, tname: str,
           random_seed: int, logger, recall_target: float = None,
           search_scoring: str = 'roc_auc', search_n_iter: int = 50,
           rfe_metric: str = 'roc_auc', rfe_tolerance: float = 0.005) -> dict:
    """
    Random Forest with RandomizedSearchCV (GroupKFold inside) + GroupKFold CV.
    Feature importance via permutation importance (more reliable than Gini).
    """
    logger.info(f"\n{'='*60}")
    logger.info(f"[RF] Running Random Forest for {tname}")

    X_arr = X_imp.values.astype(float)
    feat_names = list(X_imp.columns)

    n_neg = int((y == 0).sum()); n_pos = int((y == 1).sum())
    spw = n_neg / n_pos

    # Hyperparameter search space — include class_weight to tune recall/precision trade-off
    param_dist = {
        'n_estimators':     randint(100, 400),
        'max_depth':        [None, 5, 10, 15, 20],
        'min_samples_leaf': randint(1, 10),
        'max_features':     ['sqrt', 'log2', 0.3],
        'class_weight':     [
            'balanced',
            {0: 1, 1: spw * 1.5},
            {0: 1, 1: spw * 2.0},
            {0: 1, 1: spw * 3.0},
        ],
    }

    sgkf = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=random_seed)
    rf_base = RandomForestClassifier(random_state=random_seed, n_jobs=-1)
    search = RandomizedSearchCV(
        rf_base, param_distributions=param_dist,
        n_iter=search_n_iter, cv=sgkf, scoring=search_scoring,
        random_state=random_seed, n_jobs=-1, verbose=0,
    )
    logger.info(f"  RandomizedSearchCV ({search_n_iter} iter, 5-fold, scoring={search_scoring})...")
    t0 = time.time()
    search.fit(X_arr, y, groups=groups)
    logger.info(f"  Search done in {time.time()-t0:.0f}s. "
                f"Best params: {search.best_params_}  "
                f"Best CV {search_scoring}: {search.best_score_:.4f}")

    best_rf = search.best_estimator_

    # GroupKFold CV with best estimator
    logger.info("  Running 5-fold GroupKFold CV with best estimator...")
    cv_df = group_cv(best_rf, X_arr, y, groups, n_splits=5, recall_target=recall_target)
    summary = summarize_cv(cv_df)
    logger.info(f"  CV: ROC-AUC={summary['roc_auc_mean']:.4f}±{summary['roc_auc_std']:.4f}  "
                f"Precision={summary['precision_mean']:.4f}  Recall={summary['recall_mean']:.4f}  "
                f"F1={summary['f1_opt_mean']:.4f}±{summary['f1_opt_std']:.4f}")
    cv_df.to_csv(output_dir / 'rf_cv_results.csv', index=False)

    # Refit on all data
    best_rf.fit(X_arr, y)

    # Permutation importance (on 20% holdout using last fold's test set)
    logger.info("  Computing permutation importance (n_repeats=20)...")
    sgkf2 = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=random_seed)
    splits = list(sgkf2.split(X_arr, y, groups))
    _, te_idx = splits[-1]  # use last fold's test set
    perm = permutation_importance(
        best_rf, X_arr[te_idx], y[te_idx],
        n_repeats=20, scoring='roc_auc', random_state=random_seed, n_jobs=-1
    )
    perm_df = pd.DataFrame({
        'feature':   feat_names,
        'importance_mean': perm.importances_mean,
        'importance_std':  perm.importances_std,
    }).sort_values('importance_mean', ascending=False).reset_index(drop=True)
    perm_df.to_csv(output_dir / 'rf_permutation_importance.csv', index=False)

    # ── Spearman deduplication ────────────────────────────────────────────────
    logger.info("  Spearman deduplication (|r|>0.60)...")
    dedup_names = spearman_dedup(
        X=X_arr, feat_names=feat_names,
        importance_arr=perm.importances_mean,
        output_dir=output_dir, tname=tname, model_tag='rf',
        threshold=0.60,
        priority_keep={'log_well_top_open_ft'},
        logger=logger,
    )
    dedup_idx = [feat_names.index(c) for c in dedup_names]
    X_dedup = X_arr[:, dedup_idx]
    dedup_importance = perm.importances_mean[dedup_idx]

    # ── RFE: select minimal feature subset ───────────────────────────────────
    logger.info("  Running RFE feature selection (permutation importance ranking)...")
    rfe_cols = rfe_select(
        estimator=best_rf,
        X=X_dedup, y=y, groups=groups,
        feat_names=dedup_names,
        importance_arr=dedup_importance,
        output_dir=output_dir, tname=tname, model_tag='rf',
        tolerance=rfe_tolerance, metric=rfe_metric, logger=logger,
    )
    logger.info(f"  RF RFE: {len(feat_names)} → dedup {len(dedup_names)} → RFE {len(rfe_cols)} features")

    # Refit best RF on RFE-selected features
    rfe_idx = [dedup_names.index(c) for c in rfe_cols]
    X_rfe = X_dedup[:, rfe_idx]
    best_rf.fit(X_rfe, y)

    # Re-evaluate CV on selected features
    logger.info("  Re-running 5-fold CV on RFE-selected features...")
    cv_df_rfe, oof_y_true, oof_y_pred, oof_y_prob, oof_groups = group_cv(
        best_rf, X_rfe, y, groups, n_splits=5,
        recall_target=recall_target, return_preds=True)
    summary_rfe = summarize_cv(cv_df_rfe)
    logger.info(f"  CV (RFE): ROC-AUC={summary_rfe['roc_auc_mean']:.4f}±{summary_rfe['roc_auc_std']:.4f}  "
                f"Precision={summary_rfe['precision_mean']:.4f}  Recall={summary_rfe['recall_mean']:.4f}  "
                f"F1={summary_rfe['f1_opt_mean']:.4f}±{summary_rfe['f1_opt_std']:.4f}")
    plot_cm(oof_y_true, oof_y_pred, output_dir, tname, 'rf', logger)
    pws_agg_metrics(oof_y_true, oof_y_pred, oof_y_prob, oof_groups,
                    recall_target, output_dir, tname, 'rf', logger)
    cv_df_rfe.to_csv(output_dir / 'rf_cv_results_rfe.csv', index=False)
    summary = summary_rfe
    cv_df   = cv_df_rfe

    # Recompute permutation importance on RFE feature set
    perm2 = permutation_importance(
        best_rf, X_rfe[te_idx], y[te_idx],
        n_repeats=20, scoring='roc_auc', random_state=random_seed, n_jobs=-1
    )
    perm_df = pd.DataFrame({
        'feature':         rfe_cols,
        'importance_mean': perm2.importances_mean,
        'importance_std':  perm2.importances_std,
    }).sort_values('importance_mean', ascending=False).reset_index(drop=True)
    perm_df.to_csv(output_dir / 'rf_permutation_importance_rfe.csv', index=False)
    feat_names = rfe_cols  # update for downstream use

    # Top 10 plot
    top10_df = perm_df.head(10).sort_values('importance_mean')
    fig, ax = plt.subplots(figsize=(9, 6))
    ax.barh(top10_df['feature'], top10_df['importance_mean'],
            xerr=top10_df['importance_std'], color='#2ca02c', alpha=0.8,
            capsize=4, error_kw={'linewidth': 1.5})
    ax.axvline(0, color='red', linestyle='--', linewidth=0.8, label='No effect')
    ax.set_xlabel('Mean AUC drop when feature shuffled')
    ax.set_title(f'RF Top 10 Features (Permutation Importance, RFE) — {tname.upper()}')
    ax.legend()
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'rf_permutation_importance.png', logger)

    # Prob distribution
    y_prob_all = best_rf.predict_proba(X_rfe)[:, 1]
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.hist(y_prob_all[y == 0], bins=40, alpha=0.6, label='Not detected', color='#1f77b4')
    ax.hist(y_prob_all[y == 1], bins=40, alpha=0.6, label='Detected', color='#d62728')
    ax.set_xlabel('Predicted Probability of Detection')
    ax.set_ylabel('Count')
    ax.set_title(f'RF Predicted Probability Distribution — {tname.upper()}')
    ax.legend()
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'rf_prob_distribution.png', logger)

    # Save model
    joblib.dump({'model': best_rf, 'features': rfe_cols},
                output_dir / 'models' / 'rf_model.joblib')

    # ROC/PR from pooled out-of-fold CV predictions — NOT y_prob_all
    # (in-sample refit), which inflates RF badly given its unbounded tree
    # depth and would make the plotted curve inconsistent with the CV AUC
    # shown in the legend.
    fpr, tpr, _ = roc_curve(oof_y_true, oof_y_prob)
    prec, rec, _ = precision_recall_curve(oof_y_true, oof_y_prob)

    return {
        'name': 'Random Forest',
        'cv': summary,
        'cv_df': cv_df,
        'fpr': fpr, 'tpr': tpr,
        'prec': prec, 'rec': rec,
        'roc_auc': roc_auc_score(oof_y_true, oof_y_prob),
        'pr_auc': auc(rec, prec),
        'top10_features': perm_df.head(10)['feature'].tolist(),
        'feature_importance': perm_df.set_index('feature')['importance_mean'].to_dict(),
        'n_features': len(rfe_cols),
        'best_params': search.best_params_,
    }


# ── XGBoost model ─────────────────────────────────────────────────────────────

def run_xgb(X_raw: pd.DataFrame, y: np.ndarray, groups: np.ndarray,
            scale_pos_weight: float, output_dir: Path, tname: str,
            random_seed: int, logger, recall_target: float = None,
            search_scoring: str = 'roc_auc', search_n_iter: int = 50,
            rfe_metric: str = 'roc_auc', rfe_tolerance: float = 0.005) -> dict:
    """
    XGBoost with scale_pos_weight, RandomizedSearchCV (GroupKFold),
    GroupKFold CV, SHAP summary plot, permutation importance.
    XGBoost handles NaN natively (X_raw used directly).
    """
    logger.info(f"\n{'='*60}")
    logger.info(f"[XGB] Running XGBoost for {tname}")
    logger.info(f"  scale_pos_weight = {scale_pos_weight:.2f}")

    X_arr = X_raw.values.astype(float)
    feat_names = list(X_raw.columns)

    param_dist = {
        'n_estimators':    randint(100, 500),
        'max_depth':       randint(3, 8),
        'learning_rate':   [0.01, 0.05, 0.1, 0.2],
        'subsample':       [0.6, 0.7, 0.8, 0.9],
        'colsample_bytree':[0.6, 0.7, 0.8, 0.9],
        'min_child_weight':randint(1, 10),
        'gamma':           [0, 0.1, 0.5, 1.0],
        'reg_alpha':       [0, 0.01, 0.1, 1.0],
        'reg_lambda':      [0.5, 1.0, 2.0],
        # Tune positive-class weight to shift recall/precision balance
        'scale_pos_weight': [
            scale_pos_weight,
            scale_pos_weight * 1.5,
            scale_pos_weight * 2.0,
            scale_pos_weight * 3.0,
        ],
    }

    sgkf = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=random_seed)
    xgb_base = xgb.XGBClassifier(
        eval_metric='logloss',
        random_state=random_seed,
        n_jobs=-1,
        tree_method='hist',
    )
    search = RandomizedSearchCV(
        xgb_base, param_distributions=param_dist,
        n_iter=search_n_iter, cv=sgkf, scoring=search_scoring,
        random_state=random_seed, n_jobs=1, verbose=0,
    )
    logger.info(f"  RandomizedSearchCV ({search_n_iter} iter, 5-fold, scoring={search_scoring})...")
    t0 = time.time()
    search.fit(X_arr, y, groups=groups)
    logger.info(f"  Search done in {time.time()-t0:.0f}s. "
                f"Best CV {search_scoring}: {search.best_score_:.4f}")
    logger.info(f"  Best params: {search.best_params_}")

    best_xgb = search.best_estimator_

    # GroupKFold CV with best estimator
    logger.info("  Running 5-fold GroupKFold CV with best estimator...")
    cv_df = group_cv(best_xgb, X_arr, y, groups, n_splits=5, recall_target=recall_target)
    summary = summarize_cv(cv_df)
    logger.info(f"  CV: ROC-AUC={summary['roc_auc_mean']:.4f}±{summary['roc_auc_std']:.4f}  "
                f"Precision={summary['precision_mean']:.4f}  Recall={summary['recall_mean']:.4f}  "
                f"F1={summary['f1_opt_mean']:.4f}±{summary['f1_opt_std']:.4f}")
    cv_df.to_csv(output_dir / 'xgb_cv_results.csv', index=False)

    # Refit on all data for SHAP
    best_xgb.fit(X_arr, y)

    # ── SHAP ──────────────────────────────────────────────────────────────
    logger.info("  Computing SHAP values...")
    explainer = shap.TreeExplainer(best_xgb)
    shap_values = explainer.shap_values(X_arr)

    # Mean |SHAP| per feature (top 10)
    mean_abs_shap = np.abs(shap_values).mean(axis=0)
    shap_df = pd.DataFrame({
        'feature':        feat_names,
        'mean_abs_shap':  mean_abs_shap,
    }).sort_values('mean_abs_shap', ascending=False).reset_index(drop=True)
    shap_df.to_csv(output_dir / 'xgb_shap_importance.csv', index=False)

    # ── Spearman deduplication ────────────────────────────────────────────────
    logger.info("  Spearman deduplication (|r|>0.60)...")
    dedup_names = spearman_dedup(
        X=X_arr, feat_names=feat_names,
        importance_arr=mean_abs_shap,
        output_dir=output_dir, tname=tname, model_tag='xgb',
        threshold=0.60,
        priority_keep={'log_well_top_open_ft'},
        logger=logger,
    )
    dedup_idx = [feat_names.index(c) for c in dedup_names]
    X_dedup = X_arr[:, dedup_idx]
    dedup_importance = mean_abs_shap[dedup_idx]

    # ── RFE: select minimal feature subset using SHAP ranking ────────────────
    logger.info("  Running RFE feature selection (SHAP importance ranking)...")
    rfe_cols = rfe_select(
        estimator=best_xgb,
        X=X_dedup, y=y, groups=groups,
        feat_names=dedup_names,
        importance_arr=dedup_importance,
        output_dir=output_dir, tname=tname, model_tag='xgb',
        tolerance=rfe_tolerance, metric=rfe_metric, logger=logger,
    )
    logger.info(f"  XGB RFE: {len(feat_names)} → dedup {len(dedup_names)} → RFE {len(rfe_cols)} features")

    # Refit best XGB on RFE-selected features
    rfe_idx = [dedup_names.index(c) for c in rfe_cols]
    X_rfe = X_dedup[:, rfe_idx]
    best_xgb.fit(X_rfe, y)

    # Re-evaluate CV on selected features
    logger.info("  Re-running 5-fold CV on RFE-selected features...")
    cv_df_rfe, oof_y_true, oof_y_pred, oof_y_prob, oof_groups = group_cv(
        best_xgb, X_rfe, y, groups, n_splits=5,
        recall_target=recall_target, return_preds=True)
    summary_rfe = summarize_cv(cv_df_rfe)
    logger.info(f"  CV (RFE): ROC-AUC={summary_rfe['roc_auc_mean']:.4f}±{summary_rfe['roc_auc_std']:.4f}  "
                f"Precision={summary_rfe['precision_mean']:.4f}  Recall={summary_rfe['recall_mean']:.4f}  "
                f"F1={summary_rfe['f1_opt_mean']:.4f}±{summary_rfe['f1_opt_std']:.4f}")
    plot_cm(oof_y_true, oof_y_pred, output_dir, tname, 'xgb', logger)
    pws_agg_metrics(oof_y_true, oof_y_pred, oof_y_prob, oof_groups,
                    recall_target, output_dir, tname, 'xgb', logger)
    cv_df_rfe.to_csv(output_dir / 'xgb_cv_results_rfe.csv', index=False)
    summary = summary_rfe
    cv_df   = cv_df_rfe

    # Recompute SHAP on RFE feature set
    logger.info("  Recomputing SHAP on RFE-selected features...")
    explainer_rfe = shap.TreeExplainer(best_xgb)
    shap_values_rfe = explainer_rfe.shap_values(X_rfe)
    mean_abs_shap_rfe = np.abs(shap_values_rfe).mean(axis=0)
    shap_df = pd.DataFrame({
        'feature':       rfe_cols,
        'mean_abs_shap': mean_abs_shap_rfe,
    }).sort_values('mean_abs_shap', ascending=False).reset_index(drop=True)
    shap_df.to_csv(output_dir / 'xgb_shap_importance_rfe.csv', index=False)
    feat_names = rfe_cols  # update for downstream

    top10_df = shap_df.head(10)

    # SHAP bar plot (top 10, RFE model)
    fig, ax = plt.subplots(figsize=(9, 6))
    top10_plot = top10_df.sort_values('mean_abs_shap')
    ax.barh(top10_plot['feature'], top10_plot['mean_abs_shap'],
            color='#ff7f0e', alpha=0.85)
    ax.set_xlabel('Mean |SHAP value| (average impact on prediction)')
    ax.set_title(f'XGBoost Top 10 Features (SHAP, RFE) — {tname.upper()}')
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'xgb_shap_bar.png', logger)

    # SHAP beeswarm — top 10 features
    top10_features = shap_df.head(min(10, len(rfe_cols)))['feature'].tolist()
    top10_idx_rfe  = [rfe_cols.index(f) for f in top10_features]
    fig, ax = plt.subplots(figsize=(10, 7))
    shap.summary_plot(
        shap_values_rfe[:, top10_idx_rfe],
        X_rfe[:, top10_idx_rfe],
        feature_names=top10_features,
        plot_type='dot',
        show=False,
        max_display=10,
    )
    plt.title(f'SHAP Summary (Beeswarm, RFE) — {tname.upper()}')
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'xgb_shap_beeswarm.png', logger)

    # Permutation importance (on last fold's test set, RFE features)
    logger.info("  Computing permutation importance on RFE features...")
    sgkf2 = StratifiedGroupKFold(n_splits=5, shuffle=True, random_state=random_seed)
    splits = list(sgkf2.split(X_rfe, y, groups))
    _, te_idx = splits[-1]
    perm = permutation_importance(
        best_xgb, X_rfe[te_idx], y[te_idx],
        n_repeats=20, scoring='roc_auc', random_state=random_seed, n_jobs=-1
    )
    perm_df = pd.DataFrame({
        'feature':         rfe_cols,
        'importance_mean': perm.importances_mean,
        'importance_std':  perm.importances_std,
    }).sort_values('importance_mean', ascending=False).reset_index(drop=True)
    perm_df.to_csv(output_dir / 'xgb_permutation_importance_rfe.csv', index=False)

    # Permutation importance plot (top 10)
    top10_perm = perm_df.head(10).sort_values('importance_mean')
    fig, ax = plt.subplots(figsize=(9, 6))
    ax.barh(top10_perm['feature'], top10_perm['importance_mean'],
            xerr=top10_perm['importance_std'], color='#9467bd', alpha=0.8,
            capsize=4, error_kw={'linewidth': 1.5})
    ax.axvline(0, color='red', linestyle='--', linewidth=0.8, label='No effect')
    ax.set_xlabel('Mean AUC drop when feature shuffled')
    ax.set_title(f'XGBoost Top 10 Features (Permutation, RFE) — {tname.upper()}')
    ax.legend()
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'xgb_permutation_importance.png', logger)

    noise_feats = perm_df[perm_df['importance_mean'] <= 0]['feature'].tolist()
    if noise_feats:
        logger.info(f"  Noise candidates after RFE (perm ≤ 0): {len(noise_feats)}")

    # Prob distribution
    y_prob_all = best_xgb.predict_proba(X_rfe)[:, 1]
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.hist(y_prob_all[y == 0], bins=40, alpha=0.6, label='Not detected', color='#1f77b4')
    ax.hist(y_prob_all[y == 1], bins=40, alpha=0.6, label='Detected', color='#d62728')
    ax.set_xlabel('Predicted Probability of Detection')
    ax.set_ylabel('Count')
    ax.set_title(f'XGBoost Predicted Probability Distribution — {tname.upper()}')
    ax.legend()
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'xgb_prob_distribution.png', logger)

    # Save model
    joblib.dump({'model': best_xgb, 'features': rfe_cols},
                output_dir / 'models' / 'xgb_model.joblib')

    # ROC/PR from pooled out-of-fold CV predictions — NOT y_prob_all
    # (in-sample refit), for consistency with the CV AUC shown in the legend
    # and with the RF/LR curves (see run_rf/run_lr for the same fix).
    fpr, tpr, _ = roc_curve(oof_y_true, oof_y_prob)
    prec, rec, _ = precision_recall_curve(oof_y_true, oof_y_prob)

    return {
        'name': 'XGBoost',
        'cv': summary,
        'cv_df': cv_df,
        'fpr': fpr, 'tpr': tpr,
        'prec': prec, 'rec': rec,
        'roc_auc': roc_auc_score(oof_y_true, oof_y_prob),
        'pr_auc': auc(rec, prec),
        'top10_features': top10_df['feature'].tolist(),
        'feature_importance': shap_df.set_index('feature')['mean_abs_shap'].to_dict(),
        'n_features': len(rfe_cols),
        'best_params': search.best_params_,
        'noise_features': noise_feats,
    }


# ── Comparison plots ──────────────────────────────────────────────────────────

def plot_comparison(results: list, y: np.ndarray, output_dir: Path,
                    tname: str, logger):
    """ROC curve, PR curve, and CV metrics bar chart for all 3 models."""
    colors = ['#1f77b4', '#2ca02c', '#ff7f0e']

    # ── ROC curves ────────────────────────────────────────────────────────
    fig, ax = plt.subplots(figsize=(8, 7))
    for r, color in zip(results, colors):
        roc_auc = r['cv']['roc_auc_mean']
        ax.plot(r['fpr'], r['tpr'], color=color, lw=2,
                label=f"{r['name']} (CV AUC={roc_auc:.3f}±{r['cv']['roc_auc_std']:.3f})")
    ax.plot([0, 1], [0, 1], 'k--', lw=1)
    ax.set_xlabel('False Positive Rate')
    ax.set_ylabel('True Positive Rate')
    ax.set_title(f'ROC Curves — {tname.upper()} Detection')
    ax.legend(loc='lower right')
    ax.grid(alpha=0.3)
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'comparison_roc.png', logger)

    # ── PR curves ─────────────────────────────────────────────────────────
    baseline = y.mean()
    fig, ax = plt.subplots(figsize=(8, 7))
    for r, color in zip(results, colors):
        pr_auc = r['cv']['pr_auc_mean']
        ax.plot(r['rec'], r['prec'], color=color, lw=2,
                label=f"{r['name']} (CV PR-AUC={pr_auc:.3f}±{r['cv']['pr_auc_std']:.3f})")
    ax.axhline(baseline, color='gray', linestyle='--', lw=1,
               label=f'Baseline (prevalence={baseline:.2f})')
    ax.set_xlabel('Recall')
    ax.set_ylabel('Precision')
    ax.set_title(f'Precision-Recall Curves — {tname.upper()} Detection')
    ax.legend(loc='upper right')
    ax.grid(alpha=0.3)
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'comparison_pr.png', logger)

    # ── CV metrics comparison bar chart ───────────────────────────────────
    metrics = ['roc_auc_mean', 'pr_auc_mean', 'f1_opt_mean']
    metric_labels = ['ROC-AUC', 'PR-AUC', 'F1 (optimal threshold)']
    x = np.arange(len(metrics))
    width = 0.25

    fig, ax = plt.subplots(figsize=(10, 6))
    for i, (r, color) in enumerate(zip(results, colors)):
        vals  = [r['cv'][m] for m in metrics]
        stds  = [r['cv'][m.replace('mean', 'std')] for m in metrics]
        ax.bar(x + i * width, vals, width, label=r['name'],
               color=color, alpha=0.85, yerr=stds, capsize=4,
               error_kw={'linewidth': 1.5})
    ax.set_xticks(x + width)
    ax.set_xticklabels(metric_labels)
    ax.set_ylim(0, 1.05)
    ax.set_ylabel('Score (5-fold CV mean ± std)')
    ax.set_title(f'Model Comparison — {tname.upper()} Detection')
    ax.legend()
    ax.grid(axis='y', alpha=0.3)
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'comparison_metrics_bar.png', logger)

    # ── CV results per fold (box / strip plot) ─────────────────────────────
    rows = []
    for r in results:
        for _, row in r['cv_df'].iterrows():
            rows.append({'Model': r['name'], 'ROC-AUC': row['roc_auc'],
                         'PR-AUC': row['pr_auc'], 'F1': row['f1_opt']})
    cv_long = pd.DataFrame(rows)

    fig, axes = plt.subplots(1, 3, figsize=(14, 5))
    for ax, metric in zip(axes, ['ROC-AUC', 'PR-AUC', 'F1']):
        sns.boxplot(data=cv_long, x='Model', y=metric, ax=ax,
                    palette=['#1f77b4', '#2ca02c', '#ff7f0e'])
        sns.stripplot(data=cv_long, x='Model', y=metric, ax=ax,
                      color='black', size=5, alpha=0.6, jitter=True)
        ax.set_title(f'{metric} per Fold')
        ax.set_xlabel('')
        ax.grid(axis='y', alpha=0.3)
    fig.suptitle(f'5-Fold CV Performance — {tname.upper()}', fontsize=13)
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'comparison_cv_folds.png', logger)


# ── Summary tables ────────────────────────────────────────────────────────────

def save_summary(results: list, output_dir: Path, tname: str, logger):
    """Save top-10 feature table and metrics summary CSV."""
    # Metrics summary
    rows = []
    for r in results:
        row = {'model': r['name'], 'n_features': r['n_features']}
        row.update(r['cv'])
        rows.append(row)
    metrics_df = pd.DataFrame(rows)
    metrics_df.to_csv(output_dir / 'metrics_summary.csv', index=False)
    logger.info(f"\n{'─'*60}")
    logger.info(f"METRICS SUMMARY — {tname.upper()}")
    logger.info(f"\n{metrics_df.to_string(index=False)}")

    # Top-10 feature comparison
    top10_rows = []
    for r in results:
        for rank, feat in enumerate(r['top10_features'], 1):
            top10_rows.append({'rank': rank, 'model': r['name'], 'feature': feat})
    top10_df = pd.DataFrame(top10_rows)
    top10_df.to_csv(output_dir / 'top10_features.csv', index=False)

    # Print top 10 per model
    logger.info(f"\nTOP 10 FEATURES BY MODEL — {tname.upper()}")
    for r in results:
        logger.info(f"\n  {r['name']}:")
        for i, f in enumerate(r['top10_features'], 1):
            logger.info(f"    {i:2d}. {f}")

    # Combined top-10 importance heatmap
    all_feats = sorted(set(f for r in results for f in r['top10_features']))
    heatmap_data = pd.DataFrame(index=all_feats, columns=[r['name'] for r in results])
    for r in results:
        imp = r['feature_importance']
        max_imp = max(imp.values()) if imp else 1.0
        for feat in all_feats:
            heatmap_data.loc[feat, r['name']] = imp.get(feat, 0.0) / max_imp

    heatmap_data = heatmap_data.astype(float)
    heatmap_data['_max'] = heatmap_data.max(axis=1)
    heatmap_data = heatmap_data.sort_values('_max', ascending=False).drop(columns='_max')

    fig, ax = plt.subplots(figsize=(8, max(5, len(all_feats) * 0.4)))
    sns.heatmap(heatmap_data, annot=True, fmt='.2f', cmap='YlOrRd',
                vmin=0, vmax=1, ax=ax, linewidths=0.5)
    ax.set_title(f'Normalized Feature Importance Across Models — {tname.upper()}\n'
                 f'(features appearing in top 10 of any model; scale 0–1 within each model)')
    plt.tight_layout()
    save_fig(fig, output_dir / 'plots' / 'feature_importance_heatmap.png', logger)


# ── Per-target orchestrator ───────────────────────────────────────────────────

def run_target(target_col: str, tname: str,
               X_raw: pd.DataFrame, X_imp: pd.DataFrame,
               y_all: pd.Series, groups_all: pd.Series,
               output_dir: Path, random_seed: int, logger,
               extra_drop_cols: list = None,
               recall_target: float = None):
    """Run LR + RF + XGB for one target label."""
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / 'plots').mkdir(exist_ok=True)
    (output_dir / 'models').mkdir(exist_ok=True)

    # Filter to labeled rows only
    labeled_mask = y_all.notna()
    y       = y_all[labeled_mask].values.astype(int)
    groups  = groups_all[labeled_mask].values
    X_r     = X_raw[labeled_mask].reset_index(drop=True)
    X_i     = X_imp[labeled_mask].reset_index(drop=True)

    # Drop target-specific columns (e.g., geochem for PFAS)
    if extra_drop_cols:
        drop = [c for c in extra_drop_cols if c in X_r.columns]
        if drop:
            logger.info(f"  Dropping {len(drop)} target-specific columns: {drop[:5]}{'...' if len(drop)>5 else ''}")
            X_r = X_r.drop(columns=drop)
            X_i = X_i.drop(columns=drop)

    n_pos = int(y.sum())
    n_neg = int((y == 0).sum())
    spw   = n_neg / n_pos  # scale_pos_weight for XGB
    logger.info(f"\n{'#'*60}")
    logger.info(f"TARGET: {tname.upper()}  |  "
                f"n={len(y)}  pos={n_pos} ({n_pos/len(y)*100:.1f}%)  neg={n_neg}")
    logger.info(f"Unique PWSIDs (groups): {len(np.unique(groups))}")
    logger.info(f"scale_pos_weight = {spw:.2f}")
    if recall_target:
        logger.info(f"Recall target: {recall_target:.0%}")

    results = []

    # LR baseline
    lr_res = run_lr(X_i, y, groups, spw, output_dir, tname, logger,
                    recall_target=recall_target)
    results.append(lr_res)

    # Random Forest
    rf_res = run_rf(X_i, y, groups, spw, output_dir, tname, random_seed, logger,
                    recall_target=recall_target)
    results.append(rf_res)

    # XGBoost
    xgb_res = run_xgb(X_r, y, groups, spw, output_dir, tname, random_seed, logger,
                       recall_target=recall_target)
    results.append(xgb_res)

    # Comparison plots
    logger.info(f"\n[PLOTS] Generating comparison plots for {tname}...")
    plot_comparison(results, y, output_dir, tname, logger)
    save_summary(results, output_dir, tname, logger)

    logger.info(f"\n[DONE] {tname.upper()} complete. Outputs in: {output_dir}")


# ── Load pre-prepared feature tables ──────────────────────────────────────────

def load_prepared_features(csv_path: Path, label_col: str, logger):
    """Load a pre-engineered feature table produced by 16_prepare_features_national.py.

    Returns X_raw (NaN preserved), X_imp (median-imputed), y, groups (PWSID).
    Feature engineering and model-specific drops are already applied.
    """
    logger.info(f"Loading pre-prepared features: {csv_path.name}...")
    df = pd.read_csv(csv_path, low_memory=False)
    logger.info(f"  Shape: {df.shape}")

    df = clr_transform_land_use(df, logger=logger)

    y      = pd.to_numeric(df[label_col], errors='coerce')
    groups = df['PWSID'].astype(str)

    feature_cols = [c for c in df.columns if c not in (label_col, 'PWSID')]
    X_raw = df[feature_cols].copy()

    col_medians = X_raw.median()
    X_imp = X_raw.copy()
    for col in X_imp.columns:
        if X_imp[col].isna().any():
            X_imp[col] = X_imp[col].fillna(col_medians[col])

    logger.info(f"  Feature columns: {X_raw.shape[1]}")
    logger.info(f"  NaN in X_raw:    {X_raw.isna().sum().sum()} cells")
    logger.info(f"  NaN in X_imp:    {X_imp.isna().sum().sum()} cells (should be 0)")
    n_pos = int(y.sum())
    n_neg = int((y == 0).sum())
    logger.info(f"  Labels: n={len(y)}  pos={n_pos}  neg={n_neg}")
    return X_raw, X_imp, y, groups


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description='PFAS and Lithium detection ML pipeline (national, pre-prepared features)'
    )
    parser.add_argument('--target', choices=['pfas', 'li', 'both'], default='both',
                        help='Which target to model (default: both)')
    parser.add_argument('--output-dir', type=str, default=str(BASE / 'ml_output_national'),
                        help='Root output directory')
    parser.add_argument('--random-seed', type=int, default=42)
    parser.add_argument('--bootstrap', action='store_true',
                        help='Enable bootstrap confidence intervals (slow)')
    parser.add_argument('--n-bootstrap', type=int, default=500,
                        help='Bootstrap iterations (default 500, only used with --bootstrap)')
    parser.add_argument('--pfas-recall', type=float, default=0.60,
                        help='Minimum recall target for PFAS model threshold (default 0.60)')
    parser.add_argument('--li-recall', type=float, default=0.80,
                        help='Minimum recall target for Li model threshold (default 0.80)')
    args = parser.parse_args()

    output_root = Path(args.output_dir)
    output_root.mkdir(parents=True, exist_ok=True)
    logger = setup_logging(output_root)

    logger.info("=== UCMR5 National ML Pipeline (pre-prepared features) ===")
    logger.info(f"Target: {args.target}  |  Seed: {args.random_seed}  |  "
                f"Bootstrap: {args.bootstrap}")

    # Feature engineering already done in 16_prepare_features_national.py.
    # Load each model's feature table separately (different columns/rows).
    t_total = time.time()

    if args.target in ('pfas', 'both'):
        t0 = time.time()
        X_raw_p, X_imp_p, y_p, groups_p = load_prepared_features(
            PFAS_FEATURES_CSV, 'pfas_detected', logger)
        run_target('pfas_detected', 'pfas', X_raw_p, X_imp_p,
                   y_p, groups_p, output_root / 'pfas',
                   args.random_seed, logger,
                   extra_drop_cols=None,
                   recall_target=args.pfas_recall)
        logger.info(f"[TIMER] PFAS finished in {(time.time()-t0)/60:.1f} min")

    if args.target in ('li', 'both'):
        t0 = time.time()
        X_raw_l, X_imp_l, y_l, groups_l = load_prepared_features(
            LI_FEATURES_CSV, 'li_detected', logger)
        run_target('li_detected', 'li', X_raw_l, X_imp_l,
                   y_l, groups_l, output_root / 'li',
                   args.random_seed, logger,
                   extra_drop_cols=None,
                   recall_target=args.li_recall)
        logger.info(f"[TIMER] Li finished in {(time.time()-t0)/60:.1f} min")

    logger.info(f"\n[TIMER] Total elapsed: {(time.time()-t_total)/60:.1f} min")
    logger.info(f"All outputs in: {output_root}")


if __name__ == '__main__':
    main()
