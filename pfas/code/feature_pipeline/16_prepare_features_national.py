"""
Prepare model-ready feature tables for PFAS and Li models.

Reads national_facility_features_v9.csv and applies all feature engineering
(source-water consolidation, log transforms, one-hot encoding), then saves:

  national_pfas_features_ready.csv — PFAS-labeled rows, PFAS model columns
  national_li_features_ready.csv   — Li-labeled rows, Li model columns

These files are the direct inputs to 15_ml_pipeline_national.py, which no
longer needs to do any feature engineering itself.

All feature engineering logic mirrors ml_pipeline.py exactly. Constants kept
here as a self-contained copy so this script has no dependency on that file.
"""

import numpy as np
import pandas as pd
from pathlib import Path

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
# ──────────────────────────────────────────────────────────────────────────────

IN_CSV         = BASE / 'national_facility_features_v9.csv'
PFAS_OUT_CSV   = BASE / 'national_pfas_features_ready.csv'
LI_OUT_CSV     = BASE / 'national_li_features_ready.csv'

# ── Feature lists (mirrors ml_pipeline.py) ────────────────────────────────────

DROP_ALWAYS = {
    'PWSID', 'FacilityID', 'FacilityName', 'PWSName',
    'huc12', 'huc12_name', 'AQ_NAME', 'AQ_CODE', 'ROCK_NAME',
    'coord_source', 'query_lat', 'query_lon',
    'seller_pwsid', 'seller_pws_name',
    'seller_primary_source_code', 'seller_gw_sw_code',
    'ultimate_seller_pwsid', 'ultimate_seller_source_code', 'ultimate_seller_gw_sw',
    'seller_huc12', 'seller_huc12_name',
    'Is Outstanding Performer',
    'geochem_U_nearest', 'geochem_U_mean', 'geochem_U_max',
    'seller_geochem_U_nearest', 'seller_geochem_U_mean', 'seller_geochem_U_max',
    'total_pixels', 'seller_total_pixels',
    'State',
    'pfas_detected', 'li_detected',
    'FacilityWaterType', 'ROCK_TYPE',
    # Superseded by dist_pfas_clustered_km (pooled nearest-of-either
    # discharge/manufacturer distance); kept raw in the CSV for reference
    # but must not leak into the model unprocessed (not in LOG_COLS anymore).
    'dist_discharge_km', 'dist_manufacturer_km',
    # battery_mfg distance dropped entirely (full-segment and restricted) —
    # SHAP dependence still reversed in the far tail even after restricting
    # to plausible segments, inconsistent with known contaminant-transport
    # distances (see ml_pipeline.py for full rationale). dist_coalplant_km
    # stays — smooth, monotonic SHAP dependence, no such reversal.
    'dist_battery_mfg_km', 'dist_battery_mfg_restricted_km',
}

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

LOG_COLS = [
    'Population Served Count', 'Service Connections Count',
    '# of Violations', '# of Site Visits',
    'dist_industry_km',
    'dist_fuds_km', 'dist_military_km',
    'count_industry_in_huc12', 'count_discharge_in_huc12',
    'count_manufacturer_in_huc12', 'count_fuds_in_huc12', 'count_military_in_huc12',
    'well_top_open_ft', 'well_screen_len_ft',
    'n_snsv',
    'dist_coalplant_km', 'dist_wwtp_km',
    'count_coalplants_in_huc12', 'count_battery_mfg_in_huc12', 'count_wwtp_in_huc12',
    'dist_airport_km', 'count_airports_in_huc12',
    'dist_landfill_km', 'count_landfills_in_huc12',
    # v10: discharge+manufacturer pooled into a nearest-of-either distance
    # (Tokranov et al. 2024 Science approach) — see ml_pipeline.py.
    'dist_pfas_clustered_km',
]

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

PFAS_EXTRA_DROP = [
    'source_geochem_n',
    'source_geochem_Li_nearest', 'source_geochem_Li_mean', 'source_geochem_Li_max', 'source_geochem_Li_count',
    'source_geochem_Fe_nearest', 'source_geochem_Fe_mean', 'source_geochem_Fe_max', 'source_geochem_Fe_count',
    'source_geochem_Al_nearest', 'source_geochem_Al_mean', 'source_geochem_Al_max', 'source_geochem_Al_count',
    'source_geochem_As_nearest', 'source_geochem_As_mean', 'source_geochem_As_max', 'source_geochem_As_count',
    'source_li_class_pub', 'source_li_lt4_pub', 'source_li_4_10_pub', 'source_li_10_30_pub', 'source_li_gt30_pub',
    'seller_geochem_U_count',
    'pfas_treat_any',
    'log_# of Violations', 'log_# of Site Visits',
    'log_n_snsv',
    'log_dist_coalplant_km',
    'log_count_coalplants_in_huc12', 'log_count_battery_mfg_in_huc12',
]

LI_EXTRA_DROP = [
    'log_dist_industry_km',       'log_count_industry_in_huc12',
    'log_dist_pfas_clustered_km',
    'log_count_discharge_in_huc12', 'log_count_manufacturer_in_huc12',
    'log_dist_fuds_km',           'log_count_fuds_in_huc12',
    'log_dist_military_km',       'log_count_military_in_huc12',
    'pfas_treat_any',
    'log_# of Violations', 'log_# of Site Visits',
    'log_n_snsv',
    'source_li_class_pub', 'source_li_lt4_pub', 'source_li_4_10_pub',
    'source_li_10_30_pub', 'source_li_gt30_pub',
    'log_dist_airport_km', 'log_count_airports_in_huc12',
]


def consolidate_source_features(df: pd.DataFrame) -> pd.DataFrame:
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
    return df


def engineer_features(df: pd.DataFrame) -> pd.DataFrame:
    """Apply all feature engineering. Returns df with feature columns + PWSID + labels."""
    print(f"  Input shape: {df.shape}")

    # Binary columns
    for col in BINARY_COLS:
        if col not in df.columns:
            continue
        s = df[col].astype(str).str.strip().str.lower()
        df[col] = s.map({'1': 1.0, '0': 0.0, 'yes': 1.0, 'no': 0.0,
                         'true': 1.0, 'false': 0.0}).astype(float)
        df[col] = df[col].fillna(0.0)

    df['seller_chain_depth'] = pd.to_numeric(
        df['seller_chain_depth'], errors='coerce').fillna(0.0)

    df['is_purchased'] = (
        df['seller_pwsid'].astype(str).str.strip().ne('') &
        df['seller_pwsid'].astype(str).str.strip().ne('nan')
    ).astype(float)

    df = consolidate_source_features(df)

    # Log1p transforms
    for col in LOG_COLS:
        if col not in df.columns:
            continue
        num = pd.to_numeric(df[col], errors='coerce').clip(lower=0)
        df[f'log_{col}'] = np.log1p(num)
    df.drop(columns=[c for c in LOG_COLS if c in df.columns], inplace=True)

    # One-hot: FacilityWaterType
    fwt = df['FacilityWaterType'].astype(str).str.strip().replace(
        {'nan': 'Unknown', '': 'Unknown'})
    fwt_dummies = pd.get_dummies(fwt, prefix='FWT', drop_first=False, dtype=float)
    df = pd.concat([df, fwt_dummies], axis=1)

    # One-hot: ROCK_TYPE
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

    # Parse remaining object columns to numeric
    non_numeric = DROP_ALWAYS | set(BINARY_COLS) | {
        'seller_chain_depth', 'is_purchased', 'PWSID', 'State',
    }
    for col in df.columns:
        if col in non_numeric or col in ('pfas_detected', 'li_detected'):
            continue
        if df[col].dtype == object:
            df[col] = pd.to_numeric(df[col], errors='coerce')

    return df


def save_model_table(df: pd.DataFrame, label_col: str,
                     extra_drop: list, out_path: Path) -> None:
    """Filter to labeled rows, apply model-specific drops, save CSV."""
    # Convert label to numeric, keep only rows with valid label
    y = pd.to_numeric(df[label_col], errors='coerce')
    labeled = y.notna()
    df_out = df[labeled].copy()
    y_out = y[labeled]

    n_pos = int((y_out == 1).sum())
    n_neg = int((y_out == 0).sum())
    print(f"  {label_col}: {len(df_out):,} labeled rows  "
          f"(+{n_pos} detected / -{n_neg} not detected)")

    # Feature columns: exclude DROP_ALWAYS, labels, but keep PWSID
    drop_cols = (DROP_ALWAYS - {'pfas_detected', 'li_detected'}) | {'pfas_detected', 'li_detected'}
    feature_cols = [c for c in df_out.columns
                    if c not in drop_cols and c != 'PWSID']

    # Apply model-specific extra drops
    extra = set(extra_drop)
    feature_cols = [c for c in feature_cols if c not in extra]

    out = df_out[['PWSID', label_col] + feature_cols].copy()
    out[label_col] = y_out.values

    out.to_csv(out_path, index=False)
    print(f"  Saved: {out_path.name}  ({len(out):,} rows × {len(out.columns)} cols)")
    print(f"  Feature columns: {len(feature_cols)}")
    print(f"  NaN cells in features: {out[feature_cols].isna().sum().sum():,}")


def main():
    print(f"Loading {IN_CSV.name}...")
    df = pd.read_csv(IN_CSV, low_memory=False)
    print(f"  Raw shape: {df.shape}")

    # Save labels and PWSID before engineering (DROP_ALWAYS removes them from feature set,
    # but we explicitly keep them as separate columns throughout)
    y_pfas = pd.to_numeric(df['pfas_detected'], errors='coerce')
    y_li   = pd.to_numeric(df['li_detected'],   errors='coerce')
    print(f"  PFAS labeled: {y_pfas.notna().sum():,}  "
          f"(+{int(y_pfas.sum())} / -{int((y_pfas == 0).sum())})")
    print(f"  Li labeled:   {y_li.notna().sum():,}  "
          f"(+{int(y_li.sum())} / -{int((y_li == 0).sum())})")

    print("\nEngineering features...")
    df = engineer_features(df)

    # Restore label columns after engineering (DROP_ALWAYS would remove them
    # if they were in the df, but engineer_features preserves them since
    # they're excluded from the drop set)
    print(f"  Post-engineering shape: {df.shape}")

    print(f"\nPreparing PFAS feature table...")
    save_model_table(df, 'pfas_detected', PFAS_EXTRA_DROP, PFAS_OUT_CSV)

    print(f"\nPreparing Li feature table...")
    save_model_table(df, 'li_detected', LI_EXTRA_DROP, LI_OUT_CSV)

    print("\nDone. Feature tables ready for 15_ml_pipeline_national.py:")
    print(f"  {PFAS_OUT_CSV}")
    print(f"  {LI_OUT_CSV}")


if __name__ == '__main__':
    main()
