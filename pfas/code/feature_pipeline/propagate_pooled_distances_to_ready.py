#!/usr/bin/env python3
"""
Propagate the pooled dist_pfas_clustered_km / dist_li_clustered_km columns
(computed directly on national_facility_features_v9.csv — see the note in
ml_pipeline.py's LOG_COLS) into the two "ready" feature tables that
15_ml_pipeline_national.py's main() actually trains on. main() does not
apply LOG_COLS/DROP_ALWAYS itself (see load_prepared_features()), so this
positional-propagation pattern (same as replace_li_geochem_with_soil_kriging.py)
is required for the fix to take effect in training.

PFAS ready file: drop log_dist_discharge_km, log_dist_manufacturer_km;
add log_dist_pfas_clustered_km = log1p(dist_pfas_clustered_km).

Li ready file: drop log_dist_coalplant_km, log_dist_battery_mfg_km;
add log_dist_li_clustered_km = log1p(dist_li_clustered_km).

Data-only — does not retrain.
"""
from pathlib import Path

import numpy as np
import pandas as pd

BASE = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
FACILITY_CSV = BASE / 'national_facility_features_v9.csv'
PFAS_READY_CSV = BASE / 'national_pfas_features_ready.csv'
LI_READY_CSV = BASE / 'national_li_features_ready.csv'


def main():
    nat = pd.read_csv(FACILITY_CSV, low_memory=False)

    # ── PFAS ready file ──────────────────────────────────────────────────
    print("=== PFAS ready file ===")
    ready = pd.read_csv(PFAS_READY_CSV, low_memory=False)
    mask = nat['pfas_detected'].notna()
    assert mask.sum() == len(ready), f"Row count mismatch: {mask.sum()} vs {len(ready)}"
    v9_pwsid = nat.loc[mask, 'PWSID'].astype(str).reset_index(drop=True)
    ready_pwsid = ready['PWSID'].astype(str).reset_index(drop=True)
    assert (v9_pwsid == ready_pwsid).all(), "PWSID alignment mismatch"

    log_pooled = np.log1p(nat.loc[mask, 'dist_pfas_clustered_km'].clip(lower=0)).reset_index(drop=True)

    backup = PFAS_READY_CSV.with_name(PFAS_READY_CSV.stem + '.csv.bak_pre_pooled_distances')
    if not backup.exists():
        ready.to_csv(backup, index=False)
        print(f"  Backed up to {backup}")

    dropped = [c for c in ['log_dist_discharge_km', 'log_dist_manufacturer_km'] if c in ready.columns]
    ready = ready.drop(columns=dropped)
    ready['log_dist_pfas_clustered_km'] = log_pooled.values
    print(f"  Dropped: {dropped}")
    print(f"  Added log_dist_pfas_clustered_km: coverage={ready['log_dist_pfas_clustered_km'].notna().mean()*100:.1f}%, "
          f"median={ready['log_dist_pfas_clustered_km'].median():.3f}")
    ready.to_csv(PFAS_READY_CSV, index=False)
    print(f"  Saved {PFAS_READY_CSV}")

    # ── Li ready file ────────────────────────────────────────────────────
    print("\n=== Li ready file ===")
    ready = pd.read_csv(LI_READY_CSV, low_memory=False)
    mask = nat['li_detected'].notna()
    assert mask.sum() == len(ready), f"Row count mismatch: {mask.sum()} vs {len(ready)}"
    v9_pwsid = nat.loc[mask, 'PWSID'].astype(str).reset_index(drop=True)
    ready_pwsid = ready['PWSID'].astype(str).reset_index(drop=True)
    assert (v9_pwsid == ready_pwsid).all(), "PWSID alignment mismatch"

    log_pooled = np.log1p(nat.loc[mask, 'dist_li_clustered_km'].clip(lower=0)).reset_index(drop=True)

    backup = LI_READY_CSV.with_name(LI_READY_CSV.stem + '.csv.bak_pre_pooled_distances')
    if not backup.exists():
        ready.to_csv(backup, index=False)
        print(f"  Backed up to {backup}")

    dropped = [c for c in ['log_dist_coalplant_km', 'log_dist_battery_mfg_km'] if c in ready.columns]
    ready = ready.drop(columns=dropped)
    ready['log_dist_li_clustered_km'] = log_pooled.values
    print(f"  Dropped: {dropped}")
    print(f"  Added log_dist_li_clustered_km: coverage={ready['log_dist_li_clustered_km'].notna().mean()*100:.1f}%, "
          f"median={ready['log_dist_li_clustered_km'].median():.3f}")
    ready.to_csv(LI_READY_CSV, index=False)
    print(f"  Saved {LI_READY_CSV}")


if __name__ == '__main__':
    main()
