"""
Merge the already-built MIDWEST8 feature table with the newly-built 42-state
national table into the final 50-state national_facility_features_v9.csv that
step 16 (prepare_features) reads.

Inputs:
  midwest8_facility_features_v9.csv        (already built, 8 states)
  national_facility_features_v9_42states.csv (built by steps 00-14, remaining 42 states)

Output:
  national_facility_features_v9.csv (all 50 states)
"""

from pathlib import Path
import pandas as pd

BASE = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')

MIDWEST8_CSV = BASE / 'midwest8_facility_features_v9.csv'
NATIONAL42_CSV = BASE / 'national_facility_features_v9_42states.csv'
OUT_CSV = BASE / 'national_facility_features_v9.csv'

MIDWEST8 = {'IA', 'IL', 'IN', 'MI', 'MN', 'MO', 'OH', 'WI'}


def main():
    print(f"Loading {MIDWEST8_CSV.name}...")
    mw = pd.read_csv(MIDWEST8_CSV, low_memory=False)
    print(f"  {len(mw):,} rows, {mw.shape[1]} cols, states: {sorted(mw['State'].dropna().unique())}")

    print(f"Loading {NATIONAL42_CSV.name}...")
    nat = pd.read_csv(NATIONAL42_CSV, low_memory=False)
    print(f"  {len(nat):,} rows, {nat.shape[1]} cols, {nat['State'].nunique()} states")

    overlap = set(nat['State'].dropna().unique()) & MIDWEST8
    if overlap:
        raise ValueError(f"42-state table still contains MIDWEST8 states: {overlap} "
                          f"— check step 00's MIDWEST8 filter")

    mw_cols, nat_cols = set(mw.columns), set(nat.columns)
    if mw_cols != nat_cols:
        raise ValueError(f"Column mismatch between tables.\n"
                          f"  In midwest8 only: {mw_cols - nat_cols}\n"
                          f"  In national42 only: {nat_cols - mw_cols}")

    combined = pd.concat([mw, nat[mw.columns.tolist()]], ignore_index=True)
    dupes = combined.duplicated(subset=['PWSID', 'FacilityID']).sum()
    if dupes:
        raise ValueError(f"{dupes} duplicate (PWSID, FacilityID) rows after merge — "
                          f"check for state overlap")

    combined.to_csv(OUT_CSV, index=False)
    print(f"\nSaved: {OUT_CSV}")
    print(f"  Shape: {combined.shape}")
    print(f"  States: {combined['State'].nunique()} "
          f"({len(mw)} midwest8 rows + {len(nat)} other-state rows)")


if __name__ == '__main__':
    main()
