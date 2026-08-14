#!/usr/bin/env python3
"""
Build the Southeast-only PFAS training set by filtering the full national
ready dataset (../../../pfas/data/national_pfas_features_ready.csv) down to
PWS in a 12-state Southeast region (South Atlantic + East South Central
Census divisions, minus DE/MD/DC which are more commonly grouped with the
Mid-Atlantic/Northeast than the Southeast):
  Alabama, Arkansas, Florida, Georgia, Kentucky, Louisiana, Mississippi,
  North Carolina, South Carolina, Tennessee, Virginia, West Virginia

NC is included in training (there is no PWSID overlap with the NC STATE
validation set used for testing here -- that validation set is built from
NC's own state PFAS monitoring program, entirely independent PWS from
UCMR5-monitored systems).

Usage:
  python3 filter_region.py
Output:
  ../../data/national_pfas_features_ready.csv (same filename/schema as the
  national dataset -- the modeling scripts import 15_ml_pipeline_national.py,
  which resolves this path relative to its own location, so no other code
  changes are needed to point training at this regional subset).
"""
from pathlib import Path

import pandas as pd

HERE = Path(__file__).resolve().parent
NATIONAL_CSV = HERE.parent.parent.parent / 'pfas' / 'data' / 'national_pfas_features_ready.csv'
OUT_CSV = HERE.parent.parent / 'data' / 'national_pfas_features_ready.csv'

SOUTHEAST_STATES = ['AL', 'AR', 'FL', 'GA', 'KY', 'LA', 'MS', 'NC', 'SC', 'TN', 'VA', 'WV']


def main():
    df = pd.read_csv(NATIONAL_CSV, low_memory=False)
    df['state'] = df['PWSID'].str[:2]
    region = df[df['state'].isin(SOUTHEAST_STATES)].drop(columns=['state'])
    print(f"Southeast subset: {len(region):,} rows, {region['PWSID'].nunique():,} PWS, "
          f"{region['pfas_detected'].sum():.0f} positive ({region['pfas_detected'].mean()*100:.1f}%)")
    print(df[df['PWSID'].str[:2].isin(SOUTHEAST_STATES)]['PWSID'].str[:2].value_counts().to_string())
    region.to_csv(OUT_CSV, index=False)
    print(f"Saved: {OUT_CSV}")


if __name__ == '__main__':
    main()
