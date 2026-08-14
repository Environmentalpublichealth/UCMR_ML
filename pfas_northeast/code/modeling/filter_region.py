#!/usr/bin/env python3
"""
Build the Northeast-only PFAS training set by filtering the full national
ready dataset (../../../pfas/data/national_pfas_features_ready.csv) down to
PWS in the 9-state Census Bureau Northeast region.

Region definition (US Census Bureau Northeast, all 9 states):
  Connecticut, Maine, Massachusetts, New Hampshire, New Jersey, New York,
  Pennsylvania, Rhode Island, Vermont

PA is included in training (it's a US Census Bureau Northeast state and
there is no PWSID overlap with the PA STATE validation set used for
testing here -- that validation set is built from PA's own state PFAS
monitoring program, entirely independent PWS from UCMR5-monitored systems).

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

NORTHEAST_STATES = ['CT', 'ME', 'MA', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT']


def main():
    df = pd.read_csv(NATIONAL_CSV, low_memory=False)
    df['state'] = df['PWSID'].str[:2]
    region = df[df['state'].isin(NORTHEAST_STATES)].drop(columns=['state'])
    print(f"Northeast subset: {len(region):,} rows, {region['PWSID'].nunique():,} PWS, "
          f"{region['pfas_detected'].sum():.0f} positive ({region['pfas_detected'].mean()*100:.1f}%)")
    print(df[df['PWSID'].str[:2].isin(NORTHEAST_STATES)]['PWSID'].str[:2].value_counts().to_string())
    region.to_csv(OUT_CSV, index=False)
    print(f"Saved: {OUT_CSV}")


if __name__ == '__main__':
    main()
