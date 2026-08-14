"""
Build national_facility_huc12_aquifer.csv — base facility table for the national model.

Joins:
  UCMR5_All.txt               → unique (PWSID, FacilityID) rows for ALL states
  national_facility_huc12.csv → HUC-12 and coordinates per facility
                                 (Tier1=FRS facility, Tier2=SDWIS, Tier3=city centroid)
  pws_aquifer.csv             → principal aquifer name, code, rock type (per facility)

Output: national_facility_huc12_aquifer.csv
  Columns: PWSID, FacilityID, FacilityName, FacilityWaterType, State, PWSName,
           FacilityID_int, huc12, huc12_name, coord_source, query_lat, query_lon,
           AQ_NAME, AQ_CODE, ROCK_TYPE, ROCK_NAME

This is the national equivalent of midwest8_facility_huc12_aquifer.csv.
Run BEFORE any other national pipeline script.

NOTE: MIDWEST8 states are excluded here — they already have a finished feature
table (midwest8_facility_features_v9.csv). This script + steps 01-14 only build
the remaining states; 14b_merge_midwest8_national.py stitches both together
before step 16. AK/HI/territories are also excluded (see NO_RASTER_COVERAGE) —
no NLCD/aquifer/well-depth raster coverage there.
"""

from pathlib import Path
import pandas as pd

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE_DIR  = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
UCMR5_TXT = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/ucmr5-occurrence-data/UCMR5_All.txt')
# ──────────────────────────────────────────────────────────────────────────────

MIDWEST8 = {'IA', 'IL', 'IN', 'MI', 'MN', 'MO', 'OH', 'WI'}  # already built separately

# AK, HI, and the territories have 0% coverage from the NLCD land-cover raster
# (CONUS-only), USGS principal aquifer shapefile, and USGS well-depth grids
# (EPSG:5070 Albers, a CONUS-specific projection) — the two most important
# feature groups (well depth is the #1/#2 SHAP feature) would be pure NaN/noise
# for these facilities regardless of coordinate quality. Excluded for lack of
# raster coverage, not because of state/territory status per se — tribal
# EPA-region-coded facilities (State='01'..'10') are NOT excluded here since
# they're real mainland locations with comparable data quality to CONUS.
NO_RASTER_COVERAGE = {'AK', 'HI', 'PR', 'GU', 'AS', 'MP', 'VI'}

HUC12_CSV   = BASE_DIR / 'national_facility_huc12.csv'
AQUIFER_CSV = BASE_DIR / 'pws_aquifer.csv'
OUT_CSV     = BASE_DIR / 'national_facility_huc12_aquifer.csv'


def main():
    print("Loading UCMR5_All.txt...")
    ucmr = pd.read_csv(UCMR5_TXT, sep='\t', low_memory=False, encoding='latin1',
                       usecols=['PWSID', 'FacilityID', 'FacilityName',
                                'FacilityWaterType', 'State', 'PWSName'])
    # One row per (PWSID, FacilityID) — facility-level base table
    fac = ucmr.drop_duplicates(subset=['PWSID', 'FacilityID']).copy()
    fac['PWSID'] = fac['PWSID'].astype(str).str.strip()
    fac['FacilityID'] = fac['FacilityID'].astype(str).str.strip()
    fac['FacilityID_int'] = pd.to_numeric(fac['FacilityID'], errors='coerce')
    print(f"  {len(fac):,} unique facilities across {fac['State'].nunique()} states")

    n_before = len(fac)
    fac = fac[~fac['State'].isin(MIDWEST8)].copy()
    print(f"  Excluding MIDWEST8 (already built): dropped {n_before - len(fac):,} rows, "
          f"{len(fac):,} remain across {fac['State'].nunique()} states")

    n_before = len(fac)
    fac = fac[~fac['State'].isin(NO_RASTER_COVERAGE)].copy()
    print(f"  Excluding AK/HI/territories (no raster coverage): dropped "
          f"{n_before - len(fac):,} rows, {len(fac):,} remain across {fac['State'].nunique()} states")

    print("Loading national_facility_huc12.csv...")
    huc12 = pd.read_csv(HUC12_CSV, low_memory=False,
                        usecols=['PWSID', 'FacilityID', 'query_lat', 'query_lon',
                                 'coord_source', 'huc12', 'huc12_name'])
    huc12['PWSID'] = huc12['PWSID'].astype(str).str.strip()
    huc12['FacilityID'] = huc12['FacilityID'].astype(str).str.strip()
    print(f"  {len(huc12):,} facility HUC-12 records")

    print("Loading pws_aquifer.csv...")
    aq = pd.read_csv(AQUIFER_CSV, low_memory=False,
                     usecols=['PWSID', 'FacilityID', 'AQ_NAME', 'AQ_CODE', 'ROCK_TYPE', 'ROCK_NAME'])
    aq['PWSID'] = aq['PWSID'].astype(str).str.strip()
    aq['FacilityID'] = aq['FacilityID'].astype(str).str.strip()
    print(f"  {len(aq):,} facilities with aquifer data")

    print("Joining...")
    # Facility-level join for coordinates, HUC-12, and aquifer — a well's
    # aquifer/rock type is a property of its own location, not its PWSID
    # (facilities under the same PWSID can sit in different HUC-12s/aquifers)
    df = fac.merge(huc12, on=['PWSID', 'FacilityID'], how='left')
    df = df.merge(aq, on=['PWSID', 'FacilityID'], how='left')

    n_huc12_matched = df['huc12'].notna().sum()
    n_aq_matched    = df['AQ_CODE'].notna().sum()
    print(f"  HUC-12 matched: {n_huc12_matched:,} / {len(df):,} facilities")
    print(f"  Aquifer matched: {n_aq_matched:,} / {len(df):,} facilities")
    if n_huc12_matched > 0:
        print(f"  Coord source breakdown:")
        print(df['coord_source'].value_counts().to_string())

    # Normalize huc12 to 12-digit zero-padded string
    df['huc12'] = df['huc12'].apply(
        lambda v: str(int(float(v))).zfill(12)
        if pd.notna(v) and str(v).strip() not in ('', 'nan') else ''
    )

    col_order = ['PWSID', 'FacilityID', 'FacilityName', 'FacilityWaterType',
                 'State', 'PWSName', 'FacilityID_int', 'huc12', 'huc12_name',
                 'coord_source', 'query_lat', 'query_lon',
                 'AQ_NAME', 'AQ_CODE', 'ROCK_TYPE', 'ROCK_NAME']
    df = df[col_order]

    df.to_csv(OUT_CSV, index=False)
    print(f"\nSaved: {OUT_CSV}")
    print(f"  Shape: {df.shape}")
    print(f"  States: {sorted(df['State'].dropna().unique())}")


if __name__ == '__main__':
    main()
