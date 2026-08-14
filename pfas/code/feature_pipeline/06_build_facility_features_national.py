"""
Join all HUC-12 geo features, PFAS distances, and PWS-level features to national facility table.

KEY CHANGES from midwest version:
  - Base table: national_facility_huc12_aquifer.csv
  - PFAS distances: pfas_distance_national.csv
  - HUC-12 feature tables: *_national.csv variants
  - Output: national_facility_features.csv

Sources:
  - national_facility_huc12_aquifer.csv  (base: PWSID, FacilityID, coords, aquifer)
  - huc12_landuse_national.csv           (land use % by HUC-12)
  - huc12_geochem_national.csv           (geochem stats by HUC-12)
  - pfas_distance_national.csv           (PFAS source distances by facility)
  - SDWA_latest_downloads.zip            (PWS-level: population, violations, water type, etc.)
"""

import csv
import zipfile
import pandas as pd
from pathlib import Path

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE     = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
SDWA_ZIP = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/SDWA_latest_downloads.zip')
# ──────────────────────────────────────────────────────────────────────────────


def load_pws_features(sdwa_zip):
    """Build per-PWSID feature dict from SDWA_latest_downloads.zip."""
    with zipfile.ZipFile(sdwa_zip) as zf:
        with zf.open('SDWA_PUB_WATER_SYSTEMS.csv') as f:
            pws = pd.read_csv(f, low_memory=False, usecols=[
                'SUBMISSIONYEARQUARTER', 'PWSID', 'PWS_TYPE_CODE',
                'POPULATION_SERVED_COUNT', 'SERVICE_CONNECTIONS_COUNT',
                'IS_WHOLESALER_IND', 'PRIMARY_SOURCE_CODE',
                'OUTSTANDING_PERFORMER', 'SOURCE_WATER_PROTECTION_CODE',
            ])
        # Keep most recent quarterly submission per PWSID
        pws = (pws.sort_values('SUBMISSIONYEARQUARTER', ascending=False)
                  .drop_duplicates('PWSID', keep='first'))
        pws['PWSID'] = pws['PWSID'].astype(str).str.strip()

        with zf.open('SDWA_FACILITIES.csv') as f:
            fac = pd.read_csv(f, low_memory=False, usecols=['PWSID'])
        fac['PWSID'] = fac['PWSID'].astype(str).str.strip()
        fac_counts = fac.groupby('PWSID').size().reset_index(name='n_facilities')

        with zf.open('SDWA_VIOLATIONS_ENFORCEMENT.csv') as f:
            viol = pd.read_csv(f, low_memory=False, usecols=['PWSID'])
        viol['PWSID'] = viol['PWSID'].astype(str).str.strip()
        viol_counts = viol.groupby('PWSID').size().reset_index(name='n_violations')

        with zf.open('SDWA_SITE_VISITS.csv') as f:
            sv = pd.read_csv(f, low_memory=False, usecols=['PWSID'])
        sv['PWSID'] = sv['PWSID'].astype(str).str.strip()
        sv_counts = sv.groupby('PWSID').size().reset_index(name='n_site_visits')

        with zf.open('SDWA_SERVICE_AREAS.csv') as f:
            sa = pd.read_csv(f, low_memory=False, usecols=['PWSID', 'SERVICE_AREA_TYPE_CODE'])
        sa['PWSID'] = sa['PWSID'].astype(str).str.strip()
        sa_sets = sa.groupby('PWSID')['SERVICE_AREA_TYPE_CODE'].apply(set).reset_index()

    merged = (pws.merge(fac_counts,  on='PWSID', how='left')
                 .merge(viol_counts, on='PWSID', how='left')
                 .merge(sv_counts,   on='PWSID', how='left')
                 .merge(sa_sets,     on='PWSID', how='left'))

    result = {}
    for _, row in merged.iterrows():
        pid      = str(row['PWSID']).strip()
        pws_type = str(row.get('PWS_TYPE_CODE', '')).strip()
        src_code = str(row.get('PRIMARY_SOURCE_CODE', '')).strip()
        swp      = str(row.get('SOURCE_WATER_PROTECTION_CODE', '')).strip()
        areas    = row.get('SERVICE_AREA_TYPE_CODE', set())
        if not isinstance(areas, set):
            areas = set()
        n_fac  = int(row['n_facilities'])  if pd.notna(row.get('n_facilities'))  else 0
        n_viol = int(row['n_violations'])  if pd.notna(row.get('n_violations'))  else 0
        n_sv   = int(row['n_site_visits']) if pd.notna(row.get('n_site_visits')) else 0
        result[pid] = {
            'Is Wholesaler':              '1' if str(row.get('IS_WHOLESALER_IND', '')).upper() == 'Y' else '0',
            'Population Served Count':    str(row.get('POPULATION_SERVED_COUNT', '')),
            'Is Outstanding Performer':   '1' if str(row.get('OUTSTANDING_PERFORMER', '')).upper() == 'Y' else '0',
            'Is Source Water Protected':  '1' if swp not in ('', 'nan') else '0',
            'Service Connections Count':  str(row.get('SERVICE_CONNECTIONS_COUNT', '')),
            '# of Facilities':            str(n_fac),
            '# of Violations':            str(n_viol),
            '# of Site Visits':           str(n_sv),
            'PWS Type_Community water system':             '1' if pws_type == 'CWS'    else '0',
            'PWS Type_Non-Transient non-community system': '1' if pws_type == 'NTNCWS' else '0',
            'PWS Type_Transient non-community system':     '1' if pws_type == 'TNCWS'  else '0',
            'Primary Source_Ground water':                            '1' if src_code == 'GW'  else '0',
            'Primary Source_Ground water purchased':                  '1' if src_code == 'GWP' else '0',
            'Primary Source_Groundwater under influence of surface water': '1' if src_code == 'GU'  else '0',
            'Primary Source_Surface water':                           '1' if src_code == 'SW'  else '0',
            'Primary Source_Surface water purchased':                 '1' if src_code == 'SWP' else '0',
            'Service Area_Municipality':           '1' if 'MU' in areas else '0',
            'Service Area_Residential Area':       '1' if 'RA' in areas else '0',
            'Service Area_Institution':            '1' if 'IN' in areas else '0',
            'Service Area_School':                 '1' if 'SC' in areas else '0',
            'Service Area_Industrial/Agricultural':'1' if 'IA' in areas else '0',
        }
    return result


def load_csv_by_key(path, key_col, strip_zero_pad=False):
    data = {}
    with open(path, newline='', encoding='utf-8-sig') as f:
        for row in csv.DictReader(f):
            k = row[key_col].strip()
            if strip_zero_pad:
                k = str(int(float(k))).zfill(12) if k and k != 'nan' else k
            data[k] = row
    return data


def normalize_huc12(val):
    val = str(val).strip()
    if not val or val == 'nan':
        return ''
    try:
        return str(int(float(val))).zfill(12)
    except ValueError:
        return val


def main():
    print("Loading base facility table...")
    facilities = []
    with open(BASE / 'national_facility_huc12_aquifer.csv', newline='') as f:
        for row in csv.DictReader(f):
            row['huc12_key'] = normalize_huc12(row['huc12'])
            facilities.append(row)
    print(f"  {len(facilities):,} facilities")

    print("Loading HUC-12 geo feature tables (national)...")
    landuse = load_csv_by_key(BASE / 'huc12_landuse_national.csv',  'huc12')
    geochem = load_csv_by_key(BASE / 'huc12_geochem_national.csv',  'huc12')
    # huc12_soil_li is optional; skip if not present (Li rasters may not be on HPC)
    soil_li = {}
    soil_li_path = BASE / 'huc12_soil_li.csv'
    if soil_li_path.exists():
        soil_li = load_csv_by_key(soil_li_path, 'huc12')
        print(f"  soil_li: {len(soil_li):,} HUC-12s")
    else:
        print("  huc12_soil_li.csv not found — skipping Li raster features")

    print("Loading PFAS distance table (national, facility level)...")
    pfas_dist = {}
    with open(BASE / 'pfas_distance_national.csv', newline='') as f:
        for row in csv.DictReader(f):
            k = (row['PWSID'].strip(), row['FacilityID'].strip())
            pfas_dist[k] = row

    print("Loading PWS features from SDWA_latest_downloads.zip...")
    pws_features = load_pws_features(SDWA_ZIP)
    print(f"  {len(pws_features):,} PWS records loaded")

    pws_cols = [
        'Is Wholesaler', 'Population Served Count', 'Is Outstanding Performer',
        'Is Source Water Protected', 'Service Connections Count',
        '# of Facilities', '# of Violations', '# of Site Visits',
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

    sample_lu = next(iter(landuse.values()))
    sample_gc = next(iter(geochem.values()))

    base_cols = ['PWSID', 'FacilityID', 'FacilityName', 'FacilityWaterType',
                 'State', 'PWSName', 'huc12', 'huc12_name',
                 'coord_source', 'query_lat', 'query_lon',
                 'AQ_NAME', 'AQ_CODE', 'ROCK_TYPE', 'ROCK_NAME']
    lu_cols   = [c for c in sample_lu if c != 'huc12']
    gc_cols   = [c for c in sample_gc if c != 'huc12']
    sl_cols   = ([c for c in next(iter(soil_li.values())) if c != 'huc12']
                 if soil_li else [])
    dist_cols = ['dist_industry_km', 'dist_discharge_km', 'dist_manufacturer_km',
                 'dist_fuds_km', 'dist_military_km']

    all_cols = base_cols + pws_cols + lu_cols + gc_cols + sl_cols + dist_cols

    print("Joining...")
    joined = []
    n_lu = n_gc = n_sl = n_dist = n_pws = 0
    for fac in facilities:
        h = fac['huc12_key']
        row = {c: fac.get(c, '') for c in base_cols}
        row['huc12'] = h

        pws_row = pws_features.get(fac['PWSID'].strip(), {})
        if pws_row:
            n_pws += 1
        for c in pws_cols:
            row[c] = pws_row.get(c, '')

        lu_row = landuse.get(h, {})
        if lu_row:
            n_lu += 1
        for c in lu_cols:
            row[c] = lu_row.get(c, '')

        gc_row = geochem.get(h, {})
        if gc_row:
            n_gc += 1
        for c in gc_cols:
            row[c] = gc_row.get(c, '')

        if sl_cols:
            sl_row = soil_li.get(h, {})
            if sl_row:
                n_sl += 1
            for c in sl_cols:
                row[c] = sl_row.get(c, '')

        dist_key = (fac['PWSID'].strip(), fac['FacilityID'].strip())
        dist_row = pfas_dist.get(dist_key, {})
        if dist_row:
            n_dist += 1
        for c in dist_cols:
            row[c] = dist_row.get(c, '')

        joined.append(row)

    out_path = BASE / 'national_facility_features.csv'
    with open(out_path, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=all_cols, extrasaction='ignore')
        w.writeheader()
        w.writerows(joined)

    print(f"\nOutput: {out_path}")
    print(f"  Total rows : {len(joined):,}")
    print(f"  PWS feats  : {n_pws:,} matched")
    print(f"  Land use   : {n_lu:,} matched")
    print(f"  Geochem    : {n_gc:,} matched")
    if sl_cols:
        print(f"  Soil Li    : {n_sl:,} matched")
    print(f"  PFAS dist  : {n_dist:,} matched")
    print(f"  Columns    : {len(all_cols)}")


if __name__ == '__main__':
    main()
