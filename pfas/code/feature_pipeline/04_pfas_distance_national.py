"""
Calculate minimum distance (km) from each PWS (all US states) to each PFAS source.

KEY CHANGE from midwest version: no state filter — processes ALL states.

Output: pfas_distance_national.csv
  PWSID, FacilityID, FacilityName, State, LAT, LON,
  dist_industry_km, dist_discharge_km, dist_manufacturer_km,
  dist_fuds_km, dist_military_km
"""

import csv, sys
from pathlib import Path
import numpy as np
import openpyxl
from scipy.spatial import cKDTree

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE_DIR = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
ROOT_DIR = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR')
SRC_DIR  = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/PFASsource')
# ──────────────────────────────────────────────────────────────────────────────

FACILITY_CSV  = BASE_DIR / 'national_facility_huc12_aquifer.csv'
OUT_CSV       = BASE_DIR / 'pfas_distance_national.csv'
EARTH_KM      = 6371.0


def haversine_km(lat1, lon1, lat2, lon2):
    lat1, lon1, lat2, lon2 = (np.radians(x) for x in (lat1, lon1, lat2, lon2))
    dlat, dlon = lat2 - lat1, lon2 - lon1
    a = np.sin(dlat/2)**2 + np.cos(lat1)*np.cos(lat2)*np.sin(dlon/2)**2
    return 2 * EARTH_KM * np.arcsin(np.sqrt(a))


def haversine_min_dist(query_coords, source_coords):
    tree = cKDTree(np.radians(source_coords))
    _, idx = tree.query(np.radians(query_coords), k=1)
    nn = source_coords[idx]
    return haversine_km(query_coords[:, 0], query_coords[:, 1],
                        nn[:, 0], nn[:, 1])


def load_pws(path):
    """Load all facilities (no state filter)."""
    pws = []
    with open(path, newline='', encoding='utf-8-sig') as f:
        for row in csv.DictReader(f):
            try:
                lat = float(row['query_lat'])
                lon = float(row['query_lon'])
            except (ValueError, TypeError):
                lat = lon = float('nan')
            pws.append({
                'PWSID':        row['PWSID'].strip(),
                'FacilityID':   row['FacilityID'].strip(),
                'FacilityName': row.get('FacilityName', '').strip(),
                'State':        row.get('State', '').strip(),
                'lat': lat, 'lon': lon,
            })
    print(f"  {len(pws):,} facilities loaded across all states")
    return pws


def load_xlsx_coords(path, lat_col, lon_col, sheet=0):
    wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
    ws = wb.worksheets[sheet]
    rows = list(ws.iter_rows(values_only=True))
    header = [str(c).strip() if c else '' for c in rows[0]]
    li = header.index(lat_col)
    lo = header.index(lon_col)
    coords = []
    for row in rows[1:]:
        try:
            coords.append([float(row[li]), float(row[lo])])
        except (TypeError, ValueError):
            pass
    return np.array(coords)


def load_csv_coords(path, lat_col, lon_col, encoding='utf-8'):
    coords = []
    with open(path, newline='', encoding=encoding) as f:
        for row in csv.DictReader(f):
            try:
                coords.append([float(row[lat_col]), float(row[lon_col])])
            except (ValueError, TypeError, KeyError):
                pass
    return np.array(coords)


# Excluded because they duplicate the dedicated airport/military source
# features (count_airports_in_huc12, count_military_in_huc12, dist_airport_km,
# dist_military_km).
EXCLUDE_INDUSTRY_CATEGORIES = {'Airports', 'Airports (Part 139)', 'National Defense'}


def load_industry_sector(path):
    coords = []
    with open(path, newline='', encoding='utf-8-sig') as f:
        for row in csv.DictReader(f):
            if row.get('Industry', '').strip() in EXCLUDE_INDUSTRY_CATEGORIES:
                continue
            try:
                lat, lon = float(row['Latitude']), float(row['Longitude'])
                if not (np.isnan(lat) or np.isnan(lon)):
                    coords.append([lat, lon])
            except (ValueError, KeyError, TypeError):
                pass
    return np.array(coords) if coords else np.empty((0, 2))


def main():
    print("Loading national facility list...")
    pws = load_pws(FACILITY_CSV)
    valid = [(i, p) for i, p in enumerate(pws) if not np.isnan(p['lat'])]
    print(f"  {len(valid):,} facilities with valid coords")
    query_coords = np.array([[p['lat'], p['lon']] for _, p in valid])

    sources = {}

    # Industry sector (EPA ECHO) — lives at the UCMR root, not PFASsource/
    industry_csv = ROOT_DIR / 'industry sector.csv'
    if industry_csv.exists():
        sources['dist_industry_km'] = load_industry_sector(industry_csv)
        print(f"  Industry: {len(sources['dist_industry_km'])} sites")

    # PFAS discharge (NPDES)
    discharge_xlsx = SRC_DIR / 'PFAS discharge sites.xlsx'
    if discharge_xlsx.exists():
        sources['dist_discharge_km'] = load_xlsx_coords(discharge_xlsx, 'Latitude', 'Longitude')
        print(f"  Discharge: {len(sources['dist_discharge_km'])} sites")

    # PFAS manufacturer
    mfg_xlsx = SRC_DIR / 'PFAS manufacturer.xlsx'
    if mfg_xlsx.exists():
        sources['dist_manufacturer_km'] = load_xlsx_coords(mfg_xlsx, 'Latitude', 'Longitude')
        print(f"  Manufacturer: {len(sources['dist_manufacturer_km'])} sites")

    # FUDS
    fuds_csv = SRC_DIR / 'FUDS_Property_Polygon.csv'
    if fuds_csv.exists():
        sources['dist_fuds_km'] = load_csv_coords(fuds_csv, 'Latitude', 'Longitude', encoding='latin-1')
        print(f"  FUDS: {len(sources['dist_fuds_km'])} sites")

    # Military installations (MIRTA) — columns are lowercase x=lon, y=lat
    mil_csv = SRC_DIR / 'mirta_7227477147801274047.csv'
    if mil_csv.exists():
        sources['dist_military_km'] = load_csv_coords(
            mil_csv, 'y', 'x', encoding='utf-8-sig')
        print(f"  Military: {len(sources['dist_military_km'])} sites")

    print("Computing distances...")
    results = []
    for i, p in enumerate(pws):
        row = {k: p[k] for k in ('PWSID', 'FacilityID', 'FacilityName', 'State', 'lat', 'lon')}
        for col in sources:
            row[col] = np.nan
        results.append(row)

    for col, src_coords in sources.items():
        if len(src_coords) == 0:
            continue
        dists = haversine_min_dist(query_coords, src_coords)
        for rank, (orig_idx, _) in enumerate(valid):
            results[orig_idx][col] = round(float(dists[rank]), 4)
        print(f"  {col}: done")

    out_cols = ['PWSID', 'FacilityID', 'FacilityName', 'State', 'lat', 'lon'] + list(sources.keys())
    with open(OUT_CSV, 'w', newline='', encoding='utf-8') as f:
        writer = csv.DictWriter(f, fieldnames=out_cols, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(results)
    print(f"\nSaved: {OUT_CSV}  ({len(results):,} rows)")


if __name__ == '__main__':
    main()
