"""
Aggregate USGS geochem samples within each HUC-12 polygon (national).
Output: huc12_geochem_national.csv

Checkpoint support: saves progress every 200 polygons to
huc12_geochem_national_checkpoint.csv. Resumes automatically if interrupted.
(200 instead of 500 because each polygon is expensive: vectorized haversine over all samples.)
"""

import json, csv
from pathlib import Path
import numpy as np
import pandas as pd
from shapely.geometry import shape, Point

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE_DIR = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
# ──────────────────────────────────────────────────────────────────────────────

POLYGONS_FILE    = BASE_DIR / 'huc12_polygons_national.geojson'
GEOCHEM_FILE     = BASE_DIR / 'geochem.csv'
OUT_FILE         = BASE_DIR / 'huc12_geochem_national.csv'
CHECKPOINT_FILE  = BASE_DIR / 'huc12_geochem_national_checkpoint.csv'
CHECKPOINT_EVERY = 200

ELEMENTS = {
    'Li': ['LI_ICP40', 'LI_NURE'],
    'Fe': ['FE_ICP40', 'FE_AA'],
    'Al': ['AL_ICP40', 'AL_AA'],
    'U':  ['U_NURE', 'U_PHOS', 'U_ICP40'],
    'As': ['AS_ICP40', 'AS_AA'],
}
EARTH_KM = 6371.0


def haversine_km(lat1, lon1, lat2, lon2):
    lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])
    dlat, dlon = lat2 - lat1, lon2 - lon1
    a = np.sin(dlat/2)**2 + np.cos(lat1)*np.cos(lat2)*np.sin(dlon/2)**2
    return 2 * EARTH_KM * np.arcsin(np.sqrt(a))


def load_geochem(path):
    # Matches huc12_geochem.py (midwest) exactly: LATITUDE/LONGITUDE only (no
    # WGS84 fallback), rows with missing/invalid coords are dropped (not
    # defaulted to (0,0)), and -9000 is the USGS non-detect sentinel — NOT 0
    # (element values are legitimately encoded as negative non-detect codes,
    # e.g. -100.0 = "below 100 ppm"; filtering on v > 0 would wrongly drop
    # ~70% of As and ~99.6% of U samples as missing).
    print("Loading geochem.csv...")
    records, col_map = [], {}
    with open(path, newline='', encoding='latin-1') as f:
        reader = csv.DictReader(f)
        headers = reader.fieldnames
        for elem, candidates in ELEMENTS.items():
            for cand in candidates:
                if cand in headers:
                    col_map[elem] = cand
                    break
        print(f"  Column mapping: {col_map}")
        for row in reader:
            try:
                lat = float(row['LATITUDE'])
                lon = float(row['LONGITUDE'])
            except (ValueError, TypeError):
                continue
            rec = {'lat': lat, 'lon': lon}
            for elem, col in col_map.items():
                try:
                    v = float(row[col])
                    rec[elem] = v if v > -9000 else np.nan  # sentinel removal
                except (ValueError, TypeError):
                    rec[elem] = np.nan
            records.append(rec)
    print(f"  {len(records):,} geochem records loaded")
    return records, col_map


def main():
    records, col_map = load_geochem(GEOCHEM_FILE)
    lats = np.array([r['lat'] for r in records])
    lons = np.array([r['lon'] for r in records])
    elem_arrays = {e: np.array([r[e] for r in records]) for e in col_map}

    out_cols = ['huc12', 'geochem_n']
    for elem in col_map:
        out_cols += [f'geochem_{elem}_nearest', f'geochem_{elem}_mean',
                     f'geochem_{elem}_max',     f'geochem_{elem}_count']

    # ── Resume from checkpoint ────────────────────────────────────────────────
    results = []
    processed = set()
    if CHECKPOINT_FILE.exists():
        ckpt = pd.read_csv(CHECKPOINT_FILE, dtype={'huc12': str})
        results = ckpt.to_dict('records')
        processed = set(ckpt['huc12'].astype(str))
        print(f"  Resumed: {len(processed):,} HUC-12s already done")

    with open(POLYGONS_FILE) as f:
        all_features = json.load(f)['features']
    total = len(all_features)

    remaining = [feat for feat in all_features
                 if str(feat['properties']['huc12']).zfill(12) not in processed]
    print(f"Processing {len(remaining):,} remaining / {total:,} total HUC-12 polygons...")

    # Matches huc12_geochem.py (midwest) exactly: nearest sample is restricted
    # to points INSIDE the polygon (a single nearest-to-centroid point shared
    # across all elements), not a nationwide nearest-valid-point search per
    # element. HUC-12s with no in-polygon sample get NaN, same as midwest.
    new_count = 0
    for feat in remaining:
        huc12 = str(feat['properties']['huc12']).zfill(12)
        poly  = shape(feat['geometry'])
        minx, miny, maxx, maxy = poly.bounds

        mask_bb = ((lons >= minx) & (lons <= maxx) &
                   (lats >= miny) & (lats <= maxy))
        bb_idx = np.where(mask_bb)[0]
        in_poly = [j for j in bb_idx if poly.contains(Point(lons[j], lats[j]))]

        row = {'huc12': huc12, 'geochem_n': len(in_poly)}

        if in_poly:
            cx, cy = poly.centroid.x, poly.centroid.y
            in_lats = lats[in_poly]
            in_lons = lons[in_poly]
            dists = haversine_km(cy, cx, in_lats, in_lons)
            nearest_idx_local = np.argmin(dists)
            nearest_global = in_poly[nearest_idx_local]

            for elem in col_map:
                vals = elem_arrays[elem][in_poly]
                valid = vals[~np.isnan(vals)]
                row[f'geochem_{elem}_nearest'] = (
                    round(float(elem_arrays[elem][nearest_global]), 4)
                    if not np.isnan(elem_arrays[elem][nearest_global]) else np.nan)
                row[f'geochem_{elem}_mean']  = round(float(np.mean(valid)), 4) if len(valid) else np.nan
                row[f'geochem_{elem}_max']   = round(float(np.max(valid)), 4)  if len(valid) else np.nan
                row[f'geochem_{elem}_count'] = int(np.sum(~np.isnan(vals)))
        else:
            for elem in col_map:
                row[f'geochem_{elem}_nearest'] = np.nan
                row[f'geochem_{elem}_mean']    = np.nan
                row[f'geochem_{elem}_max']     = np.nan
                row[f'geochem_{elem}_count']   = 0

        results.append(row)
        new_count += 1

        if new_count % CHECKPOINT_EVERY == 0:
            pd.DataFrame(results, columns=out_cols).to_csv(
                CHECKPOINT_FILE, index=False)
            done = len(results)
            print(f"  Checkpoint: {done:,}/{total:,} ({done/total*100:.1f}%)",
                  flush=True)

    with open(OUT_FILE, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=out_cols, extrasaction='ignore')
        writer.writeheader()
        writer.writerows(results)

    if CHECKPOINT_FILE.exists():
        CHECKPOINT_FILE.unlink()
    print(f"Saved: {OUT_FILE}  ({len(results):,} rows)")


if __name__ == '__main__':
    main()
