"""
Add landfill distance features to national_facility_features_v8.csv → v9.csv.

Distances and HUC-12 counts use source-water coordinates:
  purely purchased  → seller lat/lon and seller HUC-12
  mixed             → min(own, seller) distance; union of own + seller HUC-12 counts
  own source / unknown → own lat/lon and own HUC-12

New columns:
  dist_landfill_km          — km to nearest open MSW landfill
  count_landfills_in_huc12  — open landfills within source HUC-12
"""

import csv
import json
import numpy as np
import pandas as pd
from pathlib import Path
from scipy.spatial import cKDTree
from shapely.geometry import shape, Point

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
# ──────────────────────────────────────────────────────────────────────────────

LANDFILL_CSV  = BASE / 'landfill_lmop_national.csv'
IN_CSV        = BASE / 'national_facility_features_v8.csv'
# NOTE: this is the 42-state (non-MIDWEST8) table. 14b_merge_midwest8_national.py
# concatenates it with midwest8_facility_features_v9.csv into national_facility_features_v9.csv
# (the file step 16 actually reads).
OUT_CSV       = BASE / 'national_facility_features_v9_42states.csv'
HUC12_GEOJSON = BASE / 'huc12_polygons_national.geojson'
SELLER_LOOKUP = BASE / 'seller_huc12_lookup_national.csv'
EARTH_KM      = 6371.0

PURCHASED_PS_COLS  = ['Primary Source_Ground water purchased',
                      'Primary Source_Surface water purchased']
OWN_SOURCE_PS_COLS = ['Primary Source_Ground water',
                      'Primary Source_Surface water',
                      'Primary Source_Groundwater under influence of surface water']


def normalize_huc12(val):
    val = str(val).strip()
    if not val or val in ('nan', ''):
        return ''
    try:
        return str(int(float(val))).zfill(12)
    except ValueError:
        return val


def haversine_min_dist(query_latlon: np.ndarray, source_latlon: np.ndarray) -> np.ndarray:
    result = np.full(len(query_latlon), np.nan)
    if len(source_latlon) == 0:
        return result
    valid = np.isfinite(query_latlon).all(axis=1)
    if not valid.any():
        return result
    q_valid = query_latlon[valid]
    tree = cKDTree(np.radians(source_latlon))
    _, idx = tree.query(np.radians(q_valid), k=1)
    s_nn = source_latlon[idx]
    lat1 = np.radians(q_valid[:, 0]); lon1 = np.radians(q_valid[:, 1])
    lat2 = np.radians(s_nn[:, 0]);    lon2 = np.radians(s_nn[:, 1])
    dlat = lat2 - lat1; dlon = lon2 - lon1
    a = np.sin(dlat / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2) ** 2
    result[valid] = 2 * EARTH_KM * np.arcsin(np.sqrt(np.clip(a, 0, 1)))
    return result


def count_per_huc12(source_coords, geojson_path, checkpoint_path=None):
    counts = {}
    if checkpoint_path and Path(checkpoint_path).exists():
        with open(checkpoint_path) as f:
            counts = json.load(f)
        print(f"  Loaded {len(counts):,} cached landfill counts from checkpoint")

    with open(geojson_path) as f:
        features = json.load(f)['features']

    polys = {}
    for feat in features:
        huc12 = str(feat['properties']['huc12']).zfill(12)
        if huc12 not in counts:
            polys[huc12] = shape(feat['geometry'])

    pts = [Point(lon, lat) for lat, lon in source_coords]
    total = len(features)
    new_count = 0

    for huc12, poly in polys.items():
        minx, miny, maxx, maxy = poly.bounds
        n = sum(1 for p in pts
                if minx <= p.x <= maxx and miny <= p.y <= maxy and poly.contains(p))
        counts[huc12] = n
        new_count += 1
        if len(counts) % 2000 == 0:
            print(f"    {len(counts):,}/{total:,} HUC-12s done...", flush=True)
        if checkpoint_path and new_count % 1000 == 0:
            with open(checkpoint_path, 'w') as f:
                json.dump(counts, f)

    if checkpoint_path:
        with open(checkpoint_path, 'w') as f:
            json.dump(counts, f)
        Path(checkpoint_path).unlink()

    print(f"  HUC-12s with ≥1 landfill: {sum(v > 0 for v in counts.values()):,} / {len(counts):,}")
    return counts


def main():
    print("Loading national LMOP landfill data...")
    lf = pd.read_csv(LANDFILL_CSV, low_memory=False)
    if 'Current Landfill Status' in lf.columns:
        open_lf = lf[lf['Current Landfill Status'] == 'Open'].copy()
    else:
        status_col = [c for c in lf.columns if 'Status' in c]
        if status_col:
            open_lf = lf[lf[status_col[0]].str.contains('Open', na=False, case=False)].copy()
        else:
            print("  WARNING: cannot identify status column — using all records")
            open_lf = lf.copy()

    lat_col = next((c for c in ['Latitude', 'LAT', 'lat'] if c in open_lf.columns), None)
    lon_col = next((c for c in ['Longitude', 'LON', 'lon'] if c in open_lf.columns), None)
    if lat_col is None or lon_col is None:
        raise ValueError(f"Cannot find lat/lon columns. Available: {open_lf.columns.tolist()}")

    open_lf = open_lf.dropna(subset=[lat_col, lon_col])
    open_lf[lat_col] = pd.to_numeric(open_lf[lat_col], errors='coerce')
    open_lf[lon_col] = pd.to_numeric(open_lf[lon_col], errors='coerce')
    open_lf = open_lf.dropna(subset=[lat_col, lon_col])
    print(f"  Open landfills with valid coords: {len(open_lf):,}")
    landfill_coords = open_lf[[lat_col, lon_col]].values

    print("\nLoading seller HUC-12 lookup...")
    seller_lookup = {}
    with open(SELLER_LOOKUP) as f:
        for r in csv.DictReader(f):
            pid = r['seller_pwsid'].strip()
            if not pid:
                continue
            try:
                lat, lon = float(r['lat']), float(r['lon'])
            except (ValueError, TypeError):
                lat = lon = None
            seller_lookup[pid] = {'lat': lat, 'lon': lon,
                                  'huc12': normalize_huc12(r.get('huc12', ''))}
    print(f"  {len(seller_lookup):,} sellers loaded")

    print("\nLoading national facility feature file...")
    df = pd.read_csv(IN_CSV, low_memory=False)
    print(f"  Shape: {df.shape}")

    # ── Determine source-water category and effective coordinates per facility ─
    print("\nDetermining source-water category per facility...")
    eff_lats, eff_lons, source_huc12s = [], [], []
    mixed_seller_lats, mixed_seller_lons, mixed_seller_huc12s = [], [], []
    category = []
    n_purely = n_own = n_mixed = n_unknown = 0

    for _, row in df.iterrows():
        ult_seller = str(row.get('ultimate_seller_pwsid', '')).strip()
        own_huc12  = normalize_huc12(row.get('huc12', ''))
        is_purch   = any(str(row.get(c, '')).strip() == '1' for c in PURCHASED_PS_COLS)
        is_own     = any(str(row.get(c, '')).strip() == '1' for c in OWN_SOURCE_PS_COLS)
        try:
            own_lat, own_lon = float(row['query_lat']), float(row['query_lon'])
        except (ValueError, TypeError):
            own_lat = own_lon = float('nan')

        seller_lat = seller_lon = None; seller_huc = ''
        if ult_seller and ult_seller in seller_lookup:
            sl = seller_lookup[ult_seller]
            if sl['lat'] is not None:
                seller_lat, seller_lon = sl['lat'], sl['lon']
                seller_huc = sl['huc12']

        if is_purch and not is_own:
            if seller_lat is not None:
                eff_lats.append(seller_lat); eff_lons.append(seller_lon)
                source_huc12s.append(seller_huc if seller_huc else own_huc12)
            else:
                eff_lats.append(own_lat); eff_lons.append(own_lon)
                source_huc12s.append(own_huc12)
            mixed_seller_lats.append(None); mixed_seller_lons.append(None)
            mixed_seller_huc12s.append('')
            category.append('purely_purchased'); n_purely += 1
        elif is_purch and is_own:
            eff_lats.append(own_lat); eff_lons.append(own_lon)
            source_huc12s.append(own_huc12)
            mixed_seller_lats.append(seller_lat); mixed_seller_lons.append(seller_lon)
            mixed_seller_huc12s.append(seller_huc)
            category.append('mixed'); n_mixed += 1
        else:
            eff_lats.append(own_lat); eff_lons.append(own_lon)
            source_huc12s.append(own_huc12)
            mixed_seller_lats.append(None); mixed_seller_lons.append(None)
            mixed_seller_huc12s.append('')
            if is_own:
                category.append('own_source'); n_own += 1
            else:
                category.append('unknown'); n_unknown += 1

    eff_latlon = np.column_stack([np.array(eff_lats, dtype=float),
                                  np.array(eff_lons, dtype=float)])
    mix_indices = [i for i, c in enumerate(category) if c == 'mixed'
                   and mixed_seller_lats[i] is not None
                   and np.isfinite(mixed_seller_lats[i])]
    mix_latlon = (np.array([[mixed_seller_lats[i], mixed_seller_lons[i]] for i in mix_indices])
                  if mix_indices else np.empty((0, 2)))
    print(f"  Purely purchased: {n_purely:,}  Own: {n_own:,}  "
          f"Mixed: {n_mixed:,}  Unknown: {n_unknown:,}")

    # ── Distance with source-water correction ─────────────────────────────────
    print("\nComputing source-water-corrected landfill distances...")
    dists = haversine_min_dist(eff_latlon, landfill_coords)
    if mix_indices and len(landfill_coords) > 0:
        dists_seller = haversine_min_dist(mix_latlon, landfill_coords)
        for k, i in enumerate(mix_indices):
            own_d, sel_d = dists[i], dists_seller[k]
            dists[i] = (min(own_d, sel_d)
                        if np.isfinite(own_d) and np.isfinite(sel_d)
                        else (own_d if np.isfinite(own_d) else sel_d))
    df['dist_landfill_km'] = np.where(np.isfinite(dists), np.round(dists, 4), np.nan)
    print(f"  min={np.nanmin(dists):.1f} km  median={np.nanmedian(dists):.1f} km  "
          f"max={np.nanmax(dists):.1f} km")

    # ── Count landfills per HUC-12 (all polygons in geojson) ──────────────────
    print("\nCounting open landfills per HUC-12 (national polygons)...")
    LANDFILL_CKPT = BASE / 'landfill_huc12_counts_checkpoint.json'
    huc12_counts = count_per_huc12(landfill_coords, HUC12_GEOJSON, LANDFILL_CKPT)

    vals = []
    for i in range(len(df)):
        shuc12   = source_huc12s[i]
        shuc12_2 = mixed_seller_huc12s[i]
        count = 0
        if shuc12:
            count += huc12_counts.get(shuc12, 0)
        if shuc12_2 and shuc12_2 != shuc12:
            count += huc12_counts.get(shuc12_2, 0)
        vals.append(count)
    df['count_landfills_in_huc12'] = vals
    print(f"  Facilities with ≥1 landfill in HUC-12: {(df['count_landfills_in_huc12'] > 0).sum():,}")

    print(f"\nSaving to {OUT_CSV}...")
    df.to_csv(OUT_CSV, index=False)
    print(f"Done. Shape: {df.shape}")
    print("New columns: dist_landfill_km, count_landfills_in_huc12")


if __name__ == '__main__':
    main()
