"""
Two corrections to national_facility_features_v3.csv → v4.csv:

1. Recalculate PFAS distances using ultimate seller lat/lon for purchased-water facilities.
2. Add count_*_in_huc12 columns using source watershed HUC-12.

KEY CHANGES from midwest version:
  - Reads national_facility_features_v3.csv, seller_huc12_lookup_national.csv
  - Reads huc12_polygons_national.geojson for polygon count
  - Output: national_facility_features_v4.csv

All distance/count logic is identical to pfas_source_corrections.py.
"""

import csv, json, time, requests
import numpy as np
import openpyxl
from scipy.spatial import cKDTree
from shapely.geometry import shape, Point
from pathlib import Path

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
SRC  = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/PFASsource')
ROOT = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR')
# ──────────────────────────────────────────────────────────────────────────────

WBD_URL = 'https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/6/query'
EARTH_RADIUS_KM = 6371.0


def normalize_huc12(val):
    val = str(val).strip()
    if not val or val in ('nan', ''):
        return ''
    try:
        return str(int(float(val))).zfill(12)
    except ValueError:
        return val


def haversine_km(lat1, lon1, lat2, lon2):
    lat1, lon1, lat2, lon2 = (np.radians(x) for x in (lat1, lon1, lat2, lon2))
    dlat = lat2 - lat1; dlon = lon2 - lon1
    a = np.sin(dlat / 2) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon / 2) ** 2
    return 2 * EARTH_RADIUS_KM * np.arcsin(np.sqrt(a))


def haversine_min_dist(query_coords, source_coords):
    if len(source_coords) == 0:
        return np.full(len(query_coords), np.nan)
    s_rad = np.radians(source_coords)
    q_rad = np.radians(query_coords)
    tree = cKDTree(s_rad)
    _, idx = tree.query(q_rad, k=1)
    s_nn = source_coords[idx]
    return haversine_km(query_coords[:, 0], query_coords[:, 1],
                        s_nn[:, 0], s_nn[:, 1])


# Excluded because they duplicate the dedicated airport/military source
# features (count_airports_in_huc12, count_military_in_huc12, dist_airport_km,
# dist_military_km) — keeping them in the general industry count was inflating
# correlation between count_industry_in_huc12 and those features.
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


def load_excel_latlon(path, lat_col, lon_col):
    wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
    ws = wb.active
    rows = list(ws.iter_rows(values_only=True))
    wb.close()
    header = None
    data_start = 0
    for i, row in enumerate(rows):
        if row and any(v == lat_col for v in row):
            header = {str(v): j for j, v in enumerate(row) if v is not None}
            data_start = i + 1
            break
    if header is None:
        return np.empty((0, 2))
    li, loi = header[lat_col], header[lon_col]
    coords = []
    for row in rows[data_start:]:
        if row is None:
            continue
        try:
            lat, lon = float(row[li]), float(row[loi])
            if not (np.isnan(lat) or np.isnan(lon)):
                coords.append([lat, lon])
        except (TypeError, ValueError, IndexError):
            pass
    return np.array(coords) if coords else np.empty((0, 2))


def load_fuds(path):
    coords = []
    with open(path, newline='', encoding='utf-8-sig') as f:
        for row in csv.DictReader(f):
            try:
                lat, lon = float(row['LATITUDE']), float(row['LONGITUDE'])
                if not (np.isnan(lat) or np.isnan(lon)):
                    coords.append([lat, lon])
            except (ValueError, KeyError, TypeError):
                pass
    return np.array(coords) if coords else np.empty((0, 2))


def load_military(path):
    coords = []
    with open(path, newline='', encoding='utf-8-sig') as f:
        for row in csv.DictReader(f):
            try:
                lat, lon = float(row['y']), float(row['x'])
                if not (np.isnan(lat) or np.isnan(lon)):
                    coords.append([lat, lon])
            except (ValueError, KeyError, TypeError):
                pass
    return np.array(coords) if coords else np.empty((0, 2))


def fetch_huc12_polygons_batch(huc12_list):
    BATCH = 50
    features = {}
    session = requests.Session()
    batches = [huc12_list[i:i+BATCH] for i in range(0, len(huc12_list), BATCH)]
    for i, batch in enumerate(batches):
        where = "huc12 IN ('" + "','".join(batch) + "')"
        for attempt in range(3):
            try:
                resp = session.get(WBD_URL, params={
                    'where': where, 'outFields': 'huc12,name',
                    'f': 'geojson', 'outSR': '4326',
                }, timeout=60)
                data = resp.json()
                for feat in data.get('features', []):
                    h = feat['properties'].get('huc12', '')
                    if h:
                        features[h] = feat
                break
            except Exception:
                if attempt < 2:
                    time.sleep(3)
        if (i + 1) % 5 == 0 or (i + 1) == len(batches):
            print(f"    batch {i+1}/{len(batches)}: {len(features)} polygons", flush=True)
        time.sleep(0.3)
    return features


def count_points_in_polygons(huc12_polygon_map, source_coords_list,
                              checkpoint_path=None):
    """Count source points in each HUC-12 polygon with checkpoint support.

    checkpoint_path: Path to a JSON file for saving/resuming progress.
    Checkpoint format: {huc12: {source_name: count}}.
    Deleted automatically on successful completion.
    """
    source_arrays = {}
    empty_sources = []
    for name, coords in source_coords_list:
        if len(coords) > 0:
            source_arrays[name] = {'lats': coords[:, 0], 'lons': coords[:, 1]}
        else:
            source_arrays[name] = {'lats': np.array([]), 'lons': np.array([])}
            empty_sources.append(name)

    # Load existing checkpoint
    cached = {}
    if checkpoint_path and Path(checkpoint_path).exists():
        with open(checkpoint_path) as f:
            cached = json.load(f)
        print(f"  Loaded {len(cached):,} cached polygon counts from checkpoint")

    results = dict(cached)
    to_process = [(h, p) for h, p in huc12_polygon_map.items() if h not in cached]
    total = len(huc12_polygon_map)

    for i, (huc12, poly) in enumerate(to_process):
        row = {}
        minlon, minlat, maxlon, maxlat = poly.bounds
        for name, arr in source_arrays.items():
            lats = arr['lats']
            lons = arr['lons']
            if len(lats) == 0:
                row[name] = 0
                continue
            bb_mask = (lons >= minlon) & (lons <= maxlon) & (lats >= minlat) & (lats <= maxlat)
            bb_idx = np.where(bb_mask)[0]
            count = sum(1 for j in bb_idx if poly.contains(Point(lons[j], lats[j])))
            row[name] = count
        results[huc12] = row

        done = len(results)
        if done % 500 == 0:
            print(f"    {done:,}/{total:,} HUC-12s processed", flush=True)
        if checkpoint_path and (i + 1) % 500 == 0:
            with open(checkpoint_path, 'w') as f:
                json.dump(results, f)

    # Save final checkpoint, then delete on success
    if checkpoint_path:
        with open(checkpoint_path, 'w') as f:
            json.dump(results, f)
        Path(checkpoint_path).unlink()

    return results


def main():
    print("Loading PFAS sources...")
    industry_coords  = load_industry_sector(ROOT / 'industry sector.csv')
    discharge_coords = load_excel_latlon(SRC / 'PFAS discharge sites.xlsx', 'Latitude', 'Longitude')
    mfg_coords       = load_excel_latlon(SRC / 'PFAS manufacturer.xlsx', 'Latitude', 'Longitude')
    fuds_coords      = load_fuds(SRC / 'FUDS_Property_Polygon.csv')
    military_coords  = load_military(SRC / 'mirta_7227477147801274047.csv')
    print(f"  Industry: {len(industry_coords):,}  Discharge: {len(discharge_coords):,}  "
          f"Mfg: {len(mfg_coords):,}  FUDS: {len(fuds_coords):,}  Military: {len(military_coords):,}")

    source_list = [
        ('industry',     industry_coords),
        ('discharge',    discharge_coords),
        ('manufacturer', mfg_coords),
        ('fuds',         fuds_coords),
        ('military',     military_coords),
    ]

    print("\nLoading seller coordinate lookup (national)...")
    seller_lookup = {}
    with open(BASE / 'seller_huc12_lookup_national.csv') as f:
        for r in csv.DictReader(f):
            pid = r['seller_pwsid'].strip()
            if not pid:
                continue
            try:
                lat = float(r['lat']); lon = float(r['lon'])
            except (ValueError, TypeError):
                lat = lon = None
            huc12 = normalize_huc12(r.get('huc12', ''))
            seller_lookup[pid] = {'lat': lat, 'lon': lon, 'huc12': huc12}
    print(f"  {len(seller_lookup):,} sellers loaded")

    print("\nLoading national_facility_features_v3.csv...")
    with open(BASE / 'national_facility_features_v3.csv', newline='', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        orig_cols  = reader.fieldnames
        facilities = list(reader)
    print(f"  {len(facilities):,} facilities, {len(orig_cols)} columns")

    PURCHASED_PS_COLS = ['Primary Source_Ground water purchased',
                         'Primary Source_Surface water purchased']
    OWN_SOURCE_PS_COLS = ['Primary Source_Ground water',
                          'Primary Source_Surface water',
                          'Primary Source_Groundwater under influence of surface water']

    eff_lats = []; eff_lons = []; source_huc12s = []
    mixed_seller_lats = []; mixed_seller_lons = []; mixed_seller_huc12s = []
    category = []
    n_purely_purch = n_own_only = n_mixed = n_unknown = n_no_seller_coords = 0

    for fac in facilities:
        ult_seller = fac.get('ultimate_seller_pwsid', '').strip()
        own_huc12  = normalize_huc12(fac.get('huc12', ''))
        is_purch_flag = any(fac.get(c, '').strip() == '1' for c in PURCHASED_PS_COLS)
        is_own_flag   = any(fac.get(c, '').strip() == '1' for c in OWN_SOURCE_PS_COLS)
        try:
            own_lat = float(fac.get('query_lat', '')); own_lon = float(fac.get('query_lon', ''))
        except (ValueError, TypeError):
            own_lat = own_lon = float('nan')

        seller_lat = seller_lon = None; seller_huc = ''
        if ult_seller and ult_seller in seller_lookup:
            sl = seller_lookup[ult_seller]
            if sl['lat'] is not None:
                seller_lat = sl['lat']; seller_lon = sl['lon']
                seller_huc = sl['huc12'] if sl['huc12'] else ''
            else:
                n_no_seller_coords += 1

        if is_purch_flag and not is_own_flag:
            if seller_lat is not None:
                eff_lats.append(seller_lat); eff_lons.append(seller_lon)
                source_huc12s.append(seller_huc if seller_huc else own_huc12)
            else:
                eff_lats.append(own_lat); eff_lons.append(own_lon)
                source_huc12s.append(own_huc12)
            mixed_seller_lats.append(None); mixed_seller_lons.append(None)
            mixed_seller_huc12s.append('')
            category.append('purely_purchased'); n_purely_purch += 1
        elif is_purch_flag and is_own_flag:
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
            if is_own_flag:
                category.append('own_source'); n_own_only += 1
            else:
                category.append('unknown'); n_unknown += 1

    eff_lats = np.array(eff_lats, dtype=float)
    eff_lons = np.array(eff_lons, dtype=float)
    print(f"\n  Purely purchased: {n_purely_purch:,}  Own: {n_own_only:,}  "
          f"Mixed: {n_mixed:,}  Unknown: {n_unknown:,}")

    print("\nRecalculating PFAS distances...")
    valid_mask = np.isfinite(eff_lats) & np.isfinite(eff_lons)
    query_coords_valid = np.column_stack([eff_lats, eff_lons])[valid_mask]
    mix_indices = [i for i, c in enumerate(category) if c == 'mixed'
                   and mixed_seller_lats[i] is not None
                   and np.isfinite(mixed_seller_lats[i])]
    mix_coords = (np.array([[mixed_seller_lats[i], mixed_seller_lons[i]]
                             for i in mix_indices])
                  if mix_indices else np.empty((0, 2)))

    dist_results = {}
    for name, coords in source_list:
        print(f"  dist_{name}_km...", flush=True)
        dists_all = np.full(len(facilities), np.nan)
        if len(query_coords_valid) > 0:
            dists_all[valid_mask] = haversine_min_dist(query_coords_valid, coords)
        if mix_indices and len(coords) > 0:
            dists_seller = haversine_min_dist(mix_coords, coords)
            for k, i in enumerate(mix_indices):
                own_d = dists_all[i]; sel_d = dists_seller[k]
                dists_all[i] = (min(own_d, sel_d)
                                if np.isfinite(own_d) and np.isfinite(sel_d)
                                else (own_d if np.isfinite(own_d) else sel_d))
        dist_results[name] = dists_all

    print("\nLoading huc12_polygons_national.geojson for boundary counts...")
    poly_map = {}
    with open(BASE / 'huc12_polygons_national.geojson') as f:
        gj = json.load(f)
    for feat in gj['features']:
        h = normalize_huc12(feat['properties'].get('huc12', ''))
        if h:
            poly_map[h] = shape(feat['geometry'])
    print(f"  Loaded {len(poly_map):,} polygons from huc12_polygons_national.geojson")

    unique_src_huc12s = set(h for h in source_huc12s if h)
    missing = sorted(unique_src_huc12s - set(poly_map.keys()))
    if missing:
        print(f"  Fetching {len(missing):,} additional polygons from USGS WBD...")
        fetched = fetch_huc12_polygons_batch(missing)
        for h, feat in fetched.items():
            poly_map[normalize_huc12(h)] = shape(feat['geometry'])

    extra_huc12s = set(h for h in mixed_seller_huc12s if h)
    still_missing_extra = sorted(extra_huc12s - set(poly_map.keys()))
    if still_missing_extra:
        print(f"  Fetching {len(still_missing_extra):,} seller HUC-12 polygons for mixed...")
        extra_fetched = fetch_huc12_polygons_batch(still_missing_extra)
        for h, feat in extra_fetched.items():
            poly_map[normalize_huc12(h)] = shape(feat['geometry'])

    all_needed = unique_src_huc12s | extra_huc12s
    needed_polys = {h: poly_map[h] for h in all_needed if h in poly_map}
    print(f"  Polygons available: {len(needed_polys):,}/{len(unique_src_huc12s):,}")

    print("\nCounting PFAS sources within source HUC-12 polygons...")
    PFAS_COUNT_CKPT = BASE / 'pfas_huc12_counts_checkpoint.json'
    huc12_counts = count_points_in_polygons(needed_polys, source_list,
                                             checkpoint_path=PFAS_COUNT_CKPT)

    print("\nAssembling output...")
    new_count_cols = ['count_industry_in_huc12', 'count_discharge_in_huc12',
                      'count_manufacturer_in_huc12', 'count_fuds_in_huc12',
                      'count_military_in_huc12']
    final_cols = list(orig_cols) + new_count_cols
    source_names = ['industry', 'discharge', 'manufacturer', 'fuds', 'military']

    n_counts_assigned = 0
    out_rows = []
    for i, fac in enumerate(facilities):
        row = dict(fac)
        d = dist_results
        row['dist_industry_km']     = round(d['industry'][i], 4)     if np.isfinite(d['industry'][i])     else ''
        row['dist_discharge_km']    = round(d['discharge'][i], 4)    if np.isfinite(d['discharge'][i])    else ''
        row['dist_manufacturer_km'] = round(d['manufacturer'][i], 4) if np.isfinite(d['manufacturer'][i]) else ''
        row['dist_fuds_km']         = round(d['fuds'][i], 4)         if np.isfinite(d['fuds'][i])         else ''
        row['dist_military_km']     = round(d['military'][i], 4)     if np.isfinite(d['military'][i])     else ''

        shuc12 = source_huc12s[i]; shuc12_2 = mixed_seller_huc12s[i]
        has_count = False
        for sn, cnt_col in zip(source_names, new_count_cols):
            count = 0; assigned = False
            if shuc12 and shuc12 in huc12_counts:
                count += huc12_counts[shuc12].get(sn, 0); assigned = True
            if shuc12_2 and shuc12_2 != shuc12 and shuc12_2 in huc12_counts:
                count += huc12_counts[shuc12_2].get(sn, 0); assigned = True
            row[cnt_col] = count if assigned else ''
            if assigned:
                has_count = True
        if has_count:
            n_counts_assigned += 1
        out_rows.append(row)

    out_path = BASE / 'national_facility_features_v4.csv'
    with open(out_path, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=final_cols, extrasaction='ignore')
        w.writeheader()
        w.writerows(out_rows)

    print(f"\nOutput: {out_path}")
    print(f"  Total rows: {len(out_rows):,}")
    print(f"  Total cols: {len(final_cols)}")
    print(f"  Facilities with PFAS-in-HUC-12 counts: {n_counts_assigned:,}")


if __name__ == '__main__':
    main()
