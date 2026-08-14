"""
Assign HUC-12 to seller PWS using USGS WBD API, then add seller watershed geo features.

KEY CHANGES from midwest version:
  - Reads from purchaser_seller_national.csv and national_facility_features_v2.csv
  - HUC-12 feature tables use *_national.csv variants
  - Output: national_facility_features_v3.csv

Seller coordinate source: SDWIS_with_loc08072024.csv (same file, has all national PWS coords).
"""

import csv, json, time, os, requests
from pathlib import Path

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE   = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
STATES = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/States')
# ──────────────────────────────────────────────────────────────────────────────

WBD_URL = 'https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/6/query'


def normalize_huc12(val):
    val = str(val).strip()
    if not val or val == 'nan':
        return ''
    try:
        return str(int(float(val))).zfill(12)
    except ValueError:
        return val


def lookup_huc12_for_point(lat, lon, session, retries=3):
    params = {
        'geometry':     json.dumps({'x': lon, 'y': lat}),
        'geometryType': 'esriGeometryPoint',
        'inSR':         '4326',
        'spatialRel':   'esriSpatialRelIntersects',
        'outFields':    'huc12,name',
        'f':            'json',
    }
    for attempt in range(retries):
        try:
            resp = session.get(WBD_URL, params=params, timeout=30)
            data = resp.json()
            features = data.get('features', [])
            if features:
                attrs = features[0].get('attributes', {})
                return attrs.get('huc12', ''), attrs.get('name', '')
            return '', ''
        except Exception:
            if attempt < retries - 1:
                time.sleep(2)
    return '', ''


def load_huc12_table(path):
    data = {}
    with open(path) as f:
        for r in csv.DictReader(f):
            h = normalize_huc12(r['huc12'])
            if h:
                data[h] = r
    return data


def main():
    print("Loading seller coordinates from SDWIS_with_loc08072024.csv...")
    sdwis_loc = {}
    with open(STATES / 'SDWIS_with_loc08072024.csv') as f:
        for r in csv.DictReader(f):
            pid = r['PWS ID'].strip()
            try:
                lat = float(r['LAT'])
                lon = float(r['LON'])
                sdwis_loc[pid] = (lat, lon)
            except (ValueError, TypeError):
                pass
    print(f"  {len(sdwis_loc):,} PWS with coordinates")

    print("Loading national purchaser-seller map...")
    seller_ids = set()
    with open(BASE / 'purchaser_seller_national.csv') as f:
        for r in csv.DictReader(f):
            seller_ids.add(r['seller_pwsid'])
            seller_ids.add(r['ultimate_seller_pwsid'])
    seller_ids.discard('')
    print(f"  Unique seller PWS IDs: {len(seller_ids):,}")

    sellers_with_coords = {sid: sdwis_loc[sid] for sid in seller_ids if sid in sdwis_loc}
    print(f"  Sellers with coordinates: {len(sellers_with_coords):,}/{len(seller_ids):,}")

    lookup_path = BASE / 'seller_huc12_lookup_national.csv'
    cached = {}
    if lookup_path.exists():
        with open(lookup_path) as f:
            for r in csv.DictReader(f):
                if r['seller_pwsid'] and r['huc12']:
                    cached[r['seller_pwsid']] = r
        print(f"  Loaded {len(cached):,} cached HUC-12 lookups")

    to_query = {sid: coords for sid, coords in sellers_with_coords.items()
                if sid not in cached}
    print(f"  Need to query USGS API for {len(to_query):,} sellers...")

    session = requests.Session()
    new_results = {}
    for i, (sid, (lat, lon)) in enumerate(to_query.items()):
        huc12, huc12_name = lookup_huc12_for_point(lat, lon, session)
        new_results[sid] = {
            'seller_pwsid': sid,
            'lat': lat, 'lon': lon,
            'huc12':      normalize_huc12(huc12) if huc12 else '',
            'huc12_name': huc12_name,
        }
        if (i + 1) % 50 == 0:
            print(f"  {i+1:,}/{len(to_query):,} queried", flush=True)
        time.sleep(0.1)

    all_results = {**cached, **new_results}
    for sid in seller_ids:
        if sid not in all_results:
            all_results[sid] = {'seller_pwsid': sid, 'lat': '', 'lon': '', 'huc12': '', 'huc12_name': ''}

    with open(lookup_path, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=['seller_pwsid', 'lat', 'lon', 'huc12', 'huc12_name'])
        w.writeheader()
        w.writerows(v for v in all_results.values() if isinstance(v, dict))
    print(f"\nSaved seller HUC-12 lookup: {lookup_path} ({len(all_results):,} entries)")

    print("Loading HUC-12 geo feature tables (national)...")
    landuse = load_huc12_table(BASE / 'huc12_landuse_national.csv')
    geochem = load_huc12_table(BASE / 'huc12_geochem_national.csv')
    soil_li = {}
    soil_li_path = BASE / 'huc12_soil_li.csv'
    if soil_li_path.exists():
        soil_li = load_huc12_table(soil_li_path)
    print(f"  landuse: {len(landuse):,}, geochem: {len(geochem):,}, soil_li: {len(soil_li):,}")

    print("Joining to national_facility_features_v2.csv...")
    with open(BASE / 'national_facility_features_v2.csv') as f:
        reader = csv.DictReader(f)
        orig_cols  = reader.fieldnames
        facilities = list(reader)

    ps_map = {}
    with open(BASE / 'purchaser_seller_national.csv') as f:
        for r in csv.DictReader(f):
            ps_map[r['purchaser_pwsid']] = r

    sample_lu = next(iter(landuse.values()))
    sample_gc = next(iter(geochem.values()))
    lu_cols = [c for c in sample_lu if c != 'huc12']
    gc_cols = [c for c in sample_gc if c != 'huc12']
    sl_cols = ([c for c in next(iter(soil_li.values())) if c != 'huc12']
               if soil_li else [])

    new_cols = (['seller_huc12', 'seller_huc12_name'] +
                [f'seller_{c}' for c in lu_cols] +
                [f'seller_{c}' for c in gc_cols] +
                [f'seller_{c}' for c in sl_cols])
    all_cols = list(orig_cols) + new_cols

    n_shuc12 = n_slu = n_sgc = n_ssl = 0
    out_rows = []
    for fac in facilities:
        row = dict(fac)
        pwsid = fac['PWSID'].strip()
        ps = ps_map.get(pwsid, {})
        ult_seller = ps.get('ultimate_seller_pwsid', '')
        imm_seller = ps.get('seller_pwsid', '')
        seller_id  = ult_seller or imm_seller
        huc_info   = all_results.get(seller_id, {}) if seller_id else {}
        shuc12     = huc_info.get('huc12', '') if isinstance(huc_info, dict) else ''

        row['seller_huc12']      = shuc12
        row['seller_huc12_name'] = huc_info.get('huc12_name', '') if isinstance(huc_info, dict) else ''
        if shuc12:
            n_shuc12 += 1

        lu_row = landuse.get(shuc12, {}) if shuc12 else {}
        gc_row = geochem.get(shuc12, {}) if shuc12 else {}
        sl_row = soil_li.get(shuc12, {}) if shuc12 else {}
        if lu_row: n_slu += 1
        if gc_row: n_sgc += 1
        if sl_row: n_ssl += 1

        for c in lu_cols:
            row[f'seller_{c}'] = lu_row.get(c, '')
        for c in gc_cols:
            row[f'seller_{c}'] = gc_row.get(c, '')
        for c in sl_cols:
            row[f'seller_{c}'] = sl_row.get(c, '')

        out_rows.append(row)

    out_path = BASE / 'national_facility_features_v3.csv'
    with open(out_path, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=all_cols, extrasaction='ignore')
        w.writeheader()
        w.writerows(out_rows)

    print(f"\nOutput: {out_path}")
    print(f"  Total rows : {len(out_rows):,}")
    print(f"  seller_huc12 assigned : {n_shuc12:,}")
    print(f"  seller land use matched: {n_slu:,}")
    print(f"  seller geochem matched : {n_sgc:,}")
    if sl_cols:
        print(f"  seller soil Li matched : {n_ssl:,}")


if __name__ == '__main__':
    main()
