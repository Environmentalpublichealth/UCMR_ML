"""
Fetch HUC-12 boundary polygons from USGS WBD API for all unique HUC-12 codes
in national_facility_huc12_aquifer.csv.

Output: huc12_polygons_national.geojson

NOTE: The national dataset has ~10k–15k unique HUC-12s vs ~1,400 for Midwest.
This script batches 50 HUC-12s per API call and retries on failure.
Expect ~4–6 hours on a single node. Run in a screen/tmux session.
"""

import csv, json, time, sys
from pathlib import Path
import requests

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE_DIR = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
# ──────────────────────────────────────────────────────────────────────────────

FACILITY_FILE = BASE_DIR / 'national_facility_huc12_aquifer.csv'
OUT_FILE      = BASE_DIR / 'huc12_polygons_national.geojson'
WBD_URL       = 'https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/6/query'
BATCH_SIZE    = 50


def load_unique_huc12s(path):
    huc12s = set()
    with open(path) as f:
        for row in csv.DictReader(f):
            h = row['huc12'].strip()
            if h and h != 'nan' and h != '':
                try:
                    huc12s.add(str(int(float(h))).zfill(12))
                except ValueError:
                    pass
    return sorted(huc12s)


def fetch_batch(codes, session):
    where = "huc12 IN ('" + "','".join(codes) + "')"
    for attempt in range(4):
        try:
            resp = session.get(WBD_URL, params={
                'where': where,
                'outFields': 'huc12,name,areaacres',
                'f': 'geojson',
                'outSR': '4326',
            }, timeout=90)
            data = resp.json()
            return data.get('features', [])
        except Exception as e:
            wait = 10 * (attempt + 1)
            print(f"    Attempt {attempt+1} failed: {e}. Retrying in {wait}s...")
            time.sleep(wait)
    print(f"    WARNING: batch failed after 4 attempts: {codes[:3]}...")
    return []


def main():
    huc12s = load_unique_huc12s(FACILITY_FILE)
    print(f"Unique HUC-12s to fetch: {len(huc12s):,}")

    # Resume support — skip already-fetched codes
    fetched = set()
    all_features = []
    if OUT_FILE.exists():
        with open(OUT_FILE) as f:
            existing = json.load(f)
        all_features = existing.get('features', [])
        fetched = {str(ft['properties']['huc12']).zfill(12) for ft in all_features}
        print(f"  Resuming: {len(fetched):,} already fetched, {len(huc12s)-len(fetched):,} remaining")

    remaining = [h for h in huc12s if h not in fetched]
    batches = [remaining[i:i+BATCH_SIZE] for i in range(0, len(remaining), BATCH_SIZE)]
    print(f"  {len(batches)} batches of {BATCH_SIZE}")

    session = requests.Session()
    for i, batch in enumerate(batches):
        feats = fetch_batch(batch, session)
        all_features.extend(feats)
        if (i + 1) % 20 == 0 or i == len(batches) - 1:
            # Checkpoint save every 20 batches
            geojson = {'type': 'FeatureCollection', 'features': all_features}
            with open(OUT_FILE, 'w') as f:
                json.dump(geojson, f)
            print(f"  Batch {i+1}/{len(batches)}: {len(all_features):,} polygons saved")
        else:
            print(f"  Batch {i+1}/{len(batches)}: +{len(feats)} features")
        time.sleep(0.3)

    geojson = {'type': 'FeatureCollection', 'features': all_features}
    with open(OUT_FILE, 'w') as f:
        json.dump(geojson, f)
    print(f"\nDone. Saved {len(all_features):,} HUC-12 polygons to {OUT_FILE}")


if __name__ == '__main__':
    main()
