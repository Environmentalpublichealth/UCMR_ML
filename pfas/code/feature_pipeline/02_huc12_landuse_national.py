"""
Extract NLCD land use percentages for each HUC-12 in huc12_polygons_national.geojson.
Output: huc12_landuse_national.csv

Checkpoint support: saves progress every 500 polygons to
huc12_landuse_national_checkpoint.csv. Resumes automatically if interrupted.
Delete the checkpoint file to force a full rerun.
"""

import json, csv
from pathlib import Path
import numpy as np
import pandas as pd
import rasterio
from rasterio.mask import mask
from shapely.geometry import shape
from pyproj import Transformer
from shapely.ops import transform as shapely_transform

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE_DIR  = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
NLCD_FILE = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/Annual_NLCD_LndCov_2024_CU_C1V1/Annual_NLCD_LndCov_2024_CU_C1V1.tif')
# ──────────────────────────────────────────────────────────────────────────────

POLYGONS_FILE    = BASE_DIR / 'huc12_polygons_national.geojson'
OUT_FILE         = BASE_DIR / 'huc12_landuse_national.csv'
CHECKPOINT_FILE  = BASE_DIR / 'huc12_landuse_national_checkpoint.csv'
CHECKPOINT_EVERY = 500

LAND_COVER = {
    11: 'water', 12: 'water',
    21: 'urban', 22: 'urban', 23: 'urban', 24: 'urban',
    31: 'barren',
    41: 'forest', 42: 'forest', 43: 'forest',
    51: 'shrub',  52: 'shrub',
    71: 'grassland', 72: 'grassland', 73: 'grassland', 74: 'grassland',
    81: 'agriculture', 82: 'agriculture',
    90: 'wetland', 95: 'wetland',
}
CATEGORIES = ['water', 'urban', 'barren', 'forest', 'shrub',
              'grassland', 'agriculture', 'wetland', 'other']
FIELDNAMES = ['huc12', 'total_pixels'] + [f'{c}_pct' for c in CATEGORIES]


def compute_landuse(pixels):
    total = len(pixels)
    if total == 0:
        return {f'{c}_pct': 0.0 for c in CATEGORIES}
    counts = {c: 0 for c in CATEGORIES}
    for v in pixels:
        cat = LAND_COVER.get(int(v), 'other')
        counts[cat] += 1
    return {f'{c}_pct': round(counts[c] / total * 100, 4) for c in CATEGORIES}


def main():
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

    new_count = 0
    with rasterio.open(NLCD_FILE) as src:
        raster_crs = src.crs
        print(f"  NLCD CRS: {raster_crs}")
        transformer = Transformer.from_crs('EPSG:4326',
                                           raster_crs.to_epsg() or str(raster_crs),
                                           always_xy=True)

        for feat in remaining:
            huc12 = str(feat['properties']['huc12']).zfill(12)
            geom_wgs84 = shape(feat['geometry'])
            geom_proj = shapely_transform(
                lambda x, y: transformer.transform(x, y), geom_wgs84)
            try:
                out_image, _ = mask(src, [geom_proj.__geo_interface__],
                                    crop=True, nodata=0)
                pixels = out_image[0].flatten()
                pixels = pixels[pixels > 0]
                lu = compute_landuse(pixels)
                lu['huc12'] = huc12
                lu['total_pixels'] = len(pixels)
            except Exception as e:
                print(f"  Warning: {huc12} failed: {e}")
                lu = {f'{c}_pct': None for c in CATEGORIES}
                lu['huc12'] = huc12
                lu['total_pixels'] = 0
            results.append(lu)
            new_count += 1

            if new_count % CHECKPOINT_EVERY == 0:
                pd.DataFrame(results, columns=FIELDNAMES).to_csv(
                    CHECKPOINT_FILE, index=False)
                done = len(results)
                print(f"  Checkpoint: {done:,}/{total:,} ({done/total*100:.1f}%)",
                      flush=True)

    with open(OUT_FILE, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(results)

    if CHECKPOINT_FILE.exists():
        CHECKPOINT_FILE.unlink()
    print(f"Saved: {OUT_FILE}  ({len(results):,} rows)")


if __name__ == '__main__':
    main()
