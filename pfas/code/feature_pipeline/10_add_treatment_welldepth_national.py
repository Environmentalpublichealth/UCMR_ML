"""
Add treatment and well-depth features to national_facility_features_v5.csv → v6.csv.

KEY CHANGES from midwest version:
  - Reads national_facility_features_v5.csv
  - Output: national_facility_features_v6.csv
  - SDWA_FACILITIES.csv and SDWA_SITE_VISITS.csv are both read directly out of
    SDWA_latest_downloads.zip via zipfile — no disk extraction, no /tmp dependency.
  - All logic identical to add_treatment_welldepth.py
"""

import csv, io, zipfile
from pathlib import Path
import numpy as np
import pandas as pd
from pyproj import Transformer

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE     = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
UCMR_DIR = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR')
# ──────────────────────────────────────────────────────────────────────────────

V5_CSV   = BASE / 'national_facility_features_v5.csv'
V6_CSV   = BASE / 'national_facility_features_v6.csv'
ADE_TXT  = UCMR_DIR / 'ucmr5-occurrence-data' / 'UCMR5_AddtlDataElem.txt'
SDWA_ZIP = UCMR_DIR / 'SDWA_latest_downloads.zip'
GRIDS_ZIP= BASE / 'public_grids.zip'
SELLER_LOOKUP = BASE / 'seller_huc12_lookup_national.csv'

CONVENTIONAL = {'CON', 'CWL', 'CWA', 'GWD', 'SSF', 'SFN', 'DAF', 'RBF', 'BIO', 'INF', 'POB'}

# Well depth is a property of the specific well's location — a purely
# purchased facility has no well of its own, so sampling its own coordinates
# measures the wrong location entirely. Same source-water categories used by
# pfas_source_corrections/add_li_sources/add_airport_features/add_landfill_features.
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


def load_seller_lookup(path):
    seller_lookup = {}
    with open(path) as f:
        for r in csv.DictReader(f):
            pid = r['seller_pwsid'].strip()
            if not pid:
                continue
            try:
                lat, lon = float(r['lat']), float(r['lon'])
            except (ValueError, TypeError):
                lat = lon = None
            seller_lookup[pid] = {'lat': lat, 'lon': lon}
    return seller_lookup


def resolve_effective_coords(feat_df: pd.DataFrame, seller_lookup: dict):
    """Per-facility (lat, lon) to sample rasters at: seller's location for
    purely-purchased facilities, own location otherwise."""
    eff_lats, eff_lons = [], []
    n_purely = n_own = n_mixed = n_unknown = n_seller_missing = 0
    for _, row in feat_df.iterrows():
        ult_seller = str(row.get('ultimate_seller_pwsid', '')).strip()
        is_purch = any(str(row.get(c, '')).strip() == '1' for c in PURCHASED_PS_COLS)
        is_own   = any(str(row.get(c, '')).strip() == '1' for c in OWN_SOURCE_PS_COLS)
        own_lat, own_lon = row.get('query_lat'), row.get('query_lon')

        seller_lat = seller_lon = None
        if ult_seller and ult_seller in seller_lookup:
            sl = seller_lookup[ult_seller]
            if sl['lat'] is not None:
                seller_lat, seller_lon = sl['lat'], sl['lon']

        if is_purch and not is_own:
            if seller_lat is not None:
                eff_lats.append(seller_lat); eff_lons.append(seller_lon)
            else:
                eff_lats.append(own_lat); eff_lons.append(own_lon)
                n_seller_missing += 1
            n_purely += 1
        else:
            eff_lats.append(own_lat); eff_lons.append(own_lon)
            if is_purch and is_own:
                n_mixed += 1
            elif is_own:
                n_own += 1
            else:
                n_unknown += 1

    print(f"  Source-water category: purely_purchased={n_purely:,} (seller coords missing "
          f"for {n_seller_missing:,} of those)  mixed={n_mixed:,}  own={n_own:,}  unknown={n_unknown:,}")
    return np.array(eff_lats, dtype=float), np.array(eff_lons, dtype=float)


def load_ade_treatment(feat_keys: set) -> pd.DataFrame:
    print("Loading UCMR5_AddtlDataElem.txt (national)...")
    ade = pd.read_csv(ADE_TXT, sep='\t', low_memory=False)
    ade['PWSID']      = ade['PWSID'].astype(str).str.strip()
    ade['FacilityID'] = ade['FacilityID'].astype(str).str.strip()
    # No state filter — use all records that match our facility keys
    ade_nat = ade[ade.apply(lambda r: (r['PWSID'], r['FacilityID']) in feat_keys, axis=1)].copy()
    print(f"  ADE national records matched: {len(ade_nat):,}")

    ti = ade_nat[ade_nat['AdditionalDataElement'] == 'TreatmentInformation']
    ti_grp = ti.groupby(['PWSID', 'FacilityID'])['Response'].apply(set)
    ti_df = pd.DataFrame(index=ti_grp.index)
    ti_df['treat_gac'] = ti_grp.apply(lambda s: 1 if 'GAC' in s else 0)
    ti_df['treat_pac'] = ti_grp.apply(lambda s: 1 if 'PAC' in s else 0)
    ti_df['treat_mfl'] = ti_grp.apply(lambda s: 1 if 'MFL' in s else 0)
    ti_df['treat_iex'] = ti_grp.apply(lambda s: 1 if 'IEX' in s else 0)
    ti_df['treat_con'] = ti_grp.apply(lambda s: 1 if s & CONVENTIONAL else 0)
    ti_df['treat_gwd'] = ti_grp.apply(lambda s: 1 if 'GWD' in s else 0)
    ti_df = ti_df.reset_index()

    pt = ade_nat[ade_nat['AdditionalDataElement'] == 'PFASTreatment']
    pt_grp = pt.groupby(['PWSID', 'FacilityID'])['Response'].apply(set)
    pt_df = pd.DataFrame({
        'PWSID':        pt_grp.index.get_level_values(0),
        'FacilityID':   pt_grp.index.get_level_values(1),
        'pfas_treat_any': pt_grp.apply(lambda s: 0 if s <= {'NMT'} else 1).values,
    })

    dt = ade_nat[ade_nat['AdditionalDataElement'] == 'DisinfectantType']
    dt_grp = dt.groupby(['PWSID', 'FacilityID'])['Response'].apply(set)
    dt_df = pd.DataFrame(index=dt_grp.index)
    dt_df['disinfect_none']  = dt_grp.apply(lambda s: 1 if 'NODU' in s else 0)
    dt_df['disinfect_uv']    = dt_grp.apply(lambda s: 1 if 'ULVL' in s else 0)
    dt_df['disinfect_ozone'] = dt_grp.apply(lambda s: 1 if 'OZON' in s else 0)
    dt_df = dt_df.reset_index()

    result = (ti_df.merge(pt_df, on=['PWSID', 'FacilityID'], how='outer')
                   .merge(dt_df, on=['PWSID', 'FacilityID'], how='outer'))
    print(f"  Combined ADE features: {len(result):,} facilities")
    return result


def load_sdwa_treatment(feat_pwsids: set) -> pd.DataFrame:
    print("\nLoading SDWA_FACILITIES for treatment...")
    with zipfile.ZipFile(SDWA_ZIP) as zf:
        with zf.open('SDWA_FACILITIES.csv') as f:
            sdwa = pd.read_csv(f, low_memory=False,
                               usecols=['PWSID', 'FACILITY_TYPE_CODE',
                                        'IS_SOURCE_TREATED_IND', 'FILTRATION_STATUS_CODE'])
    sdwa['PWSID'] = sdwa['PWSID'].astype(str).str.strip()
    sdwa = sdwa[sdwa['PWSID'].isin(feat_pwsids)]
    print(f"  SDWA rows for our PWS: {len(sdwa):,}")

    def agg_treated(series):
        vals = series.dropna().values
        if 'Y' in vals: return 1.0
        if 'N' in vals: return 0.0
        return float('nan')

    def agg_filtered(series):
        return 1.0 if len(series.dropna()) > 0 else 0.0

    pws_treat = sdwa.groupby('PWSID').agg(
        sdwa_treated=('IS_SOURCE_TREATED_IND', agg_treated),
        sdwa_filtered=('FILTRATION_STATUS_CODE', agg_filtered),
    ).reset_index()
    print(f"  PWS matched: {len(pws_treat):,}")
    return pws_treat


def load_snsv_count(feat_pwsids: set) -> pd.DataFrame:
    print("\nLoading SDWA_SITE_VISITS for sanitary survey counts...")
    with zipfile.ZipFile(SDWA_ZIP) as zf:
        with zf.open('SDWA_SITE_VISITS.csv') as f:
            sv = pd.read_csv(f, low_memory=False, usecols=['PWSID', 'VISIT_REASON_CODE'])
    sv['PWSID'] = sv['PWSID'].astype(str).str.strip()
    sv_nat = sv[sv['PWSID'].isin(feat_pwsids)].copy()
    n_snsv = (sv_nat[sv_nat['VISIT_REASON_CODE'] == 'SNSV']
              .groupby('PWSID').size().reset_index(name='n_snsv'))
    all_pws = pd.DataFrame({'PWSID': list(feat_pwsids)})
    result = all_pws.merge(n_snsv, on='PWSID', how='left')
    result['n_snsv'] = result['n_snsv'].fillna(0).astype(int)
    print(f"  PWS with ≥1 SNSV: {(result['n_snsv'] > 0).sum():,}")
    return result


def read_asc_header(lines):
    hdr = {}; n = 0
    for line in lines:
        parts = line.strip().split()
        if len(parts) == 2:
            key = parts[0].lower()
            try:
                hdr[key] = float(parts[1]); n += 1
            except ValueError:
                break
        else:
            break
    return hdr, n


def load_asc_raster(zf: zipfile.ZipFile, name: str):
    print(f"  Reading {name}...")
    with zf.open(name) as f:
        raw = f.read().decode('ascii')
    lines = raw.split('\n')
    hdr, n_hdr = read_asc_header(lines[:10])
    nodata = hdr.get('nodata_value', -9999)
    ncols = int(hdr['ncols']); nrows = int(hdr['nrows'])
    data_text = '\n'.join(lines[n_hdr:])
    arr = np.fromstring(data_text, dtype=np.float32, sep=' ')
    arr = arr[:nrows * ncols].reshape(nrows, ncols)
    arr[arr == nodata] = np.nan
    arr[arr < -9000] = np.nan
    return hdr, arr


def sample_raster(hdr, arr, x_albers, y_albers):
    xll = hdr['xllcorner']; yll = hdr['yllcorner']; cell = hdr['cellsize']
    nrows, ncols = arr.shape
    col_i = np.round((x_albers - xll) / cell).astype(int)
    row_i = np.round(nrows - 1 - (y_albers - yll) / cell).astype(int)
    valid = (col_i >= 0) & (col_i < ncols) & (row_i >= 0) & (row_i < nrows)
    result = np.full(len(x_albers), np.nan)
    result[valid] = arr[row_i[valid], col_i[valid]]
    return result


def load_well_depths(feat_df: pd.DataFrame, seller_lookup: dict) -> pd.DataFrame:
    print("\nLoading USGS well depth grids (public_grids.zip)...")
    gw_mask = feat_df['FacilityWaterType'].isin(['GW', 'GU'])
    gw = feat_df[gw_mask & feat_df['query_lat'].notna() & feat_df['query_lon'].notna()].copy()
    print(f"  GW/GU facilities with coords: {len(gw):,}")

    eff_lat, eff_lon = resolve_effective_coords(gw, seller_lookup)
    valid_eff = np.isfinite(eff_lat) & np.isfinite(eff_lon)
    gw = gw[valid_eff].copy()
    eff_lat, eff_lon = eff_lat[valid_eff], eff_lon[valid_eff]

    transformer = Transformer.from_crs('EPSG:4326', 'EPSG:5070', always_xy=True)
    x_alb, y_alb = transformer.transform(eff_lon, eff_lat)
    grids = {
        'well_top_open_ft':   'public_top_open.asc',
        'well_screen_len_ft': 'public_len_open.asc',
    }
    result = gw[['PWSID', 'FacilityID']].copy()
    with zipfile.ZipFile(GRIDS_ZIP) as zf:
        for col_name, fname in grids.items():
            hdr, arr = load_asc_raster(zf, fname)
            vals = sample_raster(hdr, arr, x_alb, y_alb)
            result[col_name] = vals
            n_valid = np.isfinite(vals).sum()
            print(f"    {col_name}: {n_valid:,}/{len(gw):,} valid")
            del arr
    return result


def main():
    print(f"Loading {V5_CSV.name}...")
    feat = pd.read_csv(V5_CSV, low_memory=False)
    feat['PWSID']      = feat['PWSID'].astype(str).str.strip()
    feat['FacilityID'] = feat['FacilityID'].astype(str).str.strip()
    feat['query_lat']  = pd.to_numeric(feat['query_lat'], errors='coerce')
    feat['query_lon']  = pd.to_numeric(feat['query_lon'], errors='coerce')
    print(f"  {feat.shape[0]:,} rows × {feat.shape[1]} cols")

    n_before = len(feat)
    feat = feat.drop_duplicates(subset=['PWSID', 'FacilityID'], keep='first').reset_index(drop=True)
    if len(feat) < n_before:
        print(f"  Dropped {n_before - len(feat)} duplicate PWSID+FacilityID rows")

    feat_keys   = set(zip(feat['PWSID'], feat['FacilityID']))
    feat_pwsids = set(feat['PWSID'])

    ade_df  = load_ade_treatment(feat_keys)
    sdwa_df = load_sdwa_treatment(feat_pwsids)
    snsv_df = load_snsv_count(feat_pwsids)

    print("\nLoading seller HUC-12 lookup for source-water well coordinates...")
    seller_lookup = load_seller_lookup(SELLER_LOOKUP)
    print(f"  {len(seller_lookup):,} sellers loaded")
    well_df = load_well_depths(feat, seller_lookup)

    print("\nMerging new features...")
    out = feat.copy()
    out = out.merge(ade_df,  on=['PWSID', 'FacilityID'], how='left')
    out = out.merge(sdwa_df, on='PWSID', how='left')
    out = out.merge(snsv_df, on='PWSID', how='left')
    out = out.merge(well_df, on=['PWSID', 'FacilityID'], how='left')
    print(f"\nFinal shape: {out.shape[0]:,} rows × {out.shape[1]} cols")

    out.to_csv(V6_CSV, index=False)
    print(f"\nOutput: {V6_CSV}")


if __name__ == '__main__':
    main()
