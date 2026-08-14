"""
Extract SELLER_PWSID from SDWA_FACILITIES.csv for ALL US states (no state filter),
then join to facility table.

KEY CHANGE from midwest version: removed MIDWEST8 state filter at the PWSID check.
All other logic is identical.

Outputs:
  purchaser_seller_national.csv
  national_facility_features_v2.csv
"""

import csv, io, zlib

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE     = '/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes'
ZIP_PATH = '/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/SDWA_latest_downloads.zip'
# ──────────────────────────────────────────────────────────────────────────────


def extract_csv_from_zip(zip_path, target_filename):
    target = target_filename.encode()
    with open(zip_path, 'rb') as f:
        data = f.read()
    pos = 0
    while True:
        pos = data.find(b'PK\x03\x04', pos)
        if pos == -1:
            break
        if pos + 30 < len(data):
            fname_len  = int.from_bytes(data[pos+26:pos+28], 'little')
            extra_len  = int.from_bytes(data[pos+28:pos+30], 'little')
            fname_end  = pos + 30 + fname_len
            fname      = data[pos+30:fname_end]
            if target in fname:
                comp_size  = int.from_bytes(data[pos+18:pos+22], 'little')
                data_start = fname_end + extra_len
                compressed = data[data_start:data_start + comp_size]
                decompressed = zlib.decompress(compressed, -15)
                return decompressed.decode('utf-8', errors='replace')
        pos += 1
    return None


def main():
    print("Extracting SDWA_FACILITIES.csv from ZIP...")
    content = extract_csv_from_zip(ZIP_PATH, 'SDWA_FACILITIES.csv')
    if content is None:
        raise RuntimeError("SDWA_FACILITIES.csv not found in ZIP")
    print(f"  Decompressed: {len(content)/1e6:.1f} MB")

    purchaser_to_sellers = {}
    reader = csv.reader(io.StringIO(content))
    headers = next(reader)
    col = {h: i for i, h in enumerate(headers)}
    print(f"  Columns: {list(col.keys())}")

    n_all = 0
    for row in reader:
        if not row:
            continue
        pwsid = row[col['PWSID']].strip()
        # ── KEY CHANGE: no state filter — process all PWSID prefixes ──
        fac_type = row[col['FACILITY_TYPE_CODE']].strip()
        if fac_type != 'CC':
            continue
        seller_pwsid = row[col['SELLER_PWSID']].strip()
        if not seller_pwsid:
            continue
        activity    = row[col['FACILITY_ACTIVITY_CODE']].strip()
        seller_name = row[col['SELLER_PWS_NAME']].strip()
        if pwsid not in purchaser_to_sellers:
            purchaser_to_sellers[pwsid] = []
        purchaser_to_sellers[pwsid].append((seller_pwsid, seller_name, activity))
        n_all += 1

    print(f"  National CC facilities with seller: {n_all:,}")
    print(f"  Unique purchaser PWS: {len(purchaser_to_sellers):,}")

    purchaser_map = {}
    for pwsid, sellers in purchaser_to_sellers.items():
        active = [(s, n, a) for s, n, a in sellers if a == 'A']
        chosen = active[0] if active else sellers[0]
        purchaser_map[pwsid] = {
            'seller_pwsid': chosen[0],
            'seller_pws_name': chosen[1],
            'immediate_seller_pwsid': chosen[0],
        }

    print("Extracting SDWA_PUB_WATER_SYSTEMS.csv from ZIP for seller source...")
    pws_content = extract_csv_from_zip(ZIP_PATH, 'SDWA_PUB_WATER_SYSTEMS.csv')
    sdwa_source = {}
    if pws_content:
        pws_reader = csv.reader(io.StringIO(pws_content))
        pws_headers = next(pws_reader)
        pws_col = {h: i for i, h in enumerate(pws_headers)}
        seen = set()
        for row in pws_reader:
            if not row:
                continue
            pid = row[pws_col['PWSID']].strip()
            if pid in seen:
                continue
            seen.add(pid)
            src_code = row[pws_col['PRIMARY_SOURCE_CODE']].strip()
            gw_sw    = row[pws_col['GW_SW_CODE']].strip()
            sdwa_source[pid] = {'primary_source_code': src_code, 'gw_sw_code': gw_sw}
        print(f"  Loaded {len(sdwa_source):,} records from SDWA_PUB_WATER_SYSTEMS")

    PURCHASED_CODES = {'SWP', 'GWP', 'GWGWI'}

    def resolve_ultimate_seller(start_pwsid, purchaser_map, sdwa_source, max_hops=10):
        visited = set()
        current = start_pwsid
        ultimate = current
        chain_depth = 0
        while chain_depth < max_hops:
            visited.add(current)
            src = sdwa_source.get(current, {})
            gw_sw = src.get('gw_sw_code', '')
            src_code = src.get('primary_source_code', '')
            if src_code not in PURCHASED_CODES and gw_sw in ('GW', 'SW', ''):
                break
            next_info = purchaser_map.get(current)
            if not next_info:
                break
            next_seller = next_info['seller_pwsid']
            if next_seller in visited:
                break
            ultimate = next_seller
            current = next_seller
            chain_depth += 1
        return ultimate, chain_depth

    print("Resolving purchase chains to ultimate source...")
    for pwsid, info in purchaser_map.items():
        immediate = info['immediate_seller_pwsid']
        ultimate, depth = resolve_ultimate_seller(immediate, purchaser_map, sdwa_source)
        info['ultimate_seller_pwsid'] = ultimate
        info['seller_chain_depth'] = depth

    n_chained = sum(1 for info in purchaser_map.values() if info['seller_chain_depth'] > 0)
    print(f"  Purchasers with multi-hop chains: {n_chained}")

    mapping_out = f'{BASE}/purchaser_seller_national.csv'
    with open(mapping_out, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=[
            'purchaser_pwsid', 'seller_pwsid', 'seller_pws_name',
            'seller_primary_source_code', 'seller_gw_sw_code',
            'ultimate_seller_pwsid', 'ultimate_seller_source_code',
            'ultimate_seller_gw_sw', 'seller_chain_depth'])
        w.writeheader()
        for pwsid, info in sorted(purchaser_map.items()):
            sid  = info['seller_pwsid']
            sdwa_row = sdwa_source.get(sid, {})
            ult  = info['ultimate_seller_pwsid']
            ult_sdwa = sdwa_source.get(ult, {})
            w.writerow({
                'purchaser_pwsid':             pwsid,
                'seller_pwsid':                sid,
                'seller_pws_name':             info['seller_pws_name'],
                'seller_primary_source_code':  sdwa_row.get('primary_source_code', ''),
                'seller_gw_sw_code':           sdwa_row.get('gw_sw_code', ''),
                'ultimate_seller_pwsid':       ult,
                'ultimate_seller_source_code': ult_sdwa.get('primary_source_code', ''),
                'ultimate_seller_gw_sw':       ult_sdwa.get('gw_sw_code', ''),
                'seller_chain_depth':          info['seller_chain_depth'],
            })
    print(f"\nSaved purchaser-seller map: {mapping_out} ({len(purchaser_map)} rows)")

    # ── Join to national facility features table ─────────────────────────────
    features_path = f'{BASE}/national_facility_features.csv'
    out_path      = f'{BASE}/national_facility_features_v2.csv'

    print(f"Joining to {features_path}...")
    with open(features_path, newline='') as f:
        reader2 = csv.DictReader(f)
        fac_cols   = reader2.fieldnames
        facilities = list(reader2)

    new_cols = ['seller_pwsid', 'seller_pws_name',
                'seller_primary_source_code', 'seller_gw_sw_code',
                'ultimate_seller_pwsid', 'ultimate_seller_source_code',
                'ultimate_seller_gw_sw', 'seller_chain_depth']
    all_cols = fac_cols + new_cols

    n_matched = 0
    out_rows = []
    for fac in facilities:
        pwsid = fac['PWSID'].strip()
        info = purchaser_map.get(pwsid, {})
        if info:
            n_matched += 1
        row = dict(fac)
        if info:
            sid = info['seller_pwsid']
            ult = info['ultimate_seller_pwsid']
            row['seller_pwsid']               = sid
            row['seller_pws_name']            = info['seller_pws_name']
            row['seller_primary_source_code'] = sdwa_source.get(sid, {}).get('primary_source_code', '')
            row['seller_gw_sw_code']          = sdwa_source.get(sid, {}).get('gw_sw_code', '')
            row['ultimate_seller_pwsid']      = ult
            row['ultimate_seller_source_code']= sdwa_source.get(ult, {}).get('primary_source_code', '')
            row['ultimate_seller_gw_sw']      = sdwa_source.get(ult, {}).get('gw_sw_code', '')
            row['seller_chain_depth']         = info['seller_chain_depth']
        else:
            for c in new_cols:
                row[c] = ''
        out_rows.append(row)

    with open(out_path, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=all_cols, extrasaction='ignore')
        w.writeheader()
        w.writerows(out_rows)

    print(f"\nOutput: {out_path}")
    print(f"  Total rows : {len(out_rows)}")
    print(f"  Facilities with seller matched: {n_matched}")


if __name__ == '__main__':
    main()
