"""
Add pfas_detected and li_detected labels for ALL US states (no state filter).

KEY CHANGE from midwest version: removed STATES_8 filter — processes ALL states.

Input:  national_facility_features_v4.csv
Output: national_facility_features_v5.csv
"""

import csv
from pathlib import Path

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE   = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
UCMR5  = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/ucmr5-occurrence-data/UCMR5_All.txt')
# ──────────────────────────────────────────────────────────────────────────────


def main():
    print("Reading UCMR5_All.txt (all states)...")
    pfas_tested = set()
    pfas_detect = set()
    li_tested   = set()
    li_detect   = set()

    with open(UCMR5, newline='', encoding='latin-1') as f:
        reader = csv.DictReader(f, delimiter='\t')
        for row in reader:
            # ── KEY CHANGE: no state filter ───────────────────────────────────
            key     = (row['PWSID'].strip(), row['FacilityID'].strip())
            contam  = row['Contaminant'].strip()
            detected = (row['AnalyticalResultsSign'].strip() == '=')

            if contam == 'lithium':
                li_tested.add(key)
                if detected:
                    li_detect.add(key)
            else:
                pfas_tested.add(key)
                if detected:
                    pfas_detect.add(key)

    print(f"  PFAS: {len(pfas_tested):,} facilities tested, {len(pfas_detect):,} with detection "
          f"({len(pfas_detect)/len(pfas_tested)*100:.1f}%)")
    print(f"  Li:   {len(li_tested):,} facilities tested,   {len(li_detect):,} with detection "
          f"({len(li_detect)/len(li_tested)*100:.1f}%)")

    in_path  = BASE / 'national_facility_features_v4.csv'
    out_path = BASE / 'national_facility_features_v5.csv'

    print(f"\nLoading {in_path.name}...")
    with open(in_path, newline='', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        orig_cols  = reader.fieldnames
        facilities = list(reader)
    print(f"  {len(facilities):,} facilities, {len(orig_cols)} columns")

    out_cols = list(orig_cols) + ['pfas_detected', 'li_detected']

    n_pfas_detect = n_pfas_not = n_pfas_missing = 0
    n_li_detect   = n_li_not   = n_li_missing   = 0
    out_rows = []

    for fac in facilities:
        key = (fac['PWSID'].strip(), fac['FacilityID'].strip())
        row = dict(fac)

        if key in pfas_tested:
            row['pfas_detected'] = 1 if key in pfas_detect else 0
            if key in pfas_detect: n_pfas_detect += 1
            else:                  n_pfas_not    += 1
        else:
            row['pfas_detected'] = ''
            n_pfas_missing += 1

        if key in li_tested:
            row['li_detected'] = 1 if key in li_detect else 0
            if key in li_detect: n_li_detect += 1
            else:                n_li_not    += 1
        else:
            row['li_detected'] = ''
            n_li_missing += 1

        out_rows.append(row)

    with open(out_path, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=out_cols, extrasaction='ignore')
        w.writeheader()
        w.writerows(out_rows)

    print(f"\nOutput: {out_path}")
    print(f"  Total rows:    {len(out_rows):,}")
    print(f"  Total columns: {len(out_cols)}")
    print()
    print("  pfas_detected:")
    print(f"    1 (detected):     {n_pfas_detect:,}")
    print(f"    0 (not detected): {n_pfas_not:,}")
    print(f"    '' (not in UCMR5):{n_pfas_missing:,}")
    print()
    print("  li_detected:")
    print(f"    1 (detected):     {n_li_detect:,}")
    print(f"    0 (not detected): {n_li_not:,}")
    print(f"    '' (not in UCMR5):{n_li_missing:,}")


if __name__ == '__main__':
    main()
