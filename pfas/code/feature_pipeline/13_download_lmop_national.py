"""
Download EPA LMOP landfill data for all 50 US states.
Output: landfill_lmop_national.csv

EPA LMOP URL pattern (March 2024 update):
  https://www.epa.gov/system/files/documents/2024-03/lmopdata{state_abbr}.xlsx

State abbreviations cover: AL AK AZ AR CA CO CT DE FL GA HI ID IL IN IA KS
KY LA ME MD MA MI MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD
TN TX UT VT VA WA WV WI WY

Only open landfills are used in add_landfill_features_national.py, but the full
dataset (including closed) is saved here for reference.
"""

import time
import requests
import pandas as pd
from pathlib import Path
from io import BytesIO

# ── EDIT THIS for your HPC environment ────────────────────────────────────────
BASE_DIR = Path('/Users/jiali/Desktop/Jiali/TAMU/environment/UCMR/codes')
# ──────────────────────────────────────────────────────────────────────────────

OUT_CSV = BASE_DIR / 'landfill_lmop_national.csv'

ALL_STATES = [
    'AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA',
    'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD',
    'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ',
    'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC',
    'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY',
]
LMOP_URL = 'https://www.epa.gov/system/files/documents/2024-03/lmopdata{state}.xlsx'


def download_state(state: str, session: requests.Session) -> pd.DataFrame | None:
    url = LMOP_URL.format(state=state.lower())
    for attempt in range(3):
        try:
            resp = session.get(url, timeout=60)
            if resp.status_code == 200:
                # Each LMOP file has two sheets; landfill data is on sheet 1 (index 1)
                # Try both sheets in case structure varies
                try:
                    df = pd.read_excel(BytesIO(resp.content), sheet_name=1,
                                       header=0, engine='openpyxl')
                except Exception:
                    df = pd.read_excel(BytesIO(resp.content), sheet_name=0,
                                       header=0, engine='openpyxl')
                if df.empty:
                    print(f"  {state}: empty sheet")
                    return None
                df['State_Downloaded'] = state
                return df
            elif resp.status_code == 404:
                print(f"  {state}: 404 (no LMOP data)")
                return None
            else:
                print(f"  {state}: HTTP {resp.status_code}, retrying...")
                time.sleep(5)
        except Exception as e:
            print(f"  {state}: error {e}, retrying...")
            time.sleep(5 * (attempt + 1))
    print(f"  {state}: failed after 3 attempts")
    return None


def main():
    session = requests.Session()
    session.headers.update({'User-Agent': 'Mozilla/5.0 UCMR-research'})

    all_frames = []
    for i, state in enumerate(ALL_STATES):
        print(f"[{i+1}/{len(ALL_STATES)}] Downloading {state}...", flush=True)
        df = download_state(state, session)
        if df is not None:
            print(f"  {state}: {len(df):,} rows, {df.shape[1]} cols")
            all_frames.append(df)
        time.sleep(1.5)  # polite rate limiting

    if not all_frames:
        print("ERROR: No data downloaded")
        return

    combined = pd.concat(all_frames, ignore_index=True, sort=False)
    print(f"\nCombined: {len(combined):,} rows across {len(all_frames)} states")

    # Status counts
    if 'Current Landfill Status' in combined.columns:
        print(combined['Current Landfill Status'].value_counts().to_string())
        open_count = (combined['Current Landfill Status'] == 'Open').sum()
        print(f"\nOpen landfills: {open_count:,}")

    # Count lat/lon coverage
    for lat_col in ['Latitude', 'LAT', 'lat']:
        if lat_col in combined.columns:
            n_coords = combined[lat_col].notna().sum()
            print(f"Records with lat/lon ({lat_col}): {n_coords:,} / {len(combined):,}")
            break

    combined.to_csv(OUT_CSV, index=False)
    print(f"\nSaved: {OUT_CSV}")
    print(f"  {len(combined):,} total landfill records")


if __name__ == '__main__':
    main()
