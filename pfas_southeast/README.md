# PFAS Detection Model — Southeast Regional Arm

A geographically-localized variant of the [national PFAS model](../pfas/):
trained only on Southeast public water systems, tested against North
Carolina's own state PFAS monitoring data. This tests whether a model
trained on similar regional geology/climate/PWS characteristics generalizes
to a new state better than the full national model does.

**Code and data are provided for training and testing but have not been
run** — no models are trained yet, `models/` and `results/` don't exist
until you run the scripts below.

## Region definition

A 12-state Southeast region (South Atlantic + East South Central Census
divisions, excluding DE/MD/DC, which are more commonly grouped with the
Mid-Atlantic/Northeast): Alabama, Arkansas, Florida, Georgia, Kentucky,
Louisiana, Mississippi, North Carolina, South Carolina, Tennessee,
Virginia, West Virginia.

NC is included in training — there's no PWSID overlap with the NC test set
(built from NC's own state PFAS monitoring program, entirely separate PWS
from UCMR5-monitored systems).

## Data (`data/`)

| File | Contents |
|---|---|
| `national_pfas_features_ready.csv` | Training set — the 12-state Southeast subset of `../pfas/data/national_pfas_features_ready.csv`, built by `code/modeling/filter_region.py`. **6,306 rows, 2,793 unique PWS, 22.3% positive.** State breakdown: FL 959, NC 875, AL 781, MS 759, LA 603, GA 590, TN 394, SC 373, KY 319, VA 301, AR 233, WV 119. |
| `nc_test_features.csv` | Test set — **9** North Carolina public water systems (out of 27 total in NC's state PFAS monitoring data; the other 18 lack a resolvable EPA FRS coordinate — this is a genuinely small test set, worth keeping in mind when interpreting results), with the full feature set built fresh via the same pipeline as training. |
| `nc_test_labels.csv` | PWSID + detection label, re-derived directly from NC's reported concentrations at a strict, uniform **4 ng/L cutoff** (NC's own lab reporting limit is 4 ppt, exactly matching EPA's threshold) — not NC's own pre-computed flag. Any compound ≥ 4 ng/L → detected. 27 labeled PWS total (9 with matching features to actually score). |

**Note on the NC test set's source**: NC has a much larger existing PWS
list in `../pfas/`'s original validation-set-construction pass (484 rows,
sourced from a broader state-characteristics file), but that list uses a
*different* PWSID universe than the raw NC PFAS concentration file used to
build the 4 ng/L relabeled set here (only 2 of 27 PWSIDs overlap between
the two). `nc_test_features.csv`/`nc_test_labels.csv` were built directly
from the properly-relabeled 27 PWS, not the larger but mislabeled set.

## Code (`code/`)

- `code/feature_pipeline/15_ml_pipeline_national.py` — shared library (CV, VIF/p-value pruning, feature-engineering utilities). Identical to the copy in `../pfas/code/feature_pipeline/`.
- `code/modeling/filter_region.py` — already run to produce `data/national_pfas_features_ready.csv`; re-run only if the upstream national dataset changes.
- `code/modeling/backward_elim_pfas.py` — backward feature elimination for the Southeast arm (PFAS only — the shared `midwest8`/`national`/lithium code paths in the original script were removed here, along with the now-broken path they relied on, since this folder never had those source files to begin with).
- `code/modeling/train_final_pfas.py` — final RF/XGB training. Uses the **national PFAS hyperparameters as a documented starting default** (no region-specific hyperparameter search has been run) — re-tune with a fresh randomized search if you want region-optimized values instead.
- `code/modeling/save_lr_model.py` — trains and saves the LR baseline (VIF-prune → p-value-prune → LogisticRegression).
- `code/modeling/score_validation.py` — loads the three saved models and scores an external validation feature table (unmodified from `../pfas/`, fully generic).

## How to run

```bash
cd code/modeling

# 1. (already done — data/national_pfas_features_ready.csv is included)
python3 filter_region.py

# 2. Feature selection + final training
python3 backward_elim_pfas.py --model rf
python3 backward_elim_pfas.py --model xgb
python3 train_final_pfas.py --model rf
python3 train_final_pfas.py --model xgb
python3 save_lr_model.py

# 3. Test against NC
python3 score_validation.py \
    --features ../../data/nc_test_features.csv \
    --labels ../../data/nc_test_labels.csv \
    --label-col label --id-col PWSID
```

**Note on paths**: like the main `pfas/` package, the feature-pipeline
scripts contain hardcoded absolute paths to raw data sources — see
`../pfas/README.md` section 6 for the full reproduction caveat. The
modeling scripts here (`filter_region.py` onward) are self-contained and
don't need any path edits.
