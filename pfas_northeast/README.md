# PFAS Detection Model — Northeast Regional Arm

A geographically-localized variant of the [national PFAS model](../pfas/):
trained only on Northeast public water systems, tested against Pennsylvania's
own state PFAS monitoring data. This tests whether a model trained on
similar regional geology/climate/PWS characteristics generalizes to a new
state better than the full national model does.

**Code and data are provided for training and testing but have not been
run** — no models are trained yet, `models/` and `results/` don't exist
until you run the scripts below.

## Region definition

US Census Bureau Northeast region (9 states): Connecticut, Maine,
Massachusetts, New Hampshire, New Jersey, New York, Pennsylvania, Rhode
Island, Vermont.

PA is included in training — it's a Census Bureau Northeast state, and
there's no PWSID overlap with the PA test set (built from PA's own state
PFAS monitoring program, entirely separate PWS from UCMR5-monitored
systems).

## Data (`data/`)

| File | Contents |
|---|---|
| `national_pfas_features_ready.csv` | Training set — the 9-state Northeast subset of `../pfas/data/national_pfas_features_ready.csv`, built by `code/modeling/filter_region.py`. **3,739 rows, 1,439 unique PWS, 37.8% positive.** State breakdown: NY 943, PA 914, NJ 791, MA 633, CT 172, NH 115, ME 62, RI 60, VT 49. |
| `pa_test_features.csv` | Test set — 44 PA public water systems (out of 87 total in PA's state PFAS monitoring data; the other 43 lack a resolvable EPA FRS coordinate), with the full feature set built fresh via the same pipeline as training (same rasters, same spatial joins, same SDWIS pull — see `../pfas/README.md` section 5 for the full external-validation methodology). |
| `pa_test_labels.csv` | PWSID + detection label, re-derived directly from PA's reported concentrations at a strict, uniform **4 ng/L cutoff** (PA's own lab reporting limit is ~1.9-2.0 ng/L, confirmed below EPA's threshold) — not PA's own pre-computed flag. Any compound ≥ 4 ng/L → detected. 87 labeled PWS total (44 with matching features to actually score). |

## Code (`code/`)

- `code/feature_pipeline/15_ml_pipeline_national.py` — shared library (CV, VIF/p-value pruning, feature-engineering utilities). Identical to the copy in `../pfas/code/feature_pipeline/`.
- `code/modeling/filter_region.py` — already run to produce `data/national_pfas_features_ready.csv`; re-run only if the upstream national dataset changes.
- `code/modeling/backward_elim_pfas_li.py` — backward feature elimination; accepts `--arm northeast` (added alongside the existing `midwest8`/`national` options).
- `code/modeling/train_final_pfas_li.py` — final RF/XGB training; also accepts `--arm northeast`. Uses the **national PFAS hyperparameters as a documented starting default** (no region-specific hyperparameter search has been run) — re-tune with a fresh randomized search if you want region-optimized values instead.
- `code/modeling/save_lr_model.py` — trains and saves the LR baseline (VIF-prune → p-value-prune → LogisticRegression).
- `code/modeling/score_validation.py` — loads the three saved models and scores an external validation feature table (unmodified from `../pfas/`, fully generic).

## How to run

```bash
cd code/modeling

# 1. (already done — data/national_pfas_features_ready.csv is included)
python3 filter_region.py

# 2. Feature selection + final training
python3 backward_elim_pfas_li.py --arm northeast --target pfas --model rf
python3 backward_elim_pfas_li.py --arm northeast --target pfas --model xgb
python3 train_final_pfas_li.py --arm northeast --target pfas --model rf
python3 train_final_pfas_li.py --arm northeast --target pfas --model xgb
python3 save_lr_model.py

# 3. Test against PA
python3 score_validation.py \
    --features ../../data/pa_test_features.csv \
    --labels ../../data/pa_test_labels.csv \
    --label-col label --id-col PWSID
```

**Note on paths**: like the main `pfas/` package, the feature-pipeline
scripts contain hardcoded absolute paths to raw data sources — see
`../pfas/README.md` section 6 for the full reproduction caveat. The
modeling scripts here (`filter_region.py` onward) are self-contained and
don't need any path edits.
