# PFAS Detection Model

Machine-learning models predicting whether a U.S. public water system (PWS)
will have a PFAS detection, trained on EPA's Fifth Unregulated Contaminant
Monitoring Rule (UCMR5) national drinking-water monitoring data plus a suite
of geospatial, hydrogeologic, and administrative features.

Three models are provided: **Logistic Regression** (interpretable baseline),
**Random Forest**, and **XGBoost** (final, best-performing models).

## Contents

```
pfas/
  code/
    feature_pipeline/   # build the national facility feature table from raw sources
    modeling/            # backward elimination, final training, model I/O
    plots/                # SHAP / ROC visualization scripts
  data/                  # finished, ready-to-train feature table + selected feature lists
  models/                 # trained model artifacts (joblib)
  results/
    metrics/              # CV results, permutation/SHAP importance, PWS-level metrics
    plots/                 # ROC curves, confusion matrices, SHAP plots
  README.md              # this file
```

## 1. Data sources

| Source | Used for |
|---|---|
| EPA UCMR5 (`UCMR5_All.txt`, `UCMR5_AddtlDataElem.txt`) | PFAS detection labels; some treatment metadata |
| EPA SDWIS / SDWA_latest_downloads.zip | PWS administrative characteristics (type, primary source, population served, source-water-protection status, treatment codes) |
| EPA FRS (Facility Registry Service) | Facility-level coordinates |
| USGS Watershed Boundary Dataset (WBD) | HUC-12 watershed assignment |
| NLCD (National Land Cover Database) | Land-use composition per HUC-12 |
| PRISM | Annual precipitation |
| USGS national well-construction grids (`public_grids.zip`) | Well top-of-open-interval / screen-length rasters |
| USGS principal aquifers shapefile | Aquifer rock-type classification (`ROCK_*`) |
| USGS NURE geochemical survey | Uranium point-density (`geochem_U_count`) |
| EPA FUDS, DoD military installations (MIRTA), airports (Part 139), landfills (LMOP), industrial dischargers/manufacturers (ECHO/CDR) | Distance-to-source and source-count-per-HUC12 features specific to PFAS exposure pathways |

## 2. Feature engineering pipeline (`code/feature_pipeline/`)

Run in numeric order to reproduce `data/national_pfas_features_ready.csv` from
raw sources:

| Script | Purpose |
|---|---|
| `00_build_national_facility_base.py` | UCMR5 facility list + FRS/SDWIS coordinates + principal aquifer join |
| `01_huc12_fetch_polygons_national.py` | Fetch HUC-12 watershed polygons from the USGS WBD API |
| `02_huc12_landuse_national.py` | NLCD land-use % composition per HUC-12 |
| `03_huc12_geochem_national.py` | NURE geochemistry point density per HUC-12 |
| `04_pfas_distance_national.py` | Distance to nearest PFAS discharge/manufacturing site, military, FUDS, airport, industry, landfill, WWTP |
| `05_extract_seller_connections_national.py` | Identify purchased-water systems and their wholesale seller |
| `06_build_facility_features_national.py` | Join PWS-level SDWIS characteristics |
| `07_add_detection_labels_national.py` | Build `pfas_detected` label from UCMR5 (any of ~29 PFAS analytes quantified) |
| `08_seller_huc12_national.py` | HUC-12 assignment for wholesale sellers (for purchased-water systems) |
| `09_pfas_source_corrections_national.py` | PFAS-specific source-distance corrections |
| `10_add_treatment_welldepth_national.py` | Treatment/disinfection codes (SDWA) + well-construction rasters (USGS grids) |
| `12_add_airport_features_national.py` | Airport distance/density (PFAS firefighting-foam pathway) |
| `13_download_lmop_national.py` | EPA LMOP landfill list |
| `14_add_landfill_features_national.py` | Landfill distance/density |
| `14b_merge_midwest8_national.py` | Merge with the separately-built 8-state Midwest pilot arm |
| `propagate_pooled_distances_to_ready.py` | Propagate the pooled PFAS-source-cluster distance fix into the ready table |
| `15_ml_pipeline_national.py` | Shared library: `DROP_ALWAYS`/feature-exclusion policy, CV, VIF/p-value pruning, model training utilities — imported by the modeling scripts, not run standalone |
| `16_prepare_features_national.py` | Final feature engineering (log1p transforms, CLR land-use transform, one-hot encoding) → `national_pfas_features_ready.csv` |

**Feature-exclusion policy** (see `DROP_ALWAYS` in `15_ml_pipeline_national.py`):
administrative/business-classification fields with no plausible PFAS exposure
mechanism and confirmed near-zero permutation importance were excluded from
training (`Service Area_*`, `# of Facilities`, `Is Wholesaler`, `is_purchased`,
`Primary Source_Ground water purchased`, `seller_chain_depth` handling) — this
was an explicit, evidence-based decision, not a blind interpretability cut;
`PWS Type_*` and `Primary Source_Surface water purchased` were deliberately
kept despite looking similar.

## 3. Model training (`code/modeling/`)

| Script | Purpose |
|---|---|
| `backward_elim_pfas_li.py` | Backward feature elimination (RFE) with Spearman collinearity dedup, producing `data/backward_elim_national_pfas_{rf,xgb}_final_features.csv` |
| `train_final_pfas_li.py` | Train the final RF/XGB models on the backward-elimination-selected features, 5-fold GroupKFold CV (grouped by PWSID) |
| `save_lr_model.py` | Train and persist the LR baseline (VIF-prune → p-value-prune → LogisticRegression) — this pipeline previously only existed inline inside the ROC-plotting script; this script makes it a reusable, saved artifact |
| `score_validation.py` | Load all three saved models and score an external validation feature table |

**Cross-validation**: 5-fold `GroupKFold`/`StratifiedGroupKFold`, grouped by
PWSID (so no PWSID's facilities span both train and test folds),
`random_state=42` throughout.

**Hyperparameters** (from randomized search, see `rf_final_summary.txt` /
`xgb_final_summary.txt` in `results/metrics/`):
- RF: `class_weight='balanced', max_depth=None, max_features=0.3, min_samples_leaf=2, n_estimators=188`
- XGB: `colsample_bytree=0.6, gamma=0, learning_rate=0.05, max_depth=7, min_child_weight=3, n_estimators=287, reg_alpha=0, reg_lambda=2.0, scale_pos_weight≈3.26, subsample=0.6`
- LR: `class_weight='balanced', C=1.0, solver='lbfgs'` (untuned baseline, by design)

## 4. Performance (in-sample, 5-fold CV)

| Model | Facility ROC-AUC | Facility PR-AUC | PWS ROC-AUC | PWS PR-AUC | PWS Precision | PWS Recall |
|---|---|---|---|---|---|---|
| LR | — (see `results/plots/roc_curve_*.png`) | — | — | — | — | — |
| RF | 0.803 ± 0.007 | 0.601 ± 0.013 | 0.808 | 0.709 | 0.671 | 0.600 |
| XGB | 0.807 ± 0.011 | 0.611 ± 0.014 | 0.804 | 0.707 | 0.662 | 0.600 |

PWS-level aggregation: max facility-level predicted probability per PWSID,
positive if any facility is positive. n=9,745 PWS, 3,256 positive (33.4%).

Full ROC curves (all three models together): `results/plots/roc_curve_facility.png`,
`results/plots/roc_curve_pws.png`. Confusion matrices, permutation importance,
and SHAP importance plots are in `results/plots/`.

## 5. External validation: state PFAS monitoring data

The trained models were validated against real, independently-collected PFAS
monitoring data from state drinking-water programs (not UCMR5, not used in
training) — a genuine out-of-sample, out-of-time, out-of-lab test.

**Key methodological choice**: UCMR5's own PFAS analyte panel uses reporting
limits clustered around ~3–5 ng/L per compound (PFOA and PFOS both exactly
4 ng/L), so the trained label is effectively "any PFAS ≥ ~4 ng/L." States run
independent monitoring programs with their own lab reporting limits, which
vary widely — some states test more sensitively than EPA, some far less
sensitively. To make a fair comparison:

1. Only states whose lab reporting limit is confirmed at or below EPA's
   ~4 ng/L threshold were used (**MA, NC, SC, CA, PA** — confirmed either from
   an explicit reporting-limit column in the raw state data, or from the
   state program's own published methodology). States with a higher
   (less-sensitive) reporting limit, or whose reporting limit could not be
   confirmed at all, were excluded outright — a "non-detect" from a less
   sensitive lab isn't a reliable negative at the model's training threshold.
2. Rather than trusting each state's own pre-computed detection flag (built
   on that state's own, more sensitive threshold), detection was **re-derived
   directly from each state's reported concentrations at a strict, uniform
   4 ng/L cutoff** — any compound ≥ 4 ng/L → detected. This makes the
   validation label apples-to-apples with what the model was actually
   trained to predict, regardless of how sensitive a given state's lab was.
3. Validation PWS coordinates use EPA FRS facility coordinates by PWSID
   (matching exactly how training-set coordinates were built), **not** any
   coordinate field in the raw state files, several of which are
   county-level approximations rather than true facility locations.
4. Every feature was rebuilt from scratch for each validation PWS using the
   identical pipeline as training (same rasters, same spatial joins, same
   SDWIS pull) — nothing was copied from a stale or pre-existing prediction
   file.

**Result** (n=826 matched PWS across MA/NC/SC/CA, 27.2% positive rate):

| Model | ROC-AUC | PR-AUC | Sensitivity | Specificity |
|---|---|---|---|---|
| LR | 0.660 | 0.384 | 0.849 | 0.348 |
| RF | 0.652 | 0.382 | 0.427 | 0.780 |
| XGB | 0.653 | 0.371 | 0.640 | 0.606 |

All three models retain real, well-above-chance discrimination on this fully
independent, cross-state, cross-lab validation set, though — as expected for
true external validation — performance is lower than the in-sample
cross-validated numbers above.

## 6. Reproducing this from scratch

**Note on paths**: these scripts were developed for a specific local/HPC
environment and contain hardcoded absolute paths to raw data sources (marked
`# EDIT THIS for your HPC environment` at the top of each script) — they are
not plug-and-play in a fresh clone. To reproduce the pipeline, edit those path
constants to point at your own copies of the raw data sources listed in
section 1, in the same relative order the scripts expect (each numbered
script reads the previous step's output). `data/national_pfas_features_ready.csv`
and the trained models are included directly so the full pipeline does not
need to be rerun just to use or validate the models.

```bash
# 1. Feature pipeline (requires raw data sources listed in section 1)
cd code/feature_pipeline
python3 00_build_national_facility_base.py
python3 01_huc12_fetch_polygons_national.py
# ... run 02 through 16 in numeric order ...
python3 16_prepare_features_national.py   # -> data/national_pfas_features_ready.csv

# 2. Feature selection + final training
cd ../modeling
python3 backward_elim_pfas_li.py --target pfas --model rf
python3 backward_elim_pfas_li.py --target pfas --model xgb
python3 train_final_pfas_li.py --arm national --target pfas --model rf
python3 train_final_pfas_li.py --arm national --target pfas --model xgb
python3 save_lr_model.py

# 3. Plots
cd ../plots
python3 plot_roc_curves.py --target pfas

# 4. Score a new validation set (any state, any PWS list with the same
#    feature schema as national_pfas_features_ready.csv)
cd ../modeling
python3 score_validation.py --features /path/to/validation_features.csv \
    --labels /path/to/validation_labels.csv --label-col label --id-col PWSID
```

## 8. Model I/O

- `models/rf_model_final.joblib`, `models/xgb_model_final.joblib`: `{'model': <sklearn/xgboost estimator>, 'features': [...]}`. Saved with `joblib.dump(..., compress=3)` — the RF model in particular has unconstrained tree depth (`max_depth=None`, `min_samples_leaf=2`, from hyperparameter search; ~845K total nodes across 188 trees), so without compression it serializes to ~65MB versus ~20MB compressed. Compression is lossless (identical predictions before/after — verified directly).
- `models/lr_model_final.joblib`: `{'model': <LogisticRegression>, 'scaler': <StandardScaler>, 'scaler_features': [...62 VIF-pruned cols, the order the scaler expects...], 'features': [...48 final p-value-pruned cols, the order the model expects...]}` — LR requires the two-stage column handling because p-value pruning happens *after* scaling; see `score_validation.py` for the correct application order.
