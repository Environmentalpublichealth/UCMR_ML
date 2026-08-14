# UCMR_ML

Machine-learning models predicting contaminant detection in U.S. public water
systems, built on EPA's Unregulated Contaminant Monitoring Rule (UCMR) data
plus geospatial, hydrogeologic, and administrative features.

## Contents

- [`preprocessing/`](preprocessing/) — R scripts for UCMR data preprocessing and state-data summarization.
- [`pfas/`](pfas/) — PFAS detection models (Logistic Regression, Random Forest, XGBoost): full feature-engineering pipeline, trained models, performance metrics, DML/SHAP statistical validation, and external state-monitoring-data validation. See [`pfas/README.md`](pfas/README.md) for details and reproduction steps.
