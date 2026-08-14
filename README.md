# UCMR_ML

Machine-learning models predicting contaminant detection in U.S. public water
systems, built on EPA's Unregulated Contaminant Monitoring Rule (UCMR) data
plus geospatial, hydrogeologic, and administrative features.

## Contents

- [`pfas/`](pfas/) — PFAS detection models (Logistic Regression, Random Forest, XGBoost), national arm: full feature-engineering pipeline, trained models, performance metrics, SHAP feature importance, and external state-monitoring-data validation. See [`pfas/README.md`](pfas/README.md) for details and reproduction steps.
- [`pfas_northeast/`](pfas_northeast/) — Regional variant trained only on Northeast states, tested against Pennsylvania's state PFAS data. Data and code provided; not yet run.
- [`pfas_southeast/`](pfas_southeast/) — Regional variant trained only on Southeast states, tested against North Carolina's state PFAS data. Data and code provided; not yet run.
