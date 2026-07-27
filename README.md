# Statistical Evaluation of Quantification Bias in Extractables and Leachables Studies

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.11](https://img.shields.io/badge/python-3.11-blue.svg)](https://www.python.org/)
[![R 4.4](https://img.shields.io/badge/R-4.4-276DC3.svg)](https://www.r-project.org/)
[![Quarto 1.5](https://img.shields.io/badge/built%20with-Quarto-39729E.svg)](https://quarto.org/)

## Table of Contents

- [Who is this repository for?](#who-is-this-repository-for)
- [Repository layout](#repository-layout)
- [Mapping the manuscript to this repository](#mapping-the-manuscript-to-this-repository)
- [Installation](#installation)
- [Reproducing the results](#reproducing-the-results)
- [Scenario naming convention](#scenario-naming-convention)
- [Data provenance](#data-provenance)
- [Citation](#citation)
- [License](#license)
- [Acknowledgements](#acknowledgements)

## Who is this repository for?

| Audience | What you will find here |
|---|---|
| **E&L SMEs** | A reproducible benchmark of every common UF correction method (fixed UF, mean/16th percentile, *RRFlow*, RRF < 1) against the FDA CLAP RRF dataset, with FN/FP rates per scenario. |
| **Data scientists / statisticians** | A clean Monte Carlo engine (R + Quarto) plus three Jupyter notebooks comparing Random Forest vs. Support Vector Regression for predicting RRFs from molecular descriptors. |

## Repository layout

```
.
├── README.md                          ← you are here
├── CITATION.cff                       ← machine-readable citation metadata
├── requirements.txt                   ← pinned Python dependencies
├── .gitignore
│
├── data/
│   └── processed/                     ← cleaned RRF distributions (one row per compound)
│       ├── CLAP_GCMS_clean.xlsx       ← GC/MS RRFs at 5, 10, 20 µg/mL (Power Query pipeline embedded)
│       ├── CLAP_LCMS_clean.xlsx       ← LC/MS± RRFs (sheets: LCMSPOS, LCMSNEG; Power Query pipeline embedded)
│       └── RRF_ML_features.xlsx       ← RDKit molecular descriptors for ML notebooks
│
├── notebooks/                         ← exploratory analysis & ML model training
│   ├── 02_explore_pooled_concentration_distributions.qmd  ← concentration distribution fitting (Nahan 2020)
│   ├── 03_explore_rrf_censored_distributions.qmd          ← RRF distribution fitting (Figure 4 in thesis)
│   ├── 04_ml_rrf_prediction_5ug.ipynb    ← RF / SVR / ElasticNet training, 5 µg/mL surrogate
│   ├── 05_ml_rrf_prediction_10ug.ipynb   ← RF / SVR / ElasticNet training, 10 µg/mL surrogate
│   ├── 06_ml_rrf_prediction_20ug.ipynb   ← RF / SVR / ElasticNet training, 20 µg/mL surrogate
│   ├── 07_external_rrf_databases_gcms.qmd ← external GC/MS RRF databases: fits + UF (Suppl. S3)
│   └── 08_external_rrf_databases_lcms.qmd ← external LC/MS RRF databases: fits + UF (Suppl. S3)
│
├── scripts/
│   ├── simulation/                    ← core Monte Carlo engine (R + Quarto, 55 files)
│   │   ├── render.R                   ← master driver: renders every .qmd in the folder
│   │   ├── GC_MS_<surrogate>_<scenario>.qmd    (15 files: surrogates 5/10/20 + cross-technique X)
│   │   ├── LC_MS+_<surrogate>_<scenario>.qmd   (15 files)
│   │   ├── LC_MS-_<surrogate>_<scenario>.qmd   (15 files)
│   │   ├── LC_MS_X_<scenario>.qmd              (5 files, cross-technique simulated RRFs)
│   │   ├── *_notitle.qmd                       (3 files: manuscript Figs. 1-3, plot titles stripped)
│   │   └── LC_MS-_5_2A_LC_convcheck.qmd        ← convergence assessment (Suppl. S7)
│   │
│   ├── feasibility/                   ← analytical-feasibility of the estimated AET (Section 4.3)
│   │   └── <technique>_5_<scenario>.qmd    (6 files, surrogate 5 µg/mL, AET = 0.45 µg/mL)
│   │
│   ├── postprocessing/                ← Python helpers (run after simulation)
│   │   ├── extract_supplementary.py   ← pulls figures from rendered Quarto docs
│   │   ├── generate_results_excel.py  ← builds results/Results_by_scenario.xlsx
│   │   └── update_aet.py              ← batch-updates AET value across all .qmd files
│   │
│   └── hpc/                           ← SLURM jobscript for cluster execution
│       └── jobscript.slurm
│
├── models/                            ← pre-trained Random Forest models (loaded by GC/MS QMDs)
│   ├── rf_rrf_model_final_20.joblib       ← RF pipeline, 5 µg/mL surrogate
│   ├── rf_rrf_model_final_20_10ug.joblib  ← RF pipeline, 10 µg/mL surrogate
│   ├── rf_rrf_model_final_20_20ug.joblib  ← RF pipeline, 20 µg/mL surrogate
│   ├── features_20.joblib                 ← feature list, 5 µg/mL
│   ├── features_20_10ug.joblib            ← feature list, 10 µg/mL
│   ├── features_20_20ug.joblib            ← feature list, 20 µg/mL
│   ├── final_model_data.csv               ← training dataset with descriptors, 5 µg/mL
│   ├── final_model_data_10ug.csv          ← training dataset, 10 µg/mL
│   └── final_model_data_20ug.csv          ← training dataset, 20 µg/mL
│
├── results/
│   ├── Results_by_scenario.xlsx       ← master results table (all platforms, AETs, methods)
│   └── scenarios/                         ← populated after each AET run (see Stage 1b)
│       ├── AET_0.45/
│       ├── AET_3/
│       ├── AET_6/
│       └── AET_36/
│
└── docs/
    └── Data Management plan.md        ← data storage, access and reproducibility policy
```

## Mapping the manuscript to this repository

The table below links each section of the manuscript *"Quantification bias in non-targeted
extractables and leachables screening: a Monte Carlo comparison of uncertainty-factor and
response-factor corrections"* to the file that produces it.

| Manuscript section | Supplementary | File |
|---|---|---|
| 2.1 Data sources — RRF distribution fitting | S1 | `notebooks/03_explore_rrf_censored_distributions.qmd` |
| 2.1 Data sources — concentration fitting | S2 | `notebooks/02_explore_pooled_concentration_distributions.qmd` |
| 3.1 Simulation convergence and reporting | S7 | `scripts/simulation/LC_MS-_5_2A_LC_convcheck.qmd` |
| 3.2 External validation of the RRF fitted distributions (Table 2), GC/MS | S3 | `notebooks/07_external_rrf_databases_gcms.qmd` |
| 3.2 External validation of the RRF fitted distributions (Table 2), LC/MS | S3 | `notebooks/08_external_rrf_databases_lcms.qmd` |
| 3.3 GC/MS results — Fig. 1 | — | `scripts/simulation/GC_MS_5_2A_GC_notitle.qmd` |
| 3.4 LC/MS POS results — Fig. 2 | — | `scripts/simulation/LC_MS+_5_2A_LC_notitle.qmd` |
| 3.5 LC/MS NEG results — Fig. 3 | — | `scripts/simulation/LC_MS-_5_2A_LC_notitle.qmd` |
| 3.6 Effect of the surrogate concentration | — | the 10 and 20 µg/mL variants in `scripts/simulation/` |
| 4.3 Analytical feasibility of the estimated AET — Fig. 5 | — | `scripts/feasibility/` (6 files) |

### About the external-database notebooks (07-08)

Notebooks `07` and `08` fit censored normal, log-normal and gamma distributions to published
RRF databases and compute UF(RSD) and UF(µRSD) for each, which is how the manuscript tests
whether the CLAP findings generalise beyond a single 106-compound list. Both notebooks are
**self-contained**: every RRF value is inlined as a vector in the document, with the source
table and surrogate stated per database, so they run without any external data file.

### About the feasibility scripts

`scripts/feasibility/` holds the six surrogate-5 scenarios extended with the section
*Practical Implementability of the Estimated AET*. Because the uncertainty factor sits in the
denominator of the AET, every correction lowers the threshold a compound must clear; these
scripts compute the estimated AET as `AET_true / UF` and classify it against the method's
sensitivity floors (GC/MS: LOQ = 0.2 µg/mL, LOD = LOQ/3 ≈ 0.067 µg/mL).

They produce four visualisations and two tables: estimated AET vs the instrumental limit,
feasibility across the AET/DBT/extraction scenarios, the point at which the filter stops
filtering, and the RRF percentile needed to reach the LOQ and LOD. Figures are written
directly as Elsevier-ready TIFF (500 dpi, LZW) via the `fig_tag` prefix in each file.

Each file sets `AET <- 0.45`, but the scenario grid is computed as `AET_true / UF`, so a single
run already spans all four AET scenarios (0.45, 3, 6 and 36 µg/mL) as reported in Fig. 5.
Feasibility classes use a colour-blind-safe palette with redundant shape encoding.

### About the `_notitle` simulation files

These are the manuscript-figure variants of the corresponding simulation scenarios. They are
identical to `GC_MS_5_2A_GC.qmd`, `LC_MS+_5_2A_LC.qmd` and `LC_MS-_5_2A_LC.qmd` except that
plot titles are stripped, since the figure captions carry that information in the typeset
article. Use the originals for exploratory work and the `_notitle` versions to regenerate the
published figures.

## Installation

The project uses **Python 3.11** for ML notebooks and post-processing helpers, and **R 4.4 + Quarto 1.5** for the Monte Carlo simulation engine.

### 1. Clone the repository

```bash
git clone https://github.com/syam3500/el_bias.git
cd el_bias
```

### 2. Python environment

```bash
python -m venv .venv
source .venv/bin/activate          # macOS / Linux
.venv\Scripts\activate             # Windows PowerShell

pip install --upgrade pip
pip install -r requirements.txt
```

### 3. R environment

Install R (≥ 4.4) and Quarto (≥ 1.5) from [quarto.org](https://quarto.org/docs/get-started/), then in an R session:

```r
install.packages(c(
  "tidyverse",    # ggplot2, dplyr, readr, tidyr, stringr
  "readxl",       # read .xlsx source files
  "patchwork",    # multi-panel ggplot composition
  "ggrepel",      # repulsive text labels
  "fitdistrplus", # distribution fitting (norm, lnorm, gamma)
  "quarto",       # render .qmd files from R
  "khroma",       # colour-blind-safe palettes (scripts/feasibility/)
  "reticulate"    # R ↔ Python bridge
))
```

> A `renv.lock` for fully pinned R reproducibility will be added in a future release.

### 4. Data files

The three processed data files are already included in `data/processed/`:

- `CLAP_GCMS_clean.xlsx` — GC/MS RRFs at 5, 10, 20 µg/mL
- `CLAP_LCMS_clean.xlsx` — LC/MS± RRFs (sheets: LCMSPOS, LCMSNEG)
- `RRF_ML_features.xlsx` — RDKit molecular descriptors used in notebooks 04–06

`CLAP_GCMS_clean.xlsx` and `CLAP_LCMS_clean.xlsx` embed a **Power Query pipeline** that performs all cleaning steps directly in Excel (column selection, type casting, handling of non-detected and below-LOD values). Opening the files in Excel and refreshing the queries will reproduce the cleaned RRF tables from the raw FDA CLAP PDFs.

Concentration values from the **Nahan et al. (2020)** dataset are not stored as a separate file. They are hard-coded directly in the relevant simulation `.qmd` files (scenarios ending in `_GC` and `_LC`) as the basis for fitting the log-normal concentration distribution parameters.

## Reproducing the results

The pipeline runs in four sequential stages.

> **Pre-trained models included.** The `models/` folder already contains the fitted Random Forest pipelines and feature lists. Stage 0 (retraining) is therefore optional — skip it unless you want to retrain on updated data.

### Stage 0 (optional) — Retrain the ML models

```bash
cd notebooks
jupyter lab
```

Run notebooks `04` → `06` in order. They train **Random Forest**, **Support Vector Regression**, and **ElasticNet** models on RDKit molecular descriptors at the three surrogate levels (5, 10, 20 µg/mL). The output `.joblib` files are saved to `notebooks/`; copy them to `models/` to make them available to the simulation QMDs.

### Stage 0b (optional) — Reproduce the external RRF database validation

```bash
quarto render notebooks/07_external_rrf_databases_gcms.qmd
quarto render notebooks/08_external_rrf_databases_lcms.qmd
```

Both notebooks are self-contained and need no data files: every RRF value is inlined as a
vector. They reproduce the descriptive statistics, distribution fits and uncertainty factors
reported in Table 2 and Supplementary Material S3.

### Stage 1 — Run the Monte Carlo simulations (R + Quarto)

```bash
cd scripts/simulation
Rscript render.R
```

Each `.qmd` file:
1. Loads cleaned CLAP RRF data from `../../data/processed/`.
2. Draws **80 hypothetical extractable compounds** per iteration from a concentration distribution.
3. Distorts each measured concentration by a sampled RRF.
4. Classifies every compound against the AET with and without each correction strategy.
5. Repeats for **10,000 iterations** and writes a `.md` summary table alongside the `.qmd`.

By default, each file uses `AET = 3 µg/mL`. To reproduce the full sensitivity analysis:

```bash
python ../postprocessing/update_aet.py --aet 0.45
Rscript render.R
# repeat for aet = 6 and 36
```

Cluster users can submit `scripts/hpc/jobscript.slurm` instead.

To reproduce the convergence assessment reported in Section 3.1 (Supplementary S7), render
`LC_MS-_5_2A_LC_convcheck.qmd`. It runs the highest-variance scenario (LC/MS NEG, log-normal
concentration, AET = 0.45 µg/mL, surrogate 5 µg/mL, no correction) and tracks the running mean
FN rate against iteration count.

To regenerate the manuscript figures rather than the exploratory versions, render the three
`*_notitle.qmd` files.

### Stage 1b — Move rendered outputs to the correct results folder

After each AET run, move the generated `.md` files from `scripts/simulation/` into the matching subfolder:

```bash
mv scripts/simulation/*.md results/scenarios/AET_3/     # for AET = 3
# repeat for each AET value
```

### Stage 2 — Aggregate the results (Python)

```bash
cd ../postprocessing
python generate_results_excel.py   # writes results/Results_by_scenario.xlsx
```

> **Smoke test.** To verify Stage 2 without running the full simulation, a small
> subset of pre-rendered `.md` files (the six scenarios shown in the thesis, at
> AET = 3 µg/mL) is committed under `results/scenarios/AET_3/`. Running
> `generate_results_excel.py` against this subset should report 6 files OK and 80
> extracted rows. Note that this **overwrites** `results/Results_by_scenario.xlsx`;
> restore it from version control afterwards to keep the full master table.

The workbook contains, for every (method × AET × surrogate × concentration distribution × RRF distribution × correction method) tuple:

> FN rate · FN SD · FP rate · FP SD · Recall · Precision · F₁ · F₅ · Applied UF · % True ≥ AET · % Reported ≥ AET

### Stage 3 — Analytical feasibility of the estimated AET

```bash
cd scripts/feasibility
quarto render LC_MS+_5_2A_LC.qmd     # Fig. 5 in the manuscript
```

Render any of the six files to reproduce Section 4.3. Each writes its figures as
Elsevier-ready TIFF into the working directory, prefixed by that file's `fig_tag`
(for example `LCMSPOS_5ug_AET045_Fig_feas_heatmap.tiff`).

## Scenario naming convention

File names follow the pattern `<technique>_<surrogate>_<scenario ID>_<optional threshold>.qmd` and map directly to the columns in `results/Results_by_scenario.xlsx`.

**Segment breakdown:**

| Segment | Values | Meaning |
|---|---|---|
| `technique` | `GC_MS`, `LC_MS+`, `LC_MS-`, `LC_MS_X` | Analytical method |
| `surrogate` | `5`, `10`, `20`, `X` | Surrogate standard concentration (µg/mL); `X` = cross-technique with simulated Normal and LogNormal RRFs |
| `scenario` | `1A`, `1A_GC/LC`, `2A`, `2A_GC/LC`, `3A`, `1B`, `2B`, `3B` | Concentration distribution + RRF source (see table below) |
| `threshold` | `0.60`, `0.70` | Uniform distribution variant: `0.60` = U[0.4·AET, 1.4·AET]; `0.70` = U[0.3·AET, 1.3·AET] |

**Full scenario lookup table** (matches `Conc. Distribution` and `RRF Distribution` columns in the workbook):

| Scenario ID | Concentration distribution | RRF distribution | Notes |
|---|---|---|---|
| `1A_0.60` | Uniform U[0.4·AET, 1.4·AET] | Empirical (CLAP) | |
| `1A_0.70` | Uniform U[0.3·AET, 1.3·AET] | Empirical (CLAP) | |
| `2A` | Log-Normal Standard | Empirical (CLAP) | Parametric log-normal |
| `2A_GC` | Log-Normal (Fitted GC) | Empirical (CLAP) | Parameters fitted to GC/MS CLAP data; informed by Nahan et al. (2020) |
| `2A_LC` | Log-Normal (Fitted LC) | Empirical (CLAP) | Parameters fitted to LC/MS CLAP data; informed by Nahan et al. (2020) |
| `3A` | Custom Mixture | Empirical (CLAP) | Mixture distribution |
| `1B_0.60` | Uniform U[0.4·AET, 1.4·AET] | Simulated (Normal) | Cross-technique |
| `1B_0.70` | Uniform U[0.3·AET, 1.3·AET] | Simulated (Normal) | Cross-technique |
| `2B` | Log-Normal Standard | Simulated (Normal/LogNormal) | Cross-technique |
| `2B_GC/LC` | Log-Normal (Fitted GC/LC) | Simulated (Normal/LogNormal) | Cross-technique; informed by Nahan et al. (2020) |
| `3B` | Custom Mixture | Simulated (Normal/LogNormal) | Cross-technique |

> **Nahan et al. (2020)** — Nahan, K., et al. Screening for extractables in additive-manufactured acrylonitrile butadiene styrene orthopedic cast. *Talanta*, 212, 120464. https://doi.org/10.1016/j.talanta.2019.120464
> This study provided real-world extractable compound concentrations from a medical device extraction study, used here to fit the log-normal distribution parameters and define the custom mixture distribution.

**Scenarios represented in the thesis** — surrogate 5 µg/mL, across all four AET values:

The thesis presents exactly **6 simulation files**, each shown as a trade-off plot and a confusion-matrix heatmap at every AET (0.45, 3, 6, 36 µg/mL), giving 48 figures in total across the results chapter and appendix.

| File | Analytical Method | Concentration distribution | Where shown |
|---|---|---|---|
| `GC_MS_5_2A_GC` | GC/MS | Log-Normal Fitted GC | Main results + Appendix (all 4 AETs) |
| `GC_MS_5_1A_0.60` | GC/MS | Uniform U[0.4·AET, 1.4·AET] | Main results + Appendix (all 4 AETs) |
| `LC_MS+_5_2A_LC` | LC/MS+ | Log-Normal Fitted LC | Main results + Appendix (all 4 AETs) |
| `LC_MS+_5_1A_0.60` | LC/MS+ | Uniform U[0.4·AET, 1.4·AET] | Main results + Appendix (all 4 AETs) |
| `LC_MS-_5_2A_LC` | LC/MS− | Log-Normal Fitted LC | Main results + Appendix (all 4 AETs) |
| `LC_MS-_5_1A_0.60` | LC/MS− | Uniform U[0.4·AET, 1.4·AET] | Main results + Appendix (all 4 AETs) |

The fitted log-normal (`_2A_GC` / `_2A_LC`) was chosen as the primary scenario because it is grounded in real extractables concentration data (Nahan et al., 2020). The uniform 40 % variant (`_1A_0.60`) was kept as a comparison to maximise the fraction of compounds above the AET, making method differences more visible. All remaining 49 scenarios (other surrogates, other distributions, cross-technique) are fully computed and available in `results/Results_by_scenario.xlsx`.

## Data provenance

Raw RRF reference data come from the **US FDA Chemicals List for Analytical Performance (CLAP)**:
<https://cdrh-rst.fda.gov/chemicals-list-analytical-performance-clap>

Three datasets (GC/MS, LC/MS positive-ion, LC/MS negative-ion) were extracted from the PDFs in `data/raw/` and stored as Excel workbooks in `data/processed/`. Non-detected values were set to **0**; values reported below the detection limit were set to **0.001 µg/mL**, following Subject Matter Expert (SME) guidance.

## Citation

If you use this code, the cleaned datasets, or any of the figures, please cite the manuscript:

> Yam, S., Feliciano, R., & Jaspers, S. Quantification bias in non-targeted extractables and leachables screening: a Monte Carlo comparison of uncertainty-factor and response-factor corrections. *Journal of Pharmaceutical and Biomedical Analysis* (submitted).

and/or the thesis it derives from:

> Yam, S. (2026). *Statistical Evaluation of Quantification Bias in Extractables and Leachables Studies* [Master's thesis, Hasselt University]. Faculty of Sciences, School for Information Technology.

```bibtex
@mastersthesis{yam2026elbias,
  author  = {Yam, Sophie},
  title   = {Statistical Evaluation of Quantification Bias in Extractables and Leachables Studies},
  school  = {Hasselt University, Faculty of Sciences -- School for Information Technology},
  year    = {2026},
  type    = {Master's thesis, Master of Statistics and Data Science},
  address = {Hasselt, Belgium}
}
```

## License

The code in this repository is released under the **MIT License**. The raw FDA CLAP PDFs are public-domain US government work. Please respect the licenses of downstream cheminformatics dependencies (RDKit: BSD; PubChemPy: MIT).

## Acknowledgements

This thesis was completed under the supervision of **Dr. Stijn Jaspers** (Hasselt University), whose statistical expertise and constructive feedback shaped the methodological foundation of this work, and **Dr. Rodrigo Feliciano** (Johnson & Johnson Innovative Medicine), who proposed the research question, provided the applied extractables and leachables context, and offered invaluable guidance throughout the project.

The author gratefully acknowledges the US Food and Drug Administration for making the CLAP reference database openly available, and the open-source communities behind R, Quarto, scikit-learn, and RDKit.
