# Data Management Plan

**Project:** Statistical Evaluation of Quantification Bias in Extractables and Leachables Studies
**Author:** Sophie Yam · Hasselt University · Master of Statistics and Data Science (2025–2026)

---

## 1. Data description

| Dataset | Format | Size (approx.) | Description |
|---|---|---|---|
| `data/raw/CLAP_GCMS.pdf` | PDF | ~2 MB | US FDA CLAP — GC/MS RRF source document |
| `data/raw/CLAP_LCMS_positive.pdf` | PDF | ~2 MB | US FDA CLAP — LC/MS positive-ion |
| `data/raw/CLAP_LCMS_negative.pdf` | PDF | ~2 MB | US FDA CLAP — LC/MS negative-ion |
| `data/processed/GCMS.xlsx` | Excel | ~500 KB | Cleaned GC/MS RRFs at 5, 10, 20 µg/mL |
| `data/processed/LCMS.xlsx` | Excel | ~500 KB | Cleaned LC/MS± RRFs (sheets: LCMSPOS, LCMSNEG) |
| `data/processed/RRFML.xlsx` | Excel | ~1 MB | RDKit molecular descriptors for ML model training |

All data originate from the **US FDA Chemicals List for Analytical Performance (CLAP)**, a publicly accessible database maintained by the Center for Devices and Radiological Health (CDRH): <https://cdrh-rst.fda.gov/chemicals-list-analytical-performance-clap>. No personal or sensitive data are involved.

---

## 2. Data collection and processing

### FDA CLAP datasets

Raw RRF values were extracted from the FDA CLAP PDFs and loaded into Excel workbooks (`GCMS.xlsx`, `LCMS.xlsx`). Each workbook embeds a **Power Query pipeline** that performs all cleaning steps in Excel. Refreshing the queries in Excel reproduces the cleaned tables from the raw PDFs. The cleaning rules applied are:

- Non-detected values → set to **0**.
- Values reported as below the detection limit → set to **0.001 µg/mL**.
- All three datasets (GC/MS, LC/MS+, LC/MS−) are loaded in every simulation file to maintain cross-technique comparability; only the dataset relevant to a given scenario is sampled in the Monte Carlo loop.

### Nahan et al. (2020) dataset

Concentration values from Nahan et al. (2020) — used to fit the log-normal concentration distribution parameters — are not stored as a separate file. They are hard-coded directly in the relevant simulation `.qmd` files (scenarios ending in `_GC` and `_LC`). The original source is:

> Nahan, K., et al. (2020). Screening for extractables in additive-manufactured acrylonitrile butadiene styrene orthopedic cast. *Talanta*, 212, 120464. https://doi.org/10.1016/j.talanta.2019.120464

### Molecular descriptors

Descriptors used in the ML notebooks were computed with **RDKit 2024.3.5** from SMILES strings retrieved via **PubChemPy** and stored in `RRFML.xlsx`.

---

## 3. Storage and backup

| Location | Purpose | Retention |
|---|---|---|
| This GitHub repository | Version-controlled code and metadata | Indefinite (public) |
| Local workstation (`processed_data/`) | Working copies of Excel workbooks | Duration of thesis project |
| University Google Drive / institutional backup | Full project archive | ≥ 10 years (Hasselt University policy) |

---

## 4. Access and sharing

All code is released under the **MIT License**. The cleaned Excel workbooks and simulation scripts are freely available in this repository. The raw FDA CLAP PDFs are public-domain US government documents.

There are no embargo periods, ethical restrictions, or data use agreements that limit sharing.

---

## 5. Reproducibility

To fully reproduce all results from this repository:

1. Install dependencies (`requirements.txt` for Python; `install.packages()` for R).
2. Place data files in `data/processed/` (see `README.md` — Installation, Step 4).
3. Run `scripts/simulation/render.R` for each AET value.
4. Run `scripts/postprocessing/generate_results_excel.py` to aggregate outputs.
5. Run Jupyter notebooks `04` → `06` for ML results.

Random seeds are fixed (`set.seed(123)` in R; `random_state=42` in scikit-learn) to ensure numerical reproducibility.

---

## 6. Long-term preservation

The repository will remain publicly accessible on GitHub at [https://github.com/syam3500/el_bias](https://github.com/syam3500/el_bias). The thesis itself is archived through Hasselt University's institutional repository, which provides long-term citable access to the full work including methodology and results.

---

*Last updated: June 2026*
