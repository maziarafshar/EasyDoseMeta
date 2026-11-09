# EasyDoseMeta (v3.7)

[![R](https://img.shields.io/badge/R-%3E%3D%204.1.0-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-blue?logo=RStudio&logoColor=white)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

**EasyDoseMeta** is a comprehensive, publication-ready R Shiny application for **One-Stage and Two-Stage Dose-Response Meta-Analysis (DRMA)** powered by the [`dosresmeta`](https://cran.r-project.org/package=dosresmeta) and [`rms`](https://cran.r-project.org/package=rms) packages.

It supports **binary / incidence rate outcomes** (Risk Ratios, Odds Ratios, Incidence Rate Ratios) using the Greenland & Longnecker (1992) covariance method and **continuous outcomes** (Mean Differences, Standardized Mean Differences, Raw Means) using the Crippa & Orsini (2016) method.

---

## Key Features

- **Flexible Input & Automated Pre-Processing**
  - Upload `.xlsx`, `.xls`, or `.csv` files, or load the **built-in 6-study clinical trial dataset** with one click.
  - Automatic computation of **Mean Difference (MD)**, **Hedges' $g$ / Cohen's $d$ (SMD)**, **Raw Mean**, or **Log RR / OR / IRR** and their variances from raw arm-level summary statistics or pre-calculated effect sizes + 95% CIs.
  - Built-in **Range Midpoint Calculator** (e.g., `"10-20"` $\rightarrow 15$) and **Dose Unit Multiplier** for instant unit conversion.
  - Interactive cell-by-cell data editor with real-time pre-analysis validation checks.

- **Comprehensive Dose-Response Modeling**
  - **Linear**, **Quadratic**, **Restricted Cubic Splines (RCS)** (via `rms::rcs` with percentile or custom knot locations), and **B-Splines** (`splines::bs`).
  - Automatic dose rescaling safeguard for extreme dose scales ($> 1000$ or $< 0.01$) to prevent numerical singular matrix errors, while keeping all plots, predictions, and knot reports on the original unscaled dose scale.
  - Supports **REML**, **Maximum Likelihood (ML)**, **Method of Moments (DerSimonian-Laird)**, and **Fixed-Effect** estimation, plus **One-Stage** (`proc = "1stage"`) automatic fallback when studies have fewer than 3 dose arms.

- **Sensitivity Analysis & Reproducibility**
  - **Leave-One-Out (LOO) Sensitivity Analysis**: Iteratively omits one study at a time and overlays all $k$ leave-one-out curves onto the primary dose-response plot alongside a full summary table ($p_{\text{overall}}$, $I^2$, and predicted effect at the median dose).
  - **One-Click Subgroup Stratification**: Automatically fits the current model configuration across every level of a categorical column (e.g., Region, Study Design, Risk of Bias) and sends all subgroup curves to the Multi-Model Comparison tab.
  - **Standalone Reproducible R Script Export (`.R`)**: Download a clean, self-contained R script that reproduces the exact data preparation, model fit, predictions, and `ggplot2` figure outside Shiny.

- **Multi-Model Comparison & Automated Reporting**
  - Overlay multiple functional forms (e.g., Linear vs. Quadratic vs. RCS) or subgroup curves on a single publication-grade plot.
  - Live editing of stored model labels, colors, and line styles, plus toggleable 95% confidence ribbons.
  - Save and reload stored model workspaces via `.rds` files.
  - Export **Word (`.docx`)** or **HTML (`.html`)** single-model and multi-model reports containing an **Executive Summary Comparison Table** ($k$, $n$, AIC, BIC, $I^2$, $Q$-test $p$, overall $p$, non-linearity Wald $p$), knot locations, LOO sensitivity tables, and high-resolution figures.
  - Export figures in **PNG**, **PDF**, **TIFF**, or **SVG** with customizable dimensions and DPI (up to 600 DPI).

---

## Quick Start

### 1. Install Required Packages

Run the included [`install_packages.R`](install_packages.R) script in R or RStudio to install all required CRAN dependencies:

```r
source("install_packages.R")
```

Or install them manually:

```r
install.packages(c(
  "shiny", "shinydashboard", "readxl", "writexl", "dplyr",
  "ggplot2", "dosresmeta", "rms", "splines", "colourpicker",
  "DT", "rmarkdown", "pandoc", "knitr", "shinybusy", "svglite"
))
```

### 2. Run the Application

**Option A — Run directly from GitHub (without cloning):**

```r
shiny::runGitHub("EasyDoseMeta", "maziarafshar")
```

**Option B — Clone and run locally:**

```r
shiny::runApp("app.R")
```

---

## Input Data Format

Your dataset must be structured in **long format** (one row per dose arm within each study, starting with the reference/control arm at `Dose = 0` or the lowest dose):

| Study ID | Dose | N (Total) | Events *(Binary)* | Mean *(Continuous)* | SD *(Continuous)* | Effect *(Pre-calc)* | Lower CI | Upper CI | Subgroup *(Optional)* |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :--- |
| Smith 2020 | 0 | 100 | 12 | 0.00 | 1.15 | 1.00 | — | — | North America |
| Smith 2020 | 50 | 98 | 18 | -1.25 | 1.20 | 1.53 | 0.78 | 3.01 | North America |
| Smith 2020 | 100 | 102 | 27 | -2.40 | 1.25 | 2.25 | 1.21 | 4.18 | North America |

### Supported Outcome Modes

1. **Continuous (Mean, SD, N)**: Requires `Study ID`, `Dose`, `Mean`, `SD`, and `N`. Computes Mean Difference (MD), Standardized Mean Difference (SMD: Hedges' $g$ or Cohen's $d$), or Raw Mean.
2. **Pre-calculated Continuous (Effect + CI / SE)**: Requires `Study ID`, `Dose`, `Effect`, and either `(Lower CI, Upper CI)` or `SE` (plus `N` for covariance estimation).
3. **Binary / Incidence Rates (RR, OR, IRR)**: Requires `Study ID`, `Dose`, `Cases / Events`, `Participants / Person-Time`, `Effect Estimate (RR / OR / IRR)`, `Lower CI`, and `Upper CI`.

---

## Typical Workflow

1. **Data Upload & Prep**: Upload your `.xlsx`/`.csv` file (or click **Load Example Dataset**), select your outcome type, map the columns, verify the green pre-analysis check banner, and click **Confirm & Prepare Data**.
2. **Analysis & Results**: Choose a functional form (Linear, Quadratic, RCS, or B-Spline), configure the reference dose and estimation method, and click **Run Analysis**.
3. **Sensitivity & Diagnostics**: Inspect the fitted curve, study-specific bubbles, non-linearity Wald test, and run a **Leave-One-Out Sensitivity Analysis** in the `Sensitivity` tab.
4. **Multi-Model Comparison**: Store multiple models or use **Subgroup Stratification** to overlay curves, compare fit statistics (AIC/BIC/$I^2$), and export a comprehensive Word/HTML report or reproducible `.R` script.

---

## Methodological References

- **Crippa, A., & Orsini, N. (2016).** Multivariate dose-response meta-analysis: the `dosresmeta` R package. *Journal of Statistical Software*, 72(Code Snippet 1), 1–15. https://doi.org/10.18637/jss.v072.c01
- **Greenland, S., & Longnecker, M. P. (1992).** Methods for trend estimation from summarized dose-response data, with applications to meta-analysis. *American Journal of Epidemiology*, 135(11), 1301–1309. https://doi.org/10.1093/oxfordjournals.aje.a116237
- **Orsini, N., Li, R., Wolk, A., Khudyakov, P., & Spiegelman, D. (2012).** Meta-analysis for linear and nonlinear dose-response relations: examples, an evaluation of approximations, and software. *American Journal of Epidemiology*, 175(1), 66–73. https://doi.org/10.1093/aje/kwr265
- **Crippa, A., Discacciati, A., Bottai, M., Spiegelman, D., & Orsini, N. (2019).** One-stage dose-response meta-analysis for aggregated data. *Statistical Methods in Medical Research*, 28(5), 1579–1596. https://doi.org/10.1177/0962280218773122
- **Harrell, F. E. (2015).** *Regression Modeling Strategies: With Applications to Linear Models, Logistic and Ordinal Regression, and Survival Analysis* (2nd ed.). Springer.

---

## License

This project is licensed under the [MIT License](LICENSE).
