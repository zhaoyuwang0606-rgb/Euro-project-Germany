# Replication Package: The Economic Effects of Euro Adoption on Germany

## Overview

This replication package contains all code and data necessary to reproduce the figures and results in the paper. All analysis is conducted in **R** (version >= 4.1.0). No Stata license is required for any of the core results outside the `Replication/` folder.

---

## Directory Structure

```
.
├── mpd2023_web.xlsx                  # PRIMARY DATA: Maddison Project Database 2023
├── README.md                         # This file
├── verify_reproducibility.R          # Pre-submission verification script
│
├── results_did/                      # DiD event study (full sample, to 2022)
├── results_did_to_2007/              # DiD event study (restricted to 2007)
├── SCM_Analysis/                     # Core SCM analysis (full sample, to 2022)
├── SCM_Analysis_to_2007/             # Core SCM analysis (restricted to 2007)
├── SCM_Germany_Custom/               # Custom time-window SCM matrix
└── SCM_Germany_Validation/           # Channel validation suite
    ├── Hybrid_MPD_PWT_Panel.csv      #   SECONDARY DATA: MPD + PWT merged panel
    └── 07_China_WTO_Trade_Suite/
        └── Biliteral trade with China.csv  # SECONDARY DATA: IMF DOTS bilateral trade
```

---

## Data Sources

### 1. Maddison Project Database 2023 (`mpd2023_web.xlsx`) [INCLUDED]
- **Source**: Maddison Project Database 2023, Bolt & van Zanden (2023)
- **URL**: https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023
- **Sheet used**: `Full data`
- **Variables**: `country`, `year`, `gdppc` (2011 USD PPP), `pop`
- **Used by**: All SCM and DiD scripts (primary dataset)

### 2. Hybrid MPD-PWT Panel (`SCM_Germany_Validation/Hybrid_MPD_PWT_Panel.csv`) [INCLUDED]
- **Sources combined**:
  - Maddison Project Database 2023 (GDP per capita, population)
  - Penn World Tables 10.01: GDP components (csh_c, csh_i, csh_g, csh_x, csh_m), price level, real GDP
- **PWT Citation**: Feenstra, Inklaar & Timmer (2015), AER
- **PWT URL**: https://www.rug.nl/ggdc/productivity/pwt/
- **Used by**: `02_PWT_Channel_Validation_Suite/run_channel_validation.R`

### 3. Penn World Tables 10.01 via R Package (`pwt10`) [REQUIRES R PACKAGE]
- **Install**: `install.packages("pwt10")`
- **Used by**: `run_nominal_gdp_pct_suite.R`

### 4. IMF Direction of Trade Statistics (`Biliteral trade with China.csv`) [INCLUDED]
- **Source**: IMF Direction of Trade Statistics (DOTS)
- **URL**: https://data.imf.org/?sk=9d6028d4-f14a-464c-a2f2-59b2cd424b85
- **Content**: Bilateral trade flows (exports + imports) with China, 1980-2019
- **Used by**: `07_China_WTO_Trade_Suite/run_china_wto_trade_scm.R`, `run_nominal_gdp_pct_suite.R`

### 5. World Bank WDI [REQUIRES INTERNET CONNECTION AT RUNTIME]
- **R package**: `WDI` -- Install: `install.packages("WDI")`
- **Indicators**: `NY.GDP.MKTP.CD` (Nominal GDP), `SP.POP.TOTL` (Population)
- **Period**: 1980-2019
- **Used by**: `07_China_WTO_Trade_Suite/run_china_wto_trade_scm.R` only
- **Note**: This is the ONLY script requiring a live internet connection.

---

## Required R Packages

```r
install.packages(c(
  "tidyverse",   # Core data manipulation + ggplot2
  "readxl",      # Read .xlsx files
  "tidysynth",   # Main SCM package
  "Synth",       # Alternative SCM package (for _Synth versions)
  "fixest",      # DiD with fixed effects (feols)
  "pwt10",       # Penn World Tables 10.01 data
  "WDI",         # World Bank API (internet required at runtime)
  "parallel",    # Parallel computing
  "zoo",         # Time-series utilities
  "pbapply"      # Progress bar for parallel loops
))
```

---

## How to Reproduce

Run the pre-submission verification check first:
```r
source("verify_reproducibility.R")
```

Each script is self-contained. Set your working directory to the folder containing the script before running.

| Script | Est. Runtime |
|--------|-------------|
| `results_did/run_did_to_2007.R` | < 1 min |
| `results_did_to_2007/run_did_to_2007.R` | < 1 min |
| `SCM_Analysis/noneu SCM result/run_scm_noneu.R` | ~5-10 min |
| `SCM_Analysis/run_synth_scm_analysis.R` | ~20-30 min |
| `SCM_Analysis/run_time_robustness_noneu.R` | ~15-20 min |
| `SCM_Analysis_to_2007/run_all_scm_to_2007.R` | ~20-30 min |
| `SCM_Germany_Custom/run_germany_custom_scm.R` | ~15-20 min |
| `SCM_Germany_Validation/run_germany_cv_scm.R` | ~10 min |
| `SCM_Germany_Validation/run_nominal_gdp_pct_suite.R` | ~15 min + internet |
| `SCM_Germany_Validation/02_PWT.../run_channel_validation.R` | ~30 min |

---

## Citation

Bolt, J. and J.L. van Zanden (2023). Maddison style estimates of the evolution of the world economy. Maddison Project Working Paper 10, University of Groningen.

Feenstra, R.C., R. Inklaar and M.P. Timmer (2015). "The Next Generation of the Penn World Table." American Economic Review, 105(10), 3150-3182.
