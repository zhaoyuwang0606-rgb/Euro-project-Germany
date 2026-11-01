# =============================================================================
# verify_reproducibility.R
# Pre-submission reproducibility verification script
# Run this from the root directory of the replication package.
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  REPRODUCIBILITY VERIFICATION SCRIPT\n")
cat("=================================================================\n\n")

find_project_root <- function() {
  candidates <- c(
    Sys.getenv("PROJECT_ROOT"),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "../..")
  )
  for (cand in candidates) {
    if (nzchar(cand) && file.exists(file.path(cand, "mpd2023_web.xlsx"))) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  return(getwd())
}
root_dir <- find_project_root()
setwd(root_dir)

pass <- 0
warn <- 0
fail <- 0

check <- function(label, ok, msg_ok = "OK", msg_fail = "FAIL", is_warn = FALSE) {
  if (ok) {
    cat(sprintf("  [PASS] %s\n", label))
    pass <<- pass + 1
  } else if (is_warn) {
    cat(sprintf("  [WARN] %s -- %s\n", label, msg_fail))
    warn <<- warn + 1
  } else {
    cat(sprintf("  [FAIL] %s -- %s\n", label, msg_fail))
    fail <<- fail + 1
  }
}

# =============================================================================
# SECTION 1: Check Required R Packages
# =============================================================================
cat("--- Section 1: R Package Availability ---\n")

required_pkgs <- c("tidyverse", "readxl", "tidysynth", "Synth", "fixest", "parallel", "zoo")
optional_pkgs <- c("pwt10", "WDI", "pbapply")

for (pkg in required_pkgs) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  check(
    sprintf("Package '%s' (required)", pkg),
    installed,
    msg_fail = sprintf("Not installed. Run: install.packages('%s')", pkg)
  )
}

for (pkg in optional_pkgs) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  check(
    sprintf("Package '%s' (optional - needed for WDI/PWT suite)", pkg),
    installed,
    msg_fail = sprintf("Not installed. Run: install.packages('%s')", pkg),
    is_warn = TRUE
  )
}

# =============================================================================
# SECTION 2: Check Data Files
# =============================================================================
cat("\n--- Section 2: Required Data Files ---\n")

required_files <- list(
  list(path = "mpd2023_web.xlsx",
       label = "Maddison Project Database 2023 (PRIMARY)"),
  list(path = "SCM_Germany_Validation/Hybrid_MPD_PWT_Panel.csv",
       label = "Hybrid MPD-PWT Panel (PWT Channel Validation)"),
  list(path = "SCM_Germany_Validation/07_China_WTO_Trade_Suite/Biliteral trade with China.csv",
       label = "IMF DOTS Bilateral Trade with China")
)

for (f in required_files) {
  exists <- file.exists(file.path(root_dir, f$path))
  size_kb <- if (exists) round(file.info(file.path(root_dir, f$path))$size / 1024, 1) else 0
  check(
    sprintf("%s [%s KB]", f$label, size_kb),
    exists,
    msg_fail = paste("MISSING:", f$path)
  )
}

# =============================================================================
# SECTION 3: Check Script Files Exist
# =============================================================================
cat("\n--- Section 3: Script Files ---\n")

scripts <- list(
  list(path = "results_did/run_did_to_2007.R", label = "DiD (full sample)"),
  list(path = "results_did_to_2007/run_did_to_2007.R", label = "DiD (to 2007)"),
  list(path = "SCM_Analysis/noneu SCM result/run_scm_noneu.R", label = "SCM NonEU baseline"),
  list(path = "SCM_Analysis/noneu SCM result log/run_scm_noneu_log.R", label = "SCM NonEU log outcome"),
  list(path = "SCM_Analysis/run_synth_scm_analysis.R", label = "SCM Synth package analysis"),
  list(path = "SCM_Analysis/run_time_robustness_noneu.R", label = "SCM Time robustness"),
  list(path = "SCM_Analysis/compute_all_aggregate.R", label = "SCM Aggregate effects"),
  list(path = "SCM_Analysis_to_2007/run_all_scm_to_2007.R", label = "SCM Analysis to 2007"),
  list(path = "SCM_Germany_Custom/run_germany_custom_scm.R", label = "SCM Custom specs (tidysynth)"),
  list(path = "SCM_Germany_Custom/run_synth_custom.R", label = "SCM Custom specs (Synth)"),
  list(path = "SCM_Germany_Custom/run_maddison_anticipation.R", label = "Anticipation effect placebo"),
  list(path = "SCM_Germany_Validation/run_germany_cv_scm.R", label = "SCM Cross-validation"),
  list(path = "SCM_Germany_Validation/run_nominal_gdp_pct_suite.R", label = "China WTO Trade suite"),
  list(path = "SCM_Germany_Validation/02_PWT_Channel_Validation_Suite/run_channel_validation.R", label = "PWT Channel validation"),
  list(path = "SCM_Germany_Validation/07_China_WTO_Trade_Suite/run_china_wto_trade_scm.R", label = "China WTO SCM (NonEU)")
)

for (s in scripts) {
  exists <- file.exists(file.path(root_dir, s$path))
  check(sprintf("Script: %s", s$label), exists,
        msg_fail = paste("MISSING:", s$path))
}

# =============================================================================
# SECTION 4: Check Existing Output Files (spot check)
# =============================================================================
cat("\n--- Section 4: Spot-Check Existing Outputs ---\n")

output_checks <- list(
  list(path = "results_did/event_study_Clean.png",           label = "DiD event study (Clean)"),
  list(path = "results_did/event_study_Western.png",         label = "DiD event study (Western)"),
  list(path = "results_did_to_2007/event_study_Clean.png",   label = "DiD to 2007 (Clean)"),
  list(path = "SCM_Analysis/noneu SCM result/SCM_Trends.png",  label = "SCM NonEU Trends"),
  list(path = "SCM_Analysis/noneu SCM result/SCM_Placebo.png", label = "SCM NonEU Placebo"),
  list(path = "SCM_Analysis/noneu SCM result/SCM_LOO.png",     label = "SCM NonEU LOO"),
  list(path = "SCM_Analysis/western SCM result/SCM_Trends.png", label = "SCM Western Trends"),
  list(path = "SCM_Germany_Custom/noneu_1970_2022/SCM_Trends.png", label = "Custom 1970-2022 Trends"),
  list(path = "SCM_Germany_Custom/noneu_1990_2022/SCM_Trends.png", label = "Custom 1990-2022 Trends"),
  list(path = "SCM_Germany_Validation/ASCM_Exports_to_China.png", label = "ASCM China Exports"),
  list(path = "SCM_Germany_Validation/ASCM_Imports_from_China.png", label = "ASCM China Imports")
)

for (o in output_checks) {
  full_path <- file.path(root_dir, o$path)
  exists <- file.exists(full_path)
  size_kb <- if (exists) round(file.info(full_path)$size / 1024, 1) else 0
  is_non_empty <- exists && size_kb > 10  # PNGs should be > 10KB
  check(sprintf("Output: %s [%s KB]", o$label, size_kb), is_non_empty,
        msg_fail = "MISSING or suspiciously small (<10KB)")
}

# =============================================================================
# SECTION 5: Smoke Test -- Load Data and Run Minimal SCM
# =============================================================================
cat("\n--- Section 5: Smoke Test (Quick Data Load + Mini SCM) ---\n")

if (requireNamespace("readxl", quietly = TRUE) && requireNamespace("tidysynth", quietly = TRUE) && requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse, warn.conflicts = FALSE)
  library(readxl)
  library(tidysynth)
  
  # Test 1: Can we read the Maddison data?
  tryCatch({
    mpd <- read_excel(file.path(root_dir, "mpd2023_web.xlsx"), sheet = "Full data")
    has_germany <- "Germany" %in% mpd$country
    has_gdppc   <- "gdppc" %in% colnames(mpd)
    has_years   <- all(c(1970, 1999, 2022) %in% mpd$year)
    check("Maddison data loads correctly", has_germany && has_gdppc && has_years,
          msg_fail = "Data structure unexpected")
  }, error = function(e) {
    check("Maddison data loads correctly", FALSE, msg_fail = e$message)
  })
  
  # Test 2: Can we read the Hybrid panel?
  tryCatch({
    hybrid <- read_csv(file.path(root_dir, "SCM_Germany_Validation/Hybrid_MPD_PWT_Panel.csv"),
                       show_col_types = FALSE)
    expected_cols <- c("country", "year", "gdppc_mpd", "csh_i", "csh_x", "csh_m", "csh_c", "csh_g")
    has_cols <- all(expected_cols %in% colnames(hybrid))
    check("Hybrid MPD-PWT panel loads correctly", has_cols,
          msg_fail = paste("Missing columns:", paste(setdiff(expected_cols, colnames(hybrid)), collapse=", ")))
  }, error = function(e) {
    check("Hybrid MPD-PWT panel loads correctly", FALSE, msg_fail = e$message)
  })
  
  # Test 3: Mini SCM (Germany vs 3 donors, 1970-2005, should finish in < 30 sec)
  cat("  Running mini SCM smoke test (Germany, 3 donors, 1970-2005)...\n")
  tryCatch({
    mpd_raw <- read_excel(file.path(root_dir, "mpd2023_web.xlsx"), sheet = "Full data")
    mini_df <- mpd_raw %>%
      filter(country %in% c("Germany", "United States", "Australia", "Canada")) %>%
      filter(year >= 1970 & year <= 2005) %>%
      select(country, year, gdppc) %>%
      drop_na() %>%
      group_by(country) %>% filter(n() == 36) %>% ungroup()
    
    scm_mini <- mini_df %>%
      synthetic_control(outcome = gdppc, unit = country, time = year,
                        i_unit = "Germany", i_time = 1999,
                        generate_placebos = FALSE) %>%
      generate_predictor(time_window = 1970:1998, avg_gdp = mean(gdppc, na.rm = TRUE)) %>%
      generate_weights(optimization_window = 1970:1998) %>%
      generate_control()
    
    gaps <- scm_mini %>% grab_synthetic_control()
    has_post <- any(gaps$time_unit >= 1999)
    check("Mini SCM runs and produces post-treatment gaps", has_post,
          msg_fail = "SCM failed to produce output")
  }, error = function(e) {
    check("Mini SCM smoke test", FALSE, msg_fail = e$message)
  })
  
} else {
  check("Smoke test (requires tidyverse, readxl, tidysynth)", FALSE,
        msg_fail = "One or more required packages missing, skipping smoke test",
        is_warn = TRUE)
}

# =============================================================================
# FINAL SUMMARY
# =============================================================================
total <- pass + warn + fail
cat("\n=================================================================\n")
cat(sprintf("  VERIFICATION SUMMARY: %d checks total\n", total))
cat(sprintf("  [PASS] %d passed\n", pass))
cat(sprintf("  [WARN] %d warnings (optional components)\n", warn))
cat(sprintf("  [FAIL] %d failed\n", fail))
cat("=================================================================\n")

if (fail == 0 && warn == 0) {
  cat("  STATUS: READY FOR SUBMISSION\n")
} else if (fail == 0) {
  cat("  STATUS: READY (with optional warnings - check WDI/pwt10 if needed)\n")
} else {
  cat("  STATUS: NOT READY -- Fix FAIL items before submission\n")
}
cat("=================================================================\n\n")
