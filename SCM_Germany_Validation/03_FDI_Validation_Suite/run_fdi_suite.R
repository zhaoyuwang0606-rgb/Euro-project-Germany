# =========================================================================
# 03_FDI_Validation_Suite/run_fdi_suite.R
# Self-contained R script to reproduce 03_FDI_Validation_Suite SCM results
# Input: Full_NonEU_OECD_Panel.csv (located in current directory)
# Output: Treat1998_Val1999_2001_Post2002/
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidysynth)
  library(zoo)
  library(parallel)
  library(future)
  library(furrr)
})

N_WORKERS <- max(1L, parallel::detectCores() - 2L)
plan(multisession, workers = N_WORKERS)

DATA_START  <- 1980L
DATA_END    <- 2022L
TREAT_YEAR  <- 1998L
VAL_END     <- 2001L
POST_START  <- 2002L
TREATED     <- "Germany"

OUT_DIR <- "Treat1998_Val1999_2001_Post2002"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

raw <- read.csv("Full_NonEU_OECD_Panel.csv", stringsAsFactors = FALSE)

panel <- raw %>%
  filter(year >= DATA_START, year <= DATA_END) %>%
  mutate(
    asinh_inflows_pct  = asinh(fdi_inflows_pct),
    asinh_outflows_pct = asinh(fdi_outflows_pct)
  ) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    across(c(fdi_inflows_pct, fdi_outflows_pct,
             asinh_inflows_pct, asinh_outflows_pct,
             gdp_current_usd, gdppc_current_usd, population),
           ~ na.approx(., na.rm = FALSE)),
    across(c(fdi_inflows_pct, fdi_outflows_pct,
             asinh_inflows_pct, asinh_outflows_pct,
             gdp_current_usd, gdppc_current_usd, population),
           ~ na.locf(., na.rm = FALSE)),
    gdppc_growth = (gdppc_current_usd - lag(gdppc_current_usd)) / lag(gdppc_current_usd)
  ) %>%
  ungroup() %>%
  group_by(country) %>%
  filter(sum(is.na(fdi_inflows_pct) & year <= TREAT_YEAR) < 3) %>%
  ungroup()

build_fdi_scm_single <- function(data, outcome_var, treat_year, target_unit) {
  pw <- DATA_START:(treat_year - 1L)
  l1 <- treat_year-1L; l2 <- treat_year-2L; l3 <- treat_year-3L; l4 <- treat_year-4L; l5 <- treat_year-5L
  
  data %>%
    synthetic_control(outcome = !!sym(outcome_var), unit = country, time = year, 
                      i_unit = target_unit, i_time = treat_year, generate_placebos = FALSE) %>%
    generate_predictor(time_window = pw,
      Avg_Target_Val = mean(!!sym(outcome_var), na.rm = TRUE),
      Avg_GDPpc      = mean(gdppc_current_usd, na.rm = TRUE),
      Avg_GDPsz      = mean(gdp_current_usd, na.rm = TRUE),
      Avg_Pop        = mean(population, na.rm = TRUE),
      Avg_GDPpc_Gr   = mean(gdppc_growth, na.rm = TRUE)) %>%
    generate_predictor(time_window = l5, L5_Target = mean(!!sym(outcome_var), na.rm = TRUE), L5_GDPpc = mean(gdppc_current_usd, na.rm = TRUE)) %>%
    generate_predictor(time_window = l4, L4_Target = mean(!!sym(outcome_var), na.rm = TRUE), L4_GDPpc = mean(gdppc_current_usd, na.rm = TRUE)) %>%
    generate_predictor(time_window = l3, L3_Target = mean(!!sym(outcome_var), na.rm = TRUE), L3_GDPpc = mean(gdppc_current_usd, na.rm = TRUE)) %>%
    generate_predictor(time_window = l2, L2_Target = mean(!!sym(outcome_var), na.rm = TRUE), L2_GDPpc = mean(gdppc_current_usd, na.rm = TRUE)) %>%
    generate_predictor(time_window = l1, L1_Target = mean(!!sym(outcome_var), na.rm = TRUE), L1_GDPpc = mean(gdppc_current_usd, na.rm = TRUE)) %>%
    generate_weights(optimization_window = pw) %>%
    generate_control()
}

run_fdi_experiment <- function(outcome_var) {
  sub_dir <- file.path(OUT_DIR, outcome_var)
  dir.create(sub_dir, showWarnings = FALSE, recursive = TRUE)
  
  scm_fit <- build_fdi_scm_single(panel, outcome_var, TREAT_YEAR, TREATED)
  
  write.csv(scm_fit %>% grab_unit_weights() %>% arrange(desc(weight)), file.path(sub_dir, "Weights.csv"), row.names = FALSE)
  write.csv(scm_fit %>% grab_balance_table(), file.path(sub_dir, "Balance.csv"), row.names = FALSE)
  
  sc_trend <- scm_fit %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y)
  write.csv(sc_trend, file.path(sub_dir, "YearByYear_Gap.csv"), row.names = FALSE)
  
  m_train <- sc_trend %>% filter(time_unit >= DATA_START & time_unit < TREAT_YEAR)
  m_val   <- sc_trend %>% filter(time_unit >= TREAT_YEAR & time_unit <= VAL_END)
  m_post  <- sc_trend %>% filter(time_unit >= POST_START & time_unit <= DATA_END)
  
  metrics_tbl <- tibble(
    Period   = c(sprintf("Training (%d-%d)", DATA_START, TREAT_YEAR - 1),
                 sprintf("Validation (%d-%d)", TREAT_YEAR, VAL_END),
                 sprintf("Post-Euro (%d-%d)", POST_START, DATA_END)),
    RMSE     = c(sqrt(mean(m_train$gap^2)), sqrt(mean(m_val$gap^2)), sqrt(mean(m_post$gap^2))),
    MAE      = c(mean(abs(m_train$gap)), mean(abs(m_val$gap)), mean(abs(m_post$gap))),
    Mean_Gap = c(mean(m_train$gap), mean(m_val$gap), mean(m_post$gap))
  )
  write.csv(metrics_tbl, file.path(sub_dir, "Metrics.csv"), row.names = FALSE)
}

for (v in c("fdi_inflows_pct", "fdi_outflows_pct", "asinh_inflows_pct", "asinh_outflows_pct")) {
  run_fdi_experiment(v)
}

cat("Saved 03_FDI_Validation_Suite outputs successfully!\n")
