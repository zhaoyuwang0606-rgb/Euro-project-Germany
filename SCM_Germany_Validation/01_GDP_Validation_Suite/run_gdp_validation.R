# =========================================================================
# 01_GDP_Validation_Suite/run_gdp_validation.R
# Self-contained R script to reproduce 01_GDP_Validation_Suite SCM results
# Input: ../Hybrid_MPD_PWT_Panel.csv
# Output: Current directory (.)
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidysynth)
  library(readxl)
})

cat("=== Running GDP Validation Suite SCM ===\n")

target_dir <- "."
raw_hybrid <- read_csv("../Hybrid_MPD_PWT_Panel.csv", show_col_types = FALSE)

clean_countries <- raw_hybrid %>%
  group_by(country) %>%
  filter(n() == 50) %>%
  summarise(
    na_g = sum(is.na(gdppc_mpd)),
    na_i = sum(is.na(csh_i))
  ) %>%
  filter(na_g == 0 & na_i == 0) %>%
  pull(country)

hybrid_clean <- raw_hybrid %>%
  filter(country %in% clean_countries & year >= 1970 & year <= 2019)

treated_country <- "Germany"
TREAT_YEAR <- 1998
EURO_YEAR  <- 1999
DATA_END   <- 2019

pre_window <- 1970:(TREAT_YEAR - 1)

scm <- hybrid_clean %>%
  synthetic_control(
    outcome = gdppc_mpd, unit = country, time = year,
    i_unit = treated_country, i_time = TREAT_YEAR, generate_placebos = FALSE
  ) %>%
  generate_predictor(time_window = pre_window,
                     Avg_GDPpc      = mean(gdppc_mpd, na.rm = TRUE),
                     Avg_GDP_Size   = mean(gdp_size,  na.rm = TRUE),
                     Avg_Population = mean(pop_mpd,   na.rm = TRUE)) %>%
  generate_predictor(time_window = 1975, Lag_GDPpc_1975 = mean(gdppc_mpd, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1980, Lag_GDPpc_1980 = mean(gdppc_mpd, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1985, Lag_GDPpc_1985 = mean(gdppc_mpd, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1990, Lag_GDPpc_1990 = mean(gdppc_mpd, na.rm = TRUE)) %>%
  generate_predictor(time_window = 1995, Lag_GDPpc_1995 = mean(gdppc_mpd, na.rm = TRUE)) %>%
  generate_weights(optimization_window = pre_window) %>%
  generate_control()

write_csv(scm %>% grab_unit_weights() %>% arrange(desc(weight)), file.path(target_dir, "Weights.csv"))
write_csv(scm %>% grab_balance_table(), file.path(target_dir, "Balance.csv"))

sc_trend <- scm %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y)
write_csv(sc_trend, file.path(target_dir, "YearByYear_Gap.csv"))

m_train <- sc_trend %>% filter(time_unit %in% pre_window)
m_val   <- sc_trend %>% filter(time_unit %in% TREAT_YEAR:(EURO_YEAR - 1))
m_post  <- sc_trend %>% filter(time_unit %in% EURO_YEAR:DATA_END)

metrics_tbl <- tibble(
  Period   = c(sprintf("Training (%d-%d)", min(pre_window), max(pre_window)),
               sprintf("Maastricht Validation (%d-%d)", TREAT_YEAR, EURO_YEAR - 1),
               sprintf("Post-Euro (%d-%d)", EURO_YEAR, DATA_END)),
  RMSE     = c(sqrt(mean(m_train$gap^2)), sqrt(mean(m_val$gap^2)), sqrt(mean(m_post$gap^2))),
  MAE      = c(mean(abs(m_train$gap)), mean(abs(m_val$gap)), mean(abs(m_post$gap))),
  Mean_Gap = c(mean(m_train$gap), mean(m_val$gap), mean(m_post$gap))
)
write_csv(metrics_tbl, file.path(target_dir, "Metrics.csv"))

cat("Saved 01_GDP_Validation_Suite outputs successfully!\n")
