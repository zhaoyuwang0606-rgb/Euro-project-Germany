# =========================================================================
# 02_PWT_Channel_Validation_Suite/run_channel_validation.R
# Self-contained R script to reproduce 02_PWT_Channel_Validation_Suite SCM results
# Input: ../Hybrid_MPD_PWT_Panel.csv
# Output: Current directory (.)
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidysynth)
  library(readxl)
})

cat("=== Running PWT Channel Validation Suite SCM ===\n")

raw_hybrid <- read_csv("../Hybrid_MPD_PWT_Panel.csv", show_col_types = FALSE)

clean_countries <- raw_hybrid %>%
  group_by(country) %>%
  filter(n() == 50) %>%
  summarise(
    na_g = sum(is.na(gdppc_mpd)),
    na_i = sum(is.na(csh_i)),
    na_x = sum(is.na(csh_x)),
    na_m = sum(is.na(csh_m)),
    na_c = sum(is.na(csh_c)),
    na_gov = sum(is.na(csh_g))
  ) %>%
  filter(na_g == 0 & na_i == 0 & na_x == 0 & na_m == 0 & na_c == 0 & na_gov == 0) %>%
  pull(country)

hybrid_clean <- raw_hybrid %>%
  filter(country %in% clean_countries & year >= 1970 & year <= 2019)

treated_country <- "Germany"
DATA_START <- 1970; DATA_END <- 2019; EURO_YEAR <- 1999

build_rich_scm <- function(data, target_var, treat_year, pre_window, i_unit_name = treated_country) {
  max_pre <- max(pre_window)
  
  scm_prep <- data %>%
    synthetic_control(
      outcome = !!sym(target_var), unit = country, time = year,
      i_unit = i_unit_name, i_time = treat_year, generate_placebos = FALSE
    ) %>%
    generate_predictor(time_window = pre_window,
                       Avg_Target_Val = mean(!!sym(target_var), na.rm = TRUE),
                       Avg_GDPpc      = mean(gdppc_mpd, na.rm = TRUE),
                       Avg_GDP_Size   = mean(gdp_size,  na.rm = TRUE),
                       Avg_Population = mean(pop_mpd,   na.rm = TRUE))
  
  # Sub-period 1: 1970-1979 (1970s Era)
  if (1979 <= max_pre) {
    scm_prep <- scm_prep %>%
      generate_predictor(time_window = 1970:1979,
                         Avg_Target_1970_1979 = mean(!!sym(target_var), na.rm = TRUE),
                         Avg_GDPpc_1970_1979  = mean(gdppc_mpd, na.rm = TRUE))
  }
  
  # Sub-period 2: 1980-1986 (Early 1980s Era)
  if (1986 <= max_pre) {
    scm_prep <- scm_prep %>%
      generate_predictor(time_window = 1980:1986,
                         Avg_Target_1980_1986 = mean(!!sym(target_var), na.rm = TRUE),
                         Avg_GDPpc_1980_1986  = mean(gdppc_mpd, na.rm = TRUE))
  }
  
  # Sub-period 3: 1987-1990 (Pre-Reunification / 1990 Era)
  if (1990 <= max_pre) {
    scm_prep <- scm_prep %>%
      generate_predictor(time_window = 1987:1990,
                         Avg_Target_1987_1990 = mean(!!sym(target_var), na.rm = TRUE),
                         Avg_GDPpc_1987_1990  = mean(gdppc_mpd, na.rm = TRUE))
  }
  
  # Sub-period 4: 1991 to max_pre (Post-Reunification Pre-Treatment Era)
  if (max_pre >= 1991) {
    recent_window <- 1991:max_pre
    scm_prep <- scm_prep %>%
      generate_predictor(time_window = recent_window,
                         Avg_Target_1991_Pre = mean(!!sym(target_var), na.rm = TRUE),
                         Avg_GDPpc_1991_Pre  = mean(gdppc_mpd, na.rm = TRUE))
  }

  scm <- scm_prep %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm)
}

run_full_component_exp <- function(target_var, var_label, treat_year) {
  exp_name <- sprintf("%s_Treat_%d", target_var, treat_year)
  target_dir <- file.path(".", exp_name)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  
  pre_window <- 1970:(treat_year - 1)
  cv_end     <- EURO_YEAR - 1
  
  cat(sprintf("\n>>> Running Channel: %s (%s) | Treat=%d <<<\n", target_var, var_label, treat_year))
  
  baseline <- build_rich_scm(hybrid_clean, target_var, treat_year, pre_window)
  
  dw <- baseline %>% grab_unit_weights() %>% arrange(desc(weight))
  write_csv(dw, file.path(target_dir, "Weights.csv"))
  write_csv(baseline %>% grab_balance_table(), file.path(target_dir, "Balance.csv"))
  
  sc_trend <- baseline %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y)
  write_csv(sc_trend, file.path(target_dir, "YearByYear_Gap.csv"))
  
  m_train <- sc_trend %>% filter(time_unit %in% pre_window)
  m_val   <- sc_trend %>% filter(time_unit %in% treat_year:cv_end)
  m_post  <- sc_trend %>% filter(time_unit %in% EURO_YEAR:DATA_END)
  
  metrics_tbl <- tibble(
    Period   = c(sprintf("Training (%d-%d)", min(pre_window), max(pre_window)),
                 sprintf("Maastricht Validation (%d-%d)", treat_year, cv_end),
                 sprintf("Post-Euro (%d-%d)", EURO_YEAR, DATA_END)),
    RMSE     = c(sqrt(mean(m_train$gap^2)), sqrt(mean(m_val$gap^2)), sqrt(mean(m_post$gap^2))),
    MAE      = c(mean(abs(m_train$gap)), mean(abs(m_val$gap)), mean(abs(m_post$gap))),
    Mean_Gap = c(mean(m_train$gap), mean(m_val$gap), mean(m_post$gap))
  )
  write_csv(metrics_tbl, file.path(target_dir, "Metrics.csv"))
}

# Run channels for csh_i, csh_x, csh_m, csh_c, csh_g
vars_list <- list(
  csh_i = "Investment Share",
  csh_x = "Export Share",
  csh_m = "Import Share",
  csh_c = "Consumption Share",
  csh_g = "Government Share"
)

years_list <- 1993:1998

for (v_name in names(vars_list)) {
  for (t_yr in years_list) {
    run_full_component_exp(v_name, vars_list[[v_name]], t_yr)
  }
}

cat("\nSaved 02_PWT_Channel_Validation_Suite outputs successfully!\n")
