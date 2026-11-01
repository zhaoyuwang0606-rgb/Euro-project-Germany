# =========================================================================
# 04_Fixed_GDP_Weight_Decomposition/run_fixed_gdp_weight_suite.R
# Estimates SCM weights on GDP per capita (w_GDP)
# Applies locked w_GDP across all PWT macro channels & FDI variables
# Outputs full suite directly inside 04_Fixed_GDP_Weight_Decomposition
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(pwt10)
  library(tidysynth)
  library(parallel)
  library(future)
  library(furrr)
})

N_WORKERS <- max(1L, parallel::detectCores() - 2L)
plan(multisession, workers = N_WORKERS)

CANDIDATE_YEARS <- c(1993L, 1994L, 1995L, 1996L, 1997L)
TREATED <- "Germany"
CV_END  <- 1998L
FIXED_OUT_DIR <- "."

data("pwt10.01")
pwt_iso3 <- c("DEU", "AUS", "CAN", "CHL", "HKG", "COL", "CRI", "ISR", "JPN", "KOR", "MEX", "NZL", "SGP", "TWN", "TUR", "USA")

pwt_raw <- pwt10.01 %>%
  filter(isocode %in% pwt_iso3 & year >= 1970 & year <= 2019) %>%
  mutate(country = case_when(
    isocode == "DEU" ~ "Germany", isocode == "AUS" ~ "Australia", isocode == "CAN" ~ "Canada",
    isocode == "CHL" ~ "Chile", isocode == "HKG" ~ "China, Hong Kong SAR", isocode == "COL" ~ "Colombia",
    isocode == "CRI" ~ "Costa Rica", isocode == "ISR" ~ "Israel", isocode == "JPN" ~ "Japan",
    isocode == "KOR" ~ "Republic of Korea", isocode == "MEX" ~ "Mexico", isocode == "NZL" ~ "New Zealand",
    isocode == "SGP" ~ "Singapore", isocode == "TWN" ~ "Taiwan", isocode == "TUR" ~ "Turkey",
    isocode == "USA" ~ "United States", TRUE ~ country
  )) %>%
  mutate(gdp_size = rgdpe, gdppc = rgdpe / pop, log_gdp_size = log(gdp_size), log_pop = log(pop))

pwt_panel <- pwt_raw %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(gdppc_growth = (gdppc - lag(gdppc)) / lag(gdppc)) %>%
  ungroup() %>%
  drop_na(csh_c, csh_i, csh_g, csh_x, csh_m, log_gdp_size, log_pop, gdppc)

pwt_panel <- pwt_panel %>% group_by(country) %>% filter(n() >= (CV_END - 1970L + 1)) %>% ungroup()

build_gdp_weight <- function(data, treat_year, target_unit = TREATED, data_start = 1970L) {
  pw <- data_start:(treat_year - 1L)
  l1 <- treat_year-1L; l2 <- treat_year-2L; l3 <- treat_year-3L; l4 <- treat_year-4L; l5 <- treat_year-5L
  
  data %>%
    synthetic_control(outcome = gdppc, unit = country, time = year, 
                      i_unit = target_unit, i_time = treat_year, generate_placebos = FALSE) %>%
    generate_predictor(time_window = pw,
      Avg_Target_Val = mean(gdppc, na.rm = TRUE),
      Avg_GDPpc      = mean(gdppc, na.rm = TRUE),
      Log_GDPsz      = mean(log_gdp_size, na.rm = TRUE),
      Log_Pop        = mean(log_pop, na.rm = TRUE),
      GDPpc_Gr       = mean(gdppc_growth, na.rm = TRUE)) %>%
    generate_predictor(time_window = l5, L5_Target = mean(gdppc, na.rm = TRUE)) %>%
    generate_predictor(time_window = l4, L4_Target = mean(gdppc, na.rm = TRUE)) %>%
    generate_predictor(time_window = l3, L3_Target = mean(gdppc, na.rm = TRUE)) %>%
    generate_predictor(time_window = l2, L2_Target = mean(gdppc, na.rm = TRUE)) %>%
    generate_predictor(time_window = l1, L1_Target = mean(gdppc, na.rm = TRUE)) %>%
    generate_weights(optimization_window = pw) %>%
    generate_control()
}

apply_fixed_weights <- function(data, outcome_var, w_df, treat_year, target_unit = TREATED, data_start = 1970L) {
  donors <- w_df %>% filter(weight > 1e-6)
  
  deu_real <- data %>% filter(country == target_unit) %>% select(year, real_y = !!sym(outcome_var))
  
  donors_y <- data %>%
    filter(country %in% donors$unit) %>%
    select(country, year, y = !!sym(outcome_var)) %>%
    inner_join(donors, by = c("country" = "unit")) %>%
    group_by(year) %>%
    summarise(synth_y = sum(y * weight, na.rm = TRUE), .groups = "drop")
  
  deu_real %>%
    left_join(donors_y, by = "year") %>%
    mutate(gap = real_y - synth_y)
}

for (t_yr in CANDIDATE_YEARS) {
  gdp_fit <- build_gdp_weight(pwt_panel, t_yr)
  w_df <- gdp_fit %>% grab_unit_weights() %>% arrange(desc(weight))
  
  for (v in c("csh_i", "csh_c", "csh_g", "csh_x", "csh_m")) {
    exp_name <- sprintf("FixedWeights_%s_Treat_%d", v, t_yr)
    exp_dir <- file.path(FIXED_OUT_DIR, exp_name)
    dir.create(exp_dir, showWarnings = FALSE, recursive = TRUE)
    
    write.csv(w_df, file.path(exp_dir, "Weights.csv"), row.names = FALSE)
    
    sc_trend <- apply_fixed_weights(pwt_panel, v, w_df, t_yr)
    write.csv(sc_trend, file.path(exp_dir, "YearByYear_Gap.csv"), row.names = FALSE)
  }
}

cat("Saved 04_Fixed_GDP_Weight_Decomposition outputs successfully!\n")
