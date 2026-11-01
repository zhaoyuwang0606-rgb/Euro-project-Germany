# =========================================================================
# SCM_Germany_Validation/run_germany_cv_scm.R
# Validation cross-validation / placebo suite
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(tidysynth)
})

main_output_dir <- "."

raw_data <- read_excel("../mpd2023_web.xlsx", sheet = "Full data")

all_donors_full <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark", 
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", 
  "Japan", "Republic of Korea", "Norway", "New Zealand", "Poland", 
  "Qatar", "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China", "United States"
)
spillover_and_japan <- c("Switzerland", "Denmark", "United Kingdom", "Iceland", 
                         "Norway", "Poland", "Sweden", "Japan",
                         "Saudi Arabia", "Qatar", "Israel")

treated_country <- "Germany"

run_scm_s1 <- function(data, exclude_country = NULL) {
  if (!is.null(exclude_country)) {
    data <- data %>% filter(country != exclude_country)
  }
  scm_obj <- data %>%
    synthetic_control(
      outcome = gdppc, unit = country, time = year,
      i_unit = treated_country, i_time = 1989, 
      generate_placebos = is.null(exclude_country)
    ) %>%
    generate_predictor(time_window = 1970:1988,
                       Average_gdppc = mean(gdppc, na.rm=TRUE),
                       Gdp_size = mean(gdp_size, na.rm=TRUE),
                       Popluation_size = mean(pop, na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970:1988,
                       GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                       Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970, GDPpc_start = gdppc) %>%
    generate_predictor(time_window = 1988, GDPpc_pre_treat = gdppc) %>%
    generate_weights(optimization_window = 1970:1988) %>%
    generate_control()
  
  return(scm_obj)
}

run_scm_generic <- function(data, treat_year, train_start, train_end, exclude_country = NULL) {
  if (!is.null(exclude_country)) {
    data <- data %>% filter(country != exclude_country)
  }
  pre_window <- train_start:train_end
  
  scm_obj <- data %>%
    synthetic_control(
      outcome = gdppc, unit = country, time = year,
      i_unit = treated_country, i_time = treat_year,
      generate_placebos = is.null(exclude_country)
    ) %>%
    generate_predictor(time_window = pre_window,
                       Average_gdppc = mean(gdppc, na.rm=TRUE),
                       Gdp_size = mean(gdp_size, na.rm=TRUE),
                       Popluation_size = mean(pop, na.rm=TRUE)) %>%
    generate_predictor(time_window = pre_window,
                       GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                       Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
    generate_predictor(time_window = train_start, GDPpc_start = gdppc) %>%
    generate_predictor(time_window = train_end, GDPpc_pre_treat = gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm_obj)
}

run_validation_spec <- function(spec_id, start_year, train_end_year, treat_year, end_year) {
  spec_name <- sprintf("noneu_Start%d_End%d", start_year, end_year)
  cat(sprintf("\n=== Validation Specification: %s (Treat=%d) ===\n", spec_name, treat_year))
  output_dir <- file.path(main_output_dir, spec_id)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  df <- raw_data %>% 
    filter(country %in% c(treated_country, all_donors_full)) %>% 
    filter(!country %in% spillover_and_japan) %>%
    filter(year >= start_year & year <= end_year) %>%
    select(country, year, gdppc, pop) %>%
    mutate(gdp_size = gdppc * pop) %>%
    drop_na() %>%
    group_by(country) %>%
    filter(n() == length(start_year:end_year)) %>%
    ungroup()
  
  scm_fit <- tryCatch({
    run_scm_generic(df, treat_year, start_year, train_end_year)
  }, error = function(e) {
    cat(sprintf("Error running SCM for %s: %s\n", spec_name, e$message))
    return(NULL)
  })
  
  if (is.null(scm_fit)) return(NULL)
  
  write_csv(scm_fit %>% grab_balance_table(), file.path(output_dir, "SCM_Balance.csv"))
  write_csv(scm_fit %>% grab_significance(), file.path(output_dir, "SCM_Significance.csv"))
  
  donor_weights <- scm_fit %>% grab_unit_weights() %>% arrange(desc(weight))
  write_csv(donor_weights, file.path(output_dir, "SCM_Weights.csv"))
  
  gaps <- scm_fit %>% grab_synthetic_control() %>%
    mutate(gap = real_y - synth_y, pct_gap = gap / synth_y * 100)
  write_csv(gaps %>% select(time_unit, real_y, synth_y, gap, pct_gap), file.path(output_dir, "Baseline_YearByYear_Gap.csv"))
  
  ggsave(file.path(output_dir, "SCM_Trends.png"), plot = scm_fit %>% plot_trends(), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_Placebo.png"), plot = scm_fit %>% plot_placebos(prune = FALSE), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_MSPE_Ratio.png"), plot = scm_fit %>% plot_mspe_ratio(), width = 8, height = 6)
  
  # Validation metrics (Pseudo-treatment evaluation)
  val_window <- (train_end_year + 1):(treat_year - 1)
  pre_gaps   <- gaps %>% filter(time_unit <= train_end_year)
  val_gaps   <- gaps %>% filter(time_unit %in% val_window)
  post_gaps  <- gaps %>% filter(time_unit >= treat_year)
  
  val_summary <- tibble(
    Specification   = spec_name,
    Pre_Train_MSPE  = mean(pre_gaps$gap^2),
    Pseudo_Val_MSPE = ifelse(nrow(val_gaps) > 0, mean(val_gaps$gap^2), NA),
    Post_Treat_MSPE = mean(post_gaps$gap^2),
    Val_Pre_MSPE_Ratio = ifelse(nrow(val_gaps) > 0, mean(val_gaps$gap^2) / mean(pre_gaps$gap^2), NA)
  )
  write_csv(val_summary, file.path(output_dir, "Validation_Performance_Summary.csv"))
  return(val_summary)
}

val_results <- list()
val_results[["noneu_70_98"]] <- run_validation_spec("noneu_Start1970_End1998", 1970, 1988, 1989, 1998)
val_results[["noneu_70_22"]] <- run_validation_spec("noneu_Start1970_End2022", 1970, 1988, 1989, 2022)
val_results[["noneu_90_98"]] <- run_validation_spec("noneu_Start1990_End1998", 1990, 1994, 1995, 1998)
val_results[["noneu_90_22"]] <- run_validation_spec("noneu_Start1990_End2022", 1990, 1994, 1995, 2022)

write_csv(bind_rows(val_results), file.path(main_output_dir, "Summary_All_Validation_Performance.csv"))
cat("=== SCM_Germany_Validation finished! ===\n")
