library(tidyverse)
library(readxl)
library(tidysynth)

# ==============================================================================
# Master Script: Compute Aggregated Treatment Effect for ALL SCM Specifications
# ==============================================================================

cat("\n== Reading Master Data ==\n")
raw_data <- read_excel("../mpd2023_web.xlsx", sheet = "Full data")

# ==============================================================================
# 定义所有 Donor Pool
# ==============================================================================
all_donors_full <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark", 
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", 
  "Japan", "Republic of Korea", "Norway", "New Zealand", "Poland", 
  "Qatar", "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China", "United States"
)

spillover_and_japan <- c("Switzerland", "Denmark", "United Kingdom", "Iceland", 
                         "Norway", "Poland", "Sweden", "Japan",
                         "Saudi Arabia", "Qatar", "Israel")

western_countries <- c("Australia", "Canada", "Switzerland", "Denmark", 
                       "United Kingdom", "Iceland", "Norway", "New Zealand", 
                       "Sweden", "United States")

western_no_uk <- c("Australia", "Canada", "Switzerland", "Denmark", 
                   "Iceland", "Norway", "New Zealand", 
                   "Sweden", "United States")

treated_country <- "Germany"

# ==============================================================================
# 辅助函数
# ==============================================================================

# 根据 pool type 和时间窗口准备数据
prepare_data <- function(pool_type, start_year) {
  if (pool_type == "wide") {
    df <- raw_data %>%
      filter(country %in% c(treated_country, all_donors_full))
  } else if (pool_type == "noneu") {
    df <- raw_data %>%
      filter(country %in% c(treated_country, all_donors_full)) %>%
      filter(!country %in% spillover_and_japan)
  } else if (pool_type == "western") {
    df <- raw_data %>%
      filter(country %in% c(treated_country, western_countries))
  } else if (pool_type == "western_no_uk") {
    df <- raw_data %>%
      filter(country %in% c(treated_country, western_no_uk))
  }
  
  df %>%
    filter(year >= start_year & year <= 2022) %>%
    select(country, year, gdppc, pop) %>%
    mutate(gdp_size = gdppc * pop) %>%
    drop_na() %>%
    group_by(country) %>%
    filter(n() == length(start_year:2022)) %>%
    ungroup()
}

# 运行 SCM
run_scm <- function(data, treat_year, start_year, exclude_country = NULL) {
  if (!is.null(exclude_country)) {
    data <- data %>% filter(country != exclude_country)
  }
  pre_window <- start_year:(treat_year - 1)
  
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
                       GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE)) %>%
    generate_predictor(time_window = start_year, GDPpc_start = gdppc) %>%
    generate_predictor(time_window = (treat_year - 1), GDPpc_pre_treat = gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm_obj)
}

# 提取 gap
extract_gaps <- function(scm_obj) {
  scm_obj %>% grab_synthetic_control() %>%
    mutate(gap = real_y - synth_y,
           pct_gap = gap / synth_y * 100)
}

# 对一个 specification 计算 baseline + LOO aggregated effect
compute_spec_aggregate <- function(spec_name, pool_type, start_year, treat_year) {
  cat(sprintf("\n============================================\n"))
  cat(sprintf("Specification: %s\n", spec_name))
  cat(sprintf("  Pool: %s | Start: %d | Treat: %d\n", pool_type, start_year, treat_year))
  cat(sprintf("============================================\n"))
  
  df <- prepare_data(pool_type, start_year)
  cat(sprintf("  Countries: %s\n", paste(unique(df$country), collapse = ", ")))
  
  # Baseline
  baseline_scm <- run_scm(df, treat_year, start_year)
  baseline_gaps <- extract_gaps(baseline_scm)
  
  post_gaps <- baseline_gaps %>% filter(time_unit >= treat_year)
  pre_gaps  <- baseline_gaps %>% filter(time_unit < treat_year)
  
  results <- list()
  results[["baseline"]] <- tibble(
    Experiment = spec_name,
    Specification = "Baseline",
    Pre_Avg_Gap = mean(pre_gaps$gap),
    Post_Avg_Gap = mean(post_gaps$gap),
    Post_Cumul_Gap = sum(post_gaps$gap),
    Post_Avg_Pct = mean(post_gaps$pct_gap),
    Post_N_Years = nrow(post_gaps),
    Direction = ifelse(mean(post_gaps$gap) > 0, "POSITIVE (+)", "NEGATIVE (-)")
  )
  
  # LOO for ALL donors
  all_unit_weights <- baseline_scm %>% grab_unit_weights()
  all_donors_list <- all_unit_weights %>% pull(unit)
  
  for (donor in all_donors_list) {
    cat(sprintf("  LOO: Dropping %s\n", donor))
    tryCatch({
      loo_scm <- run_scm(df, treat_year, start_year, exclude_country = donor)
      loo_gaps <- extract_gaps(loo_scm)
      
      post_g <- loo_gaps %>% filter(time_unit >= treat_year)
      pre_g  <- loo_gaps %>% filter(time_unit < treat_year)
      
      w <- all_unit_weights %>% filter(unit == donor) %>% pull(weight)
      
      results[[paste0("loo_", donor)]] <- tibble(
        Experiment = spec_name,
        Specification = sprintf("Drop %s (w=%.1f%%)", donor, w * 100),
        Pre_Avg_Gap = mean(pre_g$gap),
        Post_Avg_Gap = mean(post_g$gap),
        Post_Cumul_Gap = sum(post_g$gap),
        Post_Avg_Pct = mean(post_g$pct_gap),
        Post_N_Years = nrow(post_g),
        Direction = ifelse(mean(post_g$gap) > 0, "POSITIVE (+)", "NEGATIVE (-)")
      )
    }, error = function(e) {
      cat(sprintf("    ERROR dropping %s: %s\n", donor, e$message))
    })
  }
  
  return(bind_rows(results))
}

# ==============================================================================
# 执行所有 6 个 Specification
# ==============================================================================
all_results <- list()

# 1. Wide Panel (全 20 国 donor pool)
all_results[["wide"]] <- compute_spec_aggregate(
  "Wide Panel (All 20 Donors)", "wide", 1970, 1999)

# 2. Non-EU Clean Pool (baseline)
all_results[["noneu"]] <- compute_spec_aggregate(
  "Non-EU Clean Pool", "noneu", 1970, 1999)

# 3. Western OECD
all_results[["western"]] <- compute_spec_aggregate(
  "Western OECD", "western", 1970, 1999)

# 4. Western OECD No UK
all_results[["western_no_uk"]] <- compute_spec_aggregate(
  "Western OECD (No UK)", "western_no_uk", 1970, 1999)

# 5. Time Robustness: Start 1950
all_results[["time_1950"]] <- compute_spec_aggregate(
  "TimeRob Start1950 Treat1999", "noneu", 1950, 1999)

# 6. Time Robustness: Treat 2002
all_results[["time_2002"]] <- compute_spec_aggregate(
  "TimeRob Start1970 Treat2002", "noneu", 1970, 2002)

# 7. In-Time Placebo: Treat 1990
all_results[["time_1990"]] <- compute_spec_aggregate(
  "InTimePlacebo Start1970 Treat1990", "noneu", 1970, 1990)

# ==============================================================================
# 汇总输出
# ==============================================================================
final_table <- bind_rows(all_results)

cat("\n\n################################################################\n")
cat("MASTER AGGREGATED TREATMENT EFFECT TABLE\n")
cat("################################################################\n\n")
print(final_table, n = 200, width = 250)

write_csv(final_table, "All_SCM_Aggregated_Effects.csv")

# 只看 Baseline 的对比
cat("\n\n== BASELINE-ONLY COMPARISON ==\n")
baseline_only <- final_table %>% filter(Specification == "Baseline")
print(baseline_only, width = 200)

cat("\n== Saved: All_SCM_Aggregated_Effects.csv ==\n")
