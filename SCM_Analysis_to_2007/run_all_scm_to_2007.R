library(tidyverse)
library(readxl)
library(tidysynth)

# Portable path resolution
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
proj_root <- find_project_root()
main_dir <- Sys.getenv("REPL_SCM_2007_DIR")
if (!nzchar(main_dir)) {
  main_dir <- file.path(proj_root, "SCM_Analysis_to_2007")
}
dir.create(main_dir, showWarnings = FALSE, recursive = TRUE)

# Read Master Data
raw_data <- read_excel(file.path(proj_root, "mpd2023_web.xlsx"), sheet = "Full data")

# Define Donor Pools
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

# Data preparation helper (restricted to 2007)
prepare_data <- function(pool_type, start_year, drop_NZ = FALSE) {
  if (pool_type == "wide") {
    df <- raw_data %>% filter(country %in% c(treated_country, all_donors_full))
  } else if (pool_type == "noneu") {
    df <- raw_data %>% filter(country %in% c(treated_country, all_donors_full)) %>% filter(!country %in% spillover_and_japan)
  } else if (pool_type == "western") {
    df <- raw_data %>% filter(country %in% c(treated_country, western_countries))
  } else if (pool_type == "western_no_uk") {
    df <- raw_data %>% filter(country %in% c(treated_country, western_no_uk))
  }
  
  if (drop_NZ) {
    df <- df %>% filter(country != "New Zealand")
  }
  
  df %>%
    filter(year >= start_year & year <= 2007) %>% # RESTRICT TO 2007
    select(country, year, gdppc, pop) %>%
    mutate(gdp_size = gdppc * pop) %>%
    drop_na() %>%
    group_by(country) %>%
    filter(n() == length(start_year:2007)) %>%
    ungroup()
}

# Run SCM helper
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
                       GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                       Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
    generate_predictor(time_window = start_year, GDPpc_start = gdppc) %>%
    generate_predictor(time_window = (treat_year - 1), GDPpc_pre_treat = gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm_obj)
}

# Function to run a full experiment (Baseline + LOO + Plots + CSVs)
run_full_spec <- function(spec_name, pool_type, start_year, treat_year, subfolder_name, drop_NZ = FALSE) {
  cat(sprintf("\n=== Running Specification: %s ===\n", spec_name))
  output_dir <- file.path(main_dir, subfolder_name)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  df <- prepare_data(pool_type, start_year, drop_NZ)
  
  # Baseline
  baseline_scm <- run_scm(df, treat_year, start_year)
  
  # Save tables
  write_csv(baseline_scm %>% grab_balance_table(), file.path(output_dir, "SCM_Balance.csv"))
  write_csv(baseline_scm %>% grab_significance(), file.path(output_dir, "SCM_Significance.csv"))
  
  donor_weights <- baseline_scm %>% grab_unit_weights() %>% arrange(desc(weight))
  write_csv(donor_weights, file.path(output_dir, "SCM_Weights.csv"))
  
  baseline_gaps <- baseline_scm %>% grab_synthetic_control() %>%
    mutate(gap = real_y - synth_y,
           pct_gap = gap / synth_y * 100)
  write_csv(baseline_gaps %>% select(time_unit, real_y, synth_y, gap, pct_gap), file.path(output_dir, "Baseline_YearByYear_Gap.csv"))
  
  # Save plots
  ggsave(file.path(output_dir, "SCM_Trends.png"), plot = baseline_scm %>% plot_trends() + ggtitle(sprintf("Germany vs Synth (%s)", spec_name)), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_Weights.png"), plot = baseline_scm %>% plot_weights() + ggtitle("Donor Weights"), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_Placebo.png"), plot = baseline_scm %>% plot_placebos(prune = FALSE) + ggtitle("Placebo Test"), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_MSPE_Ratio.png"), plot = baseline_scm %>% plot_mspe_ratio() + ggtitle("MSPE Ratio"), width = 8, height = 6)
  
  # LOO
  top_donors <- donor_weights %>% filter(weight > 0.01) %>% pull(unit)
  loo_trends <- list()
  for (donor in top_donors) {
    cat(sprintf("  LOO: Dropping %s\n", donor))
    tryCatch({
      temp_scm <- run_scm(df, treat_year, start_year, exclude_country = donor)
      loo_trends[[donor]] <- temp_scm %>% grab_synthetic_control() %>%
        mutate(value = synth_y, type = paste("Drop", donor)) %>%
        select(time_unit, value, type)
    }, error = function(e) {
      cat(sprintf("    Error dropping %s: %s\n", donor, e$message))
    })
  }
  
  baseline_trend <- baseline_scm %>% grab_synthetic_control() %>% mutate(value = synth_y, type = "Baseline Synthetic") %>% select(time_unit, value, type)
  real_trend <- baseline_scm %>% grab_synthetic_control() %>% mutate(value = real_y, type = "Real Germany") %>% select(time_unit, value, type)
  
  all_loo_data <- bind_rows(loo_trends) %>% bind_rows(baseline_trend) %>% bind_rows(real_trend)
  types <- unique(all_loo_data$type)
  my_colors <- setNames(scales::hue_pal()(length(types)), types)
  my_colors["Real Germany"] <- "black"
  
  my_linewidths <- setNames(rep(0.6, length(types)), types)
  my_linewidths["Real Germany"] <- 1.5
  
  p_loo <- ggplot(all_loo_data, aes(x = time_unit, y = value, color = type, linewidth = type)) +
    geom_line(alpha = 0.8) +
    geom_vline(xintercept = treat_year, linetype = "dashed", color = "black") +
    scale_color_manual(values = my_colors) + scale_linewidth_manual(values = my_linewidths) +
    guides(linewidth = "none") + labs(title = "LOO Trend Check", x = "Year", y = "GDPpc", color = "Model") + theme_minimal()
  ggsave(file.path(output_dir, "SCM_LOO.png"), plot = p_loo, width = 10, height = 6)
  
  # Compute Aggregated Effects
  post_gaps <- baseline_gaps %>% filter(time_unit >= treat_year)
  pre_gaps  <- baseline_gaps %>% filter(time_unit < treat_year)
  
  agg_results <- list()
  agg_results[["Baseline"]] <- tibble(
    Specification = "Baseline",
    Pre_Avg_Gap = mean(pre_gaps$gap),
    Post_Avg_Gap = mean(post_gaps$gap),
    Post_Cumul_Gap = sum(post_gaps$gap),
    Post_Avg_Pct = mean(post_gaps$pct_gap),
    Post_N_Years = nrow(post_gaps)
  )
  
  for (donor in top_donors) {
    if (!is.null(loo_trends[[donor]])) {
      tryCatch({
        temp_scm <- run_scm(df, treat_year, start_year, exclude_country = donor)
        tg <- temp_scm %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y, pct_gap = gap / synth_y * 100)
        post_g <- tg %>% filter(time_unit >= treat_year)
        pre_g  <- tg %>% filter(time_unit < treat_year)
        agg_results[[paste0("Drop_", donor)]] <- tibble(
          Specification = paste0("Drop ", donor),
          Pre_Avg_Gap = mean(pre_g$gap),
          Post_Avg_Gap = mean(post_g$gap),
          Post_Cumul_Gap = sum(post_g$gap),
          Post_Avg_Pct = mean(post_g$pct_gap),
          Post_N_Years = nrow(post_g)
        )
      }, error = function(e) {})
    }
  }
  
  write_csv(bind_rows(agg_results), file.path(output_dir, "Aggregated_Treatment_Effect.csv"))
  
  return(tibble(
    Experiment = spec_name,
    Pre_Avg_Gap = mean(pre_gaps$gap),
    Post_Avg_Gap = mean(post_gaps$gap),
    Post_Cumul_Gap = sum(post_gaps$gap),
    Post_Avg_Pct = mean(post_gaps$pct_gap),
    Post_N_Years = nrow(post_gaps)
  ))
}

# Run all specifications
master_effects <- list()

master_effects[["wide"]] <- run_full_spec(
  "Wide Panel (All 20 Donors)", "wide", 1970, 1999, "SCM_Results wide panel")

master_effects[["noneu"]] <- run_full_spec(
  "Non-EU Clean Pool", "noneu", 1970, 1999, "noneu SCM result")

master_effects[["noneu_dropNZ"]] <- run_full_spec(
  "Non-EU Clean Pool (Drop New Zealand)", "noneu", 1970, 1999, "noneu SCM result drop New Zealand", drop_NZ = TRUE)

master_effects[["western"]] <- run_full_spec(
  "Western OECD", "western", 1970, 1999, "western SCM result")

master_effects[["western_no_uk"]] <- run_full_spec(
  "Western OECD (No UK)", "western_no_uk", 1970, 1999, "western SCM result/western leave uk")

# Time Robustness (noneu pool)
master_effects[["time_1950"]] <- run_full_spec(
  "TimeRob Start1950 Treat1999", "noneu", 1950, 1999, "TimeRob_noneu_Start1950_Treat1999")

master_effects[["time_2002"]] <- run_full_spec(
  "TimeRob Start1970 Treat2002", "noneu", 1970, 2002, "TimeRob_noneu_Start1970_Treat2002")

master_effects[["time_1990"]] <- run_full_spec(
  "InTimePlacebo Start1970 Treat1990", "noneu", 1970, 1990, "TimeRob_noneu_Start1970_Treat1990")

# Write Master Table
write_csv(bind_rows(master_effects), file.path(main_dir, "All_SCM_Aggregated_Effects.csv"))
cat("\n== SCM Analysis to 2007 Completed! ==\n")
