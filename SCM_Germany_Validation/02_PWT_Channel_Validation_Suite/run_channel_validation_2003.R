# =========================================================================
# 02_PWT_Channel_Validation_Suite/run_channel_validation_2003.R
# SCM Analysis for PWT Expenditure Channels with Treatment Year = 2003 (Agenda 2010)
# Input: ../Hybrid_MPD_PWT_Panel.csv
# Output: New directories (csh_*_Treat_2003) - No overwriting of existing runs!
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidysynth)
  library(ggplot2)
})

cat("=== Running PWT Channel Validation Suite SCM (Treatment Year = 2003) ===\n")

raw_hybrid <- read_csv("../Hybrid_MPD_PWT_Panel.csv", show_col_types = FALSE)

clean_countries <- raw_hybrid %>%
  group_by(country) %>%
  filter(n() == 50) %>%
  summarise(
    na_g   = sum(is.na(gdppc_mpd)),
    na_i   = sum(is.na(csh_i)),
    na_x   = sum(is.na(csh_x)),
    na_m   = sum(is.na(csh_m)),
    na_c   = sum(is.na(csh_c)),
    na_gov = sum(is.na(csh_g))
  ) %>%
  filter(na_g == 0 & na_i == 0 & na_x == 0 & na_m == 0 & na_c == 0 & na_gov == 0) %>%
  pull(country)

hybrid_clean <- raw_hybrid %>%
  filter(country %in% clean_countries & year >= 1970 & year <= 2019)

treated_country <- "Germany"
DATA_START <- 1970; DATA_END <- 2019; TREAT_YEAR <- 2003

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
  
  if (1986 <= max_pre) {
    scm_prep <- scm_prep %>%
      generate_predictor(time_window = 1980:1986,
                         Avg_Target_1980_1986 = mean(!!sym(target_var), na.rm = TRUE))
  }
  if (1990 <= max_pre) {
    scm_prep <- scm_prep %>%
      generate_predictor(time_window = 1987:1990,
                         Avg_Target_1987_1990 = mean(!!sym(target_var), na.rm = TRUE))
  }
  
  for (yr in c(1975, 1980, 1985, 1990)) {
    if (yr <= max_pre) {
      scm_prep <- scm_prep %>%
        generate_predictor(time_window = yr,
                           !!sym(paste0("Lag_Target_", yr)) := mean(!!sym(target_var), na.rm = TRUE),
                           !!sym(paste0("Lag_GDPpc_", yr))  := mean(gdppc_mpd, na.rm = TRUE))
    }
  }
  
  recent_yrs <- seq(max(1990, max_pre - 4), max_pre)
  for (yr in recent_yrs) {
    if (yr <= max_pre && !(yr %in% c(1975, 1980, 1985, 1990))) {
      scm_prep <- scm_prep %>%
        generate_predictor(time_window = yr,
                           !!sym(paste0("Lag_Target_", yr)) := mean(!!sym(target_var), na.rm = TRUE),
                           !!sym(paste0("Lag_GDPpc_", yr))  := mean(gdppc_mpd, na.rm = TRUE))
    }
  }

  scm <- scm_prep %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm)
}

run_full_component_exp_2003 <- function(target_var, var_label) {
  treat_year <- TREAT_YEAR
  exp_name   <- sprintf("%s_Treat_%d", target_var, treat_year)
  target_dir <- file.path(".", exp_name)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  
  pre_window  <- 1970:(treat_year - 1) # 1970:2002
  post_window <- treat_year:DATA_END  # 2003:2019
  
  cat(sprintf("\n>>> Running Channel: %s (%s) | Treat Year = %d <<<\n", target_var, var_label, treat_year))
  
  baseline <- build_rich_scm(hybrid_clean, target_var, treat_year, pre_window)
  
  # 1. Weights
  dw <- baseline %>% grab_unit_weights() %>% arrange(desc(weight))
  write_csv(dw, file.path(target_dir, "Weights.csv"))
  
  # 2. Balance Table
  bal <- baseline %>% grab_balance_table()
  write_csv(bal, file.path(target_dir, "Balance.csv"))
  
  # 3. Year by Year Gap
  sc_trend <- baseline %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y)
  write_csv(sc_trend, file.path(target_dir, "YearByYear_Gap.csv"))
  
  # 4. Evaluation Metrics
  m_train <- sc_trend %>% filter(time_unit %in% pre_window)
  m_post  <- sc_trend %>% filter(time_unit %in% post_window)
  
  metrics_tbl <- tibble(
    Period   = c(sprintf("Training (%d-%d)", min(pre_window), max(pre_window)),
                 sprintf("Post-Treatment (%d-%d)", min(post_window), max(post_window))),
    RMSE     = c(sqrt(mean(m_train$gap^2)), sqrt(mean(m_post$gap^2))),
    MAE      = c(mean(abs(m_train$gap)), mean(abs(m_post$gap))),
    Mean_Gap = c(mean(m_train$gap), mean(m_post$gap))
  )
  write_csv(metrics_tbl, file.path(target_dir, "Metrics.csv"))
  
  # 5. Plotting Trends & Gap
  p_trend <- ggplot(sc_trend, aes(x = time_unit)) +
    annotate("rect", xmin = 2002.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "#1565C0", alpha = 0.05) +
    geom_vline(xintercept = 2002.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(aes(y = real_y,  color = "Germany (Actual)"), linewidth = 1.4) +
    geom_line(aes(y = synth_y, color = "Synthetic Germany"), linetype = "longdash", linewidth = 1.2) +
    scale_color_manual(values = c("Germany (Actual)" = "#1565C0", "Synthetic Germany" = "#C62828")) +
    annotate("text", x = 2005.5, y = max(c(sc_trend$real_y, sc_trend$synth_y))*0.95, label = "Agenda 2010\n(2003)", color = "red", fontface = "bold", size = 3.5) +
    labs(
      title = sprintf("PWT Channel Validation: %s (Treat Year = 2003)", var_label),
      subtitle = sprintf("Training: 1970-2002 (RMSE: %.4f) | Post-2003 Avg Gap: %.4f", sqrt(mean(m_train$gap^2)), mean(m_post$gap)),
      x = "Year", y = sprintf("%s (Share of GDP)", var_label), color = ""
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  ggsave(file.path(target_dir, "SCM_Trends.png"), p_trend, width = 10, height = 6, dpi = 300)
  
  p_gap <- ggplot(sc_trend, aes(x = time_unit, y = gap)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.8) +
    geom_vline(xintercept = 2002.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(color = "#1565C0", linewidth = 1.4) +
    geom_point(color = "#1565C0", size = 2) +
    labs(
      title = sprintf("Treatment Effect Gap (Actual - Synthetic): %s (Treat=2003)", var_label),
      subtitle = sprintf("Post-2003 Mean Gap: %.4f | Post-2003 Cumulative Gap: %.4f", mean(m_post$gap), sum(m_post$gap)),
      x = "Year", y = "Gap (Share of GDP)"
    ) +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(target_dir, "SCM_Gap.png"), p_gap, width = 10, height = 6, dpi = 300)
  
  cat(sprintf("Completed %s (Treat=2003). Output saved to %s/\n", target_var, target_dir))
}

# Run for all 5 expenditure components
vars_list <- list(
  csh_i = "Investment Share",
  csh_x = "Export Share",
  csh_m = "Import Share",
  csh_c = "Consumption Share",
  csh_g = "Government Share"
)

for (v_name in names(vars_list)) {
  run_full_component_exp_2003(v_name, vars_list[[v_name]])
}

# Summary across all 5 channels
all_summary <- list()
for (v_name in names(vars_list)) {
  dir_path <- file.path(".", sprintf("%s_Treat_2003", v_name))
  met <- read_csv(file.path(dir_path, "Metrics.csv"), show_col_types = FALSE)
  gap_df <- read_csv(file.path(dir_path, "YearByYear_Gap.csv"), show_col_types = FALSE)
  
  post_gaps <- gap_df %>% filter(time_unit >= 2003) %>% pull(gap)
  
  all_summary[[v_name]] <- tibble(
    Channel = v_name,
    Label = vars_list[[v_name]],
    Pre_RMSE = met$RMSE[met$Period == "Training (1970-2002)"],
    Post_RMSE = met$RMSE[met$Period == "Post-Treatment (2003-2019)"],
    Post_Mean_Gap = mean(post_gaps),
    Post_Cumulative_Gap = sum(post_gaps)
  )
}

summary_df <- bind_rows(all_summary)
write_csv(summary_df, "Summary_PWT_Channels_Treat_2003.csv")

cat("\n=== SUMMARY OF PWT CHANNELS (TREATMENT YEAR = 2003) ===\n")
print(summary_df)

cat("\nSaved all 2003 Treatment SCM outputs successfully into new directories!\n")
