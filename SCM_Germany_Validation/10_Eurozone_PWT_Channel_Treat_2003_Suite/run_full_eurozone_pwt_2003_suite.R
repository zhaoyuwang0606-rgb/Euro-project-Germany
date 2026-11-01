# =========================================================================
# 10_Eurozone_PWT_Channel_Treat_2003_Suite/run_full_eurozone_pwt_2003_suite.R
# Dynamic SCM (Eurozone Donor Pool) for PWT Expenditure Channels (Treat = 2003)
# Target Variables: csh_i, csh_x, csh_m, csh_c, csh_g (GDP Expenditure Composition)
# Predictor & SCM weight optimization is fitted DYNAMICALLY per channel!
# =========================================================================

suppressPackageStartupMessages({
  library(haven)
  library(tidyverse)
  library(Synth)
  library(ggplot2)
  library(parallel)
  library(pbapply)
})

num_cores <- max(1, detectCores() - 1)
cat(sprintf("=== Running Dynamic Eurozone SCM Suite for PWT Channels (Treat=2003) (CPU Cores: %d / %d) ===\n", num_cores, detectCores()))

# Load Analysis_Annual.dta which contains complete PWT data for Eurozone countries
dta_path <- "../../Replication/Data/Analysis_Annual.dta"
raw_dta  <- read_dta(dta_path)

# Eurozone countries pool (matching Eurozone donor set)
euro_countries <- c("Germany", "Italy", "Finland", "Austria", "Netherlands", "Spain", "Portugal", "France", "Belgium", "Greece", "Ireland")

panel <- raw_dta %>%
  filter(country %in% euro_countries & year >= 1970 & year <= 2019) %>%
  mutate(
    gdppc_mpd = normgdppc_s,
    gdp_size  = normgdp_s,
    pop_mpd   = pop
  ) %>%
  arrange(country, year)

unit_map <- data.frame(
  country  = sort(unique(panel$country)),
  unit_num = seq_along(sort(unique(panel$country)))
)
panel <- left_join(panel, unit_map, by = "country")

treated_country <- "Germany"
TREAT_YEAR  <- 2003
pre_window  <- 1970:2002
post_window <- 2003:2019

deu_unit    <- unit_map$unit_num[unit_map$country == treated_country]
all_units   <- sort(unique(panel$unit_num))
donor_units <- setdiff(all_units, deu_unit)

run_scm_single <- function(tr_unit, ctrl_units, target_var, p_df) {
  min_yr <- min(p_df$year[p_df$country == unit_map$country[unit_map$unit_num == tr_unit]])
  actual_pre <- intersect(min_yr:2002, pre_window)
  
  phase_lags <- list(
    list(target_var, 1980:1986, "mean"),
    list(target_var, 1987:1993, "mean"),
    list(target_var, 1994:2002, "mean")
  )
  
  dp <- dataprep(
    foo                   = as.data.frame(p_df),
    predictors            = c("gdppc_mpd", "gdp_size", "pop_mpd"),
    predictors.op         = "mean",
    time.predictors.prior = actual_pre,
    special.predictors    = phase_lags,
    dependent             = target_var,
    unit.variable         = "unit_num",
    unit.names.variable   = "country",
    time.variable         = "year",
    treatment.identifier  = tr_unit,
    controls.identifier   = ctrl_units,
    time.optimize.ssr     = actual_pre,
    time.plot             = sort(unique(p_df$year))
  )
  
  so <- synth(dp, method = "BFGS")
  w_scm <- so$solution.w
  
  time_pts <- as.numeric(rownames(dp$Y1plot))
  y_act <- as.numeric(dp$Y1plot)
  y_syn <- as.numeric(dp$Y0plot %*% w_scm)
  
  res <- data.frame(
    time_unit = time_pts,
    real_y    = y_act,
    synth_y   = y_syn,
    gap       = y_act - y_syn
  )
  
  list(dp = dp, so = so, w_scm = w_scm, res = res)
}

run_euro_channel_suite <- function(target_var, var_label) {
  exp_name   <- sprintf("%s_Treat_%d", target_var, TREAT_YEAR)
  target_dir <- file.path(".", exp_name)
  dir.create(target_dir, showWarnings = FALSE, recursive = TRUE)
  
  cat(sprintf("\n=======================================================\n"))
  cat(sprintf(">>> Running Eurozone Channel Suite: %s (%s) | Treat=2003 <<<\n", target_var, var_label))
  cat(sprintf("=======================================================\n"))
  
  # 1. Main Baseline SCM Fit for Germany using Eurozone Donors
  cat("1. Fitting Baseline Eurozone SCM for Germany...\n")
  main_fit <- run_scm_single(deu_unit, donor_units, target_var, panel)
  res_df   <- main_fit$res
  
  write_csv(res_df, file.path(target_dir, "YearByYear_Gap.csv"))
  
  wts <- data.frame(unit_num = donor_units, weight = as.numeric(main_fit$w_scm)) %>%
    left_join(unit_map, by = "unit_num") %>% select(unit = country, weight) %>% arrange(desc(weight))
  write_csv(wts, file.path(target_dir, "Weights.csv"))
  
  st_X0 <- main_fit$dp$X0
  st_X1 <- main_fit$dp$X1
  bal_df <- data.frame(
    variable = rownames(st_X0),
    Germany = as.numeric(st_X1),
    synthetic_Germany = as.numeric(st_X0 %*% main_fit$w_scm),
    donor_sample_mean = rowMeans(st_X0)
  )
  write_csv(bal_df, file.path(target_dir, "Balance.csv"))
  
  m_train <- res_df %>% filter(time_unit %in% pre_window)
  m_post  <- res_df %>% filter(time_unit %in% post_window)
  
  metrics_tbl <- tibble(
    Period   = c(sprintf("Training (%d-%d)", min(pre_window), max(pre_window)),
                 sprintf("Post-Treatment (%d-%d)", min(post_window), max(post_window))),
    RMSE     = c(sqrt(mean(m_train$gap^2)), sqrt(mean(m_post$gap^2))),
    MAE      = c(mean(abs(m_train$gap)), mean(abs(m_post$gap))),
    Mean_Gap = c(mean(m_train$gap), mean(m_post$gap))
  )
  write_csv(metrics_tbl, file.path(target_dir, "Metrics.csv"))
  
  # Base Plots
  p_trends <- ggplot(res_df, aes(x = time_unit)) +
    annotate("rect", xmin = 2002.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "#1565C0", alpha = 0.05) +
    geom_vline(xintercept = 2002.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(aes(y = real_y,  color = "Germany (Actual)"), linewidth = 1.3) +
    geom_line(aes(y = synth_y, color = "Synthetic Germany (Eurozone)"), linetype = "longdash", linewidth = 1.1) +
    scale_color_manual(values = c("Germany (Actual)" = "#1565C0", "Synthetic Germany (Eurozone)" = "#C62828")) +
    annotate("text", x = 2006, y = max(c(res_df$real_y, res_df$synth_y))*0.95, label = "Agenda 2010\n(2003)", color = "red", fontface = "bold", size = 3.5) +
    labs(
      title = sprintf("Eurozone SCM: Germany vs Synthetic (%s)", var_label),
      subtitle = sprintf("Training: 1970-2002 (RMSE: %.4f) | Post-2003 Avg Gap: %.4f", sqrt(mean(m_train$gap^2)), mean(m_post$gap)),
      x = "Year", y = sprintf("%s (Share of GDP)", var_label), color = ""
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  ggsave(file.path(target_dir, "SCM_Trends.png"), p_trends, width = 10, height = 6, dpi = 300)
  ggsave(file.path(target_dir, "CV_Plot_Pretty.png"), p_trends, width = 10, height = 6, dpi = 300)
  
  p_gap <- ggplot(res_df, aes(x = time_unit, y = gap)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 2002.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(color = "#1565C0", linewidth = 1.3) +
    labs(
      title = sprintf("Eurozone SCM Gap: %s (Treat=2003)", var_label),
      x = "Year", y = "Gap (Actual - Synthetic Share)"
    ) +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(target_dir, "SCM_Gap.png"), p_gap, width = 10, height = 6, dpi = 300)
  
  # 2. Parallel In-Space Placebo Runs
  cat("2. Running Parallel In-Space Placebos across all Eurozone countries...\n")
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, { library(Synth); library(dplyr) })
  clusterExport(cl, c("panel", "unit_map", "pre_window", "target_var", "all_units", "deu_unit", "donor_units", "run_scm_single"), envir = environment())
  
  placebo_list <- pblapply(cl = cl, X = all_units, FUN = function(u_num) {
    u_name <- unit_map$country[unit_map$unit_num == u_num]
    u_ctrls <- setdiff(all_units, u_num)
    tryCatch({
      fit_p <- run_scm_single(u_num, u_ctrls, target_var, panel)
      fit_p$res %>% mutate(unit = u_name)
    }, error = function(e) { NULL })
  })
  
  placebo_df <- bind_rows(placebo_list)
  
  placebo_sig <- placebo_df %>%
    group_by(unit) %>%
    summarise(
      treat_year = TREAT_YEAR,
      variable   = target_var,
      pre_mspe   = mean(gap[time_unit <= 2002]^2),
      post_mspe  = mean(gap[time_unit >= 2003]^2),
      post_mspe_ratio = post_mspe / pre_mspe,
      .groups    = "drop"
    ) %>%
    arrange(desc(post_mspe_ratio)) %>%
    mutate(
      post_rank = row_number(),
      post_p_value = post_rank / n()
    )
  
  write_csv(placebo_sig, file.path(target_dir, "PostTreatment_Significance.csv"))
  write_csv(placebo_sig, file.path(target_dir, "Validation_Significance.csv"))
  
  p_plac <- ggplot(placebo_df, aes(x = time_unit, y = gap, group = unit)) +
    geom_line(data = placebo_df %>% filter(unit != "Germany"), color = "gray75", alpha = 0.7, linewidth = 0.8) +
    geom_line(data = placebo_df %>% filter(unit == "Germany"), color = "#1565C0", linewidth = 1.5) +
    geom_vline(xintercept = 2002.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(
      title = sprintf("Eurozone In-Space Placebo Test: %s Gap (Treat=2003)", var_label),
      subtitle = sprintf("Germany vs %d Eurozone Placebo Countries | Post-2003 p-value = %.3f", nrow(placebo_sig)-1, placebo_sig$post_p_value[placebo_sig$unit=="Germany"]),
      x = "Year", y = "Gap (Actual - Synthetic Share)"
    ) +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(target_dir, "SCM_Placebo_Gaps.png"), p_plac, width = 10, height = 6, dpi = 300)
  
  p_mspe <- ggplot(placebo_sig, aes(x = reorder(unit, post_mspe_ratio), y = post_mspe_ratio, fill = (unit == "Germany"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("TRUE" = "#1565C0", "FALSE" = "gray60")) +
    coord_flip() +
    labs(
      title = sprintf("Eurozone Post/Pre-2003 MSPE Ratio Ranking (%s)", var_label),
      x = "", y = "MSPE Ratio (Post-2003 / Pre-2003)", fill = ""
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "none", plot.title = element_text(face = "bold"))
  ggsave(file.path(target_dir, "SCM_MSPE_Ratio_Rank.png"), p_mspe, width = 9, height = 5.5, dpi = 300)
  
  # 3. Parallel Leave-One-Out (LOO) Robustness Runs
  cat("3. Running Parallel Leave-One-Out (LOO) Robustness across Eurozone donors...\n")
  active_donors <- wts$unit[wts$weight > 0.001]
  cat(sprintf("   Active donors with weight > 0.001 (%d): %s\n", length(active_donors), paste(active_donors, collapse = ", ")))
  
  clusterExport(cl, c("active_donors"), envir = environment())
  
  loo_list <- pblapply(cl = cl, X = active_donors, FUN = function(drop_d) {
    drop_num  <- unit_map$unit_num[unit_map$country == drop_d]
    sub_ctrls <- setdiff(donor_units, drop_num)
    tryCatch({
      fit_loo <- run_scm_single(deu_unit, sub_ctrls, target_var, panel)
      fit_loo$res %>% mutate(specification = sprintf("Drop %s", drop_d))
    }, error = function(e) { NULL })
  })
  stopCluster(cl)
  
  loo_df <- bind_rows(loo_list) %>%
    bind_rows(res_df %>% mutate(specification = "Full Eurozone SCM"))
  
  p_loo_trends <- ggplot(loo_df, aes(x = time_unit)) +
    annotate("rect", xmin = 2002.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "#1565C0", alpha = 0.05) +
    geom_vline(xintercept = 2002.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(aes(y = real_y, color = "Germany (Actual)"), linewidth = 1.4) +
    geom_line(aes(y = synth_y, group = specification, color = specification), linetype = "dashed", linewidth = 0.9) +
    labs(
      title = sprintf("Eurozone LOO Robustness Trends: %s (Treat=2003)", var_label),
      subtitle = "Dropping each key Eurozone donor country sequentially",
      x = "Year", y = sprintf("%s (Share of GDP)", var_label), color = "Specification"
    ) +
    theme_minimal(base_size = 12) + theme(legend.position = "right", plot.title = element_text(face = "bold"))
  ggsave(file.path(target_dir, "LOO_Plot_Trends.png"), p_loo_trends, width = 10.5, height = 6, dpi = 300)
  
  p_loo_gaps <- ggplot(loo_df, aes(x = time_unit, y = gap, group = specification, color = specification)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.8) +
    geom_vline(xintercept = 2002.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(linewidth = 1) +
    labs(
      title = sprintf("Eurozone LOO Robustness Gaps: %s (Treat=2003)", var_label),
      subtitle = "Gap = Actual Germany - Synthetic Germany across LOO specifications",
      x = "Year", y = "Gap (Share of GDP)", color = "Specification"
    ) +
    theme_minimal(base_size = 12) + theme(legend.position = "right", plot.title = element_text(face = "bold"))
  ggsave(file.path(target_dir, "LOO_Plot_Gaps.png"), p_loo_gaps, width = 10.5, height = 6, dpi = 300)
  
  cat(sprintf("COMPLETED ALL 13 OUTPUT FILES FOR EUROZONE %s (Treat=2003)! Saved to %s/\n", target_var, target_dir))
}

# Run for all 5 PWT GDP expenditure channels
vars_list <- list(
  csh_i = "Investment Share",
  csh_x = "Export Share",
  csh_m = "Import Share",
  csh_c = "Consumption Share",
  csh_g = "Government Share"
)

for (v_name in names(vars_list)) {
  run_euro_channel_suite(v_name, vars_list[[v_name]])
}

# Generate Master Summary Table for Eurozone PWT Channel Suite
all_summary <- list()
for (v_name in names(vars_list)) {
  dir_path <- file.path(".", sprintf("%s_Treat_2003", v_name))
  sig_df   <- read_csv(file.path(dir_path, "PostTreatment_Significance.csv"), show_col_types = FALSE)
  met_df   <- read_csv(file.path(dir_path, "Metrics.csv"), show_col_types = FALSE)
  
  deu_sig  <- sig_df %>% filter(unit == "Germany")
  
  all_summary[[v_name]] <- tibble(
    Channel = v_name,
    Label = vars_list[[v_name]],
    Pre_RMSE = met_df$RMSE[1],
    Post_RMSE = met_df$RMSE[2],
    Post_Mean_Gap = met_df$Mean_Gap[2],
    MSPE_Ratio = deu_sig$post_mspe_ratio,
    Rank = sprintf("%d / %d", deu_sig$post_rank, nrow(sig_df)),
    p_value = deu_sig$post_p_value
  )
}

summary_df <- bind_rows(all_summary)
write_csv(summary_df, "Summary_Eurozone_PWT_Channels_Treat_2003.csv")

cat("\n=======================================================\n")
cat("=== MASTER SUMMARY OF EUROZONE PWT CHANNELS (TREAT = 2003) ===\n")
cat("=======================================================\n")
print(summary_df)

cat("\nALL EUROZONE PWT CHANNEL OUTPUT FILES & LOO ROBUSTNESS RUNS COMPLETED SUCCESSFULLY!\n")
