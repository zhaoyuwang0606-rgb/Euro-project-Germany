# =========================================================================
# SCM_Germany_Custom/run_synth_custom.R
# Synth package implementation for SCM_Germany_Custom
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(Synth)
  library(ggplot2)
  library(parallel)
})

num_cores <- max(1, detectCores() - 1)
cat(sprintf("=== SCM_Germany_Custom: Synth rewrite (Cores: %d) ===\n", num_cores))

# Paths
data_path  <- "../mpd2023_web.xlsx"
custom_dir <- "."

cat("Reading Maddison data...\n")
raw_data <- read_excel(data_path, sheet = "Full data")

# Donor Pools
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

# Data prep
prepare_data <- function(start_year, end_year) {
  raw_data %>%
    filter(country %in% c(treated_country, all_donors_full)) %>%
    filter(!country %in% spillover_and_japan) %>%
    filter(year >= start_year & year <= end_year) %>%
    select(country, year, gdppc, pop) %>%
    mutate(gdp_size = gdppc * pop) %>%
    drop_na() %>%
    group_by(country) %>%
    filter(n() == length(start_year:end_year)) %>%
    ungroup()
}

# Core Synth runner
run_synth_core <- function(df, treat_year, start_year, tr_unit_num, ctrl_unit_nums, unit_map) {
  pre_window <- start_year:(treat_year - 1)
  dp <- dataprep(
    foo                   = as.data.frame(df),
    predictors            = c("gdppc", "gdp_size", "pop"),
    predictors.op         = "mean",
    time.predictors.prior = pre_window,
    special.predictors    = list(
      list("gdppc", start_year,       "identity"),
      list("gdppc", treat_year - 1,   "identity")
    ),
    dependent             = "gdppc",
    unit.variable         = "unit_num",
    unit.names.variable   = "country",
    time.variable         = "year",
    treatment.identifier  = tr_unit_num,
    controls.identifier   = ctrl_unit_nums,
    time.optimize.ssr     = pre_window,
    time.plot             = min(df$year):max(df$year)
  )
  so <- synth(dp, method = "BFGS")
  
  y_act <- as.numeric(dp$Y1plot)
  y_syn <- as.numeric(dp$Y0plot %*% so$solution.w)
  yrs   <- as.numeric(rownames(dp$Y1plot))
  
  res <- data.frame(year = yrs, actual = y_act, synth = y_syn) %>%
    mutate(gap = actual - synth, pct_gap = (gap / synth) * 100)
  
  wts <- data.frame(unit_num = ctrl_unit_nums) %>%
    left_join(unit_map, by = "unit_num") %>%
    mutate(weight = as.numeric(so$solution.w)) %>%
    select(unit = country, weight) %>%
    arrange(desc(weight))
  
  bal <- data.frame(
    variable          = rownames(dp$X0),
    Germany           = as.numeric(dp$X1),
    synthetic_Germany = as.numeric(dp$X0 %*% so$solution.w),
    donor_sample_mean = rowMeans(dp$X0)
  )
  
  list(dp = dp, so = so, res = res, wts = wts, bal = bal)
}

run_full_spec_synth <- function(folder_name, start_year, treat_year, end_year) {
  out_dir <- file.path(custom_dir, paste0(folder_name, "_Synth"))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  cat(sprintf("\n--- Synth Run: %s -> %s ---\n", folder_name, out_dir))
  
  df <- prepare_data(start_year, end_year)
  u_map <- data.frame(country = sort(unique(df$country)), unit_num = seq_along(unique(df$country)))
  df    <- left_join(df, u_map, by = "country")
  
  tr_u   <- u_map$unit_num[u_map$country == treated_country]
  ctrl_u <- u_map$unit_num[u_map$country != treated_country]
  
  fit_base <- tryCatch(
    run_synth_core(df, treat_year, start_year, tr_u, ctrl_u, u_map),
    error = function(e) { cat("  Base error:", e$message, "\n"); NULL }
  )
  if (is.null(fit_base)) return(NULL)
  
  # Write baseline files
  write_csv(fit_base$wts, file.path(out_dir, "SCM_Weights.csv"))
  write_csv(fit_base$bal, file.path(out_dir, "SCM_Balance.csv"))
  write_csv(
    fit_base$res %>% select(time_unit = year, real_y = actual, synth_y = synth, gap, pct_gap),
    file.path(out_dir, "Baseline_YearByYear_Gap.csv")
  )
  
  # Trend plot
  p_tr <- ggplot(fit_base$res, aes(x = year)) +
    geom_vline(xintercept = treat_year, linetype = "dashed", color = "red", linewidth = 0.9) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.3) +
    geom_line(aes(y = synth,  color = "Synthetic Germany"), linetype = "longdash", linewidth = 1.1) +
    scale_color_manual(values = c("Germany (Actual)" = "#1565C0", "Synthetic Germany" = "#C62828")) +
    labs(title = sprintf("Synth SCM: %s", folder_name), x = "Year", y = "GDP per capita", color = "") +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, "SCM_Trends.png"), p_tr, width = 8, height = 5.5, dpi = 300)
  
  # Parallel Placebos
  cat("  Running In-Space Placebos...\n")
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, { library(Synth); library(dplyr) })
  clusterExport(cl, c("df", "treat_year", "start_year", "u_map", "run_synth_core"), envir = environment())
  
  placebo_res <- parLapply(cl, ctrl_u, function(c_u) {
    c_name  <- u_map$country[u_map$unit_num == c_u]
    other_u <- setdiff(u_map$unit_num, c_u)
    tryCatch({
      fit_p <- run_synth_core(df, treat_year, start_year, c_u, other_u, u_map)
      data.frame(year = fit_p$res$year, country = c_name, gap = fit_p$res$gap)
    }, error = function(e) NULL)
  })
  stopCluster(cl)
  
  plac_df <- bind_rows(placebo_res) %>%
    bind_rows(data.frame(year = fit_base$res$year, country = "Germany", gap = fit_base$res$gap))
  
  mspe_df <- plac_df %>%
    group_by(country) %>%
    summarise(
      pre_mspe  = mean(gap[year < treat_year]^2),
      post_mspe = mean(gap[year >= treat_year]^2),
      post_ratio= post_mspe / pre_mspe,
      .groups   = "drop"
    ) %>% arrange(desc(post_ratio))
  
  write_csv(mspe_df, file.path(out_dir, "SCM_Significance.csv"))
  
  p_val <- which(mspe_df$country == "Germany") / nrow(mspe_df)
  p_plac <- ggplot(plac_df, aes(x = year, y = gap, group = country)) +
    geom_line(data = plac_df %>% filter(country != "Germany"), color = "gray70", alpha = 0.7, linewidth = 0.7) +
    geom_line(data = plac_df %>% filter(country == "Germany"), color = "#1565C0", linewidth = 1.4) +
    geom_vline(xintercept = treat_year, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = sprintf("Synth Placebo Test: %s (p = %.3f)", folder_name, p_val), x = "Year", y = "Gap") +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, "SCM_Placebo.png"), p_plac, width = 8, height = 5.5, dpi = 300)
  
  p_mspe <- ggplot(mspe_df, aes(x = reorder(country, post_ratio), y = post_ratio, fill = (country == "Germany"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("TRUE" = "#1565C0", "FALSE" = "gray60")) +
    coord_flip() +
    labs(title = sprintf("Synth MSPE Ratio: %s", folder_name), x = "", y = "Post/Pre MSPE Ratio", fill = "") +
    theme_minimal(base_size = 12) + theme(legend.position = "none", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, "SCM_MSPE_Ratio.png"), p_mspe, width = 8, height = 5.5, dpi = 300)
  
  # LOO
  cat("  Running LOO...\n")
  active_donors <- fit_base$wts %>% filter(weight > 0.001) %>% pull(unit)
  
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, { library(Synth); library(dplyr) })
  clusterExport(cl, c("df", "treat_year", "start_year", "tr_u", "ctrl_u", "u_map", "run_synth_core"), envir = environment())
  
  loo_res <- parLapply(cl, active_donors, function(d_name) {
    d_u    <- u_map$unit_num[u_map$country == d_name]
    sub_ctrl <- setdiff(ctrl_u, d_u)
    tryCatch({
      fit_l <- run_synth_core(df, treat_year, start_year, tr_u, sub_ctrl, u_map)
      data.frame(year = fit_l$res$year, specification = paste("Drop", d_name), synth_value = fit_l$res$synth)
    }, error = function(e) NULL)
  })
  stopCluster(cl)
  
  loo_df <- bind_rows(loo_res) %>%
    bind_rows(data.frame(year = fit_base$res$year, specification = "Full Synth", synth_value = fit_base$res$synth)) %>%
    mutate(actual = rep(fit_base$res$actual, length(unique(specification))))
  
  p_loo <- ggplot(loo_df, aes(x = year)) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.3) +
    geom_line(aes(y = synth_value, group = specification, color = specification), linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = treat_year, linetype = "dashed", color = "red") +
    labs(title = sprintf("Synth LOO: %s", folder_name), x = "Year", y = "GDP per capita", color = "Specification") +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, "SCM_LOO.png"), p_loo, width = 9.5, height = 5.5, dpi = 300)
  
  # Aggregated Treatment Effect
  post_g <- fit_base$res %>% filter(year >= treat_year)
  pre_g  <- fit_base$res %>% filter(year < treat_year)
  
  agg_base <- tibble(
    Specification  = "Baseline",
    Pre_Avg_Gap    = mean(pre_g$gap),
    Post_Avg_Gap   = mean(post_g$gap),
    Post_Cumul_Gap = sum(post_g$gap),
    Post_Avg_Pct   = mean(post_g$pct_gap),
    Post_N_Years   = nrow(post_g)
  )
  write_csv(agg_base, file.path(out_dir, "Aggregated_Treatment_Effect.csv"))
  
  cat(sprintf("  Completed %s!\n", folder_name))
}

# Run all 6 specs
run_full_spec_synth("noneu_1990_2022", 1990, 1999, 2022)
run_full_spec_synth("noneu_1990_2009", 1990, 1999, 2009)
run_full_spec_synth("noneu_1990_2007", 1990, 1999, 2007)
run_full_spec_synth("noneu_1970_2022", 1970, 1999, 2022)
run_full_spec_synth("noneu_1970_2009", 1970, 1999, 2009)
run_full_spec_synth("noneu_1970_2007", 1970, 1999, 2007)

cat("\n=== All SCM_Germany_Custom Synth runs completed! ===\n")
