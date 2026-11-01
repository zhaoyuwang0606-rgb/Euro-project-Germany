# =========================================================================
# 07_China_WTO_Trade_Suite/run_eu_china_wto_trade_scm.R
# Log-Transformed Standard SCM (EU Donors, Treat Year = 2001, No 5% constraint)
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(Synth)
  library(tidysynth)
  library(pwt10)
  library(ggplot2)
  library(parallel)
  library(pbapply)
})

num_cores <- max(1, detectCores() - 1)
cat(sprintf("=== Running Log-Transformed Standard SCM (EU Donors, Treat=2001) (Cores: %d) ===\n", num_cores))

treated_name <- "Germany"
donor_list <- c("Italy", "Portugal", "Austria", "Spain", "Netherlands", "Finland", "France")
all_countries <- c(treated_name, donor_list)
iso_map <- c("DEU", "ITA", "PRT", "AUT", "ESP", "NLD", "FIN", "FRA")

# 1. Load PWT Macro predictors locally
cat("Loading PWT Macro data...\n")
data("pwt10.01")
pwt_sub <- pwt10.01 %>%
  filter(isocode %in% iso_map, year >= 1980, year <= 2019) %>%
  mutate(
    country = case_when(
      isocode == "DEU" ~ "Germany",     isocode == "ITA" ~ "Italy",
      isocode == "PRT" ~ "Portugal",    isocode == "AUT" ~ "Austria",
      isocode == "ESP" ~ "Spain",       isocode == "NLD" ~ "Netherlands",
      isocode == "FIN" ~ "Finland",     isocode == "FRA" ~ "France"
    ),
    nom_gdp_usd_mn = (cgdpo * pop) / 1000,
    pop_mn = pop,
    nom_gdppc_usd = rgdpna / pop,
    log_nom_gdppc = log(rgdpna / pop)
  ) %>%
  filter(!is.na(country)) %>%
  select(country, year, nom_gdp_usd_mn, pop_mn, nom_gdppc_usd, log_nom_gdppc)

# 2. Load IMF DOTS Trade Data
cat("Loading trade datasets...\n")
eu_fpath <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/07_China_WTO_Trade_Suite/EU biliteral with china.csv"
donor_fpath <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/07_China_WTO_Trade_Suite/Biliteral trade with China.csv"

eu_raw <- read_csv(eu_fpath, show_col_types = FALSE)
donor_raw <- read_csv(donor_fpath, show_col_types = FALSE)

clean_trade_data <- function(df, country_list) {
  df %>%
    filter(grepl("China, People", COUNTERPART_COUNTRY)) %>%
    mutate(
      country = case_when(
        COUNTRY == "Türkiye, Republic of" ~ "Turkey",
        COUNTRY == "Netherlands, The"     ~ "Netherlands",
        TRUE ~ COUNTRY
      ),
      flow = ifelse(grepl("Export", INDICATOR), "exp", "imp")
    ) %>%
    filter(country %in% country_list, TIME_PERIOD >= 1980, TIME_PERIOD <= 2019) %>%
    group_by(country, year = TIME_PERIOD, flow) %>%
    summarise(value_mn = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = flow, values_from = value_mn, values_fill = list(value_mn = 0)) %>%
    mutate(total_mn = exp + imp)
}

trade_eu  <- clean_trade_data(eu_raw, donor_list)
trade_deu <- clean_trade_data(donor_raw, "Germany")
trade_all <- bind_rows(trade_eu, trade_deu)

panel <- expand.grid(country = all_countries, year = 1980:2019, stringsAsFactors = FALSE) %>%
  left_join(trade_all, by = c("country", "year")) %>%
  left_join(pwt_sub, by = c("country", "year")) %>%
  arrange(country, year) %>%
  mutate(
    exp = ifelse(is.na(exp), 0, exp),
    imp = ifelse(is.na(imp), 0, imp),
    total_mn = ifelse(is.na(total_mn), 0, total_mn),
    exp_pct_gdp       = (exp / nom_gdp_usd_mn) * 100,
    imp_pct_gdp       = (imp / nom_gdp_usd_mn) * 100,
    trade_pct_gdp     = (total_mn / nom_gdp_usd_mn) * 100,
    log_exp_pct_gdp   = log(exp_pct_gdp + 1e-4),
    log_imp_pct_gdp   = log(imp_pct_gdp + 1e-4),
    log_trade_pct_gdp = log(trade_pct_gdp + 1e-4)
  )

unit_map <- data.frame(
  country  = sort(unique(panel$country)),
  unit_num = seq_along(sort(unique(panel$country)))
)
panel <- left_join(panel, unit_map, by = "country")

TREAT_YEAR <- 2001
train_yrs  <- 1980:2001
post_yrs   <- 2002:2019

run_log_standard_scm_suite <- function(dep_var, outcome_label, out_subdir) {
  cat(sprintf("\n=========================================="))
  cat(sprintf("\nRunning Log-Transformed Standard SCM for: %s", outcome_label))
  cat(sprintf("\n==========================================\n"))
  
  dir.create(out_subdir, showWarnings = FALSE, recursive = TRUE)
  
  tr_unit <- unit_map$unit_num[unit_map$country == "Germany"]
  ctrl_units <- unit_map$unit_num[unit_map$country %in% donor_list]
  
  phase_lags <- list(
    list(dep_var, 1980:1987, "mean"),
    list(dep_var, 1988:1994, "mean"),
    list(dep_var, 1995:2001, "mean")
  )
  
  run_scm_single <- function(t_unit, c_units, p_df) {
    dp <- dataprep(
      foo                   = as.data.frame(p_df),
      predictors            = c("log_nom_gdppc", "pop_mn", "nom_gdp_usd_mn"),
      predictors.op         = "mean",
      time.predictors.prior = train_yrs,
      special.predictors    = phase_lags,
      dependent             = dep_var,
      unit.variable         = "unit_num",
      unit.names.variable   = "country",
      time.variable         = "year",
      treatment.identifier  = t_unit,
      controls.identifier   = c_units,
      time.optimize.ssr     = train_yrs,
      time.plot             = 1980:2019
    )
    
    so <- synth(dp, method = "BFGS")
    w_scm <- so$solution.w
    
    y_act_log <- as.numeric(dp$Y1plot)
    y_syn_log <- as.numeric(dp$Y0plot %*% w_scm)
    
    res <- data.frame(
      year        = 1980:2019,
      actual_log  = y_act_log,
      synth_log   = y_syn_log,
      actual      = exp(y_act_log),
      synth       = exp(y_syn_log)
    ) %>% mutate(
      gap_log     = actual_log - synth_log,
      gap         = actual - synth
    )
    
    list(dp = dp, so = so, w_scm = w_scm, res = res)
  }
  
  # 1. Main Fit (Log Standard SCM)
  main_fit <- run_scm_single(tr_unit, ctrl_units, panel)
  res_df <- main_fit$res
  
  write_csv(res_df, file.path(out_subdir, "Baseline_YearByYear_Gap.csv"))
  
  wts <- data.frame(unit_num = ctrl_units) %>%
    left_join(unit_map, by = "unit_num") %>%
    mutate(weight = as.numeric(main_fit$w_scm)) %>%
    select(unit = country, weight) %>%
    arrange(desc(weight))
  write_csv(wts, file.path(out_subdir, "Weights.csv"))
  write_csv(wts, file.path(out_subdir, "SCM_Weights.csv"))
  
  # Balance table
  st_X0 <- main_fit$dp$X0
  st_X1 <- main_fit$dp$X1
  bal_df <- data.frame(
    variable = rownames(st_X0),
    Germany = as.numeric(st_X1),
    synthetic_Germany = as.numeric(st_X0 %*% main_fit$w_scm),
    donor_sample_mean = rowMeans(st_X0)
  )
  write_csv(bal_df, file.path(out_subdir, "SCM_Balance.csv"))
  write_csv(bal_df, file.path(out_subdir, "Balance.csv"))
  
  # 2. Run tidysynth for comparison
  tryCatch({
    tidysynth_prep <- panel %>%
      filter(country %in% c("Germany", donor_list)) %>%
      synthetic_control(
        outcome = !!sym(dep_var), unit = country, time = year,
        i_unit = "Germany", i_time = 2001, generate_placebos = FALSE
      ) %>%
      generate_predictor(time_window = train_yrs,
                         log_nom_gdppc = mean(log_nom_gdppc, na.rm=TRUE),
                         pop_mn = mean(pop_mn, na.rm=TRUE),
                         nom_gdp_usd_mn = mean(nom_gdp_usd_mn, na.rm=TRUE)) %>%
      generate_predictor(time_window = 1980:1987, Lag_1980_1987 = mean(!!sym(dep_var), na.rm=TRUE)) %>%
      generate_predictor(time_window = 1988:1994, Lag_1988_1994 = mean(!!sym(dep_var), na.rm=TRUE)) %>%
      generate_predictor(time_window = 1995:2001, Lag_1995_2001 = mean(!!sym(dep_var), na.rm=TRUE)) %>%
      generate_weights(optimization_window = train_yrs) %>%
      generate_control()
    
    ts_wts <- tidysynth_prep %>% grab_unit_weights()
    write_csv(ts_wts, file.path(out_subdir, "Tidysynth_Weights.csv"))
    
    ts_gaps <- tidysynth_prep %>% grab_synthetic_control()
    write_csv(ts_gaps, file.path(out_subdir, "Tidysynth_YearByYear_Gap.csv"))
  }, error = function(e) {
    cat(sprintf("  tidysynth note: %s\n", e$message))
  })
  
  # MSPE Metrics (on Log scale)
  pre_rmspe  <- sqrt(mean(res_df$gap_log[res_df$year <= 2001]^2))
  post_mspe  <- mean(res_df$gap_log[res_df$year >= 2002]^2)
  post_ratio <- post_mspe / (pre_rmspe^2)
  
  # Trend Plot (Log Scale)
  p_trends_log <- ggplot(res_df, aes(x = year)) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(aes(y = actual_log, color = "Germany (Actual, Log)"), linewidth = 1.4) +
    geom_line(aes(y = synth_log,  color = "Synthetic Germany (Log SCM)"), linetype = "longdash", linewidth = 1.2) +
    scale_color_manual(values = c("Germany (Actual, Log)" = "#1565C0", "Synthetic Germany (Log SCM)" = "#C62828")) +
    annotate("text", x = 2004, y = max(res_df$actual_log)*0.95, label = "China WTO\n(2001)", color = "red", fontface = "bold", size = 3.5) +
    labs(title = sprintf("Log-Transformed SCM (Treat=2001): %s", outcome_label),
         subtitle = sprintf("Training: 1980-2001 (Pre-Log-RMSPE: %.4f) | Post/Pre MSPE Ratio: %.2f", pre_rmspe, post_ratio),
         x = "Year", y = paste0("Log(", outcome_label, ")"), color = "") +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_Trends.png"), p_trends_log, width = 10, height = 6, dpi = 300)
  
  # 3. Parallel In-Space Placebos & LOO
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, { library(Synth); library(dplyr) })
  clusterExport(cl, c("panel", "unit_map", "train_yrs", "dep_var", "tr_unit", "ctrl_units", "phase_lags", "run_scm_single"), envir = environment())
  
  placebo_list <- pblapply(cl = cl, X = ctrl_units, FUN = function(d_num) {
    d_name <- unit_map$country[unit_map$unit_num == d_num]
    d_ctrls <- setdiff(ctrl_units, d_num)
    tryCatch({
      fit_p <- run_scm_single(d_num, d_ctrls, panel)
      data.frame(year = 1980:2019, country = d_name, gap_log = fit_p$res$gap_log)
    }, error = function(e) NULL)
  })
  
  placebo_df <- bind_rows(placebo_list) %>%
    bind_rows(data.frame(year = 1980:2019, country = "Germany", gap_log = res_df$gap_log))
  
  mspe_calc <- placebo_df %>%
    group_by(country) %>%
    summarise(
      pre_mspe  = mean(gap_log[year <= 2001]^2),
      post_mspe = mean(gap_log[year >= 2002]^2),
      post_ratio= post_mspe / pre_mspe,
      .groups   = "drop"
    ) %>% arrange(desc(post_ratio))
  
  write_csv(mspe_calc, file.path(out_subdir, "SCM_Significance.csv"))
  deu_rank <- which(mspe_calc$country == "Germany")
  p_val <- deu_rank / nrow(mspe_calc)
  
  eff_summary <- data.frame(
    Pre_RMSPE = pre_rmspe,
    Post_RMSPE = sqrt(mean(res_df$gap_log[res_df$year >= 2002]^2)),
    Post_MSPE_Ratio = post_ratio,
    Rank = sprintf("%d / %d", deu_rank, nrow(mspe_calc)),
    p_value = p_val
  )
  write_csv(eff_summary, file.path(out_subdir, "PostTreatment_Significance.csv"))
  
  # Plot Placebo Gaps
  p_plac <- ggplot(placebo_df, aes(x = year, y = gap_log, group = country)) +
    geom_line(data = placebo_df %>% filter(country != "Germany"), color = "gray70", alpha = 0.7, linewidth = 0.8) +
    geom_line(data = placebo_df %>% filter(country == "Germany"), color = "#1565C0", linewidth = 1.5) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = sprintf("In-Space Placebo Test (Log Scale, Treat=2001): %s", outcome_label),
         subtitle = sprintf("Training 1980-2001 | Post-WTO p-value = %.3f", p_val),
         x = "Year", y = "Log Gap (Actual - Synthetic)") +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_Placebo.png"), p_plac, width = 10, height = 6, dpi = 300)
  
  p_mspe <- ggplot(mspe_calc, aes(x = reorder(country, post_ratio), y = post_ratio, fill = (country == "Germany"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("TRUE" = "#1565C0", "FALSE" = "gray60")) +
    coord_flip() +
    labs(title = sprintf("Post/Pre-WTO MSPE Ratio Ranking (Log Scale %s)", outcome_label),
         x = "", y = "Log MSPE Ratio (Post-2001 / Pre-2001)", fill = "") +
    theme_minimal(base_size = 13) + theme(legend.position = "none", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_MSPE_Ratio.png"), p_mspe, width = 9, height = 5.5, dpi = 300)
  
  # LOO
  active_donors <- wts$unit[wts$weight > 0.001]
  clusterExport(cl, c("active_donors"), envir = environment())
  
  loo_list <- pblapply(cl = cl, X = active_donors, FUN = function(drop_d) {
    drop_num <- unit_map$unit_num[unit_map$country == drop_d]
    sub_ctrls <- setdiff(ctrl_units, drop_num)
    tryCatch({
      fit_loo <- run_scm_single(tr_unit, sub_ctrls, panel)
      data.frame(year = 1980:2019, specification = sprintf("Drop %s", drop_d), synth_value = fit_loo$res$synth_log)
    }, error = function(e) NULL)
  })
  stopCluster(cl)
  
  loo_df <- bind_rows(loo_list) %>%
    bind_rows(data.frame(year = 1980:2019, specification = "Full Log SCM", synth_value = res_df$synth_log)) %>%
    mutate(actual = rep(res_df$actual_log, length(unique(specification))))
  
  p_loo <- ggplot(loo_df, aes(x = year)) +
    geom_line(aes(y = actual, color = "Germany (Actual, Log)"), linewidth = 1.4) +
    geom_line(aes(y = synth_value, group = specification, color = specification), linetype = "dashed", linewidth = 0.9) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
    labs(title = sprintf("Leave-One-Out Robustness (Log SCM, Treat=2001): %s", outcome_label),
         x = "Year", y = paste0("Log(", outcome_label, ")"), color = "Specification") +
    theme_minimal(base_size = 12) + theme(legend.position = "right", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_LOO.png"), p_loo, width = 10.5, height = 6, dpi = 300)
  
  cat(sprintf("Completed Log-Transformed SCM EU suite for %s. Outputs saved to %s/\n", outcome_label, out_subdir))
}

base_dir <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/07_China_WTO_Trade_Suite"
eu_out_dir <- file.path(base_dir, "EU_Results")

run_log_standard_scm_suite("log_exp_pct_gdp",   "Exports to China (% Nominal GDP)",   file.path(eu_out_dir, "exp_pct_gdp"))
run_log_standard_scm_suite("log_imp_pct_gdp",   "Imports from China (% Nominal GDP)", file.path(eu_out_dir, "imp_pct_gdp"))
run_log_standard_scm_suite("log_trade_pct_gdp", "Total Trade with China (% Nominal GDP)", file.path(eu_out_dir, "trade_pct_gdp"))

cat("\n=== ALL LOG-TRANSFORMED STANDARD SCM EU SUITES (Treat=2001, No 5%% Constraint) COMPLETED! ===\n")
