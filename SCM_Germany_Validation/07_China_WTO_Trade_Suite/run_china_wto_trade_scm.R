# =========================================================================
# 07_China_WTO_Trade_Suite/run_china_wto_trade_scm.R
# Standard SCM (Non-EU Donors) for China WTO Trade Suite
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(Synth)
  library(WDI)
  library(ggplot2)
  library(parallel)
  library(pbapply)
})

num_cores <- max(1, detectCores() - 1)
cat(sprintf("=== Running Standard SCM (Non-EU Donors) (CPU Cores: %d / %d) ===\n", num_cores, detectCores()))

clean_donors <- c("Australia", "Canada", "Chile", "Japan", "New Zealand", "Singapore", "Turkey", "United States")
all_countries <- c("Germany", clean_donors)
iso_map <- c("DEU", "AUS", "CAN", "CHL", "JPN", "NZL", "SGP", "TUR", "USA")

# Portable path resolution
find_project_root <- function() {
  candidates <- c(
    Sys.getenv("PROJECT_ROOT"),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "../.."),
    file.path(getwd(), "../../..")
  )
  for (cand in candidates) {
    if (nzchar(cand) && file.exists(file.path(cand, "mpd2023_web.xlsx"))) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  return(getwd())
}
proj_root <- find_project_root()

# Robust WDI query helper with retries and cached fallback
fetch_wdi_with_retry <- function(iso_map, indicators, start_yr, end_yr, max_retries = 3) {
  for (i in 1:max_retries) {
    cat(sprintf("Attempt %d of %d to query WDI...\n", i, max_retries))
    res <- tryCatch({
      WDI(country = iso_map, indicator = indicators, start = start_yr, end = end_yr)
    }, error = function(e) {
      NULL
    })
    if (!is.null(res) && is.data.frame(res) && all(indicators %in% colnames(res))) {
      return(res)
    }
    cat("WDI query failed or timed out. Retrying...\n")
    Sys.sleep(1)
  }
  # Fallback to cached WDI_raw.csv if available
  cached_path <- file.path(proj_root, "SCM_Germany_Validation", "Treatment_1999_Trial", "WDI_raw.csv")
  if (file.exists(cached_path)) {
    cat(sprintf("Network WDI unavailable. Using cached WDI data from: %s\n", cached_path))
    cached <- read_csv(cached_path, show_col_types = FALSE)
    return(cached)
  }
  stop("Failed to retrieve WDI data and cached file not found.")
}

# 1. Retrieve WDI nominal GDP and Population
cat("Retrieving World Bank WDI data...\n")
wdi_raw <- fetch_wdi_with_retry(iso_map, c("NY.GDP.MKTP.CD", "SP.POP.TOTL"), 1980, 2019)

wdi_sub <- wdi_raw %>%
  mutate(
    country = case_when(
      iso2c == "DE" ~ "Germany",
      iso2c == "AU" ~ "Australia",
      iso2c == "CA" ~ "Canada",
      iso2c == "CL" ~ "Chile",
      iso2c == "JP" ~ "Japan",
      iso2c == "NZ" ~ "New Zealand",
      iso2c == "SG" ~ "Singapore",
      iso2c == "TR" ~ "Turkey",
      iso2c == "US" ~ "United States"
    ),
    nom_gdp_usd_mn = NY.GDP.MKTP.CD / 1e6,
    pop_mn = SP.POP.TOTL / 1e6,
    nom_gdppc_usd = NY.GDP.MKTP.CD / SP.POP.TOTL
  ) %>%
  filter(!is.na(country)) %>%
  select(country, year, nom_gdp_usd_mn, pop_mn, nom_gdppc_usd)

# 2. Load IMF DOTS Trade Data
cat("Loading trade datasets...\n")
fpath <- file.path(proj_root, "SCM_Germany_Validation", "07_China_WTO_Trade_Suite", "Biliteral trade with China.csv")
dots_raw <- read_csv(fpath, show_col_types = FALSE)

clean_trade_data <- function(df, country_list) {
  df %>%
    filter(grepl("China, People", COUNTERPART_COUNTRY)) %>%
    mutate(
      country = case_when(
        COUNTRY == "Türkiye, Republic of" ~ "Turkey",
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

trade_all <- clean_trade_data(dots_raw, all_countries)

# 3. Create Master Panel
panel <- expand.grid(country = all_countries, year = 1980:2019, stringsAsFactors = FALSE) %>%
  left_join(trade_all, by = c("country", "year")) %>%
  left_join(wdi_sub, by = c("country", "year")) %>%
  arrange(country, year) %>%
  mutate(
    exp = ifelse(is.na(exp), 0, exp),
    imp = ifelse(is.na(imp), 0, imp),
    total_mn = ifelse(is.na(total_mn), 0, total_mn),
    exp_pct_gdp   = (exp / nom_gdp_usd_mn) * 100,
    imp_pct_gdp   = (imp / nom_gdp_usd_mn) * 100,
    trade_pct_gdp = (total_mn / nom_gdp_usd_mn) * 100
  )

unit_map <- data.frame(
  country  = sort(unique(panel$country)),
  unit_num = seq_along(sort(unique(panel$country)))
)
panel <- left_join(panel, unit_map, by = "country")

train_yrs <- 1980:1998
val_yrs   <- 1999:2001
post_yrs  <- 2002:2019

run_standard_scm_suite <- function(dep_var, outcome_label, out_subdir) {
  cat(sprintf("\n=========================================="))
  cat(sprintf("\nRunning Standard SCM Suite for: %s", outcome_label))
  cat(sprintf("\n==========================================\n"))
  
  dir.create(out_subdir, showWarnings = FALSE, recursive = TRUE)
  
  tr_unit <- unit_map$unit_num[unit_map$country == "Germany"]
  ctrl_units <- unit_map$unit_num[unit_map$country %in% clean_donors]
  
  phase_lags <- list(
    list(dep_var, 1980:1986, "mean"),
    list(dep_var, 1987:1992, "mean"),
    list(dep_var, 1993:1998, "mean")
  )
  
  run_scm_single <- function(t_unit, c_units, p_df) {
    dp <- dataprep(
      foo                   = as.data.frame(p_df),
      predictors            = c("nom_gdppc_usd", "pop_mn", "nom_gdp_usd_mn"),
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
    
    y_act <- as.numeric(dp$Y1plot)
    y_syn <- as.numeric(dp$Y0plot %*% w_scm)
    
    res <- data.frame(
      year    = 1980:2019,
      actual  = y_act,
      synth   = y_syn
    ) %>% mutate(
      gap     = actual - synth
    )
    
    list(dp = dp, so = so, w_scm = w_scm, res = res)
  }
  
  # 1. Main Fit
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
  
  # MSPE Metrics
  pre_rmspe  <- sqrt(mean(res_df$gap[res_df$year <= 1998]^2))
  val_mspe   <- mean(res_df$gap[res_df$year >= 1999 & res_df$year <= 2001]^2)
  post_mspe  <- mean(res_df$gap[res_df$year >= 2002]^2)
  val_ratio  <- val_mspe / (pre_rmspe^2)
  post_ratio <- post_mspe / (pre_rmspe^2)
  
  # SCM Level Trends Plot
  p_trends <- ggplot(res_df, aes(x = year)) +
    annotate("rect", xmin = 1998.5, xmax = 2001.5, ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.15) +
    annotate("rect", xmin = 2001.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "#1565C0", alpha = 0.05) +
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "darkorange", linewidth = 0.9) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.4) +
    geom_line(aes(y = synth,  color = "Synthetic Germany"), linetype = "longdash", linewidth = 1.2) +
    scale_color_manual(values = c("Germany (Actual)" = "#1565C0", "Synthetic Germany" = "#C62828")) +
    annotate("text", x = 2000, y = max(res_df$actual)*0.95, label = "Validation\n1999-2001", color = "darkorange", fontface = "bold", size = 3.2) +
    annotate("text", x = 2004, y = max(res_df$actual)*0.95, label = "China WTO\n(2001)", color = "red", fontface = "bold", size = 3.5) +
    labs(title = sprintf("Standard SCM: Germany vs Synthetic (%s)", outcome_label),
         subtitle = sprintf("Training: 1980-1998 (Pre-RMSPE: %.4f) | Validation MSPE Ratio: %.2f", pre_rmspe, val_ratio),
         x = "Year", y = outcome_label, color = "") +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_Trends.png"), p_trends, width = 10, height = 6, dpi = 300)
  
  # 2. Parallel In-Space Placebos & LOO
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, { library(Synth); library(dplyr) })
  clusterExport(cl, c("panel", "unit_map", "train_yrs", "dep_var", "tr_unit", "ctrl_units", "phase_lags", "run_scm_single"), envir = environment())
  
  placebo_list <- pblapply(cl = cl, X = ctrl_units, FUN = function(d_num) {
    d_name <- unit_map$country[unit_map$unit_num == d_num]
    d_ctrls <- setdiff(ctrl_units, d_num)
    tryCatch({
      fit_p <- run_scm_single(d_num, d_ctrls, panel)
      data.frame(year = 1980:2019, country = d_name, gap = fit_p$res$gap)
    }, error = function(e) { NULL })
  })
  
  placebo_df <- bind_rows(placebo_list) %>%
    bind_rows(data.frame(year = 1980:2019, country = "Germany", gap = res_df$gap))
  
  mspe_calc <- placebo_df %>%
    group_by(country) %>%
    summarise(
      pre_mspe  = mean(gap[year <= 1998]^2),
      val_mspe  = mean(gap[year >= 1999 & year <= 2001]^2),
      post_mspe = mean(gap[year >= 2002]^2),
      val_ratio = val_mspe / pre_mspe,
      post_ratio= post_mspe / pre_mspe,
      .groups   = "drop"
    ) %>% arrange(desc(post_ratio))
  
  write_csv(mspe_calc, file.path(out_subdir, "SCM_Significance.csv"))
  deu_rank <- which(mspe_calc$country == "Germany")
  p_val <- deu_rank / nrow(mspe_calc)
  
  eff_summary <- data.frame(
    Pre_RMSPE = pre_rmspe,
    Val_MSPE_Ratio = val_ratio,
    Post_RMSPE = sqrt(mean(res_df$gap[res_df$year >= 2002]^2)),
    Post_MSPE_Ratio = post_ratio,
    Rank = sprintf("%d / %d", deu_rank, nrow(mspe_calc)),
    p_value = p_val
  )
  write_csv(eff_summary, file.path(out_subdir, "PostTreatment_Significance.csv"))
  
  # Plot Placebo Gaps
  p_plac <- ggplot(placebo_df, aes(x = year, y = gap, group = country)) +
    annotate("rect", xmin = 1998.5, xmax = 2001.5, ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.15) +
    geom_line(data = placebo_df %>% filter(country != "Germany"), color = "gray70", alpha = 0.7, linewidth = 0.8) +
    geom_line(data = placebo_df %>% filter(country == "Germany"), color = "#1565C0", linewidth = 1.5) +
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "darkorange") +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = sprintf("In-Space Placebo Test (Standard SCM): %s Gap", outcome_label),
         subtitle = sprintf("Training 1980-1998 | Validation 1999-2001 | Post-WTO p-value = %.3f", p_val),
         x = "Year", y = "Gap (Actual - Synthetic)") +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_Placebo.png"), p_plac, width = 10, height = 6, dpi = 300)
  
  p_mspe <- ggplot(mspe_calc, aes(x = reorder(country, post_ratio), y = post_ratio, fill = (country == "Germany"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("TRUE" = "#1565C0", "FALSE" = "gray60")) +
    coord_flip() +
    labs(title = sprintf("Post/Pre-WTO MSPE Ratio Ranking (Standard SCM %s)", outcome_label),
         x = "", y = "MSPE Ratio (Post-2001 / Pre-1998)", fill = "") +
    theme_minimal(base_size = 13) + theme(legend.position = "none", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_MSPE_Ratio.png"), p_mspe, width = 9, height = 5.5, dpi = 300)
  
  # Parallel LOO
  active_donors <- wts$unit[abs(wts$weight) > 0.001]
  clusterExport(cl, c("active_donors"), envir = environment())
  
  loo_list <- pblapply(cl = cl, X = active_donors, FUN = function(drop_d) {
    drop_num <- unit_map$unit_num[unit_map$country == drop_d]
    sub_ctrls <- setdiff(ctrl_units, drop_num)
    tryCatch({
      fit_loo <- run_scm_single(tr_unit, sub_ctrls, panel)
      data.frame(year = 1980:2019, specification = sprintf("Drop %s", drop_d), synth_value = fit_loo$res$synth)
    }, error = function(e) { NULL })
  })
  stopCluster(cl)
  
  loo_df <- bind_rows(loo_list) %>%
    bind_rows(data.frame(year = 1980:2019, specification = "Full SCM", synth_value = res_df$synth)) %>%
    mutate(actual = rep(res_df$actual, length(unique(specification))))
  
  p_loo <- ggplot(loo_df, aes(x = year)) +
    annotate("rect", xmin = 1998.5, xmax = 2001.5, ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.15) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.4) +
    geom_line(aes(y = synth_value, group = specification, color = specification), linetype = "dashed", linewidth = 0.9) +
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "darkorange") +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
    labs(title = sprintf("Leave-One-Out Robustness (Standard SCM): %s", outcome_label),
         subtitle = "Orange Area = Out-of-Sample Validation Window (1999-2001)",
         x = "Year", y = outcome_label, color = "Specification") +
    theme_minimal(base_size = 12) + theme(legend.position = "right", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_LOO.png"), p_loo, width = 10.5, height = 6, dpi = 300)
  
  cat(sprintf("Completed Standard SCM suite for %s. Outputs saved to %s/\n", outcome_label, out_subdir))
}

base_dir <- Sys.getenv("REPL_CHINA_OUTPUT_DIR")
if (!nzchar(base_dir)) {
  base_dir <- file.path(proj_root, "SCM_Germany_Validation", "07_China_WTO_Trade_Suite")
}
run_standard_scm_suite("exp_pct_gdp",   "Exports to China (% Nominal GDP)",   file.path(base_dir, "exp_pct_gdp"))
run_standard_scm_suite("imp_pct_gdp",   "Imports from China (% Nominal GDP)", file.path(base_dir, "imp_pct_gdp"))
run_standard_scm_suite("trade_pct_gdp", "Total Trade with China (% Nominal GDP)", file.path(base_dir, "trade_pct_gdp"))

cat("\n=== ALL STANDARD SCM SUITES COMPLETED AND OVERWRITTEN! ===\n")