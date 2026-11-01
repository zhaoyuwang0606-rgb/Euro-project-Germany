# =========================================================================
# 07_China_WTO_Trade_Suite/run_synth_validation.R
# Synth package runner for Non-EU WTO Trade Placebo validation
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(Synth)
  library(pwt10)
  library(ggplot2)
  library(parallel)
})

num_cores <- max(1, detectCores() - 1)
cat(sprintf("=== SCM_Germany_Validation: Synth rewrite (Cores: %d) ===\n", num_cores))

# Paths
base_dir <- "."
out_base <- file.path(base_dir, "Synth_Results")
dir.create(out_base, showWarnings = FALSE, recursive = TRUE)

dots_path <- file.path(base_dir, "Biliteral trade with China.csv")

clean_donors  <- c("Australia","Canada","Chile","Japan","New Zealand","Singapore","Turkey","United States")
all_countries <- c("Germany", clean_donors)
iso_codes     <- c("DEU","AUS","CAN","CHL","JPN","NZL","SGP","TUR","USA")

cat("Loading trade data...\n")
dots_raw <- read_csv(dots_path, show_col_types = FALSE)

trade_clean <- dots_raw %>%
  filter(grepl("China, People", COUNTERPART_COUNTRY)) %>%
  mutate(
    country = case_when(
      COUNTRY == "Türkiye, Republic of" ~ "Turkey",
      TRUE ~ COUNTRY
    ),
    flow = ifelse(grepl("Export", INDICATOR), "exp", "imp")
  ) %>%
  filter(country %in% all_countries, TIME_PERIOD >= 1980, TIME_PERIOD <= 2019) %>%
  group_by(country, year = TIME_PERIOD, flow) %>%
  summarise(value_mn = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = flow, values_from = value_mn, values_fill = list(value_mn = 0)) %>%
  mutate(total_mn = exp + imp)

cat("Loading PWT data...\n")
data("pwt10.01")
pwt_sub <- pwt10.01 %>%
  filter(isocode %in% iso_codes, year >= 1980, year <= 2019) %>%
  mutate(
    country = case_when(
      isocode == "DEU" ~ "Germany",       isocode == "AUS" ~ "Australia",
      isocode == "CAN" ~ "Canada",        isocode == "CHL" ~ "Chile",
      isocode == "JPN" ~ "Japan",         isocode == "NZL" ~ "New Zealand",
      isocode == "SGP" ~ "Singapore",     isocode == "TUR" ~ "Turkey",
      isocode == "USA" ~ "United States"
    ),
    nom_gdp_usd_mn = (cgdpo * pop) / 1000,
    pop_mn = pop,
    nom_gdppc_usd = (cgdpo * pop * 1e6) / (pop * 1e6)
  ) %>%
  select(country, year, nom_gdp_usd_mn, pop_mn, nom_gdppc_usd)

full_panel <- expand.grid(country = all_countries, year = 1980:2019, stringsAsFactors = FALSE) %>%
  left_join(trade_clean, by = c("country", "year")) %>%
  left_join(pwt_sub,     by = c("country", "year")) %>%
  mutate(
    exp = ifelse(is.na(exp), 0, exp),
    imp = ifelse(is.na(imp), 0, imp),
    total_mn = ifelse(is.na(total_mn), 0, total_mn),
    exp_pct_gdp   = (exp / nom_gdp_usd_mn) * 100,
    imp_pct_gdp   = (imp / nom_gdp_usd_mn) * 100,
    trade_pct_gdp = (total_mn / nom_gdp_usd_mn) * 100
  ) %>%
  arrange(country, year)

u_map <- data.frame(country = sort(unique(full_panel$country)), unit_num = seq_along(sort(unique(full_panel$country))))
full_panel <- left_join(full_panel, u_map, by = "country")

run_synth_trade <- function(df, dep_var, treat_year, tr_u, ctrl_u) {
  pre_window <- 1980:treat_year
  all_years  <- 1980:2019
  
  dp <- dataprep(
    foo                   = as.data.frame(df),
    predictors            = c("nom_gdppc_usd", "pop_mn", "nom_gdp_usd_mn"),
    predictors.op         = "mean",
    time.predictors.prior = pre_window,
    special.predictors    = list(
      list(dep_var, 1980:1986, "mean"),
      list(dep_var, 1987:1992, "mean"),
      list(dep_var, 1993:treat_year, "mean")
    ),
    dependent             = dep_var,
    unit.variable         = "unit_num",
    unit.names.variable   = "country",
    time.variable         = "year",
    treatment.identifier  = tr_u,
    controls.identifier   = ctrl_u,
    time.optimize.ssr     = pre_window,
    time.plot             = all_years
  )
  
  so <- synth(dp, method = "BFGS")
  
  y_act <- as.numeric(dp$Y1plot)
  y_syn <- as.numeric(dp$Y0plot %*% so$solution.w)
  yrs   <- as.numeric(rownames(dp$Y1plot))
  
  res <- data.frame(year = yrs, actual = y_act, synth = y_syn) %>%
    mutate(gap = actual - synth, pct_gap = (gap / synth) * 100)
  
  wts <- data.frame(unit_num = ctrl_u) %>%
    left_join(u_map, by = "unit_num") %>%
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

run_experiment_synth <- function(dep_var, treat_year, folder_name) {
  out_dir <- file.path(out_base, folder_name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  cat(sprintf("\n--- Experiment: %s (Treat=%d) -> %s ---\n", folder_name, treat_year, out_dir))
  
  tr_u   <- u_map$unit_num[u_map$country == "Germany"]
  ctrl_u <- u_map$unit_num[u_map$country != "Germany"]
  
  fit_base <- tryCatch(
    run_synth_trade(full_panel, dep_var, treat_year, tr_u, ctrl_u),
    error = function(e) { cat("  Base error:", e$message, "\n"); NULL }
  )
  if (is.null(fit_base)) return(NULL)
  
  write_csv(fit_base$wts, file.path(out_dir, "SCM_Weights.csv"))
  write_csv(fit_base$bal, file.path(out_dir, "SCM_Balance.csv"))
  write_csv(
    fit_base$res %>% select(time_unit = year, real_y = actual, synth_y = synth, gap, pct_gap),
    file.path(out_dir, "Baseline_YearByYear_Gap.csv")
  )
  
  p_tr <- ggplot(fit_base$res, aes(x = year)) +
    geom_vline(xintercept = treat_year + 0.5, linetype = "dashed", color = "red", linewidth = 0.9) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.3) +
    geom_line(aes(y = synth,  color = "Synthetic Germany"), linetype = "longdash", linewidth = 1.1) +
    scale_color_manual(values = c("Germany (Actual)" = "#1565C0", "Synthetic Germany" = "#C62828")) +
    labs(title = sprintf("Synth Trade: %s (Treat=%d)", folder_name, treat_year), x = "Year", y = dep_var, color = "") +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, "SCM_Trends.png"), p_tr, width = 8, height = 5.5, dpi = 300)
  
  # Placebos
  cat("  Running In-Space Placebos...\n")
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, { library(Synth); library(dplyr) })
  clusterExport(cl, c("full_panel", "dep_var", "treat_year", "u_map", "run_synth_trade"), envir = environment())
  
  placebo_res <- parLapply(cl, ctrl_u, function(c_u) {
    c_name  <- u_map$country[u_map$unit_num == c_u]
    other_u <- setdiff(u_map$unit_num, c_u)
    tryCatch({
      fit_p <- run_synth_trade(full_panel, dep_var, treat_year, c_u, other_u)
      data.frame(year = fit_p$res$year, country = c_name, gap = fit_p$res$gap)
    }, error = function(e) NULL)
  })
  stopCluster(cl)
  
  plac_df <- bind_rows(placebo_res) %>%
    bind_rows(data.frame(year = fit_base$res$year, country = "Germany", gap = fit_base$res$gap))
  
  mspe_df <- plac_df %>%
    group_by(country) %>%
    summarise(
      pre_mspe  = mean(gap[year <= treat_year]^2),
      post_mspe = mean(gap[year > treat_year]^2),
      post_ratio= post_mspe / pre_mspe,
      .groups   = "drop"
    ) %>% arrange(desc(post_ratio))
  
  write_csv(mspe_df, file.path(out_dir, "SCM_Significance.csv"))
  
  p_val <- which(mspe_df$country == "Germany") / nrow(mspe_df)
  p_plac <- ggplot(plac_df, aes(x = year, y = gap, group = country)) +
    geom_line(data = plac_df %>% filter(country != "Germany"), color = "gray70", alpha = 0.7, linewidth = 0.7) +
    geom_line(data = plac_df %>% filter(country == "Germany"), color = "#1565C0", linewidth = 1.4) +
    geom_vline(xintercept = treat_year + 0.5, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = sprintf("Placebo Test: %s (Treat=%d, p=%.3f)", folder_name, treat_year, p_val), x = "Year", y = "Gap") +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, "SCM_Placebo.png"), p_plac, width = 8, height = 5.5, dpi = 300)
  
  # LOO
  cat("  Running LOO...\n")
  active_donors <- fit_base$wts %>% filter(weight > 0.001) %>% pull(unit)
  
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, { library(Synth); library(dplyr) })
  clusterExport(cl, c("full_panel", "dep_var", "treat_year", "tr_u", "ctrl_u", "u_map", "run_synth_trade"), envir = environment())
  
  loo_res <- parLapply(cl, active_donors, function(d_name) {
    d_u      <- u_map$unit_num[u_map$country == d_name]
    sub_ctrl <- setdiff(ctrl_u, d_u)
    tryCatch({
      fit_l <- run_synth_trade(full_panel, dep_var, treat_year, tr_u, sub_ctrl)
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
    geom_vline(xintercept = treat_year + 0.5, linetype = "dashed", color = "red") +
    labs(title = sprintf("Synth LOO: %s", folder_name), x = "Year", y = dep_var, color = "Specification") +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, "SCM_LOO.png"), p_loo, width = 9.5, height = 5.5, dpi = 300)
  
  cat(sprintf("  Completed %s!\n", folder_name))
}

dep_vars    <- c("exp_pct_gdp", "imp_pct_gdp", "trade_pct_gdp")
treat_years <- 1993:1998

for (dv in dep_vars) {
  for (ty in treat_years) {
    fname <- sprintf("%s_Treat_%d", dv, ty)
    run_experiment_synth(dv, ty, fname)
  }
}

cat("\n=== All Synth Trade Placebo Validation runs completed! ===\n")
