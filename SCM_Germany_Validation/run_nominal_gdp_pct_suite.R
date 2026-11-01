suppressPackageStartupMessages({
  library(tidyverse)
  library(Synth)
  library(pwt10)
  library(ggplot2)
  library(zoo)
})

cat("=== 1980-2019 Clean SCM Suite (Outcome = Trade % of Nominal GDP) ===\n")
cat("Predictors: UNCHANGED (log_gdppc, trade_open, inv_share, hc_idx + lag terms)\n")
cat("Donors: 8 Clean Donors (AUS, CAN, CHL, JPN, NZL, SGP, TUR, USA)\n\n")

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

# ─── 1. Load IMF DOTS Trade Data ───
fpath <- file.path(proj_root, "SCM_Germany_Validation", "07_China_WTO_Trade_Suite", "Biliteral trade with China.csv")
dots_raw <- read_csv(fpath, show_col_types = FALSE)

clean_donors <- c("Australia", "Canada", "Chile", "Japan", "New Zealand", "Singapore", "Turkey", "United States")
all_countries <- c("Germany", clean_donors)

dots <- dots_raw %>%
  filter(grepl("China, People", COUNTERPART_COUNTRY)) %>%
  mutate(
    country = case_when(
      COUNTRY == "Türkiye, Republic of" ~ "Turkey",
      COUNTRY == "Korea, Republic of"   ~ "Korea",
      grepl("Hong Kong", COUNTRY)       ~ "Hong Kong",
      TRUE ~ COUNTRY
    ),
    flow = ifelse(grepl("Export", INDICATOR), "exp", "imp")
  ) %>%
  filter(country %in% all_countries, TIME_PERIOD >= 1980, TIME_PERIOD <= 2019) %>%
  group_by(country, year = TIME_PERIOD, flow) %>%
  summarise(value_mn = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = flow, values_from = value_mn) %>%
  mutate(total_mn = exp + imp)

# ─── 2. Load PWT Macro Data for Nominal GDP & Predictors ───
data("pwt10.01")
pwt_sub <- pwt10.01 %>%
  filter(isocode %in% c("DEU","AUS","CAN","CHL","JPN","NZL","SGP","TUR","USA"), year >= 1980, year <= 2019) %>%
  mutate(
    country = case_when(
      isocode == "DEU" ~ "Germany",
      isocode == "AUS" ~ "Australia",
      isocode == "CAN" ~ "Canada",
      isocode == "CHL" ~ "Chile",
      isocode == "JPN" ~ "Japan",
      isocode == "NZL" ~ "New Zealand",
      isocode == "SGP" ~ "Singapore",
      isocode == "TUR" ~ "Turkey",
      isocode == "USA" ~ "United States"
    ),
    nom_gdp_usd = cgdpo * pop,            # Nominal GDP in Current USD Millions
    gdppc       = rgdpna / pop,
    log_gdppc   = log(gdppc),
    trade_open  = abs(csh_x) + abs(csh_m),
    inv_share   = csh_i,
    hc_idx      = hc
  ) %>%
  select(country, year, nom_gdp_usd, log_gdppc, trade_open, inv_share, hc_idx)

# Join and construct % of Nominal GDP outcomes
panel <- expand.grid(country = all_countries, year = 1980:2019, stringsAsFactors = FALSE) %>%
  left_join(dots, by = c("country", "year")) %>%
  left_join(pwt_sub, by = c("country", "year")) %>%
  arrange(country, year) %>%
  mutate(
    exp_pct_gdp   = (exp / nom_gdp_usd) * 100,
    imp_pct_gdp   = (imp / nom_gdp_usd) * 100,
    trade_pct_gdp = (total_mn / nom_gdp_usd) * 100
  )

unit_map <- data.frame(
  country  = sort(unique(panel$country)),
  unit_num = seq_along(sort(unique(panel$country)))
)
panel <- left_join(panel, unit_map, by = "country")

cat("Final Panel NA Check (should all be 0):\n")
print(colSums(is.na(panel)))

pred_yrs <- 1980:2001

# Function to run SCM suite for a specific outcome
run_full_scm_suite <- function(outcome_var, outcome_label, out_subdir) {
  cat(sprintf("\n=========================================="))
  cat(sprintf("\nRunning Full SCM Suite for: %s", outcome_label))
  cat(sprintf("\n==========================================\n"))
  
  dir.create(out_subdir, showWarnings = FALSE, recursive = TRUE)
  
  units <- sort(unique(panel$unit_num))
  deu_unit <- unit_map$unit_num[unit_map$country == "Germany"]
  donor_units <- setdiff(units, deu_unit)
  
  # 1. Main Synthetic Control Fit
  run_single_synth <- function(tr_unit, ctrl_units) {
    dp <- dataprep(
      foo                   = as.data.frame(panel),
      predictors            = c("log_gdppc", "trade_open", "inv_share", "hc_idx"),
      predictors.op         = "mean",
      time.predictors.prior = pred_yrs,
      special.predictors    = list(
        list(outcome_var, 1980:1986, "mean"),
        list(outcome_var, 1987:1993, "mean"),
        list(outcome_var, 1994:1998, "mean"),
        list(outcome_var, 1999:2001, "mean"),
        list(outcome_var, 1995, "mean"),
        list(outcome_var, 1998, "mean"),
        list(outcome_var, 2000, "mean"),
        list(outcome_var, 2001, "mean"),
        list("log_gdppc",   1990, "mean"),
        list("log_gdppc",   2000, "mean"),
        list("trade_open",  1995, "mean")
      ),
      dependent             = outcome_var,
      unit.variable         = "unit_num",
      unit.names.variable   = "country",
      time.variable         = "year",
      treatment.identifier  = tr_unit,
      controls.identifier   = ctrl_units,
      time.optimize.ssr     = pred_yrs,
      time.plot             = 1980:2019
    )
    so <- synth(dp, method = "BFGS")
    y_act <- dp$Y1plot
    y_syn <- dp$Y0plot %*% so$solution.w
    res <- data.frame(year = 1980:2019, actual = as.numeric(y_act), synth = as.numeric(y_syn)) %>%
      mutate(gap = actual - synth)
    list(dp = dp, so = so, res = res)
  }
  
  main_fit <- run_single_synth(deu_unit, donor_units)
  res_df <- main_fit$res
  
  # Save Main Results
  write_csv(res_df, file.path(out_subdir, "Baseline_YearByYear_Gap.csv"))
  
  wts <- data.frame(unit_num = donor_units, weight = as.numeric(main_fit$so$solution.w)) %>%
    left_join(unit_map, by = "unit_num") %>% select(unit = country, weight) %>% arrange(desc(weight))
  write_csv(wts, file.path(out_subdir, "SCM_Weights.csv"))
  
  # Balance table
  st <- synth.tab(synth.res = main_fit$so, dataprep.res = main_fit$dp, round.digit = 3)
  bal_df <- as.data.frame(st$tab.pred) %>% rownames_to_column("predictor")
  write_csv(bal_df, file.path(out_subdir, "SCM_Balance.csv"))
  
  # Plot Main Trends
  p_trends <- ggplot(res_df, aes(x = year)) +
    annotate("rect", xmin = 2001.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "#1565C0", alpha = 0.05) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.4) +
    geom_line(aes(y = synth,  color = "Synthetic Germany"), linetype = "longdash", linewidth = 1.2) +
    scale_color_manual(values = c("Germany (Actual)" = "#1565C0", "Synthetic Germany" = "#C62828")) +
    annotate("text", x = 2003, y = max(res_df$actual)*0.95, label = "China WTO\n(2001)", color = "red", fontface = "bold", size = 3.5) +
    labs(title = sprintf("Synthetic Control: Germany vs Synthetic (%s)", outcome_label),
         subtitle = "Donor Pool: Australia, Canada, Chile, Japan, NZ, Singapore, Turkey, USA (1980-2019)",
         x = "Year", y = sprintf("%s (%% of Nominal GDP)", outcome_label), color = "") +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_Trends.png"), p_trends, width = 10, height = 6, dpi = 300)
  
  # 2. In-space Placebo Runs (All Donors)
  placebo_res <- list()
  placebo_res[["Germany"]] <- res_df$gap
  
  for (d_num in donor_units) {
    d_name <- unit_map$country[unit_map$unit_num == d_num]
    d_ctrls <- setdiff(units, d_num)
    tryCatch({
      fit_p <- run_single_synth(d_num, d_ctrls)
      placebo_res[[d_name]] <- fit_p$res$gap
    }, error = function(e) {})
  }
  
  placebo_df <- as.data.frame(placebo_res) %>% mutate(year = 1980:2019) %>%
    pivot_longer(-year, names_to = "country", values_to = "gap")
  
  # MSPE Ratios
  mspe_calc <- placebo_df %>%
    group_by(country) %>%
    summarise(
      pre_mspe  = mean(gap[year <= 2001]^2),
      post_mspe = mean(gap[year >= 2002]^2),
      ratio     = post_mspe / pre_mspe,
      .groups   = "drop"
    ) %>% arrange(desc(ratio))
  
  write_csv(mspe_calc, file.path(out_subdir, "SCM_Significance.csv"))
  
  deu_rank <- which(mspe_calc$country == "Germany")
  p_val <- deu_rank / nrow(mspe_calc)
  
  eff_summary <- data.frame(
    Pre_RMSPE = sqrt(mspe_calc$pre_mspe[mspe_calc$country=="Germany"]),
    Post_RMSPE = sqrt(mspe_calc$post_mspe[mspe_calc$country=="Germany"]),
    MSPE_Ratio = mspe_calc$ratio[mspe_calc$country=="Germany"],
    Rank = sprintf("%d / %d", deu_rank, nrow(mspe_calc)),
    p_value = p_val
  )
  write_csv(eff_summary, file.path(out_subdir, "PostTreatment_Significance.csv"))
  
  # Plot Placebos
  p_plac <- ggplot(placebo_df, aes(x = year, y = gap, group = country)) +
    geom_line(data = placebo_df %>% filter(country != "Germany"), color = "gray70", alpha = 0.7, linewidth = 0.8) +
    geom_line(data = placebo_df %>% filter(country == "Germany"), color = "#1565C0", linewidth = 1.5) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    labs(title = sprintf("In-Space Placebo Test: %s Gap", outcome_label),
         subtitle = sprintf("Germany vs %d Placebo Countries | p-value = %.3f", nrow(mspe_calc)-1, p_val),
         x = "Year", y = "Gap (Actual - Synthetic, % of GDP)") +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_Placebo.png"), p_plac, width = 10, height = 6, dpi = 300)
  
  # Plot MSPE Ratio Rank
  p_mspe <- ggplot(mspe_calc, aes(x = reorder(country, ratio), y = ratio, fill = (country == "Germany"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("TRUE" = "#1565C0", "FALSE" = "gray60")) +
    coord_flip() +
    labs(title = sprintf("Post/Pre-WTO MSPE Ratio Ranking (%s)", outcome_label),
         x = "", y = "MSPE Ratio (Post-2001 / Pre-2001)", fill = "") +
    theme_minimal(base_size = 13) + theme(legend.position = "none", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_MSPE_Ratio.png"), p_mspe, width = 9, height = 5.5, dpi = 300)
  
  # 3. Leave-One-Out (LOO) Robustness
  active_donors <- wts$unit[wts$weight > 0.001]
  loo_res <- list()
  loo_res[["Full Synth"]] <- res_df$synth
  
  for (drop_d in active_donors) {
    drop_num <- unit_map$unit_num[unit_map$country == drop_d]
    sub_ctrls <- setdiff(donor_units, drop_num)
    tryCatch({
      fit_loo <- run_single_synth(deu_unit, sub_ctrls)
      loo_res[[sprintf("Drop %s", drop_d)]] <- fit_loo$res$synth
    }, error = function(e) {})
  }
  
  loo_df <- as.data.frame(loo_res) %>% mutate(year = 1980:2019, actual = res_df$actual) %>%
    pivot_longer(-c(year, actual), names_to = "specification", values_to = "synth_value")
  
  p_loo <- ggplot(loo_df, aes(x = year)) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.4) +
    geom_line(aes(y = synth_value, group = specification, color = specification), linetype = "dashed", linewidth = 0.9) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
    labs(title = sprintf("Leave-One-Out Robustness: %s", outcome_label),
         x = "Year", y = sprintf("%s (%% of Nominal GDP)", outcome_label), color = "Specification") +
    theme_minimal(base_size = 12) + theme(legend.position = "right", plot.title = element_text(face = "bold"))
  ggsave(file.path(out_subdir, "SCM_LOO.png"), p_loo, width = 10.5, height = 6, dpi = 300)
  
  cat(sprintf("Completed suite for %s. Outputs saved to %s/\n", outcome_label, out_subdir))
}

# Run for Exports % GDP, Imports % GDP, and Total Trade % GDP
base_dir <- "07_China_WTO_Trade_Suite"

run_full_scm_suite("exp_pct_gdp",   "Exports to China (% Nominal GDP)",   file.path(base_dir, "exp_pct_gdp"))
run_full_scm_suite("imp_pct_gdp",   "Imports from China (% Nominal GDP)", file.path(base_dir, "imp_pct_gdp"))
run_full_scm_suite("trade_pct_gdp", "Total Trade with China (% Nominal GDP)", file.path(base_dir, "trade_pct_gdp"))

cat("\n=== ALL 3 SUITES COMPLETED SUCCESSFULLY! ===\n")