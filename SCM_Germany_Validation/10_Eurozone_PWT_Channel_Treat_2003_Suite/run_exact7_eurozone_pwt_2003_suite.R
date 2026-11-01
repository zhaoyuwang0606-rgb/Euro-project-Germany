# ==============================================================================
# Dynamic Eurozone SCM Suite for PWT Channels (Treatment = 2003)
# Restricted to EXACT 7 Eurozone Donor Countries:
# France, Netherlands, Austria, Spain, Finland, Portugal, Italy
# Time range: 1970 to 2019 (PWT 10.01 native coverage)
# LOO plots label each excluded-country line explicitly
# ==============================================================================

library(pwt10)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(Synth)

# ---- Base Output Directory ----
output_base_dir <- "c:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/10_Eurozone_PWT_Channel_Treat_2003_Suite"
if (!dir.exists(output_base_dir)) dir.create(output_base_dir, recursive = TRUE)
setwd(output_base_dir)

# ---- Load PWT 10.01 (1970-2019) ----
data("pwt10.01")
target_countries <- c("Germany", "France", "Netherlands", "Austria", "Spain", "Finland", "Portugal", "Italy")

panel_data <- pwt10.01 %>%
  filter(country %in% target_countries & year >= 1970 & year <= 2019) %>%
  mutate(
    country    = as.character(country),
    gdppc      = rgdpe / pop,
    gdp_size   = rgdpe,
    pop_size   = pop
  ) %>%
  select(country, year, csh_c, csh_i, csh_g, csh_x, csh_m, gdppc, gdp_size, pop_size)

# ---- Unit Code Mapping ----
unit_map <- data.frame(
  country  = sort(unique(panel_data$country)),
  unit_num = seq_along(unique(panel_data$country))
)

panel_df  <- left_join(panel_data, unit_map, by = "country")
deu_unit  <- unit_map$unit_num[unit_map$country == "Germany"]
all_units <- unit_map$unit_num

# ---- Channel Definitions ----
channels <- list(
  csh_i = list(var = "csh_i", label = "Investment Share"),
  csh_x = list(var = "csh_x", label = "Export Share"),
  csh_m = list(var = "csh_m", label = "Import Share"),
  csh_c = list(var = "csh_c", label = "Consumption Share"),
  csh_g = list(var = "csh_g", label = "Government Share")
)

summary_rows <- list()

cat(sprintf("=== Exact 7 Eurozone SCM Suite | PWT Channels | 1970-2019 | Treat=2003 ===\n\n"))

# ---- Helper: Fit SCM for one unit ----
fit_scm_for_unit <- function(tr_unit, control_units, df_data, ch_var) {
  tryCatch({
    dp <- dataprep(
      foo                  = as.data.frame(df_data),
      predictors           = c("gdppc", "gdp_size", "pop_size"),
      predictors.op        = "mean",
      time.predictors.prior = 1970:2002,
      special.predictors   = list(
        list(ch_var, 1980:1986, "mean"),
        list(ch_var, 1987:1993, "mean"),
        list(ch_var, 1994:2002, "mean")
      ),
      dependent            = ch_var,
      unit.variable        = "unit_num",
      unit.names.variable  = "country",
      time.variable        = "year",
      treatment.identifier = tr_unit,
      controls.identifier  = control_units,
      time.optimize.ssr    = 1970:2002,
      time.plot            = 1970:2019
    )
    capture.output(so <- synth(dp, method = "BFGS"))

    res_df <- data.frame(
      year      = 1970:2019,
      actual    = as.numeric(dp$Y1plot),
      synthetic = as.numeric(dp$Y0plot %*% so$solution.w),
      gap       = as.numeric(dp$Y1plot - dp$Y0plot %*% so$solution.w)
    )

    w_df <- data.frame(unit_num = control_units, weight = as.numeric(so$solution.w)) %>%
      left_join(unit_map, by = "unit_num")

    bal_df <- data.frame(
      variable    = rownames(dp$X0),
      Germany     = as.numeric(dp$X1),
      synthetic_Germany = as.numeric(dp$X0 %*% so$solution.w),
      donor_sample_mean = rowMeans(dp$X0)
    )

    list(res = res_df, weights = w_df, balance = bal_df)
  }, error = function(e) NULL)
}

# ============================================================
# Main Loop over channels
# ============================================================
for (ch_code in names(channels)) {
  ch_var   <- channels[[ch_code]]$var
  ch_label <- channels[[ch_code]]$label

  cat(sprintf("=======================================================\n"))
  cat(sprintf(">>> %s (%s) | Treat=2003\n", ch_var, ch_label))
  cat(sprintf("=======================================================\n"))

  ch_dir <- file.path(output_base_dir, paste0(ch_var, "_Treat_2003"))
  if (!dir.exists(ch_dir)) dir.create(ch_dir, recursive = TRUE)

  deu_controls <- setdiff(all_units, deu_unit)

  # ----------------------------------------------------------
  # 1. Baseline Germany SCM
  # ----------------------------------------------------------
  cat("1. Fitting Baseline SCM for Germany...\n")
  deu_fit <- fit_scm_for_unit(deu_unit, deu_controls, panel_df, ch_var)
  if (is.null(deu_fit)) { cat("ERROR: skipping channel.\n"); next }

  # Save Weights
  weights_out <- deu_fit$weights %>%
    select(unit = country, weight) %>%
    arrange(desc(weight))
  write_csv(weights_out, file.path(ch_dir, "Weights.csv"))

  # Save Balance
  write_csv(deu_fit$balance, file.path(ch_dir, "Balance.csv"))

  # Save YearByYear Gap
  year_gap_out <- deu_fit$res %>%
    rename(actual_Germany = actual, synthetic_Germany = synthetic, gap_Germany = gap) %>%
    mutate(pct_gap = (gap_Germany / synthetic_Germany) * 100)
  write_csv(year_gap_out, file.path(ch_dir, "YearByYear_Gap.csv"))

  # Plot: Baseline Trends
  p_trends <- ggplot(deu_fit$res, aes(x = year)) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.2) +
    geom_line(aes(y = synthetic, color = "Synthetic Germany"), linewidth = 1.2, linetype = "dashed") +
    geom_vline(xintercept = 2003, linetype = "dotted", color = "red", linewidth = 1) +
    annotate("text", x = 2003.5, y = max(deu_fit$res$actual, na.rm = TRUE),
             label = "2003 Hartz Reform", color = "red", hjust = 0, fontface = "bold", size = 3.5) +
    scale_color_manual(values = c("Germany (Actual)" = "darkblue", "Synthetic Germany" = "darkred")) +
    theme_minimal(base_size = 13) +
    labs(title   = sprintf("Germany Eurozone SCM: %s (1970–2019)", ch_label),
         subtitle = "Donor Pool: 7 Eurozone Countries | Treatment Year: 2003",
         x = "Year", y = ch_label, color = "") +
    theme(legend.position = "bottom")
  ggsave(file.path(ch_dir, "SCM_Trends.png"), p_trends, width = 8, height = 5, dpi = 300)

  # Plot: Baseline Gap
  p_gap <- ggplot(deu_fit$res, aes(x = year, y = gap)) +
    geom_line(color = "darkblue", linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 2003, linetype = "dotted", color = "red", linewidth = 1) +
    theme_minimal(base_size = 13) +
    labs(title   = sprintf("Germany Eurozone SCM Gap: %s (1970–2019)", ch_label),
         subtitle = "Actual Germany minus Synthetic Germany",
         x = "Year", y = paste("Gap in", ch_label))
  ggsave(file.path(ch_dir, "SCM_Gap.png"), p_gap, width = 8, height = 5, dpi = 300)

  # ----------------------------------------------------------
  # 2. In-Space Placebo
  # ----------------------------------------------------------
  cat("2. Running In-Space Placebos (all 8 countries)...\n")
  placebo_results <- lapply(all_units, function(u) {
    ctry_name <- unit_map$country[unit_map$unit_num == u]
    ctrls     <- setdiff(all_units, u)
    fit       <- fit_scm_for_unit(u, ctrls, panel_df, ch_var)
    if (is.null(fit)) return(NULL)
    fit$res$country <- ctry_name
    fit$res
  })

  placebo_df <- bind_rows(placebo_results)

  placebo_stats <- placebo_df %>%
    group_by(country) %>%
    summarize(
      pre_rmse      = sqrt(mean(gap[year <= 2002]^2, na.rm = TRUE)),
      post_rmse     = sqrt(mean(gap[year >= 2003]^2, na.rm = TRUE)),
      post_mean_gap = mean(gap[year >= 2003], na.rm = TRUE),
      mspe_ratio    = (post_rmse^2) / (pre_rmse^2),
      .groups = "drop"
    ) %>%
    arrange(desc(mspe_ratio)) %>%
    mutate(rank = row_number(), p_value = rank / n())

  write_csv(placebo_stats, file.path(ch_dir, "PostTreatment_Significance.csv"))

  deu_stat <- placebo_stats %>% filter(country == "Germany")
  summary_rows[[ch_code]] <- tibble(
    Channel       = ch_var,
    Label         = ch_label,
    Pre_RMSE      = deu_stat$pre_rmse,
    Post_RMSE     = deu_stat$post_rmse,
    Post_Mean_Gap = deu_stat$post_mean_gap,
    MSPE_Ratio    = deu_stat$mspe_ratio,
    Rank          = paste0(deu_stat$rank, " / ", nrow(placebo_stats)),
    p_value       = deu_stat$p_value
  )

  # Plot: Placebo Gaps
  p_placebo <- ggplot() +
    geom_line(data = placebo_df %>% filter(country != "Germany"),
              aes(x = year, y = gap, group = country), color = "gray75", alpha = 0.7, linewidth = 0.8) +
    geom_line(data = placebo_df %>% filter(country == "Germany"),
              aes(x = year, y = gap), color = "red", linewidth = 1.3) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 2003, linetype = "dotted", color = "black") +
    theme_minimal(base_size = 13) +
    labs(title    = sprintf("In-Space Placebo Gaps: %s (1970–2019)", ch_label),
         subtitle = "Red = Germany | Gray = Eurozone Placebos",
         x = "Year", y = "Gap")
  ggsave(file.path(ch_dir, "SCM_Placebo_Gaps.png"), p_placebo, width = 8, height = 5, dpi = 300)

  # Plot: MSPE Rank Bar
  p_rank <- ggplot(placebo_stats,
                   aes(x = reorder(country, mspe_ratio), y = mspe_ratio,
                       fill = (country == "Germany"))) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "gray60"), guide = "none") +
    theme_minimal(base_size = 13) +
    labs(title    = sprintf("Post/Pre MSPE Ratio Rank: %s", ch_label),
         subtitle = sprintf("Germany: %s/%d  (p = %.3f)",
                            deu_stat$rank, nrow(placebo_stats), deu_stat$p_value),
         x = "", y = "MSPE Ratio (Post / Pre)")
  ggsave(file.path(ch_dir, "SCM_MSPE_Ratio_Rank.png"), p_rank, width = 8, height = 5, dpi = 300)

  # ----------------------------------------------------------
  # 3. Leave-One-Out (LOO) — label each line by excluded country
  # ----------------------------------------------------------
  cat("3. Running LOO Robustness...\n")
  active_donors <- weights_out %>% filter(weight > 0.001) %>% pull(unit)
  cat(sprintf("   Active donors (wt > 0.001) [%d]: %s\n",
              length(active_donors), paste(active_donors, collapse = ", ")))

  loo_results <- lapply(active_donors, function(d_out) {
    d_num <- unit_map$unit_num[unit_map$country == d_out]
    ctrls <- setdiff(deu_controls, d_num)
    fit   <- fit_scm_for_unit(deu_unit, ctrls, panel_df, ch_var)
    if (is.null(fit)) return(NULL)
    fit$res$excluded_donor <- d_out
    w_df <- fit$weights %>% select(unit = country, weight)
    w_df$excluded_donor <- d_out
    list(res = fit$res, weights = w_df)
  })

  loo_res_df <- bind_rows(lapply(loo_results, `[[`, "res"))
  loo_w_df   <- bind_rows(lapply(loo_results, `[[`, "weights"))

  # Save LOO tables
  loo_w_matrix <- loo_w_df %>%
    pivot_wider(names_from = unit, values_from = weight, values_fill = 0)
  write_csv(loo_w_matrix, file.path(ch_dir, "LOO_Weights_Matrix.csv"))

  loo_gap_wide <- loo_res_df %>%
    select(year, excluded_donor, gap) %>%
    pivot_wider(names_from = excluded_donor, values_from = gap, names_prefix = "LOO_Drop_") %>%
    left_join(deu_fit$res %>% select(year, Baseline_Gap = gap), by = "year")
  write_csv(loo_gap_wide, file.path(ch_dir, "LOO_YearByYear_Gaps.csv"))

  # Deviations
  loo_dev <- loo_res_df %>%
    left_join(deu_fit$res %>% select(year, base_gap = gap), by = "year") %>%
    mutate(dev = abs(gap - base_gap)) %>%
    group_by(excluded_donor) %>%
    summarize(max_dev = max(dev, na.rm = TRUE), mean_dev = mean(dev, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(max_dev))
  write_csv(loo_dev, file.path(ch_dir, "LOO_Max_Deviations.csv"))

  # ---- LOO Plot: Trends  (labeled lines) ----
  # Compute label position = last year value for each LOO run
  loo_label_trends <- loo_res_df %>%
    group_by(excluded_donor) %>%
    filter(year == max(year)) %>%
    ungroup()

  p_loo_trends <- ggplot() +
    geom_line(data = deu_fit$res,
              aes(x = year, y = actual, color = "Germany (Actual)"), linewidth = 1.3) +
    geom_line(data = deu_fit$res,
              aes(x = year, y = synthetic, color = "Baseline Synthetic"), linewidth = 1.3, linetype = "dashed") +
    geom_line(data = loo_res_df,
              aes(x = year, y = synthetic, group = excluded_donor, color = excluded_donor),
              linewidth = 0.85, alpha = 0.85, linetype = "dashed") +
    geom_label(data = loo_label_trends,
               aes(x = year, y = synthetic, label = paste0("–", excluded_donor), color = excluded_donor),
               size = 2.8, hjust = 1, show.legend = FALSE, label.padding = unit(0.15, "lines")) +
    geom_vline(xintercept = 2003, linetype = "dotted", color = "red", linewidth = 0.9) +
    scale_color_manual(
      values = c(
        "Germany (Actual)"    = "black",
        "Baseline Synthetic"  = "darkred",
        setNames(RColorBrewer::brewer.pal(max(3, length(active_donors)), "Set2")[seq_along(active_donors)],
                 active_donors)
      )
    ) +
    theme_minimal(base_size = 13) +
    labs(title    = sprintf("LOO Trends: %s (1970–2019)", ch_label),
         subtitle = "Each colored dashed line = synthetic Germany when that donor is excluded",
         x = "Year", y = ch_label, color = "") +
    theme(legend.position = "bottom", legend.text = element_text(size = 9))
  ggsave(file.path(ch_dir, "LOO_Plot_Trends.png"), p_loo_trends, width = 9, height = 5.5, dpi = 300)

  # ---- LOO Plot: Gaps  (labeled lines) ----
  loo_label_gaps <- loo_res_df %>%
    group_by(excluded_donor) %>%
    filter(year == max(year)) %>%
    ungroup()

  p_loo_gaps <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_line(data = loo_res_df,
              aes(x = year, y = gap, group = excluded_donor, color = excluded_donor),
              linewidth = 0.9, alpha = 0.9) +
    geom_label(data = loo_label_gaps,
               aes(x = year, y = gap, label = paste0("–", excluded_donor), color = excluded_donor),
               size = 2.8, hjust = 1, show.legend = FALSE, label.padding = unit(0.15, "lines")) +
    geom_line(data = deu_fit$res,
              aes(x = year, y = gap, color = "Baseline"), linewidth = 1.4) +
    geom_label(data = deu_fit$res %>% filter(year == max(year)),
               aes(x = year, y = gap, label = "Baseline"), color = "black",
               size = 2.8, hjust = 1, show.legend = FALSE, label.padding = unit(0.15, "lines")) +
    geom_vline(xintercept = 2003, linetype = "dotted", color = "red", linewidth = 0.9) +
    scale_color_manual(
      values = c(
        "Baseline" = "black",
        setNames(RColorBrewer::brewer.pal(max(3, length(active_donors)), "Set2")[seq_along(active_donors)],
                 active_donors)
      )
    ) +
    theme_minimal(base_size = 13) +
    labs(title    = sprintf("LOO Gaps: %s (1970–2019)", ch_label),
         subtitle = "Black = Baseline | Colored = LOO iteration (label shows excluded donor)",
         x = "Year", y = "Gap", color = "") +
    theme(legend.position = "bottom", legend.text = element_text(size = 9))
  ggsave(file.path(ch_dir, "LOO_Plot_Gaps.png"), p_loo_gaps, width = 9, height = 5.5, dpi = 300)

  cat(sprintf("COMPLETED: %s -> %s_Treat_2003/\n\n", ch_var, ch_var))
}

# ---- Master Summary ----
master_summary <- bind_rows(summary_rows)
print(master_summary)
write_csv(master_summary, file.path(output_base_dir, "Summary_Eurozone_PWT_Channels_Treat_2003.csv"))

cat("\nALL 5 CHANNELS DONE. Exact 7 Eurozone Donors | 1970-2019 | Treat=2003\n")
