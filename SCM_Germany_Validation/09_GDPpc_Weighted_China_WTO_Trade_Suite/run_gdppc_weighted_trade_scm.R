# =========================================================================
# 07_China_WTO_Trade_Suite/run_gdppc_weighted_trade_scm.R
# Apply Non-EU GDPpc SCM Weights (from SCM_Analysis) to China WTO Trade Suite
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggplot2)
})

cat("=== Running Non-EU GDPpc-Weighted Synthetic Control for Trade (Export & Import) ===\n")

# 1. Load GDPpc SCM Weights from SCM_Analysis
weights_path_1 <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Analysis/noneu SCM result/SCM_Weights.csv"
weights_path_2 <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Analysis/noneu_SCM_Synth/SCM_Weights.csv"

raw_w1 <- read_csv(weights_path_1, show_col_types = FALSE)
raw_w2 <- read_csv(weights_path_2, show_col_types = FALSE)

cat("\n1. Loaded tidysynth GDPpc Weights (from SCM_Analysis/noneu SCM result/SCM_Weights.csv):\n")
print(raw_w1)

cat("\n2. Loaded Synth package GDPpc Weights (from SCM_Analysis/noneu_SCM_Synth/SCM_Weights.csv):\n")
print(raw_w2)

# Country mapping between SCM_Analysis names and IMF DOTS trade dataset names
country_map <- c(
  "Canada"                     = "Canada",
  "New Zealand"                = "New Zealand",
  "United States"              = "United States",
  "Republic of Korea"          = "Korea, Republic of",
  "Australia"                  = "Australia",
  "Singapore"                  = "Singapore",
  "China, Hong Kong SAR"       = "Hong Kong Special Administrative Region, People's Republic of China",
  "Taiwan, Province of China"  = NA # Not present in IMF DOTS trade dataset
)

mpd_iso_map <- c(
  "Germany"                  = "DEU",
  "Canada"                   = "CAN",
  "New Zealand"              = "NZL",
  "United States"            = "USA",
  "Korea, Republic of"       = "KOR",
  "Australia"                = "AUS",
  "Singapore"                = "SGP",
  "Hong Kong Special Administrative Region, People's Republic of China" = "HKG"
)

# Normalize weights for available donors in trade dataset
w_df1 <- raw_w1 %>%
  mutate(trade_country = country_map[unit]) %>%
  filter(!is.na(trade_country)) %>%
  mutate(
    raw_weight = weight,
    norm_weight = weight / sum(weight)
  )

w_df2 <- raw_w2 %>%
  mutate(trade_country = country_map[unit]) %>%
  filter(!is.na(trade_country)) %>%
  mutate(
    raw_weight = weight,
    norm_weight = weight / sum(weight)
  )

cat("\nNormalized tidysynth GDPpc Weights for Available Trade Donors:\n")
print(w_df1 %>% select(unit, trade_country, raw_weight, norm_weight))

# 2. Load Maddison Project Database 2023 (mpd2023_web.xlsx) for GDP & Population
mpd_path <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/mpd2023_web.xlsx"
mpd_raw <- read_excel(mpd_path, sheet = "Full data")

mpd_sub <- mpd_raw %>%
  filter(countrycode %in% unname(mpd_iso_map), year >= 1980, year <= 2019) %>%
  mutate(
    country = case_when(
      countrycode == "DEU" ~ "Germany",
      countrycode == "CAN" ~ "Canada",
      countrycode == "NZL" ~ "New Zealand",
      countrycode == "USA" ~ "United States",
      countrycode == "KOR" ~ "Korea, Republic of",
      countrycode == "AUS" ~ "Australia",
      countrycode == "SGP" ~ "Singapore",
      countrycode == "HKG" ~ "Hong Kong Special Administrative Region, People's Republic of China"
    ),
    gdp_usd_mn = (gdppc * pop) / 1000 # convert to USD Millions equivalent
  ) %>%
  select(country, year, gdppc, pop, gdp_usd_mn)

# 3. Load IMF DOTS Bilateral Trade Data with China
trade_fpath <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/07_China_WTO_Trade_Suite/Biliteral trade with China.csv"
dots_raw <- read_csv(trade_fpath, show_col_types = FALSE)

clean_trade <- dots_raw %>%
  filter(grepl("China, People", COUNTERPART_COUNTRY)) %>%
  filter(COUNTRY %in% c("Germany", w_df1$trade_country)) %>%
  filter(TIME_PERIOD >= 1980, TIME_PERIOD <= 2019) %>%
  mutate(
    flow = ifelse(grepl("Export", INDICATOR), "exp", "imp")
  ) %>%
  group_by(country = COUNTRY, year = TIME_PERIOD, flow) %>%
  summarise(value_mn = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = flow, values_from = value_mn, values_fill = list(value_mn = 0)) %>%
  mutate(total_mn = exp + imp)

# Master Panel
panel <- expand.grid(country = c("Germany", w_df1$trade_country), year = 1980:2019, stringsAsFactors = FALSE) %>%
  left_join(clean_trade, by = c("country", "year")) %>%
  left_join(mpd_sub, by = c("country", "year")) %>%
  mutate(
    exp = ifelse(is.na(exp), 0, exp),
    imp = ifelse(is.na(imp), 0, imp),
    total_mn = ifelse(is.na(total_mn), 0, total_mn),
    exp_pct_gdp   = (exp / gdp_usd_mn) * 100,
    imp_pct_gdp   = (imp / gdp_usd_mn) * 100,
    trade_pct_gdp = (total_mn / gdp_usd_mn) * 100
  )

# Function to run fixed-weight SCM for trade outcomes
run_fixed_weight_scm <- function(w_df, weight_name, out_subdir) {
  dir.create(out_subdir, showWarnings = FALSE, recursive = TRUE)
  
  donors_trade <- w_df$trade_country
  w_vector <- setNames(w_df$norm_weight, w_df$trade_country)
  
  # Save used weights
  write_csv(w_df %>% select(unit_in_SCM = unit, trade_country, raw_weight, norm_weight), 
            file.path(out_subdir, "Used_Weights.csv"))
  
  synth_results <- list()
  
  for (metric in c("exp_pct_gdp", "imp_pct_gdp", "trade_pct_gdp", "exp", "imp", "total_mn")) {
    deu_df <- panel %>% filter(country == "Germany") %>% select(year, actual = !!sym(metric))
    
    donor_mat <- panel %>%
      filter(country %in% donors_trade) %>%
      select(country, year, val = !!sym(metric)) %>%
      pivot_wider(names_from = country, values_from = val) %>%
      arrange(year)
    
    mat_vals <- as.matrix(donor_mat[, donors_trade])
    synth_vals <- as.numeric(mat_vals %*% w_vector[colnames(mat_vals)])
    
    res_df <- deu_df %>%
      mutate(
        synth = synth_vals,
        gap   = actual - synth
      )
    synth_results[[metric]] <- res_df
  }
  
  metric_labels <- c(
    "exp_pct_gdp"   = "Exports to China (% GDP Size)",
    "imp_pct_gdp"   = "Imports from China (% GDP Size)",
    "trade_pct_gdp" = "Total Trade with China (% GDP Size)",
    "exp"           = "Exports to China (USD Millions)",
    "imp"           = "Imports from China (USD Millions)",
    "total_mn"      = "Total Trade with China (USD Millions)"
  )
  
  summary_rows <- list()
  
  for (metric in names(metric_labels)) {
    lbl <- metric_labels[metric]
    df <- synth_results[[metric]]
    
    write_csv(df, file.path(out_subdir, sprintf("Results_%s.csv", metric)))
    
    pre_rmspe  <- sqrt(mean(df$gap[df$year <= 1998]^2))
    val_mspe   <- mean(df$gap[df$year >= 1999 & df$year <= 2001]^2)
    post_rmspe <- sqrt(mean(df$gap[df$year >= 2002]^2))
    post_avg_gap <- mean(df$gap[df$year >= 2002])
    post_cum_gap <- sum(df$gap[df$year >= 2002])
    val_ratio  <- val_mspe / (pre_rmspe^2)
    post_ratio <- (post_rmspe^2) / (pre_rmspe^2)
    
    summary_rows[[metric]] <- data.frame(
      Metric = metric,
      Label = lbl,
      Pre_RMSPE = pre_rmspe,
      Val_MSPE_Ratio = val_ratio,
      Post_RMSPE = post_rmspe,
      Post_MSPE_Ratio = post_ratio,
      Post_Avg_Annual_Gap = post_avg_gap,
      Post_Cumulative_Gap = post_cum_gap
    )
    
    synth_legend_name <- sprintf("Synthetic Germany (%s)", weight_name)
    color_vals <- setNames(c("#1565C0", "#C62828"), c("Germany (Actual)", synth_legend_name))
    
    # Plot Trend
    p_trend <- ggplot(df, aes(x = year)) +
      annotate("rect", xmin = 1998.5, xmax = 2001.5, ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.15) +
      annotate("rect", xmin = 2001.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "#1565C0", alpha = 0.05) +
      geom_vline(xintercept = 1998.5, linetype = "dashed", color = "darkorange", linewidth = 0.9) +
      geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 1) +
      geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.4) +
      geom_line(aes(y = synth,  color = synth_legend_name), linetype = "longdash", linewidth = 1.2) +
      scale_color_manual(values = color_vals) +
      annotate("text", x = 2000, y = max(df$actual)*0.92, label = "Validation\n1999-2001", color = "darkorange", fontface = "bold", size = 3.2) +
      annotate("text", x = 2004.5, y = max(df$actual)*0.92, label = "China WTO\n(2001)", color = "red", fontface = "bold", size = 3.5) +
      labs(
        title = sprintf("GDPpc-Weighted SCM: Germany vs Synthetic (%s)", lbl),
        subtitle = sprintf("Weights: %s | Pre-RMSPE: %.4f | Post-2001 Avg Gap: %.4f", weight_name, pre_rmspe, post_avg_gap),
        x = "Year", y = lbl, color = ""
      ) +
      theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
    
    ggsave(file.path(out_subdir, sprintf("SCM_Trends_%s.png", metric)), p_trend, width = 10, height = 6, dpi = 300)
    
    # Plot Gap
    p_gap <- ggplot(df, aes(x = year, y = gap)) +
      annotate("rect", xmin = 1998.5, xmax = 2001.5, ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.15) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "black", linewidth = 0.8) +
      geom_vline(xintercept = 1998.5, linetype = "dashed", color = "darkorange", linewidth = 0.9) +
      geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 1) +
      geom_line(color = "#1565C0", linewidth = 1.4) +
      geom_point(color = "#1565C0", size = 2) +
      labs(
        title = sprintf("Treatment Effect Gap (Actual - Synthetic): %s", lbl),
        subtitle = sprintf("Weights: %s | Post-2001 Cumulative Gap: %.2f", weight_name, post_cum_gap),
        x = "Year", y = sprintf("Gap in %s", lbl)
      ) +
      theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
    
    ggsave(file.path(out_subdir, sprintf("SCM_Gap_%s.png", metric)), p_gap, width = 10, height = 6, dpi = 300)
  }
  
  summary_df <- bind_rows(summary_rows)
  write_csv(summary_df, file.path(out_subdir, "Trade_GDPpc_Weighted_Summary.csv"))
  
  # Facet plot Export vs Import (% GDP)
  df_exp <- synth_results[["exp_pct_gdp"]] %>% mutate(Type = "Exports to China (% GDP)")
  df_imp <- synth_results[["imp_pct_gdp"]] %>% mutate(Type = "Imports from China (% GDP)")
  df_combined <- bind_rows(df_exp, df_imp)
  
  p_facet <- ggplot(df_combined, aes(x = year)) +
    facet_wrap(~ Type, scales = "free_y") +
    annotate("rect", xmin = 1998.5, xmax = 2001.5, ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.15) +
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "darkorange", linewidth = 0.8) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 0.9) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.3) +
    geom_line(aes(y = synth,  color = "Synthetic Germany (GDPpc Weighted)"), linetype = "longdash", linewidth = 1.1) +
    scale_color_manual(values = setNames(c("#1565C0", "#C62828"), c("Germany (Actual)", "Synthetic Germany (GDPpc Weighted)"))) +
    labs(
      title = sprintf("China WTO Impact on Germany Trade (%s)", weight_name),
      subtitle = "Synthetic Germany constructed using Non-EU GDP per capita donor weights",
      x = "Year", y = "% GDP Size", color = ""
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold", size = 12))
  
  ggsave(file.path(out_subdir, "SCM_Combined_Export_Import_Trends.png"), p_facet, width = 12, height = 6, dpi = 300)
  
  p_facet_gap <- ggplot(df_combined, aes(x = year, y = gap)) +
    facet_wrap(~ Type, scales = "free_y") +
    annotate("rect", xmin = 1998.5, xmax = 2001.5, ymin = -Inf, ymax = Inf, fill = "darkorange", alpha = 0.15) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "darkorange") +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
    geom_line(color = "#1565C0", linewidth = 1.3) +
    geom_point(color = "#1565C0", size = 1.8) +
    labs(
      title = sprintf("Trade Treatment Effect Gaps (%s)", weight_name),
      subtitle = "Gap = Actual Germany - Synthetic Germany (GDPpc weights)",
      x = "Year", y = "Gap (% GDP Size)"
    ) +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold", size = 12))
  
  ggsave(file.path(out_subdir, "SCM_Combined_Export_Import_Gaps.png"), p_facet_gap, width = 12, height = 6, dpi = 300)
  
  return(summary_df)
}

base_out <- "C:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/07_China_WTO_Trade_Suite/noneu_gdppc_weighted_trade"

cat("\n--- Running with tidysynth GDPpc Weights ---\n")
s1 <- run_fixed_weight_scm(w_df1, "tidysynth GDPpc Weights", file.path(base_out, "tidysynth_weights"))
print(s1)

cat("\n--- Running with Synth Package GDPpc Weights ---\n")
s2 <- run_fixed_weight_scm(w_df2, "Synth Package GDPpc Weights", file.path(base_out, "synth_package_weights"))
print(s2)

cat("\n=== ALL NON-EU GDPpc-WEIGHTED SYNTHETIC TRADE ANALYSES COMPLETED! ===\n")
