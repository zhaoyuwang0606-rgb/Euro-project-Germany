# =========================================================================
# 09_GDPpc_Weighted_China_WTO_Trade_Suite/run_eu_weighted_trade_scm.R
# Eurozone-Weighted Synthetic Control for Germany Trade with China (1980-2019)
# Fast Offline Execution (No WDI Network Delays)
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

cat("=== Running Eurozone-Weighted Synthetic Control for Trade with China (Offline Fast) ===\n")

# 1. Defined Eurozone Donor Pool Weights (as provided from SCM exp_pct_gdp fit)
eu_weights <- tribble(
  ~unit,         ~weight,
  "Italy",       0.7704935577665154,
  "Finland",     0.22942629288346275,
  "Austria",     7.852065425536896e-5,
  "Netherlands", 8.200680368651049e-7,
  "Spain",       5.584464562071855e-7,
  "Portugal",    2.4542704150603395e-7,
  "France",      4.763419643250279e-9
)

out_dir <- "c:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/09_GDPpc_Weighted_China_WTO_Trade_Suite/eu_weighted_trade/synth_package_weights"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_csv(eu_weights, file.path(out_dir, "Used_Weights.csv"))

# 2. Load Local Trade Data
base_suite  <- "c:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Germany_Validation/07_China_WTO_Trade_Suite"
eu_fpath    <- file.path(base_suite, "EU biliteral with china.csv")
donor_fpath <- file.path(base_suite, "Biliteral trade with China.csv")

eu_raw    <- read_csv(eu_fpath, show_col_types = FALSE)
donor_raw <- read_csv(donor_fpath, show_col_types = FALSE)

clean_trade <- function(df, c_list) {
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
    filter(country %in% c_list, TIME_PERIOD >= 1980, TIME_PERIOD <= 2019) %>%
    group_by(country, year = TIME_PERIOD, flow) %>%
    summarise(value_mn = sum(OBS_VALUE, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = flow, values_from = value_mn, values_fill = list(value_mn = 0)) %>%
    mutate(total_mn = exp + imp)
}

trade_eu  <- clean_trade(eu_raw, eu_weights$unit)
trade_deu <- clean_trade(donor_raw, "Germany")
trade_all <- bind_rows(trade_eu, trade_deu)

# 3. Load Offline GDP Data from Hybrid Panel
hybrid_panel <- read_csv("../Hybrid_MPD_PWT_Panel.csv", show_col_types = FALSE) %>%
  filter(year >= 1980 & year <= 2019) %>%
  mutate(country = case_when(
    country == "Germany" ~ "Germany",
    country == "Italy" ~ "Italy",
    country == "Finland" ~ "Finland",
    country == "Austria" ~ "Austria",
    country == "Netherlands" ~ "Netherlands",
    country == "Spain" ~ "Spain",
    country == "Portugal" ~ "Portugal",
    country == "France" ~ "France",
    TRUE ~ country
  )) %>%
  filter(country %in% c("Germany", eu_weights$unit)) %>%
  select(country, year, gdp_size, gdppc_mpd, pop_mpd)

panel <- expand.grid(country = c("Germany", eu_weights$unit), year = 1980:2019, stringsAsFactors = FALSE) %>%
  left_join(trade_all, by = c("country", "year")) %>%
  left_join(hybrid_panel, by = c("country", "year")) %>%
  arrange(country, year) %>%
  mutate(
    exp = ifelse(is.na(exp), 0, exp),
    imp = ifelse(is.na(imp), 0, imp),
    total_mn = ifelse(is.na(total_mn), 0, total_mn),
    exp_pct_gdp   = (exp / gdp_size) * 100,
    imp_pct_gdp   = (imp / gdp_size) * 100,
    trade_pct_gdp = (total_mn / gdp_size) * 100
  )

# 4. Compute Synthetic Germany using Eurozone Weights
target_vars <- c("exp", "imp", "total_mn", "exp_pct_gdp", "imp_pct_gdp", "trade_pct_gdp")
labels_map  <- c(
  exp           = "Export to China (USD Millions)",
  imp           = "Import from China (USD Millions)",
  total_mn      = "Total Trade with China (USD Millions)",
  exp_pct_gdp   = "Export to China (% of GDP)",
  imp_pct_gdp   = "Import from China (% of GDP)",
  trade_pct_gdp = "Total Trade with China (% of GDP)"
)

metrics_list <- list()

for (v in target_vars) {
  deu_data <- panel %>% filter(country == "Germany") %>% select(year, actual = !!sym(v))
  
  donor_data <- panel %>%
    filter(country %in% eu_weights$unit) %>%
    select(country, year, val = !!sym(v)) %>%
    left_join(eu_weights, by = c("country" = "unit")) %>%
    group_by(year) %>%
    summarise(synth = sum(val * weight, na.rm = TRUE), .groups = "drop")
  
  res_df <- left_join(deu_data, donor_data, by = "year") %>%
    mutate(gap = actual - synth)
  
  write_csv(res_df, file.path(out_dir, sprintf("Results_%s.csv", v)))
  
  pre_df  <- res_df %>% filter(year <= 2001)
  post_df <- res_df %>% filter(year >= 2002)
  
  pre_rmse  <- sqrt(mean(pre_df$gap^2))
  post_rmse <- sqrt(mean(post_df$gap^2))
  post_avg_gap <- mean(post_df$gap)
  
  metrics_list[[v]] <- tibble(
    Variable = v,
    Label = labels_map[v],
    Pre_2001_RMSE = pre_rmse,
    Post_2001_RMSE = post_rmse,
    Post_2001_Avg_Gap = post_avg_gap
  )
  
  p_tr <- ggplot(res_df, aes(x = year)) +
    annotate("rect", xmin = 2001.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, fill = "#1565C0", alpha = 0.05) +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(aes(y = actual, color = "Germany (Actual)"), linewidth = 1.3) +
    geom_line(aes(y = synth,  color = "Synthetic Germany (Eurozone Weighted)"), linetype = "longdash", linewidth = 1.1) +
    scale_color_manual(values = c("Germany (Actual)" = "#1565C0", "Synthetic Germany (Eurozone Weighted)" = "#C62828")) +
    annotate("text", x = 2004, y = max(c(res_df$actual, res_df$synth), na.rm=TRUE)*0.95, label = "China WTO\n(2001)", color = "red", fontface = "bold", size = 3.5) +
    labs(
      title = sprintf("Eurozone-Weighted SCM: %s", labels_map[v]),
      subtitle = sprintf("Pre-2001 RMSE: %.2f | Post-2001 Avg Gap: %.2f", pre_rmse, post_avg_gap),
      x = "Year", y = labels_map[v], color = ""
    ) +
    theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  ggsave(file.path(out_dir, sprintf("SCM_Trends_%s.png", v)), p_tr, width = 10, height = 6, dpi = 300)
  
  p_gp <- ggplot(res_df, aes(x = year, y = gap)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_line(color = "#1565C0", linewidth = 1.3) +
    labs(
      title = sprintf("Eurozone-Weighted SCM Gap: %s", labels_map[v]),
      subtitle = "Gap = Germany (Actual) - Synthetic Germany (Eurozone Weighted)",
      x = "Year", y = "Gap"
    ) +
    theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(out_dir, sprintf("SCM_Gap_%s.png", v)), p_gp, width = 10, height = 6, dpi = 300)
}

summary_df <- bind_rows(metrics_list)
write_csv(summary_df, file.path(out_dir, "Trade_Eurozone_Weighted_Summary.csv"))

exp_res <- read_csv(file.path(out_dir, "Results_exp.csv"), show_col_types = FALSE)
imp_res <- read_csv(file.path(out_dir, "Results_imp.csv"), show_col_types = FALSE)

comb_df <- bind_rows(
  exp_res %>% mutate(Flow = "Export to China"),
  imp_res %>% mutate(Flow = "Import from China")
)

p_comb_tr <- ggplot(comb_df, aes(x = year)) +
  geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
  geom_line(aes(y = actual, color = Flow, linetype = "Actual Germany"), linewidth = 1.2) +
  geom_line(aes(y = synth,  color = Flow, linetype = "Synthetic Germany (Eurozone)"), linewidth = 1) +
  labs(
    title = "Eurozone-Weighted SCM: Germany Exports vs Imports with China (1980-2019)",
    x = "Year", y = "USD Millions", color = "Flow", linetype = "Type"
  ) +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(file.path(out_dir, "SCM_Combined_Export_Import_Trends.png"), p_comb_tr, width = 10.5, height = 6.5, dpi = 300)

p_comb_gp <- ggplot(comb_df, aes(x = year, y = gap, color = Flow)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2001.5, linetype = "dashed", color = "red") +
  geom_line(linewidth = 1.3) +
  labs(
    title = "Eurozone-Weighted SCM Gaps: Germany Export vs Import Gaps with China",
    x = "Year", y = "Gap (USD Millions)", color = "Flow"
  ) +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(file.path(out_dir, "SCM_Combined_Export_Import_Gaps.png"), p_comb_gp, width = 10.5, height = 6.5, dpi = 300)

cat("Successfully generated Eurozone-weighted trade SCM results (Offline Fast)!\n")
