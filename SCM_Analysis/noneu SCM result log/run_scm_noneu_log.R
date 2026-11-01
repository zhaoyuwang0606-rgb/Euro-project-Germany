library(tidyverse)
library(readxl)
library(tidysynth)

# Create output folder if it doesn't exist
dir.create("c:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Analysis/noneu SCM result log", showWarnings = FALSE, recursive = TRUE)
setwd("c:/Users/22984/Desktop/Leuven slide/Applied econometrics/Applied econometrics/SCM_Analysis/noneu SCM result log")

# ==============================================================================
# 0. 设定参数与定义 Donor Pool
# ==============================================================================
donor_countries <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark", 
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", 
  "Japan", "Republic of Korea", "Norway", "New Zealand", "Poland", 
  "Qatar", "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China", "United States"
)

# 剔除 EU/欧洲 Spillover 国家、日本，以及中东/产油国 (Saudi Arabia, Qatar, Israel)
spillover_and_japan <- c("Switzerland", "Denmark", "United Kingdom", "Iceland", 
                         "Norway", "Poland", "Sweden", "Japan",
                         "Saudi Arabia", "Qatar", "Israel")

treated_country <- "Germany"
treat_year <- 1999
pre_window <- 1970:1998

# ==============================================================================
# 1. 数据清洗
# ==============================================================================
cat("\n== Reading Data ==\n")
raw_data <- read_excel("../../mpd2023_web.xlsx", sheet = "Full data")

df_clean <- raw_data %>%
  filter(country %in% c(treated_country, donor_countries)) %>%
  filter(!country %in% spillover_and_japan) %>%
  filter(year >= 1970 & year <= 2022) %>%
  select(country, year, gdppc, pop) %>%
  mutate(
    gdp_size = gdppc * pop,
    log_gdppc = log(gdppc)
  ) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(1970:2022)) %>% 
  ungroup()

cat("Countries retained for Germany SCM (Non-EU):\n")
print(unique(df_clean$country))

# ==============================================================================
# 2. 核心：封装 SCM 逻辑 
# ==============================================================================
run_my_scm <- function(data, exclude_country = NULL, gen_placebos = TRUE) {
  
  if (!is.null(exclude_country)) {
    data <- data %>% filter(country != exclude_country)
  }
  
  scm_obj <- data %>%
    synthetic_control(
      outcome = log_gdppc, 
      unit = country,
      time = year,
      i_unit = treated_country,
      i_time = treat_year,
      generate_placebos = gen_placebos
    ) %>%
    generate_predictor(time_window = 1970:1998, 
                       Average_log_gdppc = mean(log_gdppc, na.rm=TRUE),
                       Gdp_size = mean(gdp_size, na.rm=TRUE),
                       Population_size = mean(pop, na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970:1998,
                       GDPpc_growth = mean((log_gdppc - lag(log_gdppc)), na.rm=TRUE),
                       Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970, log_GDPpc1970 = log_gdppc) %>%
    generate_predictor(time_window = 1998, log_GDPpc1998 = log_gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm_obj)
}

# ==============================================================================
# 3. 运行 Baseline SCM 并出图
# ==============================================================================
cat("\n== Running Baseline SCM ==\n")
baseline_scm <- run_my_scm(df_clean, exclude_country = NULL, gen_placebos = TRUE)

cat("\n== 1. Predictor Balance Table ==\n")
balance_tab <- baseline_scm %>% grab_balance_table()
write_csv(balance_tab, "SCM_Balance.csv")

cat("\n== 2. Significance (MSPE ratio) Table ==\n")
sig_tab <- baseline_scm %>% grab_significance()
write_csv(sig_tab, "SCM_Significance.csv")

cat("\n== 3. Donor Weights (Descending) ==\n")
donor_weights <- baseline_scm %>% grab_unit_weights() %>% arrange(desc(weight))
write_csv(donor_weights, "SCM_Weights.csv")

cat("\n== 4. Predictor Weights ==\n")
predictor_weights <- baseline_scm %>% grab_predictor_weights() %>% arrange(desc(weight))
write_csv(predictor_weights, "SCM_Predictor_Weights.csv")

# 保存 Baseline 图表 (使用默认绘图逻辑)
p_trends <- baseline_scm %>% plot_trends() + ggtitle("Germany vs. Synthetic Germany (Non-EU - log)")
ggsave("SCM_Trends.png", plot = p_trends, width = 8, height = 6)

p_weights <- baseline_scm %>% plot_weights() + ggtitle("Donor Weights (log)")
ggsave("SCM_Weights.png", plot = p_weights, width = 8, height = 6)

p_placebo <- baseline_scm %>% plot_placebos(prune = FALSE) + 
  ggtitle("Placebo Test (All Countries - log)") + theme_minimal()
ggsave("SCM_Placebo.png", plot = p_placebo, width = 8, height = 6)

p_mspe <- baseline_scm %>% plot_mspe_ratio() + ggtitle("MSPE Ratio (Significance - log)")
ggsave("SCM_MSPE_Ratio.png", plot = p_mspe, width = 8, height = 6)

# 保存 Baseline 逐年 gap
baseline_gaps <- baseline_scm %>% grab_synthetic_control() %>%
  mutate(gap = real_y - synth_y,
         pct_gap = (exp(real_y) - exp(synth_y)) / exp(synth_y) * 100)
write_csv(baseline_gaps %>% select(time_unit, real_y, synth_y, gap, pct_gap), "Baseline_YearByYear_Gap.csv")

# 计算 aggregated effect 并保存
post_gaps <- baseline_gaps %>% filter(time_unit >= treat_year)
pre_gaps  <- baseline_gaps %>% filter(time_unit < treat_year)

agg_effect <- tibble(
  Specification = "Germany Non-EU log GDP",
  Pre_Avg_Gap = mean(pre_gaps$gap),
  Post_Avg_Gap = mean(post_gaps$gap),
  Post_Cumul_Gap = sum(post_gaps$gap),
  Post_Avg_Pct = mean(post_gaps$pct_gap),
  Post_N_Years = nrow(post_gaps)
)
write_csv(agg_effect, "Aggregated_Treatment_Effect.csv")

# ==============================================================================
# 4. 自动化 Leave-One-Out (LOO) 并保存 LOO 权重
# ==============================================================================
cat("\n== Running Leave-One-Out (LOO) checks ==\n")
top_donors <- donor_weights %>% filter(weight > 0.01) %>% pull(unit)

loo_trends <- list()
loo_weights_list <- list()

# Initialize LOO weights table with baseline weights
loo_weights_tab <- donor_weights %>% select(unit, baseline_weight = weight)

for (donor in top_donors) {
  cat("Running LOO: Dropping", donor, "\n")
  temp_scm <- run_my_scm(df_clean, exclude_country = donor, gen_placebos = FALSE)
  
  # Save trend data for plotting
  trend_data <- temp_scm %>% grab_synthetic_control() %>%
    mutate(value = synth_y, type = paste("Drop", donor)) %>%
    select(time_unit, value, type)
  loo_trends[[donor]] <- trend_data
  
  # Save LOO weights for this run
  temp_weights <- temp_scm %>% grab_unit_weights() %>% select(unit, weight)
  names(temp_weights)[2] <- paste0("drop_", donor)
  loo_weights_tab <- loo_weights_tab %>% left_join(temp_weights, by = "unit")
}

# Write out the LOO weights table
write_csv(loo_weights_tab, "SCM_LOO_Weights.csv")

# Construct and save LOO plot
baseline_trend <- baseline_scm %>% grab_synthetic_control() %>%
  mutate(value = synth_y, type = "Baseline Synthetic") %>%
  select(time_unit, value, type)

real_trend <- baseline_scm %>% grab_synthetic_control() %>%
  mutate(value = real_y, type = "Real Germany") %>%
  select(time_unit, value, type)

all_loo_data <- bind_rows(loo_trends) %>% bind_rows(baseline_trend) %>% bind_rows(real_trend)

types <- unique(all_loo_data$type)
my_colors <- setNames(scales::hue_pal()(length(types)), types)
my_colors["Real Germany"] <- "black"

my_linewidths <- setNames(rep(0.6, length(types)), types)
my_linewidths["Real Germany"] <- 1.5

p_loo <- ggplot(all_loo_data, aes(x = time_unit, y = value, color = type, linewidth = type)) +
  geom_line(alpha = 0.8) +
  geom_vline(xintercept = treat_year, linetype = "dashed", color = "black") +
  scale_color_manual(values = my_colors) +
  scale_linewidth_manual(values = my_linewidths) +
  guides(linewidth = "none") +
  labs(title = "Leave-One-Out (LOO) Trend Check (log)", x = "Year", y = "log_gdppc", color = "Model") +
  theme_minimal() + theme(legend.position = "right")

ggsave("SCM_LOO.png", plot = p_loo, width = 10, height = 6)

cat("\n== Finished! All log outcomes & weights saved in noneu SCM result log/ folder. ==\n")
