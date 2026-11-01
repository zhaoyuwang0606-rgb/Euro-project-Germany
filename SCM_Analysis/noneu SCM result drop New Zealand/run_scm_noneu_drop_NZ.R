library(tidyverse)
library(readxl)
library(tidysynth)

# ==============================================================================
# 0. 设定参数与定义 Donor Pool (New Zealand Excluded)
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

# 因为脚本在 "noneu SCM result drop New Zealand" 文件夹里，所以需要用 ../ 返回上一层找数据
raw_data <- read_excel("../../mpd2023_web.xlsx", sheet = "Full data")

df_clean <- raw_data %>%
  filter(country %in% c(treated_country, donor_countries)) %>%
  filter(!country %in% spillover_and_japan) %>%
  filter(country != "New Zealand") %>% # Drop New Zealand from the donor pool from the start
  filter(year >= 1970 & year <= 2022) %>%
  select(country, year, gdppc, pop) %>%
  mutate(gdp_size = gdppc * pop) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(1970:2022)) %>% 
  ungroup()

cat("Countries retained for Non-EU SCM check (excluding New Zealand):\n")
print(unique(df_clean$country))

# ==============================================================================
# 2. 核心：封装 SCM 逻辑 
# ==============================================================================
run_my_scm <- function(data, exclude_country = NULL) {
  
  if (!is.null(exclude_country)) {
    data <- data %>% filter(country != exclude_country)
  }
  
  scm_obj <- data %>%
    synthetic_control(
      outcome = gdppc, 
      unit = country,
      time = year,
      i_unit = treated_country,
      i_time = treat_year,
      generate_placebos = is.null(exclude_country)
    ) %>%
    generate_predictor(time_window = 1970:1998, 
                       Average_gdppc = mean(gdppc, na.rm=TRUE),
                       Gdp_size = mean(gdp_size, na.rm=TRUE),
                       Popluation_size = mean(pop, na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970:1998,
                       GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                       Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970, GDPpc1970 = gdppc) %>%
    generate_predictor(time_window = 1998, GDPpc1998 = gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm_obj)
}

# ==============================================================================
# 3. 运行 Baseline SCM 并出图
# ==============================================================================
cat("\n== Running Baseline SCM ==\n")
baseline_scm <- run_my_scm(df_clean, exclude_country = NULL)

cat("\n== 1. Predictor Balance Table ==\n")
balance_tab <- baseline_scm %>% grab_balance_table()
write_csv(balance_tab, "SCM_Balance.csv")

cat("\n== 2. Significance (MSPE ratio) Table ==\n")
sig_tab <- baseline_scm %>% grab_significance()
write_csv(sig_tab, "SCM_Significance.csv")

cat("\n== 3. Donor Weights (Descending) ==\n")
donor_weights <- baseline_scm %>% grab_unit_weights() %>% arrange(desc(weight))
write_csv(donor_weights, "SCM_Weights.csv")

# 保存图表到当前文件夹
p_trends <- baseline_scm %>% plot_trends() + ggtitle("Germany vs. Synth (Non-EU, Excl. NZ)")
ggsave("SCM_Trends.png", plot = p_trends, width = 8, height = 6)

p_weights <- baseline_scm %>% plot_weights() + ggtitle("Donor Weights (Excl. NZ)")
ggsave("SCM_Weights.png", plot = p_weights, width = 8, height = 6)

p_placebo <- baseline_scm %>% plot_placebos(prune = FALSE) + 
  ggtitle("Placebo Test (All Countries, Excl. NZ)") + theme_minimal()
ggsave("SCM_Placebo.png", plot = p_placebo, width = 8, height = 6)

p_mspe <- baseline_scm %>% plot_mspe_ratio() + ggtitle("MSPE Ratio (Excl. NZ)")
ggsave("SCM_MSPE_Ratio.png", plot = p_mspe, width = 8, height = 6)

# 也保存 Baseline 的逐年 gap
baseline_gaps <- baseline_scm %>% grab_synthetic_control() %>%
  mutate(gap = real_y - synth_y,
         pct_gap = gap / synth_y * 100)
write_csv(baseline_gaps %>% select(time_unit, real_y, synth_y, gap, pct_gap), "Baseline_YearByYear_Gap.csv")

# 计算 aggregated effect 并保存
post_gaps <- baseline_gaps %>% filter(time_unit >= treat_year)
pre_gaps  <- baseline_gaps %>% filter(time_unit < treat_year)

agg_effect <- tibble(
  Specification = "Drop New Zealand",
  Pre_Avg_Gap = mean(pre_gaps$gap),
  Post_Avg_Gap = mean(post_gaps$gap),
  Post_Cumul_Gap = sum(post_gaps$gap),
  Post_Avg_Pct = mean(post_gaps$pct_gap),
  Post_N_Years = nrow(post_gaps)
)
write_csv(agg_effect, "Aggregated_Treatment_Effect.csv")

cat("\n== Plots saved as PNG files. ==\n")

# ==============================================================================
# 4. 自动化 Leave-One-Out (LOO)
# ==============================================================================
cat("\n== Running Leave-One-Out (LOO) checks ==\n")
top_donors <- donor_weights %>% filter(weight > 0.01) %>% pull(unit)

loo_trends <- list()
for (donor in top_donors) {
  cat("Running LOO: Dropping", donor, "\n")
  temp_scm <- run_my_scm(df_clean, exclude_country = donor)
  trend_data <- temp_scm %>% grab_synthetic_control() %>%
    mutate(value = synth_y, type = paste("Drop", donor)) %>%
    select(time_unit, value, type)
  loo_trends[[donor]] <- trend_data
}

baseline_trend <- baseline_scm %>% grab_synthetic_control() %>%
  mutate(value = synth_y, type = "Baseline Synthetic") %>%
  select(time_unit, value, type)

real_trend <- baseline_scm %>% grab_synthetic_control() %>%
  mutate(value = real_y, type = "Real Germany") %>%
  select(time_unit, value, type)

all_loo_data <- bind_rows(loo_trends) %>% bind_rows(baseline_trend) %>% bind_rows(real_trend)

# 动态设定颜色和线条粗细
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
  labs(title = "LOO Trend Check (Excl. NZ)", x = "Year", y = "GDPpc", color = "Model") +
  theme_minimal() + theme(legend.position = "right")

ggsave("SCM_LOO.png", plot = p_loo, width = 10, height = 6)
cat("\n== Finished! All results saved in current folder. ==\n")
