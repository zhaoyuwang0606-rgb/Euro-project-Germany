library(tidyverse)
library(readxl)
library(tidysynth)

# ==============================================================================
# 0. 设定参数与定义 Donor Pool
# ==============================================================================
donor_countries <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark", 
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", 
  "Japan", "Republic of Korea", "Norway", "New Zealand", "Poland", 
  "Qatar", "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China", "United States"
)

treated_country <- "Germany"
treat_year <- 1999
pre_window <- 1970:1998

# ==============================================================================
# 1. 数据清洗
# ==============================================================================
cat("\n== Reading Data ==\n")
# 读取上一层文件夹中的 Excel 数据
raw_data <- read_excel("../../mpd2023_web.xlsx", sheet = "Full data")

df_clean <- raw_data %>%
  filter(country %in% c(treated_country, donor_countries)) %>%
  filter(year >= 1970 & year <= 2022) %>%
  # 提取所需的列，计算 Gdp_size (gdppc * pop)
  select(country, year, gdppc, pop) %>%
  mutate(gdp_size = gdppc * pop) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(1970:2022)) %>% # 强制要求年份必须完整
  ungroup()

# 检查一下哪些国家被保留下来了
cat("Countries with full data (1970-2022):\n")
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
      outcome = gdppc, # 已改成绝对值 gdppc
      unit = country,
      time = year,
      i_unit = treated_country,
      i_time = treat_year,
      generate_placebos = is.null(exclude_country)
    ) %>%
    
    # --------------------------------------------------------------------------
    # Predictors 设定 (根据你的截图要求)
    # --------------------------------------------------------------------------
    # 1. 均值类 (Average_gdppc, Gdp_size, Popluation_size)
    generate_predictor(time_window = 1970:1998, 
                       Average_gdppc = mean(gdppc, na.rm=TRUE),
                       Gdp_size = mean(gdp_size, na.rm=TRUE),
                       Popluation_size = mean(pop, na.rm=TRUE)) %>%
    
    # 2. 增长率类 (GDPpc_growth, Population_growth)
    # 注意：计算增长率时，第一年会有 NA，na.rm=TRUE 可以忽略它
    generate_predictor(time_window = 1970:1998,
                       GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                       Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
                       
    # 3. 特定年份截面数据
    generate_predictor(time_window = 1970, GDPpc1970 = gdppc) %>%
    generate_predictor(time_window = 1998, GDPpc1998 = gdppc) %>%
    # --------------------------------------------------------------------------
    
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
p_trends <- baseline_scm %>% plot_trends() + ggtitle("Germany vs. Synthetic Germany (GDP per capita)")
ggsave("SCM_Trends.png", plot = p_trends, width = 8, height = 6)

p_weights <- baseline_scm %>% plot_weights() + ggtitle("Donor Weights")
ggsave("SCM_Weights.png", plot = p_weights, width = 8, height = 6)

# prune = FALSE 打印所有国家 (注意看 Y 轴比例会被个别极端的国家撑开)
p_placebo <- baseline_scm %>% plot_placebos(prune = FALSE) + 
  ggtitle("Placebo Test (All Countries)") + theme_minimal()
ggsave("SCM_Placebo.png", plot = p_placebo, width = 8, height = 6)

# 你要的 MSPE Ratio 柱状图 (Bar graph)
p_mspe <- baseline_scm %>% plot_mspe_ratio() + ggtitle("MSPE Ratio (Significance)")
ggsave("SCM_MSPE_Ratio.png", plot = p_mspe, width = 8, height = 6)

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

# 动态设定颜色和线条粗细，让 Real Germany 固定为纯黑色+加粗，其他的为彩色+细线
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
  guides(linewidth = "none") + # 隐藏线条粗细的图例，保持干净
  labs(title = "Leave-One-Out (LOO) Trend Check", x = "Year", y = "GDPpc", color = "Model") +
  theme_minimal() + theme(legend.position = "right")

ggsave("SCM_LOO.png", plot = p_loo, width = 10, height = 6)
cat("\n== Finished! All results saved in current folder. ==\n")
