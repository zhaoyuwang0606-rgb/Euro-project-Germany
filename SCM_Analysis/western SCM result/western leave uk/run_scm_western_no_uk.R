library(tidyverse)
library(readxl)
library(tidysynth)

# 0. 设定参数与定义 Donor Pool
treated_country <- "Germany"
treat_year <- 1999
pre_window <- 1970:1998

# 仅保留西方发达国家 (Western/OECD economies) 并且剔除 United Kingdom
western_countries <- c("Australia", "Canada", "Switzerland", "Denmark", 
                       "Iceland", "Norway", "New Zealand", 
                       "Sweden", "United States")

# 1. 数据清洗
cat("\n== Reading Data ==\n")
raw_data <- read_excel("../../../mpd2023_web.xlsx", sheet = "Full data")

df_clean <- raw_data %>%
  filter(country %in% c(treated_country, western_countries)) %>%
  filter(year >= 1970 & year <= 2022) %>%
  select(country, year, gdppc, pop) %>%
  mutate(gdp_size = gdppc * pop) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(1970:2022)) %>% 
  ungroup()

cat("Countries retained for Western OECD (No UK) check:\n")
print(unique(df_clean$country))

# 2. 封装 SCM 逻辑 
run_my_scm <- function(data, exclude_country = NULL) {
  if (!is.null(exclude_country)) {
    data <- data %>% filter(country != exclude_country)
  }
  scm_obj <- data %>%
    synthetic_control(outcome = gdppc, unit = country, time = year, i_unit = treated_country, i_time = treat_year, generate_placebos = is.null(exclude_country)) %>%
    generate_predictor(time_window = 1970:1998, Average_gdppc = mean(gdppc, na.rm=TRUE), Gdp_size = mean(gdp_size, na.rm=TRUE), Popluation_size = mean(pop, na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970:1998, GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE), Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
    generate_predictor(time_window = 1970, GDPpc1970 = gdppc) %>%
    generate_predictor(time_window = 1998, GDPpc1998 = gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  return(scm_obj)
}

# 3. 运行 Baseline SCM 并出图
cat("\n== Running Western (No UK) SCM ==\n")
baseline_scm <- run_my_scm(df_clean, exclude_country = NULL)

write_csv(baseline_scm %>% grab_balance_table(), "SCM_Balance.csv")
write_csv(baseline_scm %>% grab_significance(), "SCM_Significance.csv")
donor_weights <- baseline_scm %>% grab_unit_weights() %>% arrange(desc(weight))
write_csv(donor_weights, "SCM_Weights.csv")

ggsave("SCM_Trends.png", plot = baseline_scm %>% plot_trends() + ggtitle("Germany vs. Synth (Western OECD - No UK)"), width = 8, height = 6)
ggsave("SCM_Weights.png", plot = baseline_scm %>% plot_weights() + ggtitle("Donor Weights"), width = 8, height = 6)
ggsave("SCM_Placebo.png", plot = baseline_scm %>% plot_placebos(prune = FALSE) + ggtitle("Placebo Test"), width = 8, height = 6)
ggsave("SCM_MSPE_Ratio.png", plot = baseline_scm %>% plot_mspe_ratio() + ggtitle("MSPE Ratio"), width = 8, height = 6)

# 4. 自动化 Leave-One-Out (LOO)
cat("\n== Running Leave-One-Out (LOO) checks ==\n")
top_donors <- donor_weights %>% filter(weight > 0.01) %>% pull(unit)
loo_trends <- list()
for (donor in top_donors) {
  temp_scm <- run_my_scm(df_clean, exclude_country = donor)
  loo_trends[[donor]] <- temp_scm %>% grab_synthetic_control() %>% mutate(value = synth_y, type = paste("Drop", donor)) %>% select(time_unit, value, type)
}
baseline_trend <- baseline_scm %>% grab_synthetic_control() %>% mutate(value = synth_y, type = "Baseline Synthetic") %>% select(time_unit, value, type)
real_trend <- baseline_scm %>% grab_synthetic_control() %>% mutate(value = real_y, type = "Real Germany") %>% select(time_unit, value, type)

all_loo_data <- bind_rows(loo_trends) %>% bind_rows(baseline_trend) %>% bind_rows(real_trend)
types <- unique(all_loo_data$type)
my_colors <- setNames(scales::hue_pal()(length(types)), types)
my_colors["Real Germany"] <- "black"
my_linewidths <- setNames(rep(0.6, length(types)), types)
my_linewidths["Real Germany"] <- 1.5

p_loo <- ggplot(all_loo_data, aes(x = time_unit, y = value, color = type, linewidth = type)) +
  geom_line(alpha = 0.8) +
  geom_vline(xintercept = treat_year, linetype = "dashed", color = "black") +
  scale_color_manual(values = my_colors) + scale_linewidth_manual(values = my_linewidths) +
  guides(linewidth = "none") + labs(title = "LOO Trend Check (No UK)", x = "Year", y = "GDPpc", color = "Model") + theme_minimal()

ggsave("SCM_LOO.png", plot = p_loo, width = 10, height = 6)
cat("\n== Finished! All results saved in current folder. ==\n")
