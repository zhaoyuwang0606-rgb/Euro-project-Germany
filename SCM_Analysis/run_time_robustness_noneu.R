library(tidyverse)
library(readxl)
library(tidysynth)

# ==============================================================================
# 0. 设定参数与定义 Donor Pool (Clean Non-EU / Non-Japan / Non-Oil Pool)
# ==============================================================================
donor_countries <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark", 
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", 
  "Japan", "Republic of Korea", "Norway", "New Zealand", "Poland", 
  "Qatar", "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China", "United States"
)

# 按照你要求的，采用与 noneu SCM 相同的极致干净池子
spillover_and_japan <- c("Switzerland", "Denmark", "United Kingdom", "Iceland", 
                         "Norway", "Poland", "Sweden", "Japan",
                         "Saudi Arabia", "Qatar", "Israel")

treated_country <- "Germany"

cat("\n== Reading Master Data ==\n")
raw_data <- read_excel("../mpd2023_web.xlsx", sheet = "Full data")

# ==============================================================================
# 1. 定义实验工厂函数 (Experiment Factory)
# ==============================================================================
run_experiment <- function(start_year, treat_year, output_dir) {
  cat("\n======================================================\n")
  cat(sprintf("Running Experiment: Start=%d, Treat=%d\n", start_year, treat_year))
  cat("======================================================\n")
  
  dir.create(output_dir, showWarnings = FALSE)
  pre_window <- start_year:(treat_year - 1)
  
  # 数据筛选
  df_exp <- raw_data %>%
    filter(country %in% c(treated_country, donor_countries)) %>%
    filter(!country %in% spillover_and_japan) %>%
    filter(year >= start_year & year <= 2022) %>%
    select(country, year, gdppc, pop) %>%
    mutate(gdp_size = gdppc * pop) %>%
    drop_na() %>%
    group_by(country) %>%
    filter(n() == length(start_year:2022)) %>% 
    ungroup()
  
  run_my_scm <- function(data, exclude_country = NULL) {
    if (!is.null(exclude_country)) {
      data <- data %>% filter(country != exclude_country)
    }
    scm_obj <- data %>%
      synthetic_control(
        outcome = gdppc, unit = country, time = year, 
        i_unit = treated_country, i_time = treat_year, 
        generate_placebos = is.null(exclude_country)
      ) %>%
      generate_predictor(time_window = pre_window, Average_gdppc = mean(gdppc, na.rm=TRUE), Gdp_size = mean(gdp_size, na.rm=TRUE), Popluation_size = mean(pop, na.rm=TRUE)) %>%
      generate_predictor(time_window = pre_window, GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE)) %>%
      generate_predictor(time_window = start_year, GDPpc_start = gdppc) %>%
      generate_predictor(time_window = (treat_year - 1), GDPpc_pre_treat = gdppc) %>%
      generate_weights(optimization_window = pre_window) %>%
      generate_control()
    return(scm_obj)
  }
  
  # 运行 Baseline
  baseline_scm <- run_my_scm(df_exp, exclude_country = NULL)
  
  # 输出 CSV
  write_csv(baseline_scm %>% grab_balance_table(), file.path(output_dir, "SCM_Balance.csv"))
  write_csv(baseline_scm %>% grab_significance(), file.path(output_dir, "SCM_Significance.csv"))
  donor_weights <- baseline_scm %>% grab_unit_weights() %>% arrange(desc(weight))
  write_csv(donor_weights, file.path(output_dir, "SCM_Weights.csv"))
  
  # 输出图表
  ggsave(file.path(output_dir, "SCM_Trends.png"), plot = baseline_scm %>% plot_trends() + ggtitle(sprintf("Germany vs Synth (Start %d, Treat %d)", start_year, treat_year)), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_Weights.png"), plot = baseline_scm %>% plot_weights() + ggtitle("Donor Weights"), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_Placebo.png"), plot = baseline_scm %>% plot_placebos(prune = FALSE) + ggtitle("Placebo Test"), width = 8, height = 6)
  ggsave(file.path(output_dir, "SCM_MSPE_Ratio.png"), plot = baseline_scm %>% plot_mspe_ratio() + ggtitle("MSPE Ratio"), width = 8, height = 6)
  
  # 自动化 LOO
  cat(sprintf("Running LOO for %s\n", output_dir))
  top_donors <- donor_weights %>% filter(weight > 0.01) %>% pull(unit)
  loo_trends <- list()
  for (donor in top_donors) {
    temp_scm <- run_my_scm(df_exp, exclude_country = donor)
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
    guides(linewidth = "none") + labs(title = "LOO Trend Check", x = "Year", y = "GDPpc", color = "Model") + theme_minimal()
  ggsave(file.path(output_dir, "SCM_LOO.png"), plot = p_loo, width = 10, height = 6)
  
  cat(sprintf("Experiment saved in %s\n", output_dir))
}

# ==============================================================================
# 2. 执行指定的三个实验 (noneu Pool)
# ==============================================================================

# 实验1：只做 1950 延长时间窗口 (Start: 1950, Treat: 1999)
run_experiment(start_year = 1950, treat_year = 1999, output_dir = "TimeRob_noneu_Start1950_Treat1999")

# 实验2：2002 现金流通 (Start: 1970, Treat: 2002)
run_experiment(start_year = 1970, treat_year = 2002, output_dir = "TimeRob_noneu_Start1970_Treat2002")

# 实验3：1990 In-Time Placebo (Start: 1970, Treat: 1990)
run_experiment(start_year = 1970, treat_year = 1990, output_dir = "TimeRob_noneu_Start1970_Treat1990")

cat("\n== 🚀 ALL TIME ROBUSTNESS EXPERIMENTS COMPLETED ==\n")
