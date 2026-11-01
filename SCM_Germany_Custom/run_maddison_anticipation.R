# =========================================================================
# SCM_Germany_Custom/run_maddison_anticipation.R
# Maddison Anticipation SCM analysis
# =========================================================================

library(tidyverse)
library(readxl)
library(tidysynth)

main_dir <- "."
out_dir <- file.path(main_dir, "Anticipation_Maddison_GDP")
dir.create(out_dir, showWarnings = FALSE)

raw_data <- read_excel("../mpd2023_web.xlsx", sheet = "Full data")

all_donors_full <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark", 
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", 
  "Japan", "Republic of Korea", "Norway", "New Zealand", "Poland", 
  "Qatar", "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China", "United States"
)
spillover_and_japan <- c("Switzerland", "Denmark", "United Kingdom", "Iceland", 
                         "Norway", "Poland", "Sweden", "Japan",
                         "Saudi Arabia", "Qatar", "Israel")
treated_country <- "Germany"
start_year <- 1990
end_year <- 2022

df <- raw_data %>% 
  filter(country %in% c(treated_country, all_donors_full)) %>% 
  filter(!country %in% spillover_and_japan) %>%
  filter(year >= start_year & year <= end_year) %>%
  select(country, year, gdppc, pop) %>%
  mutate(gdp_size = gdppc * pop) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(start_year:end_year)) %>%
  ungroup()

run_scm <- function(data, treat_year, start_year, exclude_country = NULL) {
  if (!is.null(exclude_country)) {
    data <- data %>% filter(country != exclude_country)
  }
  pre_window <- start_year:(treat_year - 1)
  
  scm_obj <- data %>%
    synthetic_control(
      outcome = gdppc, unit = country, time = year,
      i_unit = treated_country, i_time = treat_year,
      generate_placebos = is.null(exclude_country)
    ) %>%
    generate_predictor(time_window = pre_window,
                       Average_gdppc = mean(gdppc, na.rm=TRUE),
                       Gdp_size = mean(gdp_size, na.rm=TRUE),
                       Popluation_size = mean(pop, na.rm=TRUE)) %>%
    generate_predictor(time_window = pre_window,
                       GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                       Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
    generate_predictor(time_window = start_year, GDPpc_start = gdppc) %>%
    generate_predictor(time_window = (treat_year - 1), GDPpc_pre_treat = gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
  
  return(scm_obj)
}

# Run Anticipation windows
anticipation_years <- 1992:1998
results_list <- list()

for (ty in anticipation_years) {
  cat(sprintf("Running Anticipation Year: %d\n", ty))
  sub_dir <- file.path(out_dir, sprintf("Anticipation_%d", ty))
  dir.create(sub_dir, showWarnings = FALSE)
  
  scm_fit <- run_scm(df, ty, start_year)
  
  write_csv(scm_fit %>% grab_balance_table(), file.path(sub_dir, "SCM_Balance.csv"))
  write_csv(scm_fit %>% grab_significance(), file.path(sub_dir, "SCM_Significance.csv"))
  write_csv(scm_fit %>% grab_unit_weights(), file.path(sub_dir, "SCM_Weights.csv"))
  
  gaps <- scm_fit %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y)
  write_csv(gaps, file.path(sub_dir, "YearByYear_Gap.csv"))
  
  ggsave(file.path(sub_dir, "Trends.png"), scm_fit %>% plot_trends(), width = 8, height = 5)
  ggsave(file.path(sub_dir, "Placebo.png"), scm_fit %>% plot_placebos(prune = FALSE), width = 8, height = 5)
}

cat("=== Maddison Anticipation SCM completed! ===\n")
