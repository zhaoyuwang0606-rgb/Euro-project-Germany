# =========================================================================
# noneu_Start1970_End1998/run_scm.R
# Self-contained SCM script for Non-EU Donor Pool (1970-1998)
# Input: ../../mpd2023_web.xlsx
# Output: Current directory (.)
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(tidysynth)
})

cat("=== Running noneu_Start1970_End1998 SCM ===\n")

raw_data <- read_excel("../../mpd2023_web.xlsx", sheet = "Full data")

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

df_s1 <- raw_data %>% 
  filter(country %in% c(treated_country, all_donors_full)) %>% 
  filter(!country %in% spillover_and_japan) %>%
  filter(year >= 1970 & year <= 1998) %>%
  select(country, year, gdppc, pop) %>%
  mutate(gdp_size = gdppc * pop) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(1970:1998)) %>%
  ungroup()

scm_obj <- df_s1 %>%
  synthetic_control(
    outcome = gdppc, unit = country, time = year,
    i_unit = treated_country, i_time = 1989, 
    generate_placebos = TRUE
  ) %>%
  generate_predictor(time_window = 1970:1988,
                     Average_gdppc = mean(gdppc, na.rm=TRUE),
                     Gdp_size = mean(gdp_size, na.rm=TRUE),
                     Popluation_size = mean(pop, na.rm=TRUE)) %>%
  generate_predictor(time_window = 1970:1988,
                     GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                     Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
  generate_predictor(time_window = 1970, GDPpc1970 = gdppc) %>%
  generate_predictor(time_window = 1988, GDPpc1988 = gdppc) %>%
  generate_weights(optimization_window = 1970:1988) %>%
  generate_control()

write_csv(scm_obj %>% grab_unit_weights() %>% arrange(desc(weight)), "Weights.csv")
write_csv(scm_obj %>% grab_balance_table(), "Balance.csv")

sc_trend <- scm_obj %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y)
write_csv(sc_trend, "YearByYear_Gap.csv")

cat("Saved noneu_Start1970_End1998 SCM outputs successfully!\n")
