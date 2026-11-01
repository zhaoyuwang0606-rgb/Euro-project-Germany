# =========================================================================
# noneu_Pre1970_2001_End2022/run_scm.R
# Self-contained SCM script for Non-EU Donor Pool (1970-2022)
# Input: ../../mpd2023_web.xlsx
# Output: Current directory (.)
# =========================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(tidysynth)
})

cat("=== Running noneu_Pre1970_2001_End2022 SCM ===\n")

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

df <- raw_data %>% 
  filter(country %in% c(treated_country, all_donors_full)) %>% 
  filter(!country %in% spillover_and_japan) %>%
  filter(year >= 1970 & year <= 2022) %>%
  select(country, year, gdppc, pop) %>%
  mutate(gdp_size = gdppc * pop) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(1970:2022)) %>%
  ungroup() %>% arrange(country, year)

scm_obj <- df %>%
  synthetic_control(
    outcome = gdppc, unit = country, time = year,
    i_unit = treated_country, i_time = 2002, 
    generate_placebos = TRUE
  ) %>%
  generate_predictor(time_window = 1970:2001,
                     Average_gdppc = mean(gdppc, na.rm=TRUE),
                     Gdp_size = mean(gdp_size, na.rm=TRUE),
                     Popluation_size = mean(pop, na.rm=TRUE)) %>%
  generate_predictor(time_window = 1970:2001,
                     GDPpc_growth = mean((gdppc - lag(gdppc))/lag(gdppc), na.rm=TRUE),
                     Population_growth = mean((pop - lag(pop))/lag(pop), na.rm=TRUE)) %>%
  generate_predictor(time_window = 1970, GDPpc1970 = gdppc) %>%
  generate_predictor(time_window = 2001, GDPpc2001 = gdppc) %>%
  generate_weights(optimization_window = 1970:2001) %>%
  generate_control()

write_csv(scm_obj %>% grab_unit_weights() %>% arrange(desc(weight)), "Weights.csv")
write_csv(scm_obj %>% grab_balance_table(), "Balance.csv")

sc_trend <- scm_obj %>% grab_synthetic_control() %>% mutate(gap = real_y - synth_y)
write_csv(sc_trend, "YearByYear_Gap.csv")

# Explicitly fit on 1970-2001; 2007 is a visual reference only.
stopifnot(min(sc_trend$time_unit) == 1970, max(sc_trend$time_unit) == 2022,
          nrow(sc_trend) == 53, all(is.finite(sc_trend$synth_y)))
trend_plot <- scm_obj %>% plot_trends() +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "#2474B5", linewidth = 0.7) +
  annotate("text", x = 2007.5, y = Inf, label = "2007", color = "#2474B5",
           hjust = 0, vjust = 1.6, size = 4) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2007, 2010, 2022)) +
  labs(title = "Germany vs Synthetic Germany",
       subtitle = "Non-EU donor pool | Pre-treatment: 1970-2001 | Post-treatment: 2002-2022",
       x = "Year", y = "GDP per capita",
       caption = "Black dashed line: 2002 (post-treatment begins). Blue dashed line: 2007 reference.")
ggsave("SCM_Trends_2007.png", trend_plot, width = 11, height = 7, dpi = 300, bg = "white")
saveRDS(scm_obj, "scm_fit.rds")
cat("Saved noneu_Pre1970_2001_End2022 SCM outputs successfully!\n")

