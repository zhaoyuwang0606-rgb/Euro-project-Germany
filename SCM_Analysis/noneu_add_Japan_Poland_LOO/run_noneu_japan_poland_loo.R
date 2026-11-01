suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(tidysynth)
})

# Portable path resolution
find_project_root <- function() {
  candidates <- c(
    Sys.getenv("PROJECT_ROOT"),
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "../..")
  )
  for (cand in candidates) {
    if (nzchar(cand) && file.exists(file.path(cand, "mpd2023_web.xlsx"))) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  # Fallback to script location if called via Rscript
  args <- commandArgs(FALSE)
  script_arg <- grep("^--file=", args, value = TRUE)
  if (length(script_arg)) {
    s_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/"))
    return(normalizePath(file.path(s_dir, "..", ".."), winslash = "/"))
  }
  return(getwd())
}
project_root <- find_project_root()
out_dir <- Sys.getenv("REPL_LOO_JP_PL_DIR")
if (!nzchar(out_dir)) {
  out_dir <- file.path(project_root, "SCM_Analysis", "noneu_add_Japan_Poland_LOO")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

treated_country <- "Germany"
treat_year <- 1999
pre_window <- 1970:1998
analysis_years <- 1970:2022

# Keep the original Non-EU donor pool and its exclusions, except re-include
# Japan and Poland for this sensitivity analysis.
donor_countries <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark",
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", "Japan",
  "Republic of Korea", "Norway", "New Zealand", "Poland", "Qatar",
  "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China",
  "United States"
)
excluded_other_donors <- c(
  "Switzerland", "Denmark", "United Kingdom", "Iceland", "Norway",
  "Sweden", "Saudi Arabia", "Qatar", "Israel"
)
loo_donors <- c("Japan", "Poland")

data_path <- file.path(project_root, "mpd2023_web.xlsx")
if (!file.exists(data_path)) stop("Input data not found: ", data_path)

raw_data <- read_excel(data_path, sheet = "Full data")
df_clean <- raw_data %>%
  filter(country %in% c(treated_country, donor_countries)) %>%
  filter(!country %in% excluded_other_donors) %>%
  filter(year >= min(analysis_years), year <= max(analysis_years)) %>%
  select(country, year, gdppc, pop) %>%
  mutate(gdp_size = gdppc * pop) %>%
  drop_na() %>%
  group_by(country) %>%
  filter(n() == length(analysis_years)) %>%
  ungroup()

retained_countries <- sort(unique(df_clean$country))
if (!all(c(treated_country, loo_donors) %in% retained_countries)) {
  stop("Germany, Japan, or Poland is absent from the complete-case analysis panel.")
}

run_my_scm <- function(data, exclude_country = NULL) {
  if (!is.null(exclude_country)) data <- data %>% filter(country != exclude_country)

  data %>%
    synthetic_control(
      outcome = gdppc,
      unit = country,
      time = year,
      i_unit = treated_country,
      i_time = treat_year,
      generate_placebos = is.null(exclude_country)
    ) %>%
    generate_predictor(
      time_window = 1970:1998,
      Average_gdppc = mean(gdppc, na.rm = TRUE),
      Gdp_size = mean(gdp_size, na.rm = TRUE),
      Popluation_size = mean(pop, na.rm = TRUE)
    ) %>%
    generate_predictor(
      time_window = 1970:1998,
      GDPpc_growth = mean((gdppc - lag(gdppc)) / lag(gdppc), na.rm = TRUE),
      Population_growth = mean((pop - lag(pop)) / lag(pop), na.rm = TRUE)
    ) %>%
    generate_predictor(time_window = 1970, GDPpc1970 = gdppc) %>%
    generate_predictor(time_window = 1998, GDPpc1998 = gdppc) %>%
    generate_weights(optimization_window = pre_window) %>%
    generate_control()
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cat("Retained Non-EU donors, with Japan and Poland added:\n")
cat(paste(setdiff(retained_countries, treated_country), collapse = ", "), "\n")

baseline_scm <- run_my_scm(df_clean)
donor_weights <- baseline_scm %>% grab_unit_weights() %>% arrange(desc(weight))
write_csv(donor_weights, file.path(out_dir, "SCM_Weights.csv"))
write_csv(baseline_scm %>% grab_balance_table(), file.path(out_dir, "SCM_Balance.csv"))

baseline_path <- baseline_scm %>%
  grab_synthetic_control() %>%
  transmute(
    series = "Baseline Synthetic",
    time_unit,
    real_y,
    synth_y,
    gap = real_y - synth_y,
    pct_gap = 100 * (real_y - synth_y) / synth_y
  )

loo_paths <- map_dfr(loo_donors, function(donor) {
  cat("Fitting LOO, dropping ", donor, "...\n", sep = "")
  fit <- run_my_scm(df_clean, exclude_country = donor)
  fit %>%
    grab_synthetic_control() %>%
    transmute(
      series = paste("Drop", donor),
      time_unit,
      real_y,
      synth_y,
      gap = real_y - synth_y,
      pct_gap = 100 * (real_y - synth_y) / synth_y
    )
})

actual_path <- baseline_path %>%
  transmute(series = "Real Germany", time_unit, real_y, synth_y = real_y,
            gap = 0, pct_gap = 0)
all_paths <- bind_rows(actual_path, baseline_path, loo_paths) %>% arrange(series, time_unit)
write_csv(all_paths, file.path(out_dir, "LOO_YearByYear.csv"))

metrics <- all_paths %>%
  filter(series != "Real Germany") %>%
  group_by(series) %>%
  summarise(
    Pre_RMSPE = sqrt(mean((real_y[time_unit < treat_year] - synth_y[time_unit < treat_year])^2)),
    Post_RMSPE = sqrt(mean((real_y[time_unit >= treat_year] - synth_y[time_unit >= treat_year])^2)),
    Mean_Gap_1999_2007 = mean(gap[time_unit >= 1999 & time_unit <= 2007]),
    Gap_2007 = gap[time_unit == 2007],
    .groups = "drop"
  )
write_csv(metrics, file.path(out_dir, "LOO_Summary.csv"))

plot_data <- all_paths %>% filter(series != "Real Germany") %>%
  select(series, time_unit, value = synth_y) %>%
  bind_rows(all_paths %>% filter(series == "Real Germany") %>%
              transmute(series, time_unit, value = real_y))

p_loo <- ggplot(plot_data, aes(x = time_unit, y = value, color = series, linewidth = series)) +
  geom_line(alpha = 0.9) +
  geom_vline(xintercept = treat_year, linetype = "dashed", color = "black") +
  scale_color_manual(values = c(
    "Real Germany" = "black",
    "Baseline Synthetic" = "grey55",
    "Drop Japan" = "#D55E00",
    "Drop Poland" = "#0072B2"
  )) +
  scale_linewidth_manual(values = c(
    "Real Germany" = 1.2,
    "Baseline Synthetic" = 0.9,
    "Drop Japan" = 0.7,
    "Drop Poland" = 0.7
  )) +
  guides(linewidth = "none") +
  labs(
    title = "Non-EU GDP per capita SCM: Leave Japan and Poland Out",
    subtitle = "Treatment year: 1999 | Pretreatment: 1970-1998",
    x = "Year", y = "GDP per capita", color = "Specification"
  ) +
  theme_minimal() + theme(legend.position = "right")
ggsave(file.path(out_dir, "SCM_LOO_Japan_Poland.png"), p_loo,
       width = 10, height = 6, dpi = 300)

config <- c(
  "Specification: Original Non-EU GDPpc Tidysynth settings, with Japan and Poland re-included.",
  "Treatment year: 1999; sample: 1970-2022; pretreatment/optimization window: 1970-1998.",
  "Outcome: gdppc. Predictors and complete-case country/year rules match SCM_Analysis/noneu SCM result/run_scm_noneu.R.",
  "LOO runs: drop Japan and drop Poland. Other original exclusions are retained.",
  paste("Donors in fitted panel:", paste(setdiff(retained_countries, treated_country), collapse = ", "))
)
writeLines(config, file.path(out_dir, "README.txt"))
cat("Saved LOO outputs in: ", out_dir, "\n", sep = "")
print(metrics)
