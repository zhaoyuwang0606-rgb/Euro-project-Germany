library(tidyverse)
library(readxl)
library(fixest)

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
  return(getwd())
}
proj_root <- find_project_root()
output_dir <- Sys.getenv("REPL_OUTPUT_DIR")
if (!nzchar(output_dir)) {
  output_dir <- file.path(proj_root, "results_did_to_2007")
}
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Read Master Data
raw_data <- read_excel(file.path(proj_root, "mpd2023_web.xlsx"), sheet = "Full data")

# Define pools
donor_countries <- c(
  "United Arab Emirates", "Australia", "Canada", "Switzerland", "Denmark", 
  "United Kingdom", "China, Hong Kong SAR", "Iceland", "Israel", 
  "Japan", "Republic of Korea", "Norway", "New Zealand", "Poland", 
  "Qatar", "Saudi Arabia", "Singapore", "Sweden", "Taiwan, Province of China", "United States"
)

spillover_and_japan <- c("Switzerland", "Denmark", "United Kingdom", "Iceland", 
                         "Norway", "Poland", "Sweden", "Japan",
                         "Saudi Arabia", "Qatar", "Israel")

western_countries <- c("Australia", "Canada", "Switzerland", "Denmark", 
                       "United Kingdom", "Iceland", "Norway", "New Zealand", 
                       "Sweden", "United States")

treated_country <- "Germany"
treat_year <- 1999

# Helpers to prepare data
get_did_data <- function(countries) {
  raw_data %>%
    filter(country %in% countries) %>%
    filter(year >= 1970 & year <= 2007) %>% # RESTRICT TO 2007
    select(country, year, gdppc) %>%
    drop_na() %>%
    group_by(country) %>%
    filter(n() == length(1970:2007)) %>%
    ungroup() %>%
    mutate(
      treated = ifelse(country == treated_country, 1, 0),
      post = ifelse(year >= treat_year, 1, 0),
      did = treated * post,
      log_gdppc = log(gdppc)
    )
}

# Run regressions for each pool
clean_data <- get_did_data(c(treated_country, setdiff(donor_countries, spillover_and_japan)))
western_data <- get_did_data(c(treated_country, western_countries))
wide_data <- get_did_data(c(treated_country, donor_countries))

# ------------------------------------------------------------------------------
# 1. Baseline Table (Clustered SEs)
# ------------------------------------------------------------------------------
run_baseline_row <- function(df, name) {
  fit <- feols(log_gdppc ~ did | country + year, data = df)
  s <- summary(fit, cluster = ~ country)
  
  tibble(
    Pool = name,
    Coefficient = s$coeftable["did", "Estimate"],
    SE = s$coeftable["did", "Std. Error"],
    P_Value = s$coeftable["did", "Pr(>|t|)"],
    Country_FE = "Yes",
    Year_FE = "Yes",
    N_Countries = length(unique(df$country)),
    N_Obs = nrow(df)
  )
}

did_baseline <- bind_rows(
  run_baseline_row(clean_data, "Clean"),
  run_baseline_row(western_data, "Western"),
  run_baseline_row(wide_data, "Wide")
)
write_csv(did_baseline, file.path(output_dir, "did_baseline.csv"))

# ------------------------------------------------------------------------------
# 2. Standard Error Robustness Table
# ------------------------------------------------------------------------------
run_se_row <- function(df, name) {
  fit <- feols(log_gdppc ~ did | country + year, data = panel(df, ~ country + year))
  
  s_cluster <- summary(fit, cluster = ~ country)
  s_hc1 <- summary(fit, se = "hetero")
  s_dk <- summary(fit, vcov = "DK")
  
  bind_rows(
    tibble(Pool = name, SE_Type = "Clustered", Estimate = s_cluster$coeftable["did", "Estimate"], SE = s_cluster$coeftable["did", "Std. Error"], P_Value = s_cluster$coeftable["did", "Pr(>|t|)"]),
    tibble(Pool = name, SE_Type = "HC1 (Robust)", Estimate = s_hc1$coeftable["did", "Estimate"], SE = s_hc1$coeftable["did", "Std. Error"], P_Value = s_hc1$coeftable["did", "Pr(>|t|)"]),
    tibble(Pool = name, SE_Type = "Driscoll-Kraay", Estimate = s_dk$coeftable["did", "Estimate"], SE = s_dk$coeftable["did", "Std. Error"], P_Value = s_dk$coeftable["did", "Pr(>|t|)"])
  )
}

se_robustness <- bind_rows(
  run_se_row(clean_data, "Clean"),
  run_se_row(western_data, "Western"),
  run_se_row(wide_data, "Wide")
)
write_csv(se_robustness, file.path(output_dir, "se_robustness.csv"))

# ------------------------------------------------------------------------------
# 3. Event Study Regressions and Plots
# ------------------------------------------------------------------------------
run_event_study <- function(df, name) {
  df_bins <- df %>%
    mutate(
      bin = case_when(
        year <= 1972 ~ "pre_27_plus",
        year >= 1973 & year <= 1977 ~ "pre_26_22",
        year >= 1978 & year <= 1982 ~ "pre_21_17",
        year >= 1983 & year <= 1987 ~ "pre_16_12",
        year >= 1988 & year <= 1992 ~ "pre_11_7",
        year >= 1993 & year <= 1997 ~ "pre_6_2",
        year == 1998 ~ "ref",
        year >= 1999 & year <= 2003 ~ "post_0_4",
        year >= 2004 & year <= 2007 ~ "post_5_7"
      )
    )
  
  bin_levels <- c("pre_27_plus", "pre_26_22", "pre_21_17", "pre_16_12", "pre_11_7", "pre_6_2", "post_0_4", "post_5_7")
  
  for(lvl in bin_levels) {
    df_bins[[paste0("did_", lvl)]] <- df_bins$treated * (df_bins$bin == lvl)
  }
  
  formula_str <- paste("log_gdppc ~", paste(paste0("did_", bin_levels), collapse = " + "), "| country + year")
  fit <- feols(as.formula(formula_str), data = df_bins)
  
  s <- summary(fit, cluster = ~ country)
  
  mapping <- tibble(
    bin = bin_levels,
    x = c(-28, -24, -19, -14, -9, -4, 2, 6.5),
    year_label = c("1970-1972", "1973-1977", "1978-1982", "1983-1987", "1988-1992", "1993-1997", "1999-2003", "2004-2007")
  )
  
  coefs <- tibble(
    bin = bin_levels,
    Estimate = s$coeftable[paste0("did_", bin_levels), "Estimate"],
    `Std. Error` = s$coeftable[paste0("did_", bin_levels), "Std. Error"],
    `t value` = s$coeftable[paste0("did_", bin_levels), "t value"],
    `Pr(>|t|)` = s$coeftable[paste0("did_", bin_levels), "Pr(>|t|)"]
  ) %>%
    left_join(mapping, by = "bin") %>%
    mutate(Pool = name)
  
  ref_row <- tibble(
    bin = "ref", Estimate = 0, `Std. Error` = 0, `t value` = NA, `Pr(>|t|)` = NA,
    x = -1, year_label = "1998 (Ref)", Pool = name
  )
  
  coefs_full <- bind_rows(coefs %>% filter(x < -1), ref_row, coefs %>% filter(x > -1))
  write_csv(coefs_full, file.path(output_dir, sprintf("event_study_coefs_%s.csv", name)))
  
  p <- ggplot(coefs_full, aes(x = x, y = Estimate)) +
    geom_point(size = 3, color = "blue") +
    geom_errorbar(aes(ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`), width = 0.5, color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
    scale_x_continuous(breaks = coefs_full$x, labels = coefs_full$year_label) +
    labs(
      title = sprintf("Event Study: Germany vs. %s Pool (Up to 2007)", name),
      x = "Period",
      y = "Effect on log GDP per capita (with 95% CI)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(output_dir, sprintf("event_study_%s.png", name)), plot = p, width = 8, height = 6)
}

run_event_study(clean_data, "Clean")
run_event_study(western_data, "Western")
run_event_study(wide_data, "Wide")

cat("\n== DiD Analysis to 2007 Completed! ==\n")
