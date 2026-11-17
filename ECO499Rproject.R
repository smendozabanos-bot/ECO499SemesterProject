# ==========================================================
# ECO499 Thesis: Interstate Migration Pre and Post COVID-19
# Revised script. Lines marked "CHANGED" or "NEW" differ
# from the original version.
# ==========================================================

library(tidyverse)   # includes dplyr, tidyr, readr, purrr, ggplot2
library(tidycensus)
library(quantmod)
library(fixest)      # NEW: for fixed-effects regressions

# CHANGED: the original contiguous_states list used full names and was never
# applied. This version uses abbreviations to match the panel and is applied
# in Section 4. state.abb already excludes DC.
lower48 <- setdiff(state.abb, c("AK", "HI"))

# ------------------------------------------
# 1. IRS Migration Data Processing (2017-2023)
# ------------------------------------------
# Year label = second filing year of each IRS file
# (e.g., stateinflow2122.csv compares returns filed in 2021 and 2022 -> year 2022)

migration_manifest <- tibble(
  year = 2017:2023,
  inflow_file = c(
    "stateinflow1617.csv", "stateinflow1718.csv", "stateinflow1819.csv",
    "stateinflow1920.csv", "stateinflow2021.csv", "stateinflow2122.csv", "stateinflow2223.csv"
  ),
  outflow_file = c(
    "stateoutflow1617.csv", "stateoutflow1718.csv", "stateoutflow1819.csv",
    "stateoutflow1920.csv", "stateoutflow2021.csv", "stateoutflow2122.csv", "stateoutflow2223.csv"
  )
)

fips_crosswalk <- tibble(
  fips = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

# NEW: the IRS marks suppressed cells with negative values (e.g., -1).
# Summing them would slightly understate totals, so they are set to NA first.
read_irs <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    mutate(
      y2_statefips = as.numeric(y2_statefips),
      y1_statefips = as.numeric(y1_statefips),
      across(c(n1, n2, agi), ~ if_else(as.numeric(.x) < 0, NA_real_, as.numeric(.x)))
    )
}

process_irs_year <- function(inflow_path, outflow_path, target_year) {
  inflow_raw  <- read_irs(inflow_path)
  outflow_raw <- read_irs(outflow_path)
  
  # Codes above 56 are IRS summary rows (totals, foreign); y1 == y2 are non-movers
  inflows <- inflow_raw %>%
    filter(y2_statefips <= 56 & y1_statefips <= 56, y2_statefips != y1_statefips) %>%
    group_by(y2_statefips) %>%
    summarise(
      inflow_returns = sum(n1, na.rm = TRUE),
      inflow_people  = sum(n2, na.rm = TRUE),
      inflow_agi     = sum(agi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(fips_crosswalk, by = c("y2_statefips" = "fips")) %>%
    select(state, inflow_returns, inflow_people, inflow_agi)
  
  outflows <- outflow_raw %>%
    filter(y2_statefips <= 56 & y1_statefips <= 56, y2_statefips != y1_statefips) %>%
    group_by(y1_statefips) %>%
    summarise(
      outflow_returns = sum(n1, na.rm = TRUE),
      outflow_people  = sum(n2, na.rm = TRUE),
      outflow_agi     = sum(agi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(fips_crosswalk, by = c("y1_statefips" = "fips")) %>%
    select(state, outflow_returns, outflow_people, outflow_agi)
  
  full_join(inflows, outflows, by = "state") %>%
    mutate(year = target_year) %>%
    filter(!is.na(state))
}

master_migration_panel <- pmap_dfr(migration_manifest, ~ process_irs_year(..2, ..3, ..1))


# ------------------------------------------
# 2. ACS Housing & FRED Mortgage Rates Panel
# ------------------------------------------
# No standard 2020 ACS 1-year release, so 2020 is skipped.

years_to_pull <- c(2017:2019, 2021:2023)

get_acs_vars_by_year <- function(yr) {
  get_acs(
    geography = "state",
    variables = c(
      median_home_value = "B25077_001",
      median_income     = "B19013_001",
      population        = "B01003_001"   # NEW: needed for migration rates
    ),
    year = yr,
    survey = "acs1"
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    rename(state = NAME) %>%
    mutate(year = yr)
}

acs_housing_panel <- map_dfr(years_to_pull, get_acs_vars_by_year)

getSymbols("MORTGAGE30US", src = "FRED", auto.assign = TRUE)
fred_rates <- data.frame(
  date = index(MORTGAGE30US),
  mortgage_rate = coredata(MORTGAGE30US)[, 1]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(annual_mortgage_rate = mean(mortgage_rate, na.rm = TRUE), .groups = "drop")

# CHANGED: built from R's built-in state lists instead of typing them out
state_abbrev_map <- tibble(
  state      = c(state.name, "District of Columbia"),
  state_abbr = c(state.abb, "DC")
)

housing_clean <- acs_housing_panel %>%
  left_join(state_abbrev_map, by = "state") %>%
  left_join(fred_rates, by = "year") %>%
  mutate(
    loan_amount        = median_home_value * 0.80,
    monthly_rate       = (annual_mortgage_rate / 100) / 12,
    n_months           = 30 * 12,
    monthly_pi_payment = loan_amount * (monthly_rate * (1 + monthly_rate)^n_months) /
      ((1 + monthly_rate)^n_months - 1),
    annual_pi_payment  = monthly_pi_payment * 12,
    affordability_index = median_income / annual_pi_payment
  ) %>%
  filter(!is.na(state_abbr)) %>%   # drops Puerto Rico
  select(year, state = state_abbr, median_home_value, median_income,
         population, annual_mortgage_rate, affordability_index)

final_sasser_panel <- master_migration_panel %>%
  left_join(housing_clean, by = c("state", "year"))


# ------------------------------------------
# 3. National & State Macro Controls (FRED)
# ------------------------------------------

getSymbols("UNRATE", src = "FRED", auto.assign = TRUE)
national_unemp_annual <- data.frame(
  date = index(UNRATE),
  unrate = coredata(UNRATE)[, 1]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(national_unemployment = mean(unrate, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 2017 & year <= 2023)

final_sasser_panel_with_macro <- final_sasser_panel %>%
  left_join(national_unemp_annual, by = "year")

get_state_unemp <- function(st) {
  ticker <- paste0(st, "UR")
  tryCatch({
    getSymbols(ticker, src = "FRED", auto.assign = TRUE)
    data.frame(
      date = index(get(ticker)),
      rate = coredata(get(ticker))[, 1]
    ) %>%
      mutate(year = as.numeric(format(date, "%Y")), state = st) %>%
      group_by(year, state) %>%
      summarise(state_unemployment = mean(rate, na.rm = TRUE), .groups = "drop")
  }, error = function(e) {
    message("Could not download ", ticker)   # NEW: tells you if a state fails
    NULL
  })
}

state_unemp_panel <- map_dfr(lower48, get_state_unemp) %>%
  filter(year >= 2017 & year <= 2023)

final_sasser_panel_with_unemp <- final_sasser_panel_with_macro %>%
  left_join(state_unemp_panel, by = c("state", "year"))


# ------------------------------------------
# 4. Sasser Transformations, Lags & Dependent Variables
# ------------------------------------------
# Lags are computed on the full 2017-2023 panel BEFORE any years are dropped,
# so each lag always comes from the previous calendar year.

final_sasser_regression_ready <- final_sasser_panel_with_unemp %>%
  filter(state %in% lower48) %>%                 # CHANGED: now 48 states, not 51
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(
    log_income         = log(median_income),
    log_income_lag1    = lag(log_income, n = 1),
    affordability_lag1 = lag(affordability_index, n = 1),
    unemployment_lag1  = lag(state_unemployment, n = 1),
    population_lag1    = lag(population, n = 1),  # NEW: residents at start of period
    
    # Dependent variables
    net_migration_people = inflow_people - outflow_people,
    log_outflow_people   = log(outflow_people),
    log_inflow_people    = log(inflow_people),
    
    # CHANGED: replaced the signed log of net migration with a rate per
    # 1,000 residents, which is easier to interpret (similar to Sasser 2010)
    net_migration_rate = net_migration_people / population_lag1 * 1000
  ) %>%
  ungroup()

# Check that lags line up (2022 lag should equal 2021 value)
final_sasser_regression_ready %>%
  filter(state == "NC", year %in% 2021:2022) %>%
  select(state, year, log_income, log_income_lag1)


# ------------------------------------------
# 5. Regression Sample (NEW)
# ------------------------------------------
# Pre-COVID: 2018, 2019. Post-COVID: 2022, 2023.
# 2020 excluded as a transition year; 2021 has no lagged ACS values.

reg_sample <- final_sasser_regression_ready %>%
  filter(year %in% c(2018, 2019, 2022, 2023)) %>%
  mutate(post = as.integer(year >= 2022)) %>%
  select(state, year, post,
         log_outflow_people, log_inflow_people, net_migration_rate,
         affordability_lag1, log_income_lag1, unemployment_lag1,
         national_unemployment)

# Checks: expect 48 per year, 192 total, and no missing values
reg_sample %>% count(year)
nrow(reg_sample)
reg_sample %>% summarise(across(everything(), ~ sum(is.na(.x))))

write_csv(reg_sample, "final_sasser_regression_clean.csv")


# ------------------------------------------
# 6. Summary Statistics (regression sample only)
# ------------------------------------------

summary_table <- reg_sample %>%
  select(-state, -year, -post) %>%
  summarise(across(everything(), list(
    Mean   = ~mean(.x, na.rm = TRUE),
    Median = ~median(.x, na.rm = TRUE),
    SD     = ~sd(.x, na.rm = TRUE),
    Min    = ~min(.x, na.rm = TRUE),
    Max    = ~max(.x, na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to = c("Variable", "Stat"), names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Stat, values_from = value)

print(summary_table)




# ------------------------------------------
# 1. Prep the data
# ------------------------------------------
# Migration rates per 1,000 residents, so big states don't look
# like they have more migration just because they have more people.

eda <- final_sasser_regression_ready %>%
  mutate(
    out_rate     = outflow_people / population_lag1 * 1000,
    in_rate      = inflow_people  / population_lag1 * 1000,
    log_out_rate = log(out_rate),
    log_in_rate  = log(in_rate)
  )

# Census regions (R's built-in state.region; "North Central" = Midwest)
regions <- tibble(state = state.abb, region = as.character(state.region)) %>%
  mutate(region = if_else(region == "North Central", "Midwest", region))

eda_sample <- eda %>%
  filter(year %in% c(2018, 2019, 2022, 2023)) %>%
  left_join(regions, by = "state") %>%
  mutate(
    period      = fct_relevel(if_else(year >= 2022, "Post-COVID", "Pre-COVID"),
                              "Pre-COVID", "Post-COVID"),
    income_lag1 = exp(log_income_lag1)   # lagged median income in dollars
  )

glimpse(eda_sample)
eda_sample %>% count(period, year)


# ------------------------------------------
# 2. Summary statistics table (overall and by period)
# ------------------------------------------
# Mean, median, standard deviation, min, max, range, and IQR

stats_long <- eda_sample %>%
  pivot_longer(
    cols = c(out_rate, in_rate, net_migration_rate,
             unemployment_lag1, income_lag1, affordability_lag1),
    names_to = "variable",
    values_to = "value"
  )

# Overall (all four years)
summary_overall <- stats_long %>%
  group_by(variable) %>%
  summarize(
    mean   = mean(value),
    median = median(value),
    sd     = sd(value),
    min    = min(value),
    max    = max(value),
    range  = max(value) - min(value),
    IQR    = IQR(value)
  )
View(summary_overall)

# Before vs. after COVID
summary_by_period <- stats_long %>%
  group_by(variable, period) %>%
  summarize(
    mean   = mean(value),
    median = median(value),
    sd     = sd(value),
    min    = min(value),
    max    = max(value),
    range  = max(value) - min(value),
    IQR    = IQR(value),
    .groups = "drop"
  )
View(summary_by_period)


# ------------------------------------------
# 3. Distributions (one plot with facets)
# ------------------------------------------

eda_long <- eda_sample %>%
  pivot_longer(
    cols = c(out_rate, in_rate, net_migration_rate,
             unemployment_lag1, log_income_lag1, affordability_lag1),
    names_to = "variable",
    values_to = "value"
  )

ggplot(eda_long, aes(x = value)) +
  geom_histogram(bins = 20, color = "white", fill = "steelblue") +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(title = "Distributions of migration and economic variables",
       x = NULL, y = "Count")


# ------------------------------------------
# 4. Pre vs. post COVID
# ------------------------------------------

ggplot(eda_long, aes(x = period, y = value, fill = period)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Before and after COVID", x = NULL, y = NULL)


# ------------------------------------------
# 5. Net migration over time for selected states
# ------------------------------------------

eda %>%
  filter(state %in% c("CA", "NY", "TX", "FL", "NC")) %>%
  ggplot(aes(x = year, y = net_migration_rate, group = state, color = state)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Net migration per 1,000 residents",
       x = "Year", y = "Net migrants per 1,000", color = "State")


# ------------------------------------------
# 6. Relationships: does the slope change after COVID?
# ------------------------------------------
# Parallel lines = relationship stayed the same (main effects)
# Different slopes = relationship changed (interaction effects)

ggplot(eda_sample, aes(x = unemployment_lag1, y = log_out_rate, color = period)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Out-migration vs. unemployment",
       x = "Unemployment rate (t-1)", y = "Log out-migration rate", color = NULL)

ggplot(eda_sample, aes(x = log_income_lag1, y = log_out_rate, color = period)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Out-migration vs. income",
       x = "Log median income (t-1)", y = "Log out-migration rate", color = NULL)

ggplot(eda_sample, aes(x = affordability_lag1, y = log_out_rate, color = period)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Out-migration vs. housing affordability",
       x = "Affordability index (t-1)", y = "Log out-migration rate", color = NULL)

# To check in-migration or net migration, change y = log_out_rate
# to y = log_in_rate or y = net_migration_rate in the three plots above.


# ------------------------------------------
# 7. Region table: before vs. after COVID
# ------------------------------------------

region_table <- eda_sample %>%
  group_by(region, period) %>%
  summarize(
    states             = n_distinct(state),
    net_migration_rate = mean(net_migration_rate),
    out_rate           = mean(out_rate),
    in_rate            = mean(in_rate),
    unemployment       = mean(unemployment_lag1),
    income             = mean(income_lag1),
    affordability      = mean(affordability_lag1),
    .groups = "drop"
  )
View(region_table)


# ------------------------------------------
# 8. Region graphs: before vs. after COVID
# ------------------------------------------

# Net migration by region
eda_sample %>%
  group_by(region, period) %>%
  summarize(net_migration_rate = mean(net_migration_rate), .groups = "drop") %>%
  ggplot(aes(x = region, y = net_migration_rate, fill = period)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  scale_fill_viridis_d(begin = 0.3, end = 0.8) +
  theme_minimal() +
  labs(title = "Average net migration by region, before and after COVID",
       x = NULL, y = "Net migrants per 1,000", fill = NULL)

# Housing affordability by region
eda_sample %>%
  group_by(region, period) %>%
  summarize(affordability = mean(affordability_lag1), .groups = "drop") %>%
  ggplot(aes(x = region, y = affordability, fill = period)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(begin = 0.3, end = 0.8) +
  theme_minimal() +
  labs(title = "Average housing affordability by region, before and after COVID",
       x = NULL, y = "Affordability index (t-1)", fill = NULL)

library(flextable)
library(officer)

# 1. Turn your data into a Word table
my_table <- flextable(region_table)

# 2. Save it directly to a Word file
read_docx() %>% 
  body_add_flextable(my_table) %>% 
  print(target = "Region_Summary_Table.docx")

# 1. Turn your data into a Word table
my_table1 <- flextable(summary_overall)

# 2. Save it directly to a Word file
read_docx() %>% 
  body_add_flextable(my_table1) %>% 
  print(target = "Summary_Overall_Table.docx")