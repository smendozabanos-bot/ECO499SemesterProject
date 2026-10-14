library(dplyr)
library(tidyr)
library(tidyverse)
# 1. Start with R's built-in 50 states and exclude Alaska and Hawaii (48 contiguous states)
contiguous_states <- state.name[!state.name %in% c("Alaska", "Hawaii")]

# 2. Define the year vector
years <- 2017:2023

# 3. Build the state-by-year skeleton panel
panel_df <- expand.grid(
  state = contiguous_states,
  year = years,
  stringsAsFactors = FALSE
) %>%
  arrange(year, state)

# Preview the empty panel (432 rows total)
head(panel_df, 10)


# 1. Define the manifest mapping each year to its exact filenames
migration_manifest <- tibble(
  year = 2017:2023,
  inflow_file = c(
    "stateinflow1617.csv",
    "stateinflow1718.csv",
    "stateinflow1819.csv",
    "stateinflow1920.csv",
    "stateinflow2021.csv",
    "stateinflow2122.csv",
    "stateinflow2223.csv"
  ),
  outflow_file = c(
    "stateoutflow1617.csv",
    "stateoutflow1718.csv",
    "stateoutflow1819.csv",
    "stateoutflow1920.csv",
    "stateoutflow2021.csv",
    "stateoutflow2122.csv",
    "stateoutflow2223.csv"
  )
)

# 2. Standard FIPS crosswalk for the 50 states + DC (FIPS 1 to 56)
fips_crosswalk <- tibble(
  fips = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

# 3. Processing function for a single year pair
process_irs_year <- function(inflow_path, outflow_path, target_year) {
  
  inflow_raw <- read_csv(inflow_path, show_col_types = FALSE) %>% 
    rename_with(tolower) %>%
    mutate(
      y2_statefips = as.numeric(y2_statefips),
      y1_statefips = as.numeric(y1_statefips)
    )
  
  outflow_raw <- read_csv(outflow_path, show_col_types = FALSE) %>% 
    rename_with(tolower) %>%
    mutate(
      y2_statefips = as.numeric(y2_statefips),
      y1_statefips = as.numeric(y1_statefips)
    )
  
  # --- Inflows: group by destination state FIPS (y2_statefips) ---
  inflows <- inflow_raw %>%
    filter(y2_statefips <= 56 & y1_statefips <= 56) %>%
    filter(y2_statefips != y1_statefips) %>% 
    group_by(y2_statefips) %>%
    summarise(
      inflow_returns = sum(n1, na.rm = TRUE),
      inflow_people  = sum(n2, na.rm = TRUE),
      inflow_agi     = sum(agi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(fips_crosswalk, by = c("y2_statefips" = "fips")) %>%
    select(state, inflow_returns, inflow_people, inflow_agi)
  
  # --- Outflows: group by origin state FIPS (y1_statefips) ---
  outflows <- outflow_raw %>%
    filter(y2_statefips <= 56 & y1_statefips <= 56) %>%
    filter(y2_statefips != y1_statefips) %>%
    group_by(y1_statefips) %>%
    summarise(
      outflow_returns = sum(n1, na.rm = TRUE),
      outflow_people  = sum(n2, na.rm = TRUE),
      outflow_agi     = sum(agi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(fips_crosswalk, by = c("y1_statefips" = "fips")) %>%
    select(state, outflow_returns, outflow_people, outflow_agi)
  
  # Merge inflows and outflows together for this year
  yearly_panel <- full_join(inflows, outflows, by = "state") %>%
    mutate(year = target_year) %>%
    filter(!is.na(state))
  
  return(yearly_panel)
}

# 4. Run across all files and combine into one master panel dataset
master_migration_panel <- pmap_dfr(
  migration_manifest,
  ~ process_irs_year(..2, ..3, ..1)
)

# 5. Preview your final master panel dataset
head(master_migration_panel)

library(tidycensus)
library(quantmod)


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

process_irs_year <- function(inflow_path, outflow_path, target_year) {
  inflow_raw <- read_csv(inflow_path, show_col_types = FALSE) %>% 
    rename_with(tolower) %>%
    mutate(y2_statefips = as.numeric(y2_statefips), y1_statefips = as.numeric(y1_statefips))
  
  outflow_raw <- read_csv(outflow_path, show_col_types = FALSE) %>% 
    rename_with(tolower) %>%
    mutate(y2_statefips = as.numeric(y2_statefips), y1_statefips = as.numeric(y1_statefips))
  
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


# ==========================================
# PART B: Build Housing Affordability Panel (Excluding 2020)
# ==========================================

# ==========================================
# PART B: Build Housing Affordability Panel (2017-2023, excluding 2020)
# ==========================================

years_to_pull <- c(2017:2019, 2021:2023)

get_acs_vars_by_year <- function(yr) {
  df <- get_acs(
    geography = "state",
    variables = c(median_home_value = "B25077_001", median_income = "B19013_001"),
    year = yr,
    survey = "acs1"
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    rename(state = NAME)
  
  df$year <- yr
  return(df)
}

acs_housing_panel <- map_dfr(years_to_pull, get_acs_vars_by_year)

# Pull FRED mortgage rates and safely format using zoo
getSymbols("MORTGAGE30US", src = "FRED", auto.assign = TRUE)
fred_rates <- data.frame(
  date = index(MORTGAGE30US), 
  mortgage_rate = coredata(MORTGAGE30US)[, 1]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(annual_mortgage_rate = mean(mortgage_rate, na.rm = TRUE), .groups = "drop")

state_abbrev_map <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  state_abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

housing_clean <- acs_housing_panel %>%
  left_join(state_abbrev_map, by = "state") %>%
  left_join(fred_rates, by = "year") %>%
  mutate(
    loan_amount = median_home_value * 0.80,
    monthly_rate = (annual_mortgage_rate / 100) / 12,
    n_months = 30 * 12,
    monthly_pi_payment = loan_amount * (monthly_rate * (1 + monthly_rate)^n_months) / ((1 + monthly_rate)^n_months - 1),
    annual_pi_payment = monthly_pi_payment * 12,
    affordability_index = median_income / annual_pi_payment
  ) %>%
  select(year, state = state_abbr, median_home_value, median_income, annual_mortgage_rate, loan_amount, annual_pi_payment, affordability_index)
# ==========================================
# PART C: Final Merge into Master Thesis Dataset
# ==========================================

final_sasser_panel <- master_migration_panel %>%
  left_join(housing_clean, by = c("state", "year"))

# Preview your final dataset
head(final_sasser_panel)

# Save your final panel to your project folder
# write_csv(final_sasser_panel, "sasser_migration_housing_panel.csv")


# 1. Pull National Unemployment Rate from FRED
getSymbols("UNRATE", src = "FRED", auto.assign = TRUE)

# 2. Convert to a clean annual data frame
national_unemp_annual <- data.frame(
  date = index(UNRATE), 
  unrate = coredata(UNRATE)[, 1]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(national_unemployment = mean(unrate, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 2017 & year <= 2023) # Match your thesis panel years

# 3. Merge it into your existing master panel dataset
# (Assuming your main dataset is named 'final_sasser_panel' and has a 'year' column)
final_sasser_panel_with_macro <- final_sasser_panel %>%
  left_join(national_unemp_annual, by = "year")

# Preview the result to confirm every state gets the national rate for that year
head(final_sasser_panel_with_macro)

#Adding real per capita variable 
# ==========================================
# PART C: Final Merge into Master Thesis Dataset (National Macro)
# ==========================================

# 1. Pull National Unemployment Rate from FRED
getSymbols("UNRATE", src = "FRED", auto.assign = TRUE)

# 2. Convert to a clean annual data frame
national_unemp_annual <- data.frame(
  date = index(UNRATE), 
  unrate = coredata(UNRATE)[, 1]
) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(national_unemployment = mean(unrate, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 2017 & year <= 2023)

# 3. Merge it into your existing master panel dataset
final_sasser_panel_with_macro <- final_sasser_panel %>%
  left_join(national_unemp_annual, by = "year")


# ==========================================
# PART D: Add State Unemployment & Lagged Income (Sasser Specification)
# ==========================================

# 1. Pull State Unemployment Rates for all states + DC from FRED
state_abbrs <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
                 "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", 
                 "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
                 "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", 
                 "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

get_state_unemp <- function(st) {
  ticker <- paste0(st, "UR")
  tryCatch({
    getSymbols(ticker, src = "FRED", auto.assign = TRUE)
    df <- data.frame(
      date = index(get(ticker)), 
      rate = coredata(get(ticker))[, 1]
    ) %>%
      mutate(
        year = as.numeric(format(date, "%Y")),
        state = st
      ) %>%
      group_by(year, state) %>%
      summarise(state_unemployment = mean(rate, na.rm = TRUE), .groups = "drop")
    return(df)
  }, error = function(e) { 
    return(NULL) 
  })
}

state_unemp_panel <- map_dfr(state_abbrs, get_state_unemp) %>%
  filter(year >= 2017 & year <= 2023)

# 2. Merge State Unemployment into your macro panel dataset
final_sasser_panel_with_unemp <- final_sasser_panel_with_macro %>%
  left_join(state_unemp_panel, by = c("state", "year"))

# 3. Apply Sasser's Log and One-Year Lag Structure (t-1) to Income and Affordability
final_sasser_regression_ready <- final_sasser_panel_with_unemp %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(
    log_income         = log(median_income),
    log_income_lag1    = lag(log_income, n = 1),
    affordability_lag1 = lag(affordability_index, n = 1),
    unemployment_lag1  = lag(state_unemployment, n = 1)
  ) %>%
  ungroup()

# 4. Final preview and export
glimpse(final_sasser_regression_ready)
write_csv(final_sasser_regression_ready, "final_sasser_thesis_regression_panel.csv")


# ==========================================
# PART E: Calculate State-Level Dependent Variables (Path A)
# ==========================================

final_sasser_regression_ready <- final_sasser_regression_ready %>%
  mutate(
    # 1. Calculate Total Net Migration (Inflows minus Outflows of people)
    net_migration_people = inflow_people - outflow_people,
    
    # 2. Calculate Out-Migration Rate (Outflow / Total Inflow+Outflow scale, or base population proxy)
    # Using outflow_people relative to total state economic baseline or lagged population
    outmigration_rate = outflow_people / (outflow_people + inflow_people), # or scale by median_income / population proxy
    
    # 3. Take Natural Log of Migration Flows / Rates (Sasser log specification)
    log_outflow_people = log(outflow_people),
    log_inflow_people  = log(inflow_people),
    log_net_migration  = sign(net_migration_people) * log(abs(net_migration_people) + 1) # handles potential negative net migration safely
  )

# Preview your newly created dependent variables alongside your lagged economic controls
final_sasser_regression_ready %>%
  select(state, year, outflow_people, log_outflow_people, affordability_lag1, log_income_lag1, unemployment_lag1) %>%
  head(10)