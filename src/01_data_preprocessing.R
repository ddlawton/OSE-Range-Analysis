# Test script for pivot_longer transformation of the senegal migration data (referred to as 'raw_data') to long format
# Author: ddlawton
# Date: 2025-11-07

library(dplyr)
library(tidyr)
library(testthat)

# ---- Load raw_data here ----
# For actual use, load or create your raw_data tibble here.
# For the purpose of this example we assume `raw_data` is already in the workspace
# Please uncomment and adapt the following if loading from file:
raw_data <- read_excel("data/raw/Toure_OSE2021data_v03.xlsx") |> # or use read_csv/read_excel as needed
    clean_names()

# ---- Pivot transformation ----
# Step 1: Standardize column names and convert all columns to character
raw_data_long <- raw_data |>
  # Fix typo in 'famer' to 'farmer'
  rename(farmer = famer) |>
  # Convert all columns to character for consistent processing
  mutate(across(everything(), as.character)) |>
  # Correct additional column name typos for missions
  rename(
    mission2_percent_ground_cover = mission2_percent_grond_cover,
    mission3_ose_count = mission3_ose_cont
  )

# Step 2: Pivot mission columns from wide to long format
raw_data_long <- raw_data_long |>
  pivot_longer(
    # Select columns that begin with mission1_, mission2_, mission3_, mission_1_, mission_2_, mission_3_, etc.
    cols = matches("^mission[1-3]_.*|^mission_[1-3]_.*"),
    # Split column names into 'mission_number' and the remainder
    names_to = c("mission_number", ".value"),
    # Regex pattern to extract the mission number and the variable name
    names_pattern = "mission_?([123])_?([a-zA-Z_]+)"
  )

# Step 3: Clean specific columns and set appropriate data types
raw_data_long <- raw_data_long |>
  mutate(
    # Ensure 'mission_number' is a factor for categorical analysis
    mission_number = as.factor(mission_number),
    # Convert percent_ground_cover to numeric for quantitative analysis
    percent_ground_cover = as.numeric(percent_ground_cover)
  )

# Step 4: Select the columns of interest to keep in your final dataset
raw_data_long <- raw_data_long |>
  select(
    year,
    region,
    farmer,
    fertilizer_treatement,
    code,
    mission_number,
    date_surveyed,
    ose_count,
    temperature,
    percent_ground_cover,
    yield_date_havested,
    rendement_en_kg_ha
  )

# Step 5: Set proper data types for each column (character, factor, or numeric as appropriate)
raw_data_long <- raw_data_long |>
  mutate(
    # Set categorical variables to factors
    across(c(
      'farmer',
      'fertilizer_treatement',
      'code',
      'year',
      'mission_number',
      'yield_date_havested',
      'date_surveyed'
    ), as.factor),
    # Set measurement variables to numeric
    across(c(
      'ose_count',
      'temperature',
      'percent_ground_cover',
      'rendement_en_kg_ha'
    ), as.numeric),
    # Ensure 'year' and 'region' specifically are factors
    year = as.factor(year),
    region = as.factor(region)
  )



# ---- Begin tests ----

test_that("Row count is exactly nrow(raw_data) * 3", {
  expect_equal(nrow(raw_data_long), nrow(raw_data) * 3)
})

test_that("All static columns are preserved for each mission", {
  # By grouping by all static columns and yield columns, there should be 3 rows per group (for the 3 missions)
  groups <- raw_data_long |>
    group_by(year, region, farmer, fertilizer_treatement, code,
             yield_date_havested, rendement_en_kg_ha) |>
    tally()
  expect_true(all(groups$n == 3))
})

test_that("Mission numbers are only 1, 2, and 3", {
  expect_true(setequal(sort(unique(raw_data_long$mission_number)), c(1,2,3)))
})

cat("All tests completed\n")


# If tests pass, save the transformed data as RDS
saveRDS(raw_data_long, file = "data/processed/raw_data_long.rds")

# You can run this script with:
# source("test_raw_data_pivot_longer.R")
# Or use test_file() from testthat package if you split into formal test files