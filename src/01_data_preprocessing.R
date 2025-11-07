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
raw_data <- read_excel("data/Toure_OSE2021data_v03.xlsx") |> # or use read_csv/read_excel as needed
    clean_names()
    
# ---- Pivot transformation ----

raw_data_long <- raw_data |>
  rename(farmer = famer) |>
  mutate(across(everything(), as.character)) |>
  rename(
    mission2_percent_ground_cover = mission2_percent_grond_cover,
    mission3_ose_count = mission3_ose_cont
  ) |>
  pivot_longer(
    cols = matches("^mission[1-3]_.*|^mission_[1-3]_.*"),
    names_to = c("mission_number", ".value"),
    names_pattern = "mission_?([123])_?([a-zA-Z_]+)"
  ) |>
  mutate(
    mission_number = as.factor(mission_number),
    percent_ground_cover = as.numeric(percent_ground_cover)
  ) |>
  select(
    year, region, farmer, fertilizer_treatement, code,
    mission_number, date_surveyed, ose_count,
    temperature, percent_ground_cover,
    yield_date_havested, rendement_en_kg_ha
  ) |>
    mutate(
        across(c('farmer', 'fertilizer_treatement', 'code','year','mission_number','yield_date_havested','date_surveyed'), as.factor),
        across(c('ose_count','temperature','percent_ground_cover','rendement_en_kg_ha'), as.numeric),

        year = as.factor(year),
        region = as.factor(region),

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

# You can run this script with:
# source("test_raw_data_pivot_longer.R")
# Or use test_file() from testthat package if you split into formal test files