#' Senegal Migration Data Transformation Functions
#' 
#' Modular, well-documented R functions for pivoting and cleaning the 'Toure_OSE2021data_v05.xlsx' dataset.
#' Intended for use in Quarto QMD analytics for manuscript work.
#' 
#' Author: ddlawton
#' Date: 2025-11-07
#' Updated: 2025-12-25 - Added support for farmer gender and OSE leaf damage percent columns
#'          Output changed from RDS to CSV format

library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(testthat)

#' Load and clean raw Senegal migration data
#'
#' @param path Path to the Excel file of raw data
#' @return A tibble with standardized column names and character columns
load_and_clean_raw_data <- function(path) {
  raw_data <- read_excel(path) |> 
    clean_names() |> 
    mutate(across(everything(), as.character))
  
  # Fix known column name typos (only if they exist)
  if ("mission2_percent_grond_cover" %in% names(raw_data)) {
    raw_data <- raw_data |> rename(mission2_percent_ground_cover = mission2_percent_grond_cover)
  }
  if ("mission3_ose_cont" %in% names(raw_data)) {
    raw_data <- raw_data |> rename(mission3_ose_count = mission3_ose_cont)
  }
  
  # Standardize damage column names if they exist (handle potential variations)
  # Expected format: mission1_ose_damage_percent, mission2_ose_damage_percent, mission3_ose_damage_percent
  # Handle both "Mission1 OSE damage %" and "Mission3 OSE damage" (missing %) formats
  damage_pattern <- "mission[1-3].*ose.*damage"
  damage_cols <- grep(damage_pattern, names(raw_data), value = TRUE, ignore.case = TRUE)
  
  if (length(damage_cols) > 0) {
    # Rename to standardized format: mission1_ose_damage_percent, etc.
    for (col in damage_cols) {
      mission_num <- gsub(".*(mission)?[_]?([1-3]).*", "\\2", col, ignore.case = TRUE)
      new_name <- paste0("mission", mission_num, "_ose_damage_percent")
      raw_data <- raw_data |> rename(!!new_name := !!col)
    }
  }
  
  # Standardize gender column name to farmer_gender if it exists
  if ("gender" %in% names(raw_data)) {
    raw_data <- raw_data |> rename(farmer_gender = gender)
  }
  
  # Standardize yield column name variations
  if ("millet_yield_kg_ha" %in% names(raw_data)) {
    raw_data <- raw_data |> rename(rendement_en_kg_ha = millet_yield_kg_ha)
  }
  
  return(raw_data)
}

#' Pivot missions columns from wide to long format
#'
#' Pivots mission-specific columns (those prefixed with mission1_, mission2_, mission3_)
#' from wide to long format. Non-mission columns (like yield_date_havested and 
#' rendement_en_kg_ha) are preserved as-is since they apply to the entire field.
#'
#' @param df Tibble, preprocessed raw data
#' @return Tibble in long format: each static row is repeated for three missions
pivot_missions_long <- function(df) {
  # Check which columns will be pivoted
  mission_cols <- grep("^mission[1-3]_.*|^mission_[1-3]_.*", names(df), value = TRUE)
  
  df_long <- df |>
    pivot_longer(
      cols = matches("^mission[1-3]_.*|^mission_[1-3]_.*"),
      names_to = c("mission_number", ".value"),
      names_pattern = "mission_?([123])_?([a-zA-Z0-9_]+)"
    )
  return(df_long)
}

# Note: The pattern above handles mission1_ose_damage_percent by extracting:
# mission_number = "1" and creates a column ose_damage_percent with values from each mission
# Yield columns should NOT have mission prefixes and will be preserved in wide format

#' Further clean and correct mission columns data types
#'
#' Note: OSE count (ose_count) represents OSE density that has already been 
#' adjusted for the proportion of OSE in the grasshopper population by Mamour.
#'
#' @param df Tibble, pivoted long format
#' @return Tibble with mission_number as factor, percent_ground_cover and ose_damage_percent numeric
clean_mission_cols <- function(df) {
  # Convert mission_number and percent_ground_cover
  df <- df |>
    mutate(
      mission_number = as.factor(mission_number),
      percent_ground_cover = as.numeric(percent_ground_cover)
    )
  
  # Add ose_damage_percent column if it exists, otherwise create it with NA
  if ("ose_damage_percent" %in% names(df)) {
    df <- df |> mutate(ose_damage_percent = as.numeric(ose_damage_percent))
  } else {
    df <- df |> mutate(ose_damage_percent = NA_real_)
  }
  
  return(df)
}

#' Fix fertilizer treatment typos and remove erroneous columns
#'
#' @param df Tibble, with potential fertilizer_treatement typo column
#' @return Tibble with corrected fertilizer_treatment column
fix_fertilizer_treatment <- function(df) {
  df |>
    mutate(fertilizer_treatment = case_when(
      fertilizer_treatement == "Id C NF" ~ "control",
      fertilizer_treatement == "IdC NF" ~ "control",
      fertilizer_treatement == "Id C F" ~ "fertilized",
      TRUE ~ fertilizer_treatement
    )) |>
    select(-fertilizer_treatement)
}

add_farmer_id <- function(df, farmer_id = "farmer", direction = 'down') {
  df |>
    fill(all_of(farmer_id), .direction = direction)
}

#' Fill farmer gender down (metadata column)
#'
#' @param df Tibble with farmer_gender column containing NA values
#' @param direction Direction to fill (default: 'down')
#' @return Tibble with farmer_gender filled
add_farmer_gender <- function(df, direction = 'down') {
  # Check if farmer_gender column exists
  if ("farmer_gender" %in% names(df)) {
    df |>
      fill(farmer_gender, .direction = direction)
  } else {
    # If column doesn't exist, just return the dataframe
    df
  }
}

#' Select and reorder relevant columns for final dataset
#'
#' @param df Tibble, cleaned and pivoted
#' @return Final tibble with subset of variables
select_final_columns <- function(df) {
  # Define core columns that should always be present
  core_cols <- c(
    "year",
    "region",
    "farmer",
    "farmer_gender",
    "fertilizer_treatment",
    "code",
    "mission_number",
    "date_surveyed",
    "ose_count",
    "temperature",
    "percent_ground_cover",
    "ose_damage_percent"
  )
  
  # Add yield columns if they exist (names may vary)
  yield_cols <- c("yield_date_havested", "yield_date_harvested", "rendement_en_kg_ha", "rendement_kg_ha")
  
  # Only select columns that actually exist (core + yield cols that exist)
  cols_to_select <- c(intersect(core_cols, names(df)), intersect(yield_cols, names(df)))
  
  df |>
    select(all_of(cols_to_select))
}

#' Set proper data types for all columns
#'
#' @param df Tibble with selected columns
#' @return Tibble with categorical/factor and numeric columns set
finalize_datatypes <- function(df) {
  # Factor columns that should always exist
  factor_cols <- c(
    'farmer',
    'farmer_gender',
    'fertilizer_treatment',
    'code',
    'year',
    'mission_number',
    'date_surveyed'
  )
  
  # Numeric columns that should always exist
  numeric_cols <- c(
    'ose_count',
    'temperature',
    'percent_ground_cover',
    'ose_damage_percent'
  )
  
  # Add optional factor columns if they exist
  optional_factor_cols <- c('yield_date_havested', 'yield_date_harvested')
  factor_cols <- c(factor_cols, intersect(optional_factor_cols, names(df)))
  
  # Add optional numeric columns if they exist
  optional_numeric_cols <- c('rendement_en_kg_ha', 'rendement_kg_ha')
  numeric_cols <- c(numeric_cols, intersect(optional_numeric_cols, names(df)))
  
  df |>
    mutate(
      across(all_of(intersect(factor_cols, names(df))), as.factor),
      across(all_of(intersect(numeric_cols, names(df))), as.numeric),
      year = as.factor(year),
      region = as.factor(region)
    )
}

#' Adjust OSE damage to represent total grasshopper damage
#'
#' The raw data contains OSE-specific damage values that were already adjusted
#' for the proportion of OSE in the total grasshopper population. To get total
#' grasshopper damage, we divide by the OSE proportion for each region.
#' 
#' OSE proportions by region (averaged across missions):
#' - Kaffrine: 0.93
#' - Fatick: 0.91
#' - Thies: 0.79
#' - Saint Louis: 0.65
#'
#' @param df Tibble with ose_damage_percent column
#' @return Tibble with adjusted ose_damage_percent representing total grasshopper damage
adjust_damage_for_total_grasshoppers <- function(df) {
  # Define OSE proportions by region
  ose_proportions <- c(
    "Kaffrine" = 0.93,
    "Fatick" = 0.91,
    "Thies" = 0.79,
    "Saint Louis" = 0.65
  )
  
  # Only adjust if ose_damage_percent column exists
  if ("ose_damage_percent" %in% names(df) && "region" %in% names(df)) {
    df <- df |>
      mutate(
        ose_damage_percent = case_when(
          region == "Kaffrine" ~ ose_damage_percent / ose_proportions["Kaffrine"],
          region == "Fatick" ~ ose_damage_percent / ose_proportions["Fatick"],
          region == "Thies" ~ ose_damage_percent / ose_proportions["Thies"],
          region == "Saint Louis" ~ ose_damage_percent / ose_proportions["Saint Louis"],
          TRUE ~ ose_damage_percent
        )
      )
  }
  
  return(df)
}


#' Convenience pipeline to fully process raw Senegal migration data to long analytic format
#'
#' @param path Path to the raw Excel file
#' @param verbose Print diagnostic messages (default: FALSE)
#' @return Final processed tibble, long format
process_senegal_data <- function(path, verbose = FALSE) {
  if (verbose) cat("Loading raw data...\n")
  raw_data <- load_and_clean_raw_data(path)
  
  if (verbose) {
    cat("After loading, columns:", ncol(raw_data), "\n")
    cat("  Key columns present:\n")
    cat("    - farmer:", "farmer" %in% names(raw_data), "\n")
    cat("    - yield cols:", paste(grep("yield|rendement", names(raw_data), value=TRUE), collapse=", "), "\n")
  }
  
  if (verbose) cat("Processing pipeline...\n")
  long <- raw_data |>
    add_farmer_id() |>
    add_farmer_gender()
  
  if (verbose) {
    cat("Before pivot, columns:", ncol(long), "\n")
    mission_cols <- grep("^mission[1-3]", names(long), value=TRUE)
    cat("  Mission columns to pivot:", length(mission_cols), "\n")
  }
  
  long <- long |> pivot_missions_long()
  
  if (verbose) {
    cat("After pivot, columns:", ncol(long), "\n")
    cat("  Available columns:\n")
    for (col in sort(names(long))) {
      cat("    -", col, "\n")
    }
  }
  
  long <- long |>
    clean_mission_cols()  |>
    fix_fertilizer_treatment() |>
    select_final_columns() |>
    finalize_datatypes() |>
    adjust_damage_for_total_grasshoppers()
  
  return(long)
}

#' Save data to CSV if all tests pass
#'
#' @param raw_data_long The processed long format tibble
#' @param outfile Path to save CSV file
save_processed_data <- function(raw_data_long, outfile) {
  write_csv(raw_data_long, file = outfile)
}
