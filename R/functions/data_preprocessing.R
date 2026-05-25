# ==============================================================================
# Data Preprocessing Functions for OSE Range Analysis
# ==============================================================================
# 
# Transform raw Excel data into clean, analysis-ready format
# Pipeline: Load → Clean → Pivot → Finalize → Save
#
# Author: ddlawton
# Created: 2025-11-07
# Updated: 2026-05-24 - Optimized, standardized documentation, reduced redundancy
# ==============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(readr)

# ==============================================================================
# Core Data Loading and Cleaning
# ==============================================================================

#' Load and Clean Raw Senegal Migration Data
#'
#' Reads Excel file, standardizes column names, and fixes common typos using
#' a centralized mapping approach for better maintainability.
#'
#' @param path Character. Path to Excel file
#' @return Tibble with cleaned column names (all character type)
#' @export
load_and_clean_raw_data <- function(path) {
  raw_data <- read_excel(path) |> 
    clean_names() |> 
    mutate(across(everything(), as.character))
  
  # Centralized column name corrections
  column_fixes <- list(
    # Fix typos
    "mission2_percent_grond_cover" = "mission2_percent_ground_cover",
    "mission3_ose_cont" = "mission3_ose_count",
    # Standardize names
    "gender" = "farmer_gender",
    "millet_yield_kg_ha" = "rendement_en_kg_ha"
  )
  
  # Apply fixes for existing columns
  for (old_name in names(column_fixes)) {
    if (old_name %in% names(raw_data)) {
      raw_data <- rename(raw_data, !!column_fixes[[old_name]] := !!old_name)
    }
  }
  
  # Standardize damage column names via pattern matching
  damage_cols <- grep("mission[1-3].*ose.*damage", names(raw_data), 
                      value = TRUE, ignore.case = TRUE)
  
  for (col in damage_cols) {
    mission_num <- gsub(".*mission[_]?([1-3]).*", "\\1", col, ignore.case = TRUE)
    new_name <- paste0("mission", mission_num, "_ose_damage_percent")
    raw_data <- rename(raw_data, !!new_name := !!col)
  }
  
  return(raw_data)
}

# ==============================================================================
# Data Transformation
# ==============================================================================

#' Pivot Mission Columns from Wide to Long Format
#'
#' Transforms mission-specific columns (mission1_*, mission2_*, mission3_*)
#' from wide to long format. Non-mission columns (e.g., yield) are preserved
#' as they apply to the entire field rather than individual missions.
#'
#' @param df Tibble with wide-format mission columns
#' @return Tibble in long format with mission_number column
#' @export
pivot_missions_long <- function(df) {
  df |>
    pivot_longer(
      cols = matches("^mission[1-3]_"),
      names_to = c("mission_number", ".value"),
      names_pattern = "mission_?([123])_?([a-zA-Z0-9_]+)"
    )
}

#' Clean and Convert Mission Column Data Types
#'
#' Converts mission_number to factor and numeric columns to appropriate types.
#' Note: OSE count represents density already adjusted for OSE proportion.
#'
#' @param df Tibble in long format
#' @return Tibble with corrected data types
#' @export
clean_mission_cols <- function(df) {
  df |>
    mutate(
      mission_number = as.factor(mission_number),
      percent_ground_cover = as.numeric(percent_ground_cover),
      ose_damage_percent = if ("ose_damage_percent" %in% names(df)) {
        as.numeric(ose_damage_percent)
      } else {
        NA_real_
      }
    )
}

#' Fix Fertilizer Treatment Typos and Standardize Values
#'
#' Corrects data entry errors in fertilizer treatment column.
#'
#' @param df Tibble with fertilizer_treatement column (note typo in source data)
#' @return Tibble with corrected fertilizer_treatment column
#' @export
fix_fertilizer_treatment <- function(df) {
  df |>
    mutate(fertilizer_treatment = case_when(
      fertilizer_treatement %in% c("Id C NF", "IdC NF") ~ "control",
      fertilizer_treatement == "Id C F" ~ "fertilized",
      TRUE ~ fertilizer_treatement
    )) |>
    select(-fertilizer_treatement)
}

#' Fill Farmer ID Down Through Rows
#'
#' Propagates farmer identifier through grouped observations.
#'
#' @param df Tibble with farmer column containing NAs
#' @param farmer_id Character. Name of farmer ID column (default: "farmer")
#' @param direction Character. Fill direction (default: 'down')
#' @return Tibble with filled farmer IDs
#' @export
add_farmer_id <- function(df, farmer_id = "farmer", direction = "down") {
  df |> fill(all_of(farmer_id), .direction = direction)
}

#' Fill Farmer Gender Down Through Rows
#'
#' Propagates farmer gender metadata through grouped observations.
#'
#' @param df Tibble with farmer_gender column
#' @param direction Character. Fill direction (default: 'down')
#' @return Tibble with filled farmer_gender values
#' @export
add_farmer_gender <- function(df, direction = "down") {
  if ("farmer_gender" %in% names(df)) {
    df |> fill(farmer_gender, .direction = direction)
  } else {
    df
  }
}

# ==============================================================================
# Finalization
# ==============================================================================

#' Select and Reorder Final Analysis Columns
#'
#' Extracts relevant columns for analysis, handling optional columns gracefully.
#'
#' @param df Tibble with full set of processed columns
#' @return Tibble with selected columns only
#' @export
select_final_columns <- function(df) {
  # Core columns required for analysis
  core_cols <- c(
    "year", "region", "farmer", "farmer_gender", "fertilizer_treatment",
    "code", "mission_number", "date_surveyed", "ose_count", 
    "temperature", "percent_ground_cover", "ose_damage_percent"
  )
  
  # Optional yield columns (names may vary across data versions)
  yield_cols <- c("yield_date_havested", "yield_date_harvested", 
                  "rendement_en_kg_ha", "rendement_kg_ha")
  
  # Select only existing columns
  available_cols <- c(
    intersect(core_cols, names(df)), 
    intersect(yield_cols, names(df))
  )
  
  df |> select(all_of(available_cols))
}

#' Set Proper Data Types for All Columns
#'
#' Converts columns to appropriate types (factor, numeric) for analysis.
#'
#' @param df Tibble with selected columns
#' @return Tibble with finalized data types
#' @export
finalize_datatypes <- function(df) {
  # Core factor columns
  factor_cols <- c(
    "farmer", "farmer_gender", "fertilizer_treatment", "code",
    "year", "mission_number", "date_surveyed"
  )
  
  # Core numeric columns
  numeric_cols <- c(
    "ose_count", "temperature", "percent_ground_cover", "ose_damage_percent"
  )
  
  # Add optional columns if they exist
  optional_factor_cols <- c("yield_date_havested", "yield_date_harvested")
  optional_numeric_cols <- c("rendement_en_kg_ha", "rendement_kg_ha")
  
  factor_cols <- c(factor_cols, intersect(optional_factor_cols, names(df)))
  numeric_cols <- c(numeric_cols, intersect(optional_numeric_cols, names(df)))
  
  # Apply transformations
  df |>
    mutate(
      across(all_of(intersect(factor_cols, names(df))), as.factor),
      across(all_of(intersect(numeric_cols, names(df))), as.numeric)
    )
}

#' Adjust OSE Damage to Represent Total Grasshopper Damage
#'
#' Raw data contains OSE-specific damage adjusted for OSE proportion.
#' This back-calculates total grasshopper damage by dividing by
#' region-specific OSE proportions.
#' 
#' OSE proportions by region (averaged across missions):
#' Kaffrine: 0.93, Fatick: 0.91, Thies: 0.79, Saint Louis: 0.65
#'
#' @param df Tibble with ose_damage_percent and region columns
#' @return Tibble with adjusted ose_damage_percent (total grasshopper damage)
#' @export
adjust_damage_for_total_grasshoppers <- function(df) {
  # Region-specific OSE proportions
  ose_proportions <- c(
    "Kaffrine" = 0.93, "Fatick" = 0.91, 
    "Thies" = 0.79, "Saint Louis" = 0.65
  )
  
  if (all(c("ose_damage_percent", "region") %in% names(df))) {
    df |>
      mutate(
        ose_damage_percent = ose_damage_percent / ose_proportions[as.character(region)]
      )
  } else {
    df
  }
}

# ==============================================================================
# Pipeline Functions
# ==============================================================================

#' Complete Data Processing Pipeline
#'
#' Executes full transformation from raw Excel to analysis-ready format.
#' All preprocessing steps are applied in sequence.
#'
#' @param path Character. Path to raw Excel file
#' @param verbose Logical. Print diagnostic messages (default: FALSE)
#' @return Tibble in long format, ready for analysis
#' @export
#' 
#' @examples
#' \dontrun{
#' data <- process_senegal_data("data/raw/Toure_OSE2021data_v05.xlsx")
#' data_verbose <- process_senegal_data("data/raw/file.xlsx", verbose = TRUE)
#' }
process_senegal_data <- function(path, verbose = FALSE) {
  if (verbose) cat("Loading raw data...\n")
  
  raw_data <- load_and_clean_raw_data(path)
  
  if (verbose) {
    cat("✓ Loaded", nrow(raw_data), "rows,", ncol(raw_data), "columns\n")
  }
  
  if (verbose) cat("Processing pipeline...\n")
  
  processed <- raw_data |>
    add_farmer_id() |>
    add_farmer_gender() |>
    pivot_missions_long() |>
    clean_mission_cols() |>
    fix_fertilizer_treatment() |>
    select_final_columns() |>
    finalize_datatypes() |>
    adjust_damage_for_total_grasshoppers()
  
  if (verbose) {
    cat("✓ Processed to", nrow(processed), "rows,", ncol(processed), "columns\n")
  }
  
  return(processed)
}

#' Save Processed Data to CSV
#'
#' Simple wrapper for write_csv with consistent behavior.
#'
#' @param data Tibble to save
#' @param outfile Character. Output file path
#' @return Invisibly returns the data
#' @export
save_processed_data <- function(data, outfile) {
  write_csv(data, file = outfile)
  invisible(data)
}
