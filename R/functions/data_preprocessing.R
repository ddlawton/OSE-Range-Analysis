#' Senegal Migration Data Transformation Functions
#' 
#' Modular, well-documented R functions for pivoting and cleaning the 'Toure_OSE2021data_v03.xlsx' dataset.
#' Intended for use in Quarto QMD analytics for manuscript work.
#' 
#' Author: ddlawton
#' Date: 2025-11-07

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
    rename(farmer = famer) |> 
    mutate(across(everything(), as.character)) |> 
    rename(
      mission2_percent_ground_cover = mission2_percent_grond_cover,
      mission3_ose_count = mission3_ose_cont
    )
  return(raw_data)
}

#' Pivot missions columns from wide to long format
#'
#' @param df Tibble, preprocessed raw data
#' @return Tibble in long format: each static row is repeated for three missions
pivot_missions_long <- function(df) {
  df_long <- df |>
    pivot_longer(
      cols = matches("^mission[1-3]_.*|^mission_[1-3]_.*"),
      names_to = c("mission_number", ".value"),
      names_pattern = "mission_?([123])_?([a-zA-Z_]+)"
    )
  return(df_long)
}

#' Further clean and correct mission columns data types
#'
#' @param df Tibble, pivoted long format
#' @return Tibble with mission_number as factor, percent_ground_cover numeric
clean_mission_cols <- function(df) {
  df |>
    mutate(
      mission_number = as.factor(mission_number),
      percent_ground_cover = as.numeric(percent_ground_cover)
    )
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
    fill(farmer_id,.direction = direction)
}

#' Select and reorder relevant columns for final dataset
#'
#' @param df Tibble, cleaned and pivoted
#' @return Final tibble with subset of variables
select_final_columns <- function(df) {
  df |>
    select(
      year,
      region,
      farmer,
      fertilizer_treatment,
      code,
      mission_number,
      date_surveyed,
      ose_count,
      temperature,
      percent_ground_cover,
      yield_date_havested,
      rendement_en_kg_ha
    )
}

#' Set proper data types for all columns
#'
#' @param df Tibble with selected columns
#' @return Tibble with categorical/factor and numeric columns set
finalize_datatypes <- function(df) {
  df |>
    mutate(
      across(c(
        'farmer',
        'fertilizer_treatment',
        'code',
        'year',
        'mission_number',
        'yield_date_havested',
        'date_surveyed'
      ), as.factor),
      across(c(
        'ose_count',
        'temperature',
        'percent_ground_cover',
        'rendement_en_kg_ha'
      ), as.numeric),
      year = as.factor(year),
      region = as.factor(region)
    )
}


#' Convenience pipeline to fully process raw Senegal migration data to long analytic format
#'
#' @param path Path to the raw Excel file
#' @return Final processed tibble, long format
process_senegal_data <- function(path) {
  raw_data <- load_and_clean_raw_data(path)
  long <- raw_data |>
    add_farmer_id() |>
    pivot_missions_long() |>
    clean_mission_cols()  |>
    fix_fertilizer_treatment() |>
    select_final_columns() |>
    finalize_datatypes()
  return(long)
}

#' Save data to RDS if all tests pass
#'
#' @param raw_data_long The processed long format tibble
#' @param outfile Path to save RDS file
save_processed_data <- function(raw_data_long, outfile) {
  saveRDS(raw_data_long, file = outfile)
}
