#' Common Setup for OSE Range Analysis
#' 
#' This file contains shared configuration, packages, and functions
#' that are used across multiple analysis files.
#' 
#' Usage: source("R/_common.R") at the top of analysis files

# === PACKAGE LOADING ===
# Suppress package startup messages
suppressPackageStartupMessages({
  library(tidyverse)       # Data manipulation and visualization
  library(rnaturalearth)   # Geographic data
  library(rnaturalearthhires) # High-resolution geographic data  
  library(janitor)         # Data cleaning
  library(here)            # Path management
  library(patchwork)       # Plot composition
  library(ggpubr)          # Publication-ready plots
  library(gt)              # Tables
  library(glmmTMB)         # Mixed models
  library(mgcv)            # GAMs
  library(gratia)          # GAM utilities
  library(emmeans)         # Marginal means
  library(MetBrewer)       # Color palettes (optional)
  library(broom.mixed)     # Model tidying
  library(DHARMa)          # Model diagnostics
  library(digest)          # Hash generation for caching
  library(readr)           # CSV reading/writing
})

# === PROJECT CONFIGURATION ===
# Set project root using here package
here::i_am('README.md')

# Study regions in order for consistent factor levels
STUDY_REGIONS <- c('Saint-Louis', 'Thiès', 'Fatick', 'Kaffrine')

# Alternative region names used in some analyses
ALT_STUDY_REGIONS <- c('Saint Louis', 'Thies', 'Fatick', 'Kaffrine')

# Mission labels for plotting
MISSION_LABELS <- c(
  'Mission 1 (July)', 
  'Mission 2 (September)', 
  'Mission 3 (October)'
)

# Color palette configuration
DEFAULT_PALETTE <- "Degas"
FERTILIZER_COLORS <- c('control' = 'black', 'fertilized' = 'dark green')

# Plot styling defaults
DEFAULT_POINT_SIZE <- 5
DEFAULT_EMMEAN_POINT_SIZE <- 8
DEFAULT_FIGURE_DPI <- 2

# === DATA LOADING ===
#' Load and process the main dataset
#' @param cache_processed If TRUE, save/load processed data as CSV for faster loading
#' @return Processed tibble ready for analysis
load_ose_data <- function(cache_processed = TRUE) {
  processed_file <- here::here("data", "processed", "ose_data_processed.csv")
  raw_file <- here::here("data", "raw", "Toure_OSE2021data_v05.xlsx")
  
  # Check if processed data exists and is newer than raw data
  if (cache_processed && file.exists(processed_file) && 
      file.mtime(processed_file) > file.mtime(raw_file)) {
    message("Loading cached processed data...")
    return(read_csv(processed_file, show_col_types = FALSE))
  }
  
  # Process raw data
  message("Processing raw data...")
  data <- process_senegal_data(raw_file)
  
  # Cache processed data
  if (cache_processed) {
    dir.create(dirname(processed_file), showWarnings = FALSE, recursive = TRUE)
    write_csv(data, processed_file)
    message("Processed data cached to: ", processed_file)
  }
  
  return(data)
}

# === MODEL CACHING ===
#' Cache or load a fitted model object
#'
#' This function implements smart caching for computationally expensive models.
#' In CI environments (GitHub Actions), it can optionally disable caching to
#' avoid rebuilding models unnecessarily, or use simpler model specifications.
#'
#' @param model_name Character string identifying the model (e.g., "ground_cover_gam")
#' @param data Data frame used for model fitting
#' @param fit_function Function that fits the model when cache miss occurs
#' @param fit_function_simple Optional simpler/faster model function for CI
#' @param ... Additional arguments passed to fit_function
#' @param cache_models Logical, whether to use caching (default: TRUE)
#' @param force_refit Logical, force model refitting even if cache exists (default: FALSE)
#' @param use_simple_in_ci Logical, use simpler model in CI environments (default: TRUE)
#' @return Fitted model object
cache_model <- function(model_name, data, fit_function, fit_function_simple = NULL, ..., 
                       cache_models = TRUE, force_refit = FALSE, use_simple_in_ci = TRUE) {
  
  # Detect if we're in a CI environment
  is_ci <- Sys.getenv("CI") != "" || Sys.getenv("GITHUB_ACTIONS") != ""
  
  # In CI, optionally use simpler models to speed up builds
  if (is_ci && use_simple_in_ci && !is.null(fit_function_simple)) {
    message("CI environment detected - using simplified model: ", model_name)
    return(fit_function_simple(data, ...))
  }
  
  # In CI without caching, just fit the model
  if (is_ci && !cache_models) {
    message("CI environment - fitting model: ", model_name, " (no caching)")
    return(fit_function(data, ...))
  }
  
  if (!cache_models || force_refit) {
    message("Fitting model: ", model_name, " (caching disabled)")
    return(fit_function(data, ...))
  }
  
  # Create cache directory
  cache_dir <- here::here("data", "model_objects")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Generate cache key based on model name, data hash, and arguments
  data_hash <- digest::digest(data, algo = "md5")
  args_hash <- digest::digest(list(...), algo = "md5")
  cache_key <- paste0(model_name, "_", substr(data_hash, 1, 8), "_", substr(args_hash, 1, 8))
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
  
  # Check if cached model exists and is valid
  if (file.exists(cache_file)) {
    tryCatch({
      message("Loading cached model: ", model_name)
      model <- readRDS(cache_file)
      
      # Validate model object
      if (is.null(model) || (!inherits(model, c("glmmTMB", "gam", "bam", "lm", "glm")))) {
        warning("Invalid cached model for ", model_name, ". Refitting...")
        file.remove(cache_file)
      } else {
        return(model)
      }
    }, error = function(e) {
      warning("Error loading cached model for ", model_name, ": ", e$message, ". Refitting...")
      if (file.exists(cache_file)) file.remove(cache_file)
    })
  }
  
  # Fit new model
  message("Fitting model: ", model_name, " (this may take a moment...)")
  start_time <- Sys.time()
  model <- fit_function(data, ...)
  end_time <- Sys.time()
  
  # Cache the model (skip in CI if desired)
  if (!is_ci) {
    tryCatch({
      saveRDS(model, cache_file)
      message("Model fitted and cached in ", round(as.numeric(end_time - start_time, units = "secs"), 2), " seconds")
    }, error = function(e) {
      warning("Could not cache model ", model_name, ": ", e$message)
    })
  } else {
    message("Model fitted in ", round(as.numeric(end_time - start_time, units = "secs"), 2), " seconds (CI - not cached)")
  }
  
  return(model)
}

#' Clear model cache
#' @param model_name Optional specific model name to clear, or NULL to clear all
clear_model_cache <- function(model_name = NULL) {
  cache_dir <- here::here("data", "model_objects")
  if (!dir.exists(cache_dir)) {
    message("No model cache directory found")
    return(invisible())
  }
  
  if (is.null(model_name)) {
    files_removed <- file.remove(list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE))
    message("Cleared all cached models (", sum(files_removed), " files)")
  } else {
    pattern <- paste0("^", model_name, "_.*\\.rds$")
    files_to_remove <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
    if (length(files_to_remove) > 0) {
      file.remove(files_to_remove)
      message("Cleared cached models for: ", model_name, " (", length(files_to_remove), " files)")
    } else {
      message("No cached models found for: ", model_name)
    }
  }
}

#' List cached models
list_cached_models <- function() {
  cache_dir <- here::here("data", "model_objects")
  if (!dir.exists(cache_dir)) {
    message("No model cache directory found")
    return(invisible())
  }
  
  cached_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(cached_files) == 0) {
    message("No cached models found")
    return(invisible())
  }
  
  # Extract model info
  file_info <- file.info(cached_files)
  model_info <- data.frame(
    Model = gsub("_[a-f0-9]{8}_[a-f0-9]{8}\\.rds$", "", basename(cached_files)),
    Size_MB = round(file_info$size / 1024^2, 2),
    Created = file_info$ctime,
    stringsAsFactors = FALSE
  )
  
  print(model_info)
  message("\\nTotal cached models: ", nrow(model_info))
  message("Total cache size: ", round(sum(model_info$Size_MB), 2), " MB")
  return(invisible(model_info))
}

# === FUNCTION LOADING ===
# Source all function files
function_files <- list.files(
  here::here("R", "functions"),
  pattern = "\\.R$",
  full.names = TRUE
)

for (file in function_files) {
  source(file, local = knitr::knit_global())
}

# === CHUNK OPTIONS ===
# Default knitr options for all analysis files
if (require(knitr, quietly = TRUE)) {
  knitr::opts_chunk$set(
    echo = FALSE,
    message = FALSE, 
    warning = FALSE,
    fig.retina = DEFAULT_FIGURE_DPI,
    cache = FALSE
  )
}

# === GGPLOT THEME ===
# Set default theme for all plots
ggplot2::theme_set(ggpubr::theme_pubr())

# Utility function to get consistent region factor levels
standardize_regions <- function(data, region_col = "region", alt_names = FALSE) {
  regions <- if (alt_names) ALT_STUDY_REGIONS else STUDY_REGIONS
  data[[region_col]] <- factor(data[[region_col]], levels = regions)
  return(data)
}

message("OSE Range Analysis common setup loaded successfully")
message("Available regions: ", paste(STUDY_REGIONS, collapse = ", "))
message("Functions sourced from R/functions/")

# Environment-specific messages
is_ci <- Sys.getenv("CI") != "" || Sys.getenv("GITHUB_ACTIONS") != ""
if (is_ci) {
  message("CI environment detected - using GitHub Actions cache for models")
} else {
  message("Model caching enabled - models saved to data/model_objects/")
}