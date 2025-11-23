#' Enhanced Figure and Table Export System
#' 
#' This system is designed to complement Quarto's built-in figure rendering
#' by providing optional high-resolution exports and comprehensive table exports.
#' The main figure collection now happens from the website output via scripts.
#' 
#' Author: ddlawton
#' Date: 2025-11-23

# Global variables to track current analysis
.current_analysis <- NULL

#' Set the current analysis name for organizing outputs
#' @param analysis_name Character string identifying the current analysis (e.g., "basic_stats")
set_current_analysis <- function(analysis_name) {
  .current_analysis <<- analysis_name
  
  # Create directory structure for tables (figures handled by Quarto)
  tables_dir <- file.path("outputs", "tables", analysis_name)
  
  if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
  
  message(paste("✓ Set output directories for", analysis_name))
}

#' Save model summary tables as CSV
#' @param model_obj A model object (glmm, gam, etc.)
#' @param table_name Name for the output CSV file
#' @param summary_function Function to extract summary (default: broom.mixed::tidy)
save_model_summary <- function(model_obj, table_name, summary_function = NULL) {
  if (is.null(.current_analysis)) {
    warning("No analysis set. Call set_current_analysis() first.")
    return(NULL)
  }
  
  # Default summary function
  if (is.null(summary_function)) {
    if (requireNamespace("broom.mixed", quietly = TRUE)) {
      summary_function <- broom.mixed::tidy
    } else {
      warning("broom.mixed not available. Please provide summary_function.")
      return(NULL)
    }
  }
  
  # Generate summary
  tryCatch({
    summary_df <- summary_function(model_obj)
    
    # Clean filename
    if (!endsWith(table_name, ".csv")) {
      table_name <- paste0(table_name, ".csv")
    }
    
    filepath <- file.path("outputs", "tables", .current_analysis, table_name)
    
    # Save CSV
    readr::write_csv(summary_df, filepath)
    message(paste("✓ Saved model summary:", filepath))
    
    return(summary_df)
  }, error = function(e) {
    warning(paste("Failed to save model summary:", e$message))
    return(NULL)
  })
}
  
#' Save emmeans results as CSV
#' @param emmeans_obj Emmeans object or tibble
#' @param table_name Name for the output CSV file
save_emmeans_table <- function(emmeans_obj, table_name) {
  if (is.null(.current_analysis)) {
    warning("No analysis set. Call set_current_analysis() first.")
    return(NULL)
  }
  
  tryCatch({
    # Convert to tibble if needed
    if ("emmGrid" %in% class(emmeans_obj)) {
      emmeans_df <- tibble::as_tibble(emmeans_obj)
    } else {
      emmeans_df <- emmeans_obj
    }
    
    # Clean filename
    if (!endsWith(table_name, ".csv")) {
      table_name <- paste0(table_name, ".csv")
    }
    
    filepath <- file.path("outputs", "tables", .current_analysis, table_name)
    
    # Save CSV
    readr::write_csv(emmeans_df, filepath)
    message(paste("✓ Saved emmeans table:", filepath))
    
    return(emmeans_df)
  }, error = function(e) {
    warning(paste("Failed to save emmeans table:", e$message))
    return(NULL)
  })
}

#' Save any data frame as CSV
#' @param df Data frame or tibble
#' @param table_name Name for the output CSV file
save_data_table <- function(df, table_name) {
  if (is.null(.current_analysis)) {
    warning("No analysis set. Call set_current_analysis() first.")
    return(NULL)
  }
  
  # Clean filename
  if (!endsWith(table_name, ".csv")) {
    table_name <- paste0(table_name, ".csv")
  }
  
  filepath <- file.path("outputs", "tables", .current_analysis, table_name)
  
  # Save CSV
  readr::write_csv(df, filepath)
  message(paste("✓ Saved data table:", filepath))
  
  return(df)
}

# === LEGACY WRAPPER FUNCTIONS ===
# These functions are kept for backward compatibility but now just return the objects
# since figures are collected from Quarto's website output

#' Optional high-resolution plot export (for supplementary materials)
#' @param plot_obj A ggplot object
#' @param filename Optional filename for high-res export
#' @param width Plot width in inches (default: 12 for high-res)
#' @param height Plot height in inches (default: 9 for high-res)
#' @param dpi Resolution (default: 600 for publication)
auto_save_plot <- function(plot_obj, filename = NULL, width = 12, height = 9, dpi = 600) {
  # Optionally save high-resolution version for supplementary materials
  if (!is.null(filename) && !is.null(.current_analysis)) {
    if (!endsWith(filename, ".png")) filename <- paste0(filename, ".png")
    
    # Create high-res directory
    hires_dir <- file.path("outputs", "figures", .current_analysis, "high_resolution")
    if (!dir.exists(hires_dir)) dir.create(hires_dir, recursive = TRUE)
    
    filepath <- file.path(hires_dir, filename)
    
    tryCatch({
      ggplot2::ggsave(
        filename = filepath,
        plot = plot_obj,
        width = width,
        height = height,
        dpi = dpi,
        bg = "white"
      )
      message(paste("✓ Saved high-res version:", filepath))
    }, error = function(e) {
      message(paste("Could not save high-res version:", e$message))
    })
  }
  
  return(plot_obj)  # Always return the plot object for display
}

# Wrapper functions for backward compatibility
auto_save_map <- function(map_obj, filename = NULL, width = 14, height = 10, dpi = 600) {
  auto_save_plot(map_obj, filename, width, height, dpi)
}

save_senegal_map <- function(..., filename = "senegal_map") {
  map_obj <- plot_senegal_map(...)
  auto_save_map(map_obj, filename)
}

save_mission_density <- function(..., filename = "mission_density") {
  plot_obj <- plot_mission_density(...)
  auto_save_plot(plot_obj, filename, width = 14, height = 12)
}

save_temperature_smooth <- function(..., filename = "temperature_smooth") {
  plot_obj <- plot_temperature_smooth(...)
  auto_save_plot(plot_obj, filename, width = 14, height = 10)
}

save_gam_smooths <- function(..., filename = "gam_smooths") {
  plot_obj <- plot_gam_smooths_gratia(...)
  auto_save_plot(plot_obj, filename, width = 14, height = 12)
}

save_emmeans_plot <- function(..., filename = "emmeans_plot") {
  plot_obj <- plot_emmeans(...)
  auto_save_plot(plot_obj, filename, width = 12, height = 10)
}

save_diagnostic_plots <- function(plots_obj, filename = "diagnostic_plots") {
  # Handle patchwork objects or individual plots
  auto_save_plot(plots_obj, filename, width = 16, height = 12)
}