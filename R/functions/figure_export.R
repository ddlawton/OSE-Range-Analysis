# ==============================================================================
# Figure Export and Table Management Functions
# ==============================================================================
# 
# Centralized system for exporting plots and tables with consistent structure
# Complements Quarto's built-in figure rendering
#
# Author: ddlawton
# Created: 2025-11-23
# Updated: 2026-05-24 - Optimized, improved error handling
# ==============================================================================

# Global state for current analysis context
.current_analysis <- NULL

# ==============================================================================
# Directory Management
# ==============================================================================

#' Ensure Directory Exists
#'
#' Creates directory and all parent directories if they don't exist.
#'
#' @param path Character. Directory path to create
#' @return Invisibly returns the path
#' @export
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  invisible(path)
}

#' Set Current Analysis Context
#'
#' Establishes the current analysis name for organizing outputs.
#' Creates necessary directory structure for tables.
#'
#' @param analysis_name Character. Analysis identifier (e.g., "basic_stats")
#' @return Invisibly returns the analysis name
#' @export
set_current_analysis <- function(analysis_name) {
  .current_analysis <<- analysis_name
  
  # Create table directory (figures handled by Quarto)
  tables_dir <- file.path("outputs", "tables", analysis_name)
  ensure_dir(tables_dir)
  
  message("✓ Set output context for: ", analysis_name)
  invisible(analysis_name)
}

# ==============================================================================
# Plot Export
# ==============================================================================

#' Save Plot to File with Error Handling
#'
#' Centralized plot saving with fallback mechanisms for different plot types.
#' Handles ggplot2, grid, and base graphics.
#'
#' @param plot_obj Plot object or NULL
#' @param filepath Character. Full output path including extension
#' @param width Numeric. Width in inches (default: 8)
#' @param height Numeric. Height in inches (default: 6)
#' @param dpi Numeric. Resolution in dots per inch (default: 300)
#' @param placeholder Character. Text to write if plot is NULL
#' @return Invisibly returns the filepath
#' @export
save_plot_file <- function(plot_obj, filepath, width = 8, height = 6, 
                           dpi = 300, placeholder = "plot unavailable") {
  ensure_dir(dirname(filepath))
  
  if (is.null(plot_obj)) {
    writeLines(placeholder, filepath)
    return(invisible(filepath))
  }
  
  # Try ggplot2::ggsave first (handles ggplot and patchwork)
  tryCatch({
    ggplot2::ggsave(
      filename = filepath, 
      plot = plot_obj, 
      width = width, 
      height = height, 
      dpi = dpi, 
      bg = "white"
    )
  }, error = function(e) {
    # Fallback for grid/base graphics
    tryCatch({
      grDevices::png(filepath, width = width * dpi, height = height * dpi, res = dpi)
      print(plot_obj)
      grDevices::dev.off()
    }, error = function(e2) {
      warning("Failed to save plot to ", filepath, ": ", e2$message)
      writeLines(placeholder, filepath)
    })
  })
  
  invisible(filepath)
}

# ==============================================================================
# Table Export
# ==============================================================================

#' Save Model Summary as CSV
#'
#' Extracts model summary using provided function and saves as CSV.
#'
#' @param model_obj Model object (glmmTMB, lm, etc.)
#' @param table_name Character. Output filename (or basename)
#' @param summary_function Function. Extracts summary (default: broom.mixed::tidy)
#' @param out_path Character. Optional explicit directory path
#' @return Data frame of model summary (invisibly)
#' @export
save_model_summary <- function(model_obj, table_name, summary_function = NULL, out_path = NULL) {
  # Validate context
  if (is.null(.current_analysis) && is.null(out_path)) {
    warning("No analysis context set. Call set_current_analysis() or provide out_path.")
    return(invisible(NULL))
  }
  
  # Default summary function
  if (is.null(summary_function)) {
    if (requireNamespace("broom.mixed", quietly = TRUE)) {
      summary_function <- broom.mixed::tidy
    } else {
      warning("broom.mixed not available. Provide summary_function.")
      return(invisible(NULL))
    }
  }
  
  # Extract and save summary
  tryCatch({
    summary_df <- summary_function(model_obj)
    
    # Ensure .csv extension
    if (!endsWith(table_name, ".csv")) {
      table_name <- paste0(table_name, ".csv")
    }
    
    # Determine filepath
    filepath <- if (!is.null(out_path)) {
      file.path(out_path, table_name)
    } else {
      file.path("outputs", "tables", .current_analysis, table_name)
    }
    
    ensure_dir(dirname(filepath))
    readr::write_csv(summary_df, filepath)
    message("✓ Saved model summary: ", filepath)
    
    invisible(summary_df)
  }, error = function(e) {
    warning("Failed to save model summary: ", e$message)
    invisible(NULL)
  })
}

#' Save Emmeans Results as CSV
#'
#' Converts emmGrid object to tibble and saves as CSV.
#'
#' @param emmeans_obj emmGrid object or data frame
#' @param table_name Character. Output filename (or basename)
#' @param out_path Character. Optional explicit directory path
#' @return Data frame of emmeans (invisibly)
#' @export
save_emmeans_table <- function(emmeans_obj, table_name, out_path = NULL) {
  # Validate context
  if (is.null(.current_analysis) && is.null(out_path)) {
    warning("No analysis context set. Call set_current_analysis() or provide out_path.")
    return(invisible(NULL))
  }
  
  tryCatch({
    # Convert to tibble if needed
    emmeans_df <- if ("emmGrid" %in% class(emmeans_obj)) {
      tibble::as_tibble(emmeans_obj)
    } else {
      emmeans_obj
    }
    
    # Ensure .csv extension
    if (!endsWith(table_name, ".csv")) {
      table_name <- paste0(table_name, ".csv")
    }
    
    # Determine filepath
    filepath <- if (!is.null(out_path)) {
      file.path(out_path, table_name)
    } else {
      file.path("outputs", "tables", .current_analysis, table_name)
    }
    
    ensure_dir(dirname(filepath))
    readr::write_csv(emmeans_df, filepath)
    message("✓ Saved emmeans table: ", filepath)
    
    invisible(emmeans_df)
  }, error = function(e) {
    warning("Failed to save emmeans table: ", e$message)
    invisible(NULL)
  })
}

#' Save Data Frame as CSV
#'
#' Generic function to save any data frame as CSV with consistent behavior.
#'
#' @param df Data frame or tibble
#' @param table_name Character. Output filename (or basename)
#' @param out_path Character. Optional explicit directory path
#' @return Data frame (invisibly)
#' @export
save_data_table <- function(df, table_name, out_path = NULL) {
  # Validate context
  if (is.null(.current_analysis) && is.null(out_path)) {
    warning("No analysis context set. Call set_current_analysis() or provide out_path.")
    return(invisible(NULL))
  }
  
  # Ensure .csv extension
  if (!endsWith(table_name, ".csv")) {
    table_name <- paste0(table_name, ".csv")
  }
  
  # Determine filepath
  filepath <- if (!is.null(out_path)) {
    file.path(out_path, table_name)
  } else {
    file.path("outputs", "tables", .current_analysis, table_name)
  }
  
  ensure_dir(dirname(filepath))
  readr::write_csv(df, filepath)
  message("✓ Saved data table: ", filepath)
  
  invisible(df)
}
