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
  
  ext <- tolower(tools::file_ext(filepath))

  save_with_device <- function(path) {
    device <- NULL
    if (tolower(tools::file_ext(path)) == "pdf") {
      device <- grDevices::cairo_pdf
    } else if (tolower(tools::file_ext(path)) == "svg") {
      if (requireNamespace("svglite", quietly = TRUE)) {
        device <- svglite::svglite
      } else {
        warning("svglite not installed; skipping SVG export for ", path)
        return(invisible(NULL))
      }
    }

    ggplot2::ggsave(
      filename = path,
      plot = plot_obj,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white",
      device = device
    )
  }

  # Try ggplot2::ggsave first (handles ggplot and patchwork)
  tryCatch({
    save_with_device(filepath)
  }, error = function(e) {
    # Fallback for raster devices only
    if (ext %in% c("png", "jpg", "jpeg", "tif", "tiff")) {
      tryCatch({
        grDevices::png(filepath, width = width * dpi, height = height * dpi, res = dpi)
        print(plot_obj)
        grDevices::dev.off()
      }, error = function(e2) {
        warning("Failed to save plot to ", filepath, ": ", e2$message)
        writeLines(placeholder, filepath)
      })
    } else {
      warning("Failed to save plot to ", filepath, ": ", e$message)
      writeLines(placeholder, filepath)
    }
  })

  # For raster exports, also write vector companions with matching dimensions.
  if (ext %in% c("png", "jpg", "jpeg", "tif", "tiff")) {
    stem <- tools::file_path_sans_ext(filepath)
    vector_paths <- c(paste0(stem, ".pdf"), paste0(stem, ".svg"))
    for (vector_path in vector_paths) {
      tryCatch({
        save_with_device(vector_path)
      }, error = function(e) {
        warning("Failed to save vector companion ", vector_path, ": ", e$message)
      })
    }
  }
  
  invisible(filepath)
}

#' Save GT Table Outputs (PNG/PDF/CSV)
#'
#' Saves a gt table as PNG and PDF with matching capture width, and also exports
#' the underlying data as CSV. An HTML export is also written for portability.
#'
#' @param gt_obj A gt table object
#' @param filename Character. Preferred output filename (typically .png)
#' @param subfolder Character. Analysis subfolder under outputs/tables
#' @param vwidth Numeric. Virtual capture width passed to gtsave
#' @return Invisibly returns paths to exported files
#' @export
save_gt_outputs <- function(gt_obj, filename, subfolder, vwidth = 3000) {
  png_dir <- here::here("outputs", "tables", subfolder, "png")
  pdf_dir <- here::here("outputs", "tables", subfolder, "pdf")
  csv_dir <- here::here("outputs", "tables", subfolder, "csv")
  html_dir <- here::here("outputs", "tables", subfolder, "html")

  ensure_dir(png_dir)
  ensure_dir(pdf_dir)
  ensure_dir(csv_dir)
  ensure_dir(html_dir)

  stem <- tools::file_path_sans_ext(filename)
  png_path <- file.path(png_dir, paste0(stem, ".png"))
  pdf_path <- file.path(pdf_dir, paste0(stem, ".pdf"))
  html_path <- file.path(html_dir, paste0(stem, ".html"))
  csv_path <- file.path(csv_dir, paste0(stem, ".csv"))

  gt::gtsave(gt_obj, png_path, vwidth = vwidth)

  tryCatch({
    gt::gtsave(gt_obj, pdf_path, vwidth = vwidth)
  }, error = function(e) {
    warning("Could not export GT table PDF for ", stem, ": ", e$message)
  })

  tryCatch({
    gt::gtsave(gt_obj, html_path)
  }, error = function(e) {
    warning("Could not export GT table HTML for ", stem, ": ", e$message)
  })

  gt_data <- gt_obj$`_data`
  readr::write_csv(gt_data, csv_path)

  invisible(list(png = png_path, pdf = pdf_path, html = html_path, csv = csv_path))
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
