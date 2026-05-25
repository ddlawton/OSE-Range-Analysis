# ==============================================================================
# Variable Summary Table Function
# ==============================================================================
# 
# Generate formatted gt table summarizing all variables in a dataset
# Includes type information, summary statistics, and missing value counts
#
# Author: ddlawton
# Created: 2025-11-22
# Updated: 2026-05-24 - Optimized, improved documentation
# ==============================================================================

#' Create Summary Table for Dataset Variables
#'
#' Generates a formatted gt table summarizing each variable in a data frame.
#' For numeric variables: min, max, mean, median, SD
#' For categorical variables: number of levels
#' For all variables: data type and missing count
#'
#' @param data Data frame or tibble to summarize
#' @return gt table object with formatted summary statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' summary_table <- table_variable_summary(iris)
#' 
#' # In Quarto document
#' table_variable_summary(ose_data)
#' }
table_variable_summary <- function(data) {
  # Validate dependencies
  required_pkgs <- c("dplyr", "gt", "tibble")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Please install it.")
    }
  }
  
  #' Summarize Single Column
  #'
  #' Internal helper to extract statistics from one variable
  #'
  #' @param x Vector of values
  #' @return Tibble with summary statistics
  summarise_column <- function(x) {
    x_nona <- x[!is.na(x)]
    type <- class(x)[1]
    n_missing <- sum(is.na(x))
    
    if (is.numeric(x)) {
      # Numeric variables
      tibble::tibble(
        type = type,
        min = min(x_nona, na.rm = TRUE),
        max = max(x_nona, na.rm = TRUE),
        mean = mean(x_nona, na.rm = TRUE),
        median = median(x_nona, na.rm = TRUE),
        sd = sd(x_nona, na.rm = TRUE),
        missing = n_missing,
        levels = NA_integer_
      )
    } else if (is.factor(x) || is.character(x)) {
      # Categorical variables
      lvl <- if (is.factor(x)) nlevels(x) else length(unique(x_nona))
      tibble::tibble(
        type = type,
        min = NA_real_,
        max = NA_real_,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        missing = n_missing,
        levels = lvl
      )
    } else {
      # Other types
      tibble::tibble(
        type = type,
        min = NA_real_,
        max = NA_real_,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        missing = n_missing,
        levels = NA_integer_
      )
    }
  }
  
  # Apply to all columns
  stats_tbl <- dplyr::bind_rows(
    lapply(names(data), function(nm) {
      out <- summarise_column(data[[nm]])
      out$variable <- nm
      dplyr::select(out, variable, dplyr::everything())
    })
  )
  
  # Create formatted gt table
  stats_tbl |>
    gt::gt(rowname_col = "variable") |>
    gt::tab_header(title = "Summary of Survey Data Variables") |>
    gt::fmt_number(
      columns = c(min, max, mean, median, sd),
      decimals = 2
    ) |>
    gt::fmt_missing(
      columns = dplyr::everything(),
      missing_text = "-"
    ) |>
    gt::cols_label(
      type = "Type",
      min = "Min",
      max = "Max",
      mean = "Mean",
      median = "Median",
      sd = "SD",
      missing = "Missing",
      levels = "Levels"
    )
}
