#' Table Variable Summary
#'
#' Summarize the variables in a data.frame or tibble for reporting.
#'
#' @param data A data.frame or tibble
#' @return A gt table summarizing data types and basic statistics per variable.
#' @import dplyr gt tibble
#' @export
table_variable_summary <- function(data) {
  # Helper: most frequent level extractor
  top_n_levels <- function(x, n = 3) {
    x <- as.character(x[!is.na(x)])
    if (length(x) == 0) return(rep(NA, n))
    tbl <- sort(table(x), decreasing = TRUE)
    vals <- names(tbl)[1:n]
    # Pad with NA if fewer than n levels
    if (length(vals) < n) vals <- c(vals, rep(NA, n - length(vals)))
    return(vals)
  }

  # Main stats extraction per column
  summarise_column <- function(x) {
    x_nona <- x[!is.na(x)]
    type <- class(x)[1]
    n_missing <- sum(is.na(x))
    if (is.numeric(x)) {
      tibble::tibble(
        type      = type,
        min       = min(x_nona, na.rm = TRUE),
        max       = max(x_nona, na.rm = TRUE),
        mean      = mean(x_nona, na.rm = TRUE),
        median    = median(x_nona, na.rm = TRUE),
        sd        = sd(x_nona, na.rm = TRUE),
        missing   = n_missing,
        levels    = NA
      )
    } else if (is.factor(x) || is.character(x)) {
      lvl <- if(is.factor(x)) nlevels(x) else length(unique(x_nona))
      vals <- top_n_levels(x, 3)
      tibble::tibble(
        type      = type,
        min       = NA,
        max       = NA,
        mean      = NA,
        median    = NA,
        sd        = NA,
        missing   = n_missing,
        levels    = lvl
      )
    } else {
      tibble::tibble(
        type      = type,
        min       = NA,
        max       = NA,
        mean      = NA,
        median    = NA,
        sd        = NA,
        missing   = n_missing,
        levels    = NA
      )
    }
  }

  stats_tbl <- dplyr::bind_rows(
    lapply(names(data), function(nm) {
      out <- summarise_column(data[[nm]])
      out$variable <- nm
      dplyr::select(out, variable, dplyr::everything())
    })
  )

  out <- stats_tbl %>%
    gt::gt(rowname_col = "variable") %>%
    gt::tab_header(title = "Summary of Survey Data Variables") %>%
    gt::fmt_number(
      columns = c(min, max, mean, median, sd), decimals = 2
    ) %>%
    gt::fmt_missing(
      columns = dplyr::everything(), missing_text = "-"
    ) %>%
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
  return(out)
}