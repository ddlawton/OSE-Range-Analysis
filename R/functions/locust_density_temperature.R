#' Temperature Analysis Functions for OSE Range Analysis
#' 
#' Specialized functions for analyzing temperature effects on locust density.
#' These complement the general statistical modeling functions.
#' 
#' Author: ddlawton
#' Date: 2025-11-22

# NOTE: General GAM building and model summary functions have been moved to statistical_models.R
# This file now contains only temperature-specific analysis functions

#' Create a temperature x region smooth plot for a model (for use in Quarto)
#'
#' This function wraps your original pipeline into a reusable plotting function.
#' It uses smooth_estimates(mod) and expects the resulting table to contain
#' columns `.smooth`, `.estimate` and `.se`. The intercept of `mod` is added
#' to the smooth estimates (same behaviour as your snippet).
#'
#' Dependencies: ggplot2, ggpubr, dplyr (MetBrewer optional).
#'
#' @param mod A fitted model object that works with smooth_estimates().
#' @param smooth A character string naming the smooth to filter for (default "s(temperature,region)").
#' @param xvar The name of the x variable column in the smooth_estimates output (default "temperature").
#' @param group The name of the grouping column to use for color/group aesthetics (default "region").
#' @param xlab X-axis label (defaults to xvar).
#' @param ylab Y-axis label (defaults to "estimate (adjusted)").
#' @param title Plot title.
#' @param palette Palette name passed to MetBrewer::scale_color_met_d() if MetBrewer is installed.
#' @param ribbon_fill Fill colour for ribbon.
#' @param ribbon_alpha Alpha for ribbon.
#' @return A ggplot object.
#' @export
plot_temperature_smooth <- function(mod,
                                    smooth = "s(temperature,region)",
                                    xvar = "temperature",
                                    group = "region",
                                    xlab = NULL,
                                    ylab = NULL,
                                    title = "locust density x temperature",
                                    palette = "Degas",
                                    ribbon_fill = "grey70",
                                    ribbon_alpha = 0.3) {
  # lazy defaults
  if (is.null(xlab)) xlab <- xvar
  if (is.null(ylab)) ylab <- "estimate (adjusted)"
  
  # validate smooth_estimates function
  if (!exists("smooth_estimates", mode = "function")) {
    stop("smooth_estimates() not found. Please load the package that provides it (e.g. 'gratia').")
  }
  
  est <- smooth_estimates(mod)
  
  if (!(".smooth" %in% names(est))) stop("smooth_estimates output does not contain a `.smooth` column.")
  if (!(".estimate" %in% names(est))) stop("smooth_estimates output does not contain a `.estimate` column.")
  if (!(".se" %in% names(est))) stop("smooth_estimates output does not contain a `.se` column.")
  if (!(xvar %in% names(est))) stop(sprintf("x variable '%s' not found in smooth_estimates output.", xvar))
  if (!(group %in% names(est))) stop(sprintf("group variable '%s' not found in smooth_estimates output.", group))
  
  # compute adjusted estimate and ribbon endpoints
  intercept <- tryCatch(stats::coef(mod)[1], error = function(e) {
    warning("Could not extract intercept from model with coef(mod)[1]: ", conditionMessage(e), 
            "\nUsing 0 as intercept offset.")
    0
  })
  
  est <- est |>
    dplyr::filter(.data$.smooth == smooth) |>
    dplyr::mutate(
      adj_est = .data$.estimate + intercept,
      ymin = adj_est - .data$.se,
      ymax = adj_est + .data$.se
    )
  
  # build plot
  p <- ggplot2::ggplot(est, ggplot2::aes_string(x = xvar)) +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "ymin", ymax = "ymax", group = group),
                         fill = ribbon_fill, alpha = ribbon_alpha) +
    ggplot2::geom_line(ggplot2::aes_string(y = "adj_est", color = group), size = 1) +
    ggpubr::theme_pubr() +
    ggplot2::labs(title = title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  # Try to add MetBrewer palette; fall back to viridis if not available or fails
  if (requireNamespace("MetBrewer", quietly = TRUE)) {
    p <- tryCatch(
      p + MetBrewer::scale_color_met_d(name = palette),
      error = function(e) p + ggplot2::scale_color_viridis_d()
    )
  } else {
    p <- p + ggplot2::scale_color_viridis_d()
  }
  
  return(p)
}

# NOTE: model_summary_gt function has been moved to statistical_models.R to avoid duplication