#' Build a GAM model for plant cover data
#'
#' @param data Data frame containing plant cover data.
#' @param formula Model formula.
#' @param nthreads Number of threads for parallel computation.
#' @param k Parameter for basis dimensions (default 10).
#' @return Fitted bam model object.
build_gam <- function(
  data,
  formula = NULL,
  nthreads = 9,
  family = mgcv::tw(), 
  k = 10
) {
  # Load mgcv if needed
  if (!requireNamespace("mgcv", quietly = TRUE)) stop("mgcv must be installed")
  gam_mod <- mgcv::bam(
    formula = formula,
    data = data,
    select = TRUE,
    discrete = TRUE,
    nthreads = nthreads,
    family = family
  )
  return(gam_mod)
}

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


#' Create a nicely formatted gt table summarising a mgcv::gam / gamm model
#'
#' This function extracts parametric coefficients, smooth-term summaries,
#' and high-level model metrics (adjusted R^2, deviance explained, REML,
#' scale estimate, n) from a model summary and returns a gt table suitable
#' for inclusion in a Quarto document.
#'
#' The function tries to use the structured pieces from summary(mod) (p.table,
#' s.table, r.sq, dev.expl). Where pieces are missing it will fall back to
#' broom.mixed::tidy() for fixed/smooth terms and will parse the printed
#' summary output to pick up REML/scale/n if needed.
#'
#' @param mod A fitted model object (typically an mgcv::gam or similar).
#' @param title Optional title to show atop the gt table.
#' @param digits Number of decimal places for numeric columns.
#' @return A gt table object.
#' @examples
#' # library(mgcv); library(gt)
#' # m <- gam(ose_count ~ s(temperature, region, bs = "fs", k = 10) + s(farmer, bs="re") + s(mission_number, bs="re"),
#' #          family = tweedie(var.power = 1.377, link.power = 0), data = dat, method = "REML")
#' # tbl <- model_summary_gt(m, title = "Model summary: locust density")
#' # print(tbl)
model_summary_gt <- function(mod, title = NULL, digits = 3) {
  # Imports (lazy)
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Please install the 'gt' package to render the table: install.packages('gt')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Please install 'dplyr' (or attach tidyverse).")
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Please install 'tibble'.")
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Please install 'rlang'.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Please install 'stringr'.")
  }
  # Helper
  as_tib <- function(x) {
    tibble::as_tibble(x, rownames = "term")
  }
  
  s <- tryCatch(summary(mod), error = function(e) NULL)
  txt <- capture.output(tryCatch(summary(mod), error = function(e) mod))
  
  # 1) Parametric coefficients -------------------------------------------------
  param_df <- NULL
  if (!is.null(s) && !is.null(s$p.table)) {
    param_df <- as_tib(s$p.table)
    # Column names from typical summary: Estimate Std. Error t value Pr(>|t|)
    # Normalize column names
    names(param_df) <- make.names(names(param_df))
    param_df <- dplyr::rename_with(param_df, ~ c("term", "estimate", "std.error", "statistic", "p.value")[seq_along(.)])
    param_df <- dplyr::mutate(param_df, component = "Parametric")
  } else {
    # fallback to broom.mixed (or broom) for fixed effects
    if (requireNamespace("broom.mixed", quietly = TRUE)) {
      try({
        pm <- broom.mixed::tidy(mod, effects = "fixed")
        if (nrow(pm) > 0) {
          param_df <- pm |> dplyr::select(term, estimate = estimate, std.error = std.error,
                                          statistic = statistic, p.value = p.value)
          param_df$component <- "Parametric"
        }
      }, silent = TRUE)
    }
  }
  
  # 2) Smooth terms ------------------------------------------------------------
  smooth_df <- NULL
  if (!is.null(s) && !is.null(s$s.table)) {
    smooth_df <- as_tib(s$s.table)
    # typical columns: edf Ref.df F p-value
    names(smooth_df) <- make.names(names(smooth_df))
    # rename to consistent names
    smooth_df <- dplyr::rename_with(smooth_df, ~ c("term", "edf", "ref.df", "statistic", "p.value")[seq_along(.)])
    smooth_df <- dplyr::mutate(smooth_df, component = "Smooth")
  } else {
    # fallback to broom.mixed::tidy (user already had tidy_mod)
    if (requireNamespace("broom.mixed", quietly = TRUE)) {
      try({
        sm <- broom.mixed::tidy(mod)
        if (nrow(sm) > 0) {
          # pick rows that look like smooth terms if present (term names starting with "s(")
          sm2 <- sm |> dplyr::filter(grepl("^s\\(", term))
          if (nrow(sm2) > 0) {
            # broom.mixed::tidy for smooths often includes edf/ref.df/statistic/p.value
            # keep those columns where present
            keep <- intersect(c("term", "edf", "ref.df", "statistic", "p.value"), names(sm2))
            smooth_df <- sm2 |> dplyr::select(dplyr::all_of(keep))
            # ensure columns present
            if (!"edf" %in% names(smooth_df)) smooth_df$edf <- NA_real_
            if (!"ref.df" %in% names(smooth_df)) smooth_df$ref.df <- NA_real_
            if (!"statistic" %in% names(smooth_df)) smooth_df$statistic <- NA_real_
            if (!"p.value" %in% names(smooth_df)) smooth_df$p.value <- NA_real_
            smooth_df$component <- "Smooth"
          }
        }
      }, silent = TRUE)
    }
  }
  
  # 3) Model-level metrics ----------------------------------------------------
  r2_adj <- NA_real_
  dev_expl <- NA_real_
  reml_val <- NA_real_
  scale_est <- NA_real_
  n_obs <- NA_integer_
  
  if (!is.null(s)) {
    # mgcv::summary.gam stores these as numeric elements
    if (!is.null(s$r.sq)) r2_adj <- s$r.sq
    if (!is.null(s$dev.expl)) dev_expl <- s$dev.expl
  }
  # fallback: parse printed summary text for R-sq and deviance explained and REML/Scale/n
  if (is.na(r2_adj) || is.na(dev_expl) || is.na(reml_val) || is.na(scale_est) || is.na(n_obs)) {
    # lines may contain R-sq and Deviance explained
    # e.g. "R-sq.(adj) =  0.381   Deviance explained = 39.4%"
    line_r <- txt[grepl("R-sq|Deviance explained", txt)]
    if (length(line_r) > 0) {
      lr <- paste(line_r, collapse = " ")
      m1 <- stringr::str_match(lr, "R[- ]?sq(?:\\(adj\\))?\\s*=\\s*([0-9.]+)")
      if (!is.na(m1[1,2])) r2_adj <- as.numeric(m1[1,2])
      m2 <- stringr::str_match(lr, "Deviance explained\\s*=\\s*([0-9.]+)\\%")
      if (!is.na(m2[1,2])) dev_expl <- as.numeric(m2[1,2]) / 100
    }
    # REML / Scale / n line example:
    # "-REML = 2504.3  Scale est. = 2.4146    n = 624"
    line_rs <- txt[grepl("REML|Scale est|Scale", txt)]
    if (length(line_rs) > 0) {
      lrs <- paste(line_rs, collapse = " ")
      m_reml <- stringr::str_match(lrs, "REML\\s*=\\s*([0-9.\\-eE]+)")
      if (!is.na(m_reml[1,2])) reml_val <- as.numeric(m_reml[1,2])
      m_scale <- stringr::str_match(lrs, "Scale est\\.?\\s*=\\s*([0-9.\\-eE]+)")
      if (!is.na(m_scale[1,2])) scale_est <- as.numeric(m_scale[1,2])
      m_n <- stringr::str_match(lrs, "n\\s*=\\s*([0-9]+)")
      if (!is.na(m_n[1,2])) n_obs <- as.integer(m_n[1,2])
    }
  }
  
  # If dev_expl was a fraction (0-1) keep; if parsed as percent ensure fraction
  if (!is.na(dev_expl) && dev_expl > 1) dev_expl <- dev_expl / 100
  
  # Compose model metrics tibble
  model_rows <- tibble::tibble(
    component = "Model",
    term = c("R-sq (adj)", "Deviance explained", "REML", "Scale est.", "n"),
    estimate = c(r2_adj, dev_expl, reml_val, scale_est, as.numeric(n_obs)),
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    edf = NA_real_,
    ref.df = NA_real_
  )
  
  # Normalize param and smooth tibbles to same columns
  empty_cols <- function(df) {
    cols <- c("term", "estimate", "std.error", "statistic", "p.value", "edf", "ref.df", "component")
    for (nm in cols) {
      if (!nm %in% names(df)) df[[nm]] <- NA
    }
    # ensure ordering
    df <- df[, cols]
    df
  }
  if (!is.null(param_df)) param_df <- empty_cols(param_df)
  if (!is.null(smooth_df)) smooth_df <- empty_cols(smooth_df)
  model_rows <- empty_cols(model_rows)
  
  # Combine rows (preserve order: Parametric, Smooth, Model)
  combined <- dplyr::bind_rows(
    if (!is.null(param_df)) param_df else tibble::tibble(),
    if (!is.null(smooth_df)) smooth_df else tibble::tibble(),
    model_rows
  )
  
  # Coerce numeric columns to numeric
  numeric_cols <- c("estimate", "std.error", "statistic", "p.value", "edf", "ref.df")
  for (nc in numeric_cols) {
    if (nc %in% names(combined)) {
      combined[[nc]] <- suppressWarnings(as.numeric(combined[[nc]]))
    }
  }
  
  # Prepare display columns
  display <- combined |>
    dplyr::mutate(
      # Pretty strings for the "estimate" column when component == Model for deviance already fraction
      value = dplyr::case_when(
        component == "Model" & term == "Deviance explained" ~ paste0(formatC(100 * estimate, digits = digits, format = "f"), "%"),
        component == "Model" & term == "R-sq (adj)" ~ formatC(estimate, digits = digits, format = "f"),
        component == "Model" & term %in% c("REML", "Scale est.") ~ formatC(estimate, digits = digits, format = "f"),
        component == "Model" & term == "n" ~ formatC(estimate, digits = 0, format = "f"),
        TRUE ~ NA_character_
      ),
      estimate = dplyr::if_else(component == "Model", NA_real_, estimate),
      p.value = dplyr::if_else(!is.na(p.value) & !is.na(p.value), p.value, NA_real_)
    ) |>
    # select the visible columns and leave component for grouping
    dplyr::select(component, term, estimate, std.error, statistic, p.value, edf, ref.df, value)
  
  # Build gt table
  gt_tbl <- gt::gt(display)
  
  # Optionally add a title
  if (!is.null(title)) {
    gt_tbl <- gt::tab_header(gt_tbl, title = title)
  }
  
  # Labels
  gt_tbl <- gt_tbl |>
    gt::cols_label(
      component = "Component",
      term = "Term",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t / F",
      p.value = "p-value",
      edf = "edf",
      ref.df = "Ref.df",
      value = "Value"
    ) |>
    # Format numeric columns
    gt::fmt_number(columns = c("estimate", "std.error", "statistic", "edf", "ref.df"), decimals = digits) |>
    gt::fmt_number(columns = "p.value", decimals = digits) |>
    # Align
    gt::cols_align(align = "left", columns = c("component", "term")) |>
    gt::cols_align(align = "right", columns = c("estimate", "std.error", "statistic", "p.value", "edf", "ref.df", "value")) |>
    # Group rows visually by component using row shading
    gt::opt_row_striping()   
  return(gt_tbl)
}