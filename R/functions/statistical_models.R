# ==============================================================================
# Statistical Modeling Functions for OSE Range Analysis
# ==============================================================================
# 
# Standardized model fitting, diagnostics, and summary functions
# Supports GLMM (glmmTMB), GAM (mgcv), emmeans extraction, and formatted tables
#
# Author: ddlawton
# Created: 2025-11-22
# Updated: 2026-05-24 - Optimized, improved documentation, reduced redundancy
# ==============================================================================

# ==============================================================================
# GLMM Model Fitting
# ==============================================================================

#' Fit GLMM for OSE Count Data
#'
#' Fits generalized linear mixed model with Poisson family for count data.
#' Includes fertilizer treatment × region interaction and random farmer intercept.
#'
#' @param df Data frame with: ose_count, fertilizer_treatment, region, farmer
#' @return Fitted glmmTMB object
#' @export
#' 
#' @examples
#' \dontrun{
#' model <- create_count_glmm_model(mission1_data)
#' }
create_count_glmm_model <- function(df) {
  glmmTMB::glmmTMB(
    ose_count ~ fertilizer_treatment * region + (1 | farmer),
    data = df,
    family = poisson()
  )
}

#' Fit GLMM for OSE Damage Data
#'
#' Fits generalized linear mixed model with Tweedie family for damage percentage data.
#' Includes fertilizer treatment × region interaction and random farmer intercept.
#'
#' @param df Data frame with: ose_damage_percent, fertilizer_treatment, region, farmer
#' @return Fitted glmmTMB object
#' @export
#' 
#' @examples
#' \dontrun{
#' model <- create_damage_glmm_model(mission1_data)
#' }
create_damage_glmm_model <- function(df) {
  glmmTMB::glmmTMB(
    ose_damage_percent ~ fertilizer_treatment * region + (1 | farmer),
    data = df,
    family = glmmTMB::tweedie()
  )
}

# ==============================================================================
# Model Diagnostics
# ==============================================================================

#' Plot Model Diagnostics Using Simulated Residuals
#'
#' Generates QQ plot of simulated residuals using DHARMa package
#' for evaluating model fit, overdispersion, and violations.
#'
#' @param glmm_mod Fitted glmmTMB model object
#' @param n Integer. Number of simulations (default: 1000)
#' @param plot_title Character. Optional title for diagnostic plot
#' @return ggplot object with QQ plot
#' @export
#' 
#' @examples
#' \dontrun{
#' plot_model_diagnostics(model, plot_title = "Mission 1 Diagnostics")
#' }
plot_model_diagnostics <- function(glmm_mod, n = 1000, plot_title = NULL) {
  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    stop("DHARMa package is required for model diagnostics")
  }
  
  sim_res <- DHARMa::simulateResiduals(glmm_mod, n = n)
  
  ggplot2::ggplot(
    data = data.frame(y = sim_res$scaledResiduals),
    mapping = ggplot2::aes(sample = y)
  ) +
    ggplot2::stat_qq(distribution = stats::qunif) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::ggtitle(plot_title) +
    ggpubr::theme_pubr()
}

# ==============================================================================
# Emmeans and Pairwise Comparisons
# ==============================================================================

#' Compute Emmeans and Tukey Pairwise Comparisons
#'
#' Calculates estimated marginal means for fertilizer × region interaction
#' and Tukey-adjusted pairwise comparisons.
#'
#' @param glmm_mod Fitted glmmTMB model object
#' @param mission_num Integer. Mission identifier to add to emmeans (default: 1)
#' @return Named list with two tibbles:
#'   \item{emmeans}{Estimated marginal means with mission column}
#'   \item{pairwise}{Tukey-adjusted pairwise comparisons}
#' @export
#' 
#' @examples
#' \dontrun{
#' result <- get_emmeans_and_pairs(model, mission_num = 1)
#' result$emmeans   # EMMs tibble
#' result$pairwise  # Comparisons tibble
#' }
get_emmeans_and_pairs <- function(glmm_mod, mission_num = 1) {
  emmeans_obj <- emmeans::emmeans(
    glmm_mod, 
    ~ fertilizer_treatment * region, 
    type = "response"
  )
  
  emmeans_tbl <- tibble::as_tibble(emmeans_obj) |>
    dplyr::mutate(mission = mission_num)
  
  pairs_tbl <- tibble::as_tibble(
    emmeans::contrast(emmeans_obj, method = "pairwise", adjust = "tukey")
  )
  
  list(
    emmeans = emmeans_tbl,
    pairwise = pairs_tbl
  )
}

# ==============================================================================
# GAM Model Fitting
# ==============================================================================

#' Build GAM Model for Ecological Data
#'
#' Fits Generalized Additive Model using mgcv::bam with parallel computation.
#' Supports smooth terms and mixed effects.
#'
#' @param data Data frame with response and predictor variables
#' @param formula Model formula for GAM (required)
#' @param nthreads Integer. Number of threads for parallel computation (default: 4)
#' @param family Error distribution family (default: mgcv::tw() for Tweedie)
#' @param method Character. Fitting method (default: "REML")
#' @return Fitted GAM model object from mgcv::bam
#' @export
#' 
#' @examples
#' \dontrun{
#' gam_mod <- build_gam(
#'   data = data,
#'   formula = ose_count ~ s(temperature, by = region) + s(farmer, bs = "re"),
#'   nthreads = 8
#' )
#' }
build_gam <- function(data, formula = NULL, nthreads = 4, 
                     family = mgcv::tw(), method = "REML") {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv package is required for GAM fitting")
  }
  
  if (is.null(formula)) {
    stop("Formula must be provided")
  }
  
  mgcv::bam(
    formula = formula,
    data = data,
    select = TRUE,
    discrete = TRUE,
    nthreads = nthreads,
    family = family,
    method = method
  )
}

# ==============================================================================
# Model Summary Tables
# ==============================================================================

#' Create Formatted GT Table for GAM Model Summary
#'
#' Extracts parametric coefficients, smooth terms, and model-level metrics
#' (R², deviance explained, REML, scale, n) and formats as gt table.
#'
#' @param mod Fitted model object (mgcv::gam or similar)
#' @param title Character. Optional title for table
#' @param digits Integer. Decimal places for numeric columns (default: 3)
#' @return gt table object
#' @export
#' 
#' @examples
#' \dontrun{
#' model_summary_gt(gam_model, title = "Temperature Model Summary")
#' }
model_summary_gt <- function(mod, title = NULL, digits = 3) {
  # Validate dependencies
  required_pkgs <- c("gt", "dplyr", "tibble", "stringr")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Please install the '", pkg, "' package")
    }
  }
  
  # Helper to convert to tibble with row names
  as_tib <- function(x) {
    tibble::as_tibble(x, rownames = "term")
  }
  
  s <- tryCatch(summary(mod), error = function(e) NULL)
  txt <- capture.output(tryCatch(summary(mod), error = function(e) mod))
  
  # -------------------------
  # 1. Parametric Coefficients
  # -------------------------
  param_df <- NULL
  if (!is.null(s) && !is.null(s$p.table)) {
    param_df <- as_tib(s$p.table)
    names(param_df) <- make.names(names(param_df))
    param_df <- dplyr::rename_with(
      param_df, 
      ~ c("term", "estimate", "std.error", "statistic", "p.value")[seq_along(.)]
    )
    param_df$component <- "Parametric"
  } else if (requireNamespace("broom.mixed", quietly = TRUE)) {
    try({
      pm <- broom.mixed::tidy(mod, effects = "fixed")
      if (nrow(pm) > 0) {
        param_df <- pm |>
          dplyr::select(term, estimate, std.error, statistic, p.value)
        param_df$component <- "Parametric"
      }
    }, silent = TRUE)
  }
  
  # -------------------------
  # 2. Smooth Terms
  # -------------------------
  smooth_df <- NULL
  if (!is.null(s) && !is.null(s$s.table)) {
    smooth_df <- as_tib(s$s.table)
    names(smooth_df) <- make.names(names(smooth_df))
    smooth_df <- dplyr::rename_with(
      smooth_df,
      ~ c("term", "edf", "ref.df", "statistic", "p.value")[seq_along(.)]
    )
    smooth_df$component <- "Smooth"
  } else if (requireNamespace("broom.mixed", quietly = TRUE)) {
    try({
      sm <- broom.mixed::tidy(mod)
      if (nrow(sm) > 0) {
        sm2 <- sm |> dplyr::filter(grepl("^s\\(", term))
        if (nrow(sm2) > 0) {
          keep <- intersect(
            c("term", "edf", "ref.df", "statistic", "p.value"),
            names(sm2)
          )
          smooth_df <- sm2 |> dplyr::select(dplyr::all_of(keep))
          # Ensure all columns exist
          if (!"edf" %in% names(smooth_df)) smooth_df$edf <- NA_real_
          if (!"ref.df" %in% names(smooth_df)) smooth_df$ref.df <- NA_real_
          if (!"statistic" %in% names(smooth_df)) smooth_df$statistic <- NA_real_
          if (!"p.value" %in% names(smooth_df)) smooth_df$p.value <- NA_real_
          smooth_df$component <- "Smooth"
        }
      }
    }, silent = TRUE)
  }
  
  # -------------------------
  # 3. Model-Level Metrics
  # -------------------------
  r2_adj <- NA_real_
  dev_expl <- NA_real_
  reml_val <- NA_real_
  scale_est <- NA_real_
  n_obs <- NA_integer_
  
  if (!is.null(s)) {
    if (!is.null(s$r.sq)) r2_adj <- s$r.sq
    if (!is.null(s$dev.expl)) dev_expl <- s$dev.expl
  }
  
  # Parse printed summary for additional metrics
  if (any(is.na(c(r2_adj, dev_expl, reml_val, scale_est, n_obs)))) {
    # Extract R-squared and deviance explained
    line_r <- txt[grepl("R-sq|Deviance explained", txt)]
    if (length(line_r) > 0) {
      lr <- paste(line_r, collapse = " ")
      m1 <- stringr::str_match(lr, "R[- ]?sq(?:\\(adj\\))?\\s*=\\s*([0-9.]+)")
      if (!is.na(m1[1, 2])) r2_adj <- as.numeric(m1[1, 2])
      m2 <- stringr::str_match(lr, "Deviance explained\\s*=\\s*([0-9.]+)\\%")
      if (!is.na(m2[1, 2])) dev_expl <- as.numeric(m2[1, 2]) / 100
    }
    
    # Extract REML, scale, and n
    line_rs <- txt[grepl("REML|Scale est|Scale", txt)]
    if (length(line_rs) > 0) {
      lrs <- paste(line_rs, collapse = " ")
      m_reml <- stringr::str_match(lrs, "REML\\s*=\\s*([0-9.\\-eE]+)")
      if (!is.na(m_reml[1, 2])) reml_val <- as.numeric(m_reml[1, 2])
      m_scale <- stringr::str_match(lrs, "Scale est\\.?\\s*=\\s*([0-9.\\-eE]+)")
      if (!is.na(m_scale[1, 2])) scale_est <- as.numeric(m_scale[1, 2])
      m_n <- stringr::str_match(lrs, "n\\s*=\\s*([0-9]+)")
      if (!is.na(m_n[1, 2])) n_obs <- as.integer(m_n[1, 2])
    }
  }
  
  # Ensure deviance explained is a fraction
  if (!is.na(dev_expl) && dev_expl > 1) dev_expl <- dev_expl / 100
  
  # Create model metrics tibble
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
  
  # -------------------------
  # 4. Combine and Format
  # -------------------------
  # Normalize column structure
  empty_cols <- function(df) {
    cols <- c("term", "estimate", "std.error", "statistic", "p.value", 
              "edf", "ref.df", "component")
    for (nm in cols) {
      if (!nm %in% names(df)) df[[nm]] <- NA
    }
    df[, cols]
  }
  
  if (!is.null(param_df)) param_df <- empty_cols(param_df)
  if (!is.null(smooth_df)) smooth_df <- empty_cols(smooth_df)
  model_rows <- empty_cols(model_rows)
  
  # Combine all rows
  combined <- dplyr::bind_rows(
    if (!is.null(param_df)) param_df else tibble::tibble(),
    if (!is.null(smooth_df)) smooth_df else tibble::tibble(),
    model_rows
  )
  
  # Create display column for model metrics
  display <- combined |>
    dplyr::mutate(
      value = dplyr::case_when(
        component == "Model" & term == "Deviance explained" ~ 
          paste0(formatC(100 * estimate, digits = digits, format = "f"), "%"),
        component == "Model" & term %in% c("R-sq (adj)", "REML", "Scale est.") ~ 
          formatC(estimate, digits = digits, format = "f"),
        component == "Model" & term == "n" ~ 
          formatC(estimate, digits = 0, format = "f"),
        TRUE ~ NA_character_
      ),
      estimate = dplyr::if_else(component == "Model", NA_real_, estimate)
    ) |>
    dplyr::select(component, term, estimate, std.error, statistic, p.value, 
                  edf, ref.df, value)
  
  # Build gt table
  gt_tbl <- gt::gt(display)
  
  if (!is.null(title)) {
    gt_tbl <- gt::tab_header(gt_tbl, title = title)
  }
  
  gt_tbl |>
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
    gt::fmt_number(
      columns = c("estimate", "std.error", "statistic", "edf", "ref.df"),
      decimals = digits
    ) |>
    gt::fmt_number(columns = "p.value", decimals = digits) |>
    gt::cols_align(align = "left", columns = c("component", "term")) |>
    gt::cols_align(
      align = "right",
      columns = c("estimate", "std.error", "statistic", "p.value", 
                  "edf", "ref.df", "value")
    ) |>
    gt::opt_row_striping()
}
