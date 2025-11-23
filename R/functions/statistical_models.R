#' Statistical Modeling Functions for OSE Range Analysis
#' 
#' Functions for fitting and diagnosing statistical models (GLMM, GAM, etc.)
#' Used across multiple analysis files for consistency.
#' 
#' Author: ddlawton
#' Date: 2025-11-22

#' Fit a GLMM for ose_count with fertilizer_treatment, region, and random farmer effects
#'
#' This function fits a generalized linear mixed model (GLMM) using the
#' glmmTMB package for the response variable `ose_count`. The model includes
#' fixed effects for fertilizer treatment and region, their interaction, and
#' a random intercept for each farmer.
#'
#' @param df A data frame containing the columns:
#'   - ose_count: (integer) Response variable (count data)
#'   - fertilizer_treatment: Treatment applied (factor or character)
#'   - region: Region identifier (factor or character)
#'   - farmer: Farmer identifier (factor or character)
#'
#' @return A fitted glmmTMB object.
#' @examples
#' # Example usage:
#' # mod1 <- create_glmm_model(mission_dat[[1]])
#' @export
create_glmm_model <- function(df) {
  glmmTMB(
    ose_count ~ fertilizer_treatment * region + (1|farmer),
    data = df,
    family = poisson()
  )
}

#' Plot model diagnostics for a fitted glmmTMB object using simulated residuals
#'
#' This function generates diagnostic plots for a fitted glmmTMB model by simulating 
#' residuals using the DHARMa package and plotting them. It is helpful for evaluating 
#' model fit, checking for overdispersion, zero-inflation, outliers, and other typical 
#' model violations.
#'
#' @param glmm_mod A fitted glmmTMB model object.
#' @param n Number of simulations for residuals (default: 1000)
#' @param plot_title Optional title for the diagnostic plot
#' @return A ggplot object with diagnostic plots
#'
#' @details
#' Requires the DHARMa package to be installed and loaded.
#'
#' @examples
#' # Example usage with a glmmTMB model 'mod1':
#' # plot_model_diagnostics(mod1)
#' @export
plot_model_diagnostics <- function(glmm_mod, n = 1000, plot_title = NULL) {
    if (!requireNamespace("DHARMa", quietly = TRUE)) {
      stop("DHARMa package is required for model diagnostics")
    }
    
    sim_res <- DHARMa::simulateResiduals(glmm_mod, n = n)
    
    ggplot(data = data.frame(y = sim_res$scaledResiduals), mapping = aes(sample = y)) + 
        stat_qq(distribution = stats::qunif) +
        geom_abline(slope = 1, intercept = 0, color = 'red') +
        ggtitle(plot_title) +
        theme_pubr()
}

#' Compute estimated marginal means and Tukey pairwise comparisons for a GLMM
#'
#' This function calculates the estimated marginal means (EMMs) for the interaction
#' between fertilizer_treatment and region using a fitted glmmTMB model. It returns
#' the EMMs as a tibble, with an added mission column (integer, user-supplied), and
#' Tukey-adjusted pairwise comparisons as a tibble. Both results are returned as
#' elements in a list.
#'
#' @param glmm_mod A fitted glmmTMB model object.
#' @param mission_num Integer, mission identifier to add to the EMMs tibble (default: 1).
#'
#' @return A named list containing:
#'   - emmeans: a tibble with the estimated marginal means, including the mission column.
#'   - pairwise: a tibble of Tukey-adjusted pairwise comparisons.
#'
#' @details
#' Requires the emmeans, tibble, and dplyr packages to be installed and loaded.
#'
#' @examples
#' # Example usage for a model 'mod1':
#' # result <- get_emmeans_and_pairs(mod1, mission_num = 1)
#' # result$emmeans   # Estimated marginal means tibble
#' # result$pairwise  # Pairwise comparisons tibble
#' @export
get_emmeans_and_pairs <- function(glmm_mod, mission_num = 1) {
  emmeans_obj <- emmeans(glmm_mod, ~ fertilizer_treatment * region, type = "response")
  
  emmeans_tbl <- as_tibble(emmeans_obj) |>
    mutate(mission = mission_num)
  
  pairs_tbl <- as_tibble(pairs(emmeans_obj, adjust = "tukey"))
  
  list(
    emmeans = emmeans_tbl,
    pairwise = pairs_tbl
  )
}

#' Build a GAM model for ecological data
#'
#' @param data Data frame containing the response and predictor variables
#' @param formula Model formula for the GAM
#' @param nthreads Number of threads for parallel computation (default: 4)
#' @param family Error distribution family (default: tweedie)
#' @param method Fitting method (default: "REML")
#' @return Fitted GAM model object from mgcv::bam
build_gam <- function(
  data,
  formula = NULL,
  nthreads = 4,
  family = mgcv::tw(), 
  method = "REML"
) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv package is required for GAM fitting")
  }
  
  if (is.null(formula)) {
    stop("Formula must be provided")
  }
  
  gam_mod <- mgcv::bam(
    formula = formula,
    data = data,
    select = TRUE,
    discrete = TRUE,
    nthreads = nthreads,
    family = family,
    method = method
  )
  return(gam_mod)
}

#' Create a nicely formatted gt table summarising a mgcv::gam / gamm model
#'
#' This function extracts parametric coefficients, smooth-term summaries,
#' and high-level model metrics (adjusted R^2, deviance explained, REML,
#' scale estimate, n) from a model summary and returns a gt table suitable
#' for inclusion in a Quarto document.
#'
#' @param mod A fitted model object (typically an mgcv::gam or similar).
#' @param title Optional title to show atop the gt table.
#' @param digits Number of decimal places for numeric columns.
#' @return A gt table object.
model_summary_gt <- function(mod, title = NULL, digits = 3) {
  # Imports (lazy)
  required_pkgs <- c("gt", "dplyr", "tibble", "rlang", "stringr")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Please install the '", pkg, "' package")
    }
  }
  
  # Helper function
  as_tib <- function(x) {
    tibble::as_tibble(x, rownames = "term")
  }
  
  s <- tryCatch(summary(mod), error = function(e) NULL)
  txt <- capture.output(tryCatch(summary(mod), error = function(e) mod))
  
  # 1) Parametric coefficients -------------------------------------------------
  param_df <- NULL
  if (!is.null(s) && !is.null(s$p.table)) {
    param_df <- as_tib(s$p.table)
    names(param_df) <- make.names(names(param_df))
    param_df <- dplyr::rename_with(param_df, ~ c("term", "estimate", "std.error", "statistic", "p.value")[seq_along(.)])
    param_df <- dplyr::mutate(param_df, component = "Parametric")
  } else {
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
    names(smooth_df) <- make.names(names(smooth_df))
    smooth_df <- dplyr::rename_with(smooth_df, ~ c("term", "edf", "ref.df", "statistic", "p.value")[seq_along(.)])
    smooth_df <- dplyr::mutate(smooth_df, component = "Smooth")
  } else {
    if (requireNamespace("broom.mixed", quietly = TRUE)) {
      try({
        sm <- broom.mixed::tidy(mod)
        if (nrow(sm) > 0) {
          sm2 <- sm |> dplyr::filter(grepl("^s\\(", term))
          if (nrow(sm2) > 0) {
            keep <- intersect(c("term", "edf", "ref.df", "statistic", "p.value"), names(sm2))
            smooth_df <- sm2 |> dplyr::select(dplyr::all_of(keep))
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
    if (!is.null(s$r.sq)) r2_adj <- s$r.sq
    if (!is.null(s$dev.expl)) dev_expl <- s$dev.expl
  }
  
  # Parse printed summary for additional metrics
  if (is.na(r2_adj) || is.na(dev_expl) || is.na(reml_val) || is.na(scale_est) || is.na(n_obs)) {
    line_r <- txt[grepl("R-sq|Deviance explained", txt)]
    if (length(line_r) > 0) {
      lr <- paste(line_r, collapse = " ")
      m1 <- stringr::str_match(lr, "R[- ]?sq(?:\\(adj\\))?\\s*=\\s*([0-9.]+)")
      if (!is.na(m1[1,2])) r2_adj <- as.numeric(m1[1,2])
      m2 <- stringr::str_match(lr, "Deviance explained\\s*=\\s*([0-9.]+)\\%")
      if (!is.na(m2[1,2])) dev_expl <- as.numeric(m2[1,2]) / 100
    }
    
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
  
  # Ensure dev_expl is a fraction
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
  
  # Normalize all tibbles to same columns
  empty_cols <- function(df) {
    cols <- c("term", "estimate", "std.error", "statistic", "p.value", "edf", "ref.df", "component")
    for (nm in cols) {
      if (!nm %in% names(df)) df[[nm]] <- NA
    }
    df[, cols]
  }
  
  if (!is.null(param_df)) param_df <- empty_cols(param_df)
  if (!is.null(smooth_df)) smooth_df <- empty_cols(smooth_df)
  model_rows <- empty_cols(model_rows)
  
  # Combine rows
  combined <- dplyr::bind_rows(
    if (!is.null(param_df)) param_df else tibble::tibble(),
    if (!is.null(smooth_df)) smooth_df else tibble::tibble(),
    model_rows
  )
  
  # Format display columns  
  display <- combined |>
    dplyr::mutate(
      value = dplyr::case_when(
        component == "Model" & term == "Deviance explained" ~ paste0(formatC(100 * estimate, digits = digits, format = "f"), "%"),
        component == "Model" & term == "R-sq (adj)" ~ formatC(estimate, digits = digits, format = "f"),
        component == "Model" & term %in% c("REML", "Scale est.") ~ formatC(estimate, digits = digits, format = "f"),
        component == "Model" & term == "n" ~ formatC(estimate, digits = 0, format = "f"),
        TRUE ~ NA_character_
      ),
      estimate = dplyr::if_else(component == "Model", NA_real_, estimate)
    ) |>
    dplyr::select(component, term, estimate, std.error, statistic, p.value, edf, ref.df, value)
  
  # Build gt table
  gt_tbl <- gt::gt(display)
  
  if (!is.null(title)) {
    gt_tbl <- gt::tab_header(gt_tbl, title = title)
  }
  
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
    gt::fmt_number(columns = c("estimate", "std.error", "statistic", "edf", "ref.df"), decimals = digits) |>
    gt::fmt_number(columns = "p.value", decimals = digits) |>
    gt::cols_align(align = "left", columns = c("component", "term")) |>
    gt::cols_align(align = "right", columns = c("estimate", "std.error", "statistic", "p.value", "edf", "ref.df", "value")) |>
    gt::opt_row_striping()
    
  return(gt_tbl)
}