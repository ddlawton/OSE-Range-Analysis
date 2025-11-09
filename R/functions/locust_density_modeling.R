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
#' @return The diagnostic plot as produced by DHARMa::plot.DHARMa(). No return value, called for side effects.
#'
#' @details
#' Requires the DHARMa package to be installed and loaded.
#'
#' @examples
#' # Example usage with a glmmTMB model 'mod1':
#' # plot_model_diagnostics(mod1)
#' @export
plot_model_diagnostics <- function(glmm_mod, n = 1000, plot_title = NULL) {
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

#' Plot OSE density by region and mission date, faceted by mission
#'
#' This function creates a series of ggplot jitter plots of OSE/100m counts,
#' colored by fertilizer treatment, faceted by mission and region. Plots are
#' arranged using patchwork, with consistent legend placement and plot tagging.
#'
#' @param data A data.frame or tibble containing columns: year, region, farmer,
#'   fertilizer_treatment, mission_number, ose_count
#' @param regions A character vector of region names to use as factor levels.
#'   Defaults to c('Saint Louis','Thies','Fatick','Kaffrine').
#' @param legend_position Position of legend in combined plot. Defaults to 'bottom'.
#' @return A patchwork object with combined, annotated plots.
#' @import ggplot2 dplyr patchwork rstatix
#' @examples
#' # data must have required columns
plot_mission_density <- function(
    data,
    emmeans,
    emmean_point_size = 5,
    regions = c('Saint Louis','Thies','Fatick','Kaffrine'),
    legend_position = 'bottom'
) {
    # Data prep
    density_dat <- data |>
        dplyr::select(year,region,farmer,fertilizer_treatment,mission_number,ose_count) |>
        dplyr::mutate(region = factor(region, levels = regions)) |>
        dplyr::mutate(mission_date = dplyr::case_when(
            mission_number == 1 ~ 'Mission 1 (July)',
            mission_number == 2 ~ 'Mission 2 (September)',
            mission_number == 3 ~ 'Mission 3 (October)'
        ))

    mission_levels <- c('Mission 1 (July)', 'Mission 2 (September)', 'Mission 3 (October)')

    # ---- Map mission_numer in emmeans to mission_date labels ----
    emmeans <- emmeans |>
        dplyr::mutate(mission_date = dplyr::case_when(
            mission_numer == "1" | mission_numer == 1 ~ "Mission 1 (July)",
            mission_numer == "2" | mission_numer == 2 ~ "Mission 2 (September)",
            mission_numer == "3" | mission_numer == 3 ~ "Mission 3 (October)"
        )) |>
        dplyr::mutate(region = factor(region, levels = regions))

    plot_mission <- function(md) {
        ggplot(
            dplyr::filter(density_dat, mission_date == md),
            aes(x=region, y=ose_count, color=fertilizer_treatment)
        ) +
        geom_jitter(position = position_jitterdodge(jitter.width=0.2, jitter.height=0),
            pch=21, alpha=0.3) +
        geom_jitter(
            data = dplyr::filter(emmeans, mission_date == md),
            aes(y = rate, x = region, fill = fertilizer_treatment), # Keep mappings explicit!
            position = position_jitterdodge(jitter.width=0.00001, jitter.height=0),
            pch=21,
            color = 'black',
            size = emmean_point_size
        ) +
        scale_color_manual(values = c('black','dark green')) +
        scale_fill_manual(values = c('black','dark green')) +
        ylab('OSE/100m') +
        labs(title = md) +
        theme_pubr(legend = 'bottom') +
        theme(legend.title = element_blank())
    }

    plots <- lapply(mission_levels, plot_mission)

    combined_plot <- patchwork::wrap_plots(plots, ncol=2) +
        patchwork::plot_annotation(tag_levels = 'a') +
        patchwork::plot_layout(guides = "collect") &
        theme(legend.position = legend_position)

    return(combined_plot)
}

#' Plot Estimated Marginal Means (EMMs) by Mission, Region, and Fertilizer Treatment
#'
#' This function creates a publication-ready plot showing the estimated marginal means
#' (EMMs) of OSE counts across missions, grouped by region and fertilizer treatment.
#' It includes point estimates, 95% confidence intervals, and connected trend lines.
#'
#' @param emeans A data frame or tibble containing the estimated marginal means data.
#'   Must include columns:
#'   - `mission`: numeric or factor identifying missions (will be coerced to integer for x-axis)
#'   - `rate`: numeric response variable (e.g., estimated marginal mean)
#'   - `region`: grouping variable for color
#'   - `fertilizer_treatment`: grouping variable for line type
#'   - `asymp.LCL` and `asymp.UCL`: lower and upper confidence limits
#'
#' @param color_palette Character. Name of a MetBrewer palette to use for color scale.
#'   Default is `"Degas"`.
#'
#' @param ylab Character. Label for the y-axis. Default is `"OSE count"`.
#' @param xlab Character. Label for the x-axis. Default is `"Mission"`.
#'
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' # Example usage:
#' my_plot <- plot_emmeans(emeans)
#' print(my_plot)
#' }
#'
#' @export
plot_emmeans <- function(emeans,
                         color_palette = "Degas",
                         ylab = "OSE count",
                         xlab = "Mission") {
  library(ggplot2)
  library(ggpubr)
  library(MetBrewer)

  ggplot(emeans, aes(
    x = as.integer(mission),
    y = rate,
    color = region,
    linetype = fertilizer_treatment
  )) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0) +
    geom_line() +
    theme_pubr() +
    scale_color_met_d(name = color_palette) +
    scale_x_continuous(breaks = c(1, 2, 3)) +
    ylab(ylab) +
    xlab(xlab)
}

#' Build a GAM model for plant cover data
#'
#' @param data Data frame containing plant cover data.
#' @param formula Model formula.
#' @param nthreads Number of threads for parallel computation.
#' @param k Parameter for basis dimensions (default 10).
#' @return Fitted bam model object.
build_gam <- function(
  data,
  formula = ose_count ~ te(percent_ground_cover, region_treat, bs = 'fs', k = 10) + s(farmer, bs = 're'),
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

#' Plot GAM term smooths using gratia and ggplot2
#'
#' @param gam_mod Fitted GAM model object from mgcv::bam.
#' @return A ggplot object of the smooths (excluding random effect) faceted by region.
plot_gam_smooths_gratia <- function(gam_mod,xlab = 'ground cover (%)',ylab = 'OSE count (modeled)') {
  if (!requireNamespace("gratia", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("patchwork", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("ggpubr", quietly = TRUE)) {
    stop("gratia, ggplot2, patchwork, tidyr, ggpubr must be installed")
  }
  
  ests <- gratia::smooth_estimates(gam_mod)
  ests$adj_est <- ests$.estimate + coef(gam_mod)[1]
  ests <- ests[ests$.smooth != "s(farmer)", ]
  
  ests <- tidyr::separate(ests, region_treat, sep = '_', into = c('region', 'treatment'), remove = FALSE)
  
  plots <- lapply(unique(ests$region), function(reg) {
    df_region <- subset(ests, region == reg)
    ggplot2::ggplot(df_region, ggplot2::aes(x = percent_ground_cover)) +
      ggplot2::geom_ribbon(aes(ymin = adj_est - .se, ymax = adj_est + .se, linetype = treatment),
                           fill = "grey70", alpha = 0.3) +
      ggplot2::geom_line(aes(y = adj_est, linetype = treatment), size = 1) +
      ggpubr::theme_pubr() +
      ggplot2::labs(title = reg) +
      xlab(xlab) +
      ylab(ylab)
  })
  
  combined <- Reduce(`+`, plots) + patchwork::plot_annotation(tag_levels = "a")
  return(combined)
}