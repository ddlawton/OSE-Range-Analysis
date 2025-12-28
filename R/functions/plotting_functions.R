#' Plotting Functions for OSE Range Analysis
#' 
#' Functions for creating consistent, publication-ready plots across analyses.
#' Includes specialized plots for locust density, yield analysis, and GAM smooths.
#' 
#' Author: ddlawton
#' Date: 2025-11-22

#' Plot OSE density by region and mission date, faceted by mission
#'
#' This function creates a series of ggplot jitter plots of OSE/100m counts,
#' colored by fertilizer treatment, faceted by mission and region. Plots are
#' arranged using patchwork, with consistent legend placement and plot tagging.
#'
#' @param data A data.frame or tibble containing columns: year, region, farmer,
#'   fertilizer_treatment, mission_number, ose_count
#' @param emmeans Data frame with estimated marginal means for overlaying
#' @param ncol Number of columns for plot arrangement (default: 1)
#' @param emmean_point_size Size of emmean points (default: 5)
#' @param regions Character vector of region names to use as factor levels
#' @param legend_position Position of legend in combined plot (default: 'bottom')
#' @return A patchwork object with combined, annotated plots
plot_mission_density <- function(
    data,
    emmeans,
    ncol = 1,
    emmean_point_size = DEFAULT_EMMEAN_POINT_SIZE,
    regions = ALT_STUDY_REGIONS,
    legend_position = 'bottom'
) {
    # Data prep
    density_dat <- data |>
        dplyr::select(year, region, farmer, fertilizer_treatment, mission_number, ose_count) |>
        dplyr::mutate(region = factor(region, levels = regions)) |>
        dplyr::mutate(mission_date = dplyr::case_when(
            mission_number == 1 ~ MISSION_LABELS[1],
            mission_number == 2 ~ MISSION_LABELS[2],
            mission_number == 3 ~ MISSION_LABELS[3]
        ))

    # Map mission_numer in emmeans to mission_date labels
    emmeans <- emmeans |>
        dplyr::mutate(mission_date = dplyr::case_when(
            mission_numer == "1" | mission_numer == 1 ~ MISSION_LABELS[1],
            mission_numer == "2" | mission_numer == 2 ~ MISSION_LABELS[2],
            mission_numer == "3" | mission_numer == 3 ~ MISSION_LABELS[3]
        )) |>
        dplyr::mutate(region = factor(region, levels = regions))

    mission_titles <- sub(" \\(.*\\)", "", MISSION_LABELS)

    plot_mission <- function(md, title) {
        ggplot(
            dplyr::filter(density_dat, mission_date == md),
            aes(x = region, y = ose_count, color = fertilizer_treatment)
        ) +
        geom_jitter(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0),
            pch = 21, alpha = 0.3) +
        geom_jitter(
            data = dplyr::filter(emmeans, mission_date == md),
            aes(y = rate, x = region, fill = fertilizer_treatment),
            position = position_jitterdodge(jitter.width = 0.00001, jitter.height = 0),
            pch = 21,
            color = 'black',
            size = emmean_point_size
        ) +
        scale_color_manual(values = FERTILIZER_COLORS) +
        scale_fill_manual(values = FERTILIZER_COLORS) +
        scale_x_discrete(drop = FALSE) +
        ylab(bquote('individuals'~bold('•')~100~m^-2)) +
        labs(title = title) +
        theme_pubr(legend = 'bottom') +
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
        )
    }
    plots <- Map(plot_mission, MISSION_LABELS, mission_titles)

    combined_plot <- patchwork::wrap_plots(plots, ncol = ncol) +
        patchwork::plot_annotation(tag_levels = 'a') +
        patchwork::plot_layout(guides = "collect") &
        theme(legend.position = legend_position)

    return(combined_plot)
}

#' Plot OSE damage by region and mission date, faceted by mission
#'
#' This function creates a series of ggplot jitter plots of OSE/100m counts,
#' colored by fertilizer treatment, faceted by mission and region. Plots are
#' arranged using patchwork, with consistent legend placement and plot tagging.
#'
#' @param data A data.frame or tibble containing columns: year, region, farmer,
#'   fertilizer_treatment, mission_number, ose_damage_percent
#' @param emmeans Data frame with estimated marginal means for overlaying
#' @param ncol Number of columns for plot arrangement (default: 1)
#' @param emmean_point_size Size of emmean points (default: 5)
#' @param regions Character vector of region names to use as factor levels
#' @param legend_position Position of legend in combined plot (default: 'bottom')
#' @return A patchwork object with combined, annotated plots
plot_mission_damage <- function(
    data,
    emmeans,
    ncol = 1,
    emmean_point_size = DEFAULT_EMMEAN_POINT_SIZE,
    regions = ALT_STUDY_REGIONS,
    legend_position = 'bottom'
) {
    # Data prep
    density_dat <- data |>
        dplyr::select(year, region, farmer, fertilizer_treatment, mission_number, ose_damage_percent) |>
        dplyr::mutate(region = factor(region, levels = regions)) |>
        dplyr::mutate(mission_date = dplyr::case_when(
            mission_number == 1 ~ MISSION_LABELS[1],
            mission_number == 2 ~ MISSION_LABELS[2],
            mission_number == 3 ~ MISSION_LABELS[3]
        ))

    # Map mission_numer in emmeans to mission_date labels
    emmeans <- emmeans |>
        dplyr::mutate(mission_date = dplyr::case_when(
            mission_numer == "1" | mission_numer == 1 ~ MISSION_LABELS[1],
            mission_numer == "2" | mission_numer == 2 ~ MISSION_LABELS[2],
            mission_numer == "3" | mission_numer == 3 ~ MISSION_LABELS[3]
        )) |>
        dplyr::mutate(region = factor(region, levels = regions))

    plot_mission <- function(md) {
        ggplot(
            dplyr::filter(density_dat, mission_date == md),
            aes(x = region, y = ose_damage_percent, color = fertilizer_treatment)
        ) +
        geom_jitter(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0),
            pch = 21, alpha = 0.3) +
        geom_jitter(
            data = dplyr::filter(emmeans, mission_date == md),
            aes(y = response, x = region, fill = fertilizer_treatment),
            position = position_jitterdodge(jitter.width = 0.00001, jitter.height = 0),
            pch = 21,
            color = 'black',
            size = emmean_point_size
        ) +
        scale_color_manual(values = FERTILIZER_COLORS) +
        scale_fill_manual(values = FERTILIZER_COLORS) +
        scale_x_discrete(drop = FALSE) +
        ylab('leaf damage proportion') +
        labs(title = md) +
        theme_pubr(legend = 'bottom') +
        theme(
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank(),
        )
    }

    plots <- lapply(MISSION_LABELS, plot_mission)

    combined_plot <- patchwork::wrap_plots(plots, ncol = ncol) +
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
#' @param emeans A data frame containing the estimated marginal means data
#' @param color_palette Character. Name of a MetBrewer palette (default: "Degas")
#' @param ylab Character. Label for the y-axis (default: "OSE count")
#' @param xlab Character. Label for the x-axis (default: "Mission")
#' @return A `ggplot` object
plot_emmeans <- function(emeans,
                         color_palette = DEFAULT_PALETTE,
                         ylab = "OSE count",
                         xlab = "Mission") {

  p <- ggplot(emeans, aes(
    x = as.integer(mission),
    y = rate,
    color = region,
    linetype = fertilizer_treatment
  )) +
    geom_point(size = DEFAULT_POINT_SIZE) +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0) +
    geom_line() +
    theme_pubr() +
    scale_x_continuous(breaks = c(1, 2, 3)) +
    ylab(ylab) +
    xlab(xlab)
  
  # Add color palette if MetBrewer is available
  if (requireNamespace("MetBrewer", quietly = TRUE)) {
    p <- p + MetBrewer::scale_color_met_d(name = color_palette)
  } else {
    p <- p + scale_color_viridis_d()
  }
  
  return(p)
}

#' Create a temperature x region smooth plot for a GAM model
#'
#' This function creates smoothed plots for temperature effects by region
#' using GAM model output.
#'
#' @param mod A fitted GAM model object
#' @param smooth A character string naming the smooth to filter for
#' @param xvar The name of the x variable column (default "temperature")
#' @param group The name of the grouping column (default "region")
#' @param xlab X-axis label (defaults to xvar)
#' @param ylab Y-axis label (default: "estimate (adjusted)")
#' @param title Plot title (default: "locust density x temperature")
#' @param palette Palette name for colors (default: "Degas")
#' @param ribbon_fill Fill colour for ribbon (default: "grey70")
#' @param ribbon_alpha Alpha for ribbon (default: 0.3)
#' @return A ggplot object
plot_temperature_smooth <- function(mod,
                                    smooth = "s(temperature,region)",
                                    xvar = "temperature",
                                    group = "region",
                                    xlab = NULL,
                                    ylab = NULL,
                                    title = "locust density x temperature",
                                    palette = DEFAULT_PALETTE,
                                    ribbon_fill = "grey70",
                                    ribbon_alpha = 0.3) {
  # Default labels
  if (is.null(xlab)) xlab <- xvar
  if (is.null(ylab)) ylab <- "estimate (adjusted)"
  
  # Validate smooth_estimates function availability
  if (!exists("smooth_estimates", mode = "function")) {
    stop("smooth_estimates() not found. Please load gratia package.")
  }
  
  est <- smooth_estimates(mod)
  
  # Validate required columns
  required_cols <- c(".smooth", ".estimate", ".se", xvar, group)
  missing_cols <- setdiff(required_cols, names(est))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Compute adjusted estimate and ribbon endpoints
  intercept <- tryCatch(stats::coef(mod)[1], error = function(e) {
    warning("Could not extract intercept from model: ", conditionMessage(e))
    0
  })
  
  est <- est |>
    dplyr::filter(.data$.smooth == smooth) |>
    dplyr::mutate(
      adj_est = .data$.estimate + intercept,
      ymin = adj_est - .data$.se,
      ymax = adj_est + .data$.se
    )
  
  # Build plot
  p <- ggplot2::ggplot(est, ggplot2::aes_string(x = xvar)) +
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin = "ymin", ymax = "ymax", group = group),
                         fill = ribbon_fill, alpha = ribbon_alpha) +
    ggplot2::geom_line(ggplot2::aes_string(y = "adj_est", color = group), size = 1) +
    ggpubr::theme_pubr() +
    ggplot2::labs(title = title) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  
  # Add color palette
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

#' Plot GAM term smooths using gratia and ggplot2
#'
#' @param gam_mod Fitted GAM model object from mgcv::bam
#' @param xlab Label for x-axis (default: 'ground cover (%)')
#' @param ylab Label for y-axis (default: 'OSE count (modeled)')
#' @return A ggplot object of the smooths faceted by region
plot_gam_smooths_gratia <- function(gam_mod, 
                                   xlab = 'ground cover (%)', 
                                   ylab = 'OSE count (modeled)') {
  required_pkgs <- c("gratia", "tidyr", "patchwork")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for GAM plotting")
    }
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