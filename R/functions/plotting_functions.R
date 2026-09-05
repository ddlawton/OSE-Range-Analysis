# ==============================================================================
# Plotting Functions for OSE Range Analysis
# ==============================================================================
# 
# Standardized plotting functions for consistent, publication-ready figures
# Handles locust density, damage, GAM smooths, and emmeans visualizations
#
# Author: ddlawton
# Created: 2025-11-22
# Updated: 2026-05-24 - Optimized, removed redundancy, improved documentation
# ==============================================================================

# ==============================================================================
# Mission-Based Density and Damage Plots
# ==============================================================================

normalize_region_names <- function(x) {
  x_chr <- as.character(x)
  x_chr <- gsub("Thiès", "Thies", x_chr, fixed = TRUE)
  x_chr <- gsub("Saint-Louis", "Saint Louis", x_chr, fixed = TRUE)
  x_ascii <- iconv(x_chr, from = "", to = "ASCII//TRANSLIT")
  x_ascii[is.na(x_ascii)] <- x_chr[is.na(x_ascii)]
  x_ascii <- gsub("-", " ", x_ascii)
  x_ascii <- gsub("\\s+", " ", x_ascii)
  trimws(x_ascii)
}

extract_mission_number <- function(x) {
  x_chr <- as.character(x)
  direct_num <- suppressWarnings(as.integer(x_chr))
  digit_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", x_chr)))
  dplyr::coalesce(direct_num, digit_num)
}

#' Plot OSE Density by Region and Mission
#'
#' Creates faceted jitter plots of OSE density colored by fertilizer treatment.
#' Overlays estimated marginal means as larger filled points.
#'
#' @param data Data frame with: year, region, farmer, fertilizer_treatment, 
#'   mission_number, ose_count
#' @param emmeans Data frame with estimated marginal means
#' @param ncol Integer. Number of columns for plot arrangement (default: 1)
#' @param emmean_point_size Numeric. Size of emmean points (default: DEFAULT_EMMEAN_POINT_SIZE)
#' @param regions Character vector. Region names for factor levels (default: ALT_STUDY_REGIONS)
#' @param legend_position Character. Legend position in combined plot (default: 'bottom')
#' @return Patchwork object with combined, annotated plots
#' @export
plot_mission_density <- function(data, emmeans, ncol = 1,
                                emmean_point_size = DEFAULT_EMMEAN_POINT_SIZE,
                                regions = ALT_STUDY_REGIONS,
                                legend_position = "bottom") {
  regions_norm <- normalize_region_names(regions)
  mission_source <- if ("mission" %in% names(emmeans)) {
    emmeans$mission
  } else if ("mission_number" %in% names(emmeans)) {
    emmeans$mission_number
  } else if ("mission_numer" %in% names(emmeans)) {
    emmeans$mission_numer
  } else if ("source" %in% names(emmeans)) {
    emmeans$source
  } else {
    rep(NA_character_, nrow(emmeans))
  }



  # Prepare data with mission date labels
  density_dat <- data |>
    dplyr::select(year, region, farmer, fertilizer_treatment, mission_number, ose_count) |>
    dplyr::mutate(
      mission_number = extract_mission_number(mission_number),
      region = normalize_region_names(region),
      region = factor(region, levels = regions_norm),
      mission_date = dplyr::case_when(
        mission_number == 1 ~ MISSION_LABELS[1],
        mission_number == 2 ~ MISSION_LABELS[2],
        mission_number == 3 ~ MISSION_LABELS[3]
      )
    )
  
  # Prepare emmeans with mission date labels
  emmeans <- emmeans |>
    dplyr::mutate(
      mission_number = extract_mission_number(mission_source),
      mission_date = dplyr::case_when(
        mission_number == 1 ~ MISSION_LABELS[1],
        mission_number == 2 ~ MISSION_LABELS[2],
        mission_number == 3 ~ MISSION_LABELS[3]
      ),
      region = normalize_region_names(region),
      region = factor(region, levels = regions_norm)
    )
  
  mission_titles <- sub(" \\(.*\\)", "", MISSION_LABELS)
  
  # Create individual mission plots
  plot_mission <- function(md, title) {
    ggplot2::ggplot(
      dplyr::filter(density_dat, mission_date == md),
      ggplot2::aes(x = region, y = ose_count, color = fertilizer_treatment,
      group = fertilizer_treatment)
    ) +
      ggplot2::geom_jitter(
        position = ggplot2::position_jitterdodge(jitter.width = 0.2, jitter.height = 0),
        pch = 21, alpha = 0.3,
      ) +
      ggplot2::geom_jitter(
        data = dplyr::filter(emmeans, mission_date == md),
        ggplot2::aes(y = rate, fill = fertilizer_treatment),
        position = ggplot2::position_jitterdodge(jitter.width = 0.00001, jitter.height = 0),
        pch = 23, color = "black", size = emmean_point_size
      ) +
      ggplot2::scale_color_manual(values = FERTILIZER_COLORS) +
      ggplot2::scale_fill_manual(values = FERTILIZER_COLORS) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::ylab(bquote('individuals'~bold('•')~100~m^-2)) +
      ggplot2::labs(title = title) +
      ggpubr::theme_pubr(legend = "bottom") +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.title.x = ggplot2::element_blank()
      )
  }
  
  plots <- Map(plot_mission, MISSION_LABELS, mission_titles)
  
  # Combine with patchwork
  patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_annotation(tag_levels = "a") +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = legend_position)
}

#' Plot OSE Damage by Region and Mission
#'
#' Creates faceted jitter plots of OSE leaf damage colored by fertilizer treatment.
#' Overlays estimated marginal means as larger filled points.
#'
#' @param data Data frame with: year, region, farmer, fertilizer_treatment, 
#'   mission_number, ose_damage_percent
#' @param emmeans Data frame with estimated marginal means
#' @param ncol Integer. Number of columns for plot arrangement (default: 1)
#' @param emmean_point_size Numeric. Size of emmean points (default: DEFAULT_EMMEAN_POINT_SIZE)
#' @param regions Character vector. Region names for factor levels (default: ALT_STUDY_REGIONS)
#' @param legend_position Character. Legend position in combined plot (default: 'bottom')
#' @return Patchwork object with combined, annotated plots
#' @export
plot_mission_damage <- function(data, emmeans, ncol = 1,
                               emmean_point_size = DEFAULT_EMMEAN_POINT_SIZE,
                               regions = ALT_STUDY_REGIONS,
                               legend_position = "bottom") {
  regions_norm <- normalize_region_names(regions)
  mission_source <- if ("mission" %in% names(emmeans)) {
    emmeans$mission
  } else if ("mission_number" %in% names(emmeans)) {
    emmeans$mission_number
  } else if ("mission_numer" %in% names(emmeans)) {
    emmeans$mission_numer
  } else if ("source" %in% names(emmeans)) {
    emmeans$source
  } else {
    rep(NA_character_, nrow(emmeans))
  }

  # Prepare data with mission date labels
  damage_dat <- data |>
    dplyr::select(year, region, farmer, fertilizer_treatment, mission_number, ose_damage_percent) |>
    dplyr::mutate(
      mission_number = extract_mission_number(mission_number),
      region = normalize_region_names(region),
      region = factor(region, levels = regions_norm),
      mission_date = dplyr::case_when(
        mission_number == 1 ~ MISSION_LABELS[1],
        mission_number == 2 ~ MISSION_LABELS[2],
        mission_number == 3 ~ MISSION_LABELS[3]
      )
    )
  
  # Prepare emmeans with mission date labels
  emmeans <- emmeans |>
    dplyr::mutate(
      mission_number = extract_mission_number(mission_source),
      mission_date = dplyr::case_when(
        mission_number == 1 ~ MISSION_LABELS[1],
        mission_number == 2 ~ MISSION_LABELS[2],
        mission_number == 3 ~ MISSION_LABELS[3]
      ),
      region = normalize_region_names(region),
      region = factor(region, levels = regions_norm)
    )
  
  mission_titles <- sub(" \\(.*\\)", "", MISSION_LABELS)
  
  # Create individual mission plots
  plot_mission <- function(md, title) {
    ggplot2::ggplot(
      dplyr::filter(damage_dat, mission_date == md),
      ggplot2::aes(x = region, y = ose_damage_percent, color = fertilizer_treatment,group = fertilizer_treatment)
    ) +
      ggplot2::geom_jitter(
        position = ggplot2::position_jitterdodge(jitter.width = 0.2, jitter.height = 0),
        pch = 21, alpha = 0.3,
      ) +
      ggplot2::geom_jitter(
        data = dplyr::filter(emmeans, mission_date == md),
        ggplot2::aes(y = response, fill = fertilizer_treatment),
        position = ggplot2::position_jitterdodge(jitter.width = 0.00001, jitter.height = 0),
        pch = 23, size = emmean_point_size
      ) +
      ggplot2::scale_color_manual(values = FERTILIZER_COLORS) +
      ggplot2::scale_fill_manual(values = FERTILIZER_COLORS) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::ylab("leaf damage (%)") +
      ggplot2::labs(title = title) +
      ggpubr::theme_pubr(legend = "bottom") +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.title.x = ggplot2::element_blank()
      )
  }
  
  plots <- Map(plot_mission, MISSION_LABELS, mission_titles)
  
  # Combine with patchwork
  patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_annotation(tag_levels = "a") +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = legend_position)
}

# ==============================================================================
# Emmeans and Trend Plots
# ==============================================================================

#' Plot Estimated Marginal Means by Mission, Region, and Treatment
#'
#' Creates publication-ready plot showing EMMs with 95% confidence intervals
#' and connected trend lines across missions.
#'
#' @param emeans Data frame with estimated marginal means
#' @param color_palette Character. MetBrewer palette name (default: DEFAULT_PALETTE)
#' @param ylab Character. Y-axis label (default: "OSE count")
#' @param xlab Character. X-axis label (default: "Mission")
#' @return ggplot object
#' @export
plot_emmeans <- function(emeans, color_palette = DEFAULT_PALETTE,
                        ylab = "OSE count", xlab = "Mission") {
  p <- ggplot2::ggplot(
    emeans,
    ggplot2::aes(
      x = as.integer(mission),
      y = rate,
      color = region,
      linetype = fertilizer_treatment
    )
  ) +
    ggplot2::geom_point(size = DEFAULT_POINT_SIZE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = asymp.LCL, ymax = asymp.UCL),
      width = 0
    ) +
    ggplot2::geom_line() +
    ggpubr::theme_pubr() +
    ggplot2::scale_x_continuous(breaks = c(1, 2, 3)) +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab)
  
  # Add color palette if available
  if (requireNamespace("MetBrewer", quietly = TRUE)) {
    p <- p + MetBrewer::scale_color_met_d(name = color_palette)
  } else {
    p <- p + ggplot2::scale_color_viridis_d()
  }
  
  return(p)
}

# ==============================================================================
# GAM Smooth Plots
# ==============================================================================

#' Plot Temperature × Region Smooth from GAM Model
#'
#' Visualizes GAM smooth effects of temperature by region with confidence ribbons.
#'
#' @param mod Fitted GAM model object
#' @param smooth Character. Smooth term to filter for
#' @param xvar Character. X variable column name (default: "temperature")
#' @param group Character. Grouping column name (default: "region")
#' @param xlab Character. X-axis label (default: NULL, uses xvar)
#' @param ylab Character. Y-axis label (default: NULL, uses "estimate (adjusted)")
#' @param title Character. Plot title (default: "locust density x temperature")
#' @param palette Character. Color palette name (default: DEFAULT_PALETTE)
#' @param ribbon_fill Character. Ribbon fill color (default: "grey70")
#' @param ribbon_alpha Numeric. Ribbon transparency (default: 0.3)
#' @return ggplot object
#' @export
plot_temperature_smooth <- function(mod, smooth = "s(temperature,region)",
                                   xvar = "temperature", group = "region",
                                   xlab = NULL, ylab = NULL,
                                   title = "locust density x temperature",
                                   palette = DEFAULT_PALETTE,
                                   ribbon_fill = "grey70", ribbon_alpha = 0.3) {
  # Set default labels
  if (is.null(xlab)) xlab <- xvar
  if (is.null(ylab)) ylab <- "estimate (adjusted)"
  
  # Validate gratia availability
  if (!requireNamespace("gratia", quietly = TRUE)) {
    stop("gratia package required for smooth_estimates()")
  }
  
  # Extract smooth estimates
  est <- gratia::smooth_estimates(mod) |>
    dplyr::filter(.data$.smooth == smooth)
  
  # Adjust estimates by intercept
  intercept <- tryCatch(
    stats::coef(mod)[1],
    error = function(e) {
      warning("Could not extract intercept: ", conditionMessage(e))
      0
    }
  )
  
  est <- est |>
    dplyr::mutate(
      adj_est = .data$.estimate + intercept,
      ymin = adj_est - .data$.se,
      ymax = adj_est + .data$.se
    )
  
  # Create plot
  p <- ggplot2::ggplot(est, ggplot2::aes(x = .data[[xvar]])) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ymin, ymax = ymax, group = .data[[group]]),
      fill = ribbon_fill, alpha = ribbon_alpha
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = adj_est, color = .data[[group]]),
      linewidth = 1
    ) +
    ggpubr::theme_pubr() +
    ggplot2::labs(title = title, x = xlab, y = ylab)
  
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

#' Plot GAM Smooths Using Gratia
#'
#' Creates faceted smooth plots for GAM models with region × treatment interactions.
#'
#' @param gam_mod Fitted GAM model from mgcv::bam
#' @param xlab Character. X-axis label (default: 'ground cover (%)')
#' @param ylab Character. Y-axis label (default: 'OSE count (modeled)')
#' @return Patchwork object with combined plots
#' @export
plot_gam_smooths_gratia <- function(gam_mod, 
                                   xlab = "ground cover (%)", 
                                   ylab = "OSE count (modeled)") {
  # Validate dependencies
  required_pkgs <- c("gratia", "tidyr", "patchwork")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for GAM plotting")
    }
  }
  
  # Extract and prepare smooth estimates
  ests <- gratia::smooth_estimates(gam_mod)
  ests$adj_est <- ests$.estimate + stats::coef(gam_mod)[1]
  ests <- ests[ests$.smooth != "s(farmer)", ]
  ests <- tidyr::separate(
    ests, region_treat, 
    sep = "_", into = c("region", "treatment"), 
    remove = FALSE
  )
  
  # Create plots for each region
  plots <- lapply(unique(ests$region), function(reg) {
    df_region <- subset(ests, region == reg)
    ggplot2::ggplot(df_region, ggplot2::aes(x = percent_ground_cover)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = adj_est - .se, ymax = adj_est + .se, linetype = treatment),
        fill = "grey70", alpha = 0.3
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = adj_est, linetype = treatment),
        linewidth = 1
      ) +
      ggpubr::theme_pubr() +
      ggplot2::labs(title = reg, x = xlab, y = ylab)
  })
  
  # Combine plots
  Reduce(`+`, plots) + patchwork::plot_annotation(tag_levels = "a")
}
