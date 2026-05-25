# ==============================================================================
# Senegal Map Plotting Function
# ==============================================================================
# 
# Generate map of Senegal highlighting regions of interest for the study
# Uses rnaturalearth for geographic data and ggplot2 for visualization
#
# Author: ddlawton
# Created: 2025-11-22
# Updated: 2026-05-24 - Optimized, improved documentation
# ==============================================================================

#' Plot Map of Senegal with Highlighted Regions
#'
#' Creates a map of Senegal using sf and ggplot2, highlighting specified
#' regions of interest while showing others in a neutral color.
#'
#' @param regions_of_interest Character vector. Region names to highlight
#'   (default: c('Saint-Louis', 'Thiès', 'Fatick', 'Kaffrine'))
#' @param roi_color Character. Color for highlighted regions (default: "tomato")
#' @param other_color Character. Color for non-highlighted regions (default: "grey80")
#' @param show_legend Logical. Whether to display legend (default: FALSE)
#' @param label_size Numeric. Size for region label text (default: 4)
#' @return ggplot map object
#' @export
#'
#' @examples
#' # Basic usage with defaults
#' plot_senegal_map()
#'
#' # Specify different regions and show legend
#' plot_senegal_map(
#'   regions_of_interest = c("Dakar", "Saint-Louis"),
#'   show_legend = TRUE
#' )
#'
#' # Customize colors and label size
#' plot_senegal_map(
#'   roi_color = "darkblue",
#'   other_color = "lightgrey",
#'   label_size = 3
#' )
#'
#' # Use in Quarto document:
#' # ```{r}
#' # source("R/functions/senegal_map.R")
#' # plot_senegal_map()
#' # ```
plot_senegal_map <- function(
  regions_of_interest = c('Saint-Louis', 'Thiès', 'Fatick', 'Kaffrine'),
  roi_color = "tomato",
  other_color = "grey80",
  show_legend = FALSE,
  label_size = 4
) {
  # Validate dependencies
  required_pkgs <- c("rnaturalearth", "ggplot2", "dplyr", "janitor")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Please install it.")
    }
  }
  
  # Load libraries for pipe and functions
  library(rnaturalearth)
  library(ggplot2)
  library(dplyr)
  library(janitor)
  
  # Get Senegal geographic data and prepare
  senegal <- rnaturalearth::ne_states(country = "Senegal", returnclass = "sf") |>
    janitor::clean_names() |>
    dplyr::select(name_en, geometry) |>
    dplyr::mutate(
      roi = ifelse(
        name_en %in% regions_of_interest,
        "Region of Interest",
        "Other"
      )
    )
  
  # Build plot
  gg <- ggplot2::ggplot(senegal) +
    ggplot2::geom_sf(ggplot2::aes(fill = roi)) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = name_en),
      size = label_size
    ) +
    ggplot2::scale_fill_manual(
      values = c("Region of Interest" = roi_color, "Other" = other_color),
      name = "Region"
    ) +
    ggplot2::theme_void()
  
  # Optionally hide legend
  if (!show_legend) {
    gg <- gg + ggplot2::theme(legend.position = 'none')
  }
  
  return(gg)
}
