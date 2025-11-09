#' Plot the Map of Senegal Highlighting Regions of Interest
#'
#' This function generates a map of Senegal using the `rnaturalearth`, `ggplot2`, `dplyr`, and `janitor` packages.
#' Regions specified in `regions_of_interest` are highlighted, while others are shown in a neutral color.
#' You can use this function directly within Quarto documents, R scripts, or interactive R sessions.
#'
#' @param regions_of_interest Character vector of region names in Senegal to highlight on the map.
#'        Defaults to c('Saint-Louis', 'Thiès', 'Fatick', 'Kaffrine').
#' @param show_legend Logical; whether to display the legend. Default is FALSE (hide legend).
#' @param label_size Numeric; size for region label text. Default is 4.
#'
#' @return A ggplot map object. Print it to display the map.
#'
#' @examples
#' # Basic usage
#' plot_senegal_map()
#'
#' # Specify different regions and show legend
#' plot_senegal_map(regions_of_interest = c("Dakar", "Saint-Louis"), show_legend = TRUE)
#'
#' # Change the label size
#' plot_senegal_map(label_size = 3)
#'
#' # Use inside Quarto:
#' # ```{r}
#' # source("plot_senegal_map.R")
#' # plot_senegal_map()
#' # ```
#'
#' @export
plot_senegal_map <- function(
  regions_of_interest = c('Saint-Louis', 'Thiès', 'Fatick', 'Kaffrine'),
  roi_color = "tomato",
  other_color = "grey80",
  show_legend = FALSE,
  label_size = 4
) {
  # Check for required packages
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) stop("Package 'rnaturalearth' needed. Please install it.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' needed. Please install it.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' needed. Please install it.")
  if (!requireNamespace("janitor", quietly = TRUE)) stop("Package 'janitor' needed. Please install it.")

  # Load libraries (to ensure pipe and functions are available)
  library(rnaturalearth)
  library(ggplot2)
  library(dplyr)
  library(janitor)

  # Get Senegal states and tidy columns
  senegal <- rnaturalearth::ne_states(country = "Senegal", returnclass = "sf") |>
    janitor::clean_names() |>
    dplyr::select(name_en, geometry) |>
    dplyr::mutate(
      roi = ifelse(name_en %in% regions_of_interest, "Region of Interest", "Other")
    )

  # Build plot
  gg <- ggplot2::ggplot(senegal) +
    ggplot2::geom_sf(ggplot2::aes(fill = roi)) +
    ggplot2::geom_sf_text(
      ggplot2::aes(label = name_en),
      size = label_size
    ) +
    ggplot2::scale_fill_manual(
      values = c("Region of Interest" = roi_color, "Other" = "grey80"),
      name = "Region"
    ) +
    ggplot2::theme_void()

  # Optionally hide legend
  if (!show_legend) {
    gg <- gg + ggplot2::theme(legend.position = 'none')
  }

  return(gg)
}