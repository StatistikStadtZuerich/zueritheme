#' ssz_theme_base_web
#'
#' this creates a ggplot theme that can be used as a base theme on top of which to
#' replace the items that depend on the orientation of the plot
#'
#' @param base_size optional, basic font size, parameter passed to minimal theme
#' @param base_family optional, basic font family, parameter to be passed to minimal theme
#' @param base_line_size optional, basic line size, parameter passed to minimal theme
#' @param base_rect_size optional, basic rect size, parameter passed to minimal theme
#'
#' @return a ggplot theme
ssz_theme_web_base <- function(base_size,
                               base_family,
                               base_line_size,
                               base_rect_size) {
  # Base theme_bw
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%

    # SSZ specific theme components
    theme(

      # Title
      plot.title.position = "plot",
      plot.title = element_blank(),

      # Subtitle
      plot.subtitle = element_blank(),

      # Axis text and title
      axis.title.x = element_text(
        color = "#737373",
        size = rel(1),
        hjust = 0,
        margin = margin(
          t = 10,
          r = 0,
          b = 0,
          l = 0
        )
      ),
      axis.title.y = element_text(
        color = "#737373",
        size = rel(1),
        hjust = 0,
        vjust = 1,
        margin = margin(
          t = 0,
          r = 0,
          b = 0,
          l = 0
        )
      ),
      axis.text = element_text(
        color = "#737373",
        size = rel(1)
      ),
      axis.ticks = element_blank(),

      # Panel Grid
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.spacing = unit(base_size / 11, "lines"),

      # Legend
      legend.position = "bottom",
      legend.justification = c(0, 0),
      legend.margin = margin(t = 20, r = 0, b = 0, l = 0),
      legend.title = element_blank(),
      legend.text = element_text(color = "#737373", size = rel(1), vjust = 0.5),
      # legend.key.height = unit(base_size / 11, "line"),
      # legend.key.width = unit(base_size / 11, "line"),

      # Facets
      strip.background = element_blank(),
      strip.text = element_text(
        color = "#737373",
        face = "bold",
        size = rel(1),
        hjust = 0,
        margin = margin(
          t = 0,
          r = 0,
          b = 5,
          l = 0
        )
      ),

      # Caption
      plot.caption.position = "plot",
      plot.caption = element_text(
        color = "#737373",
        size = rel(1),
        margin = margin(
          t = 15,
          r = 0,
          b = 0,
          l = 0
        ),
        hjust = 0
      ),
      complete = FALSE
    )
}
