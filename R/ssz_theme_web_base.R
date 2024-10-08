#' ssz_theme_base_web
#'
#' @description Function that creates a ggplot theme that can be used as a base
#' theme on top of which to replace the theme's items that depend on the orientation
#' of the web plot.
#'
#' @param base_size optional, defines the base font size used throughout the plot.
#' This parameter is passed to the theme_bw function, ensuring consistent typography
#' across ggplot elements.
#' @param base_family optional, specifies the base font family for the text elements
#' in the ggplot object. This parameter is passed to the theme_bw function, ensuring
#' a consistent font style across ggplot elements.
#' @param base_line_size optional, sets the base line size for lines and borders
#' within the ggplot object. This parameter is passed to the theme_bw function, ensuring
#' consistent line thickness across visual elements.
#' @param base_rect_size optional, specifies the base size for rectangular elements
#' such as legend boxes or plot backgrounds within the ggplot object. This parameter
#' is passed to the theme_bw function, ensuring consistent sizing of rectangular
#' shapes across ggplot elements.
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
