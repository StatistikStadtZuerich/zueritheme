#' ssz_theme_void
#'
#' @description Function for styling ggplots graphs according to Stadt Zürich's
#' corporate design. Used for plots with a void theme, i.e. no grid lines and no
#' axis texts.
#'
#' @param base_size optional, defines the base font size used throughout the plot.
#' This parameter is passed to the theme_void function, ensuring consistent typography
#' across ggplot elements. Adjust this value to control the overall text size
#' in the ggplot object for better readability or stylistic preference.
#' @param base_family optional, specifies the base font family for the text elements
#' in the ggplot object. This parameter is passed to the theme_void function,
#' ensuring a consistent font style across ggplot elements. Adjust this value to
#' control the overall font family in the ggplot object.
#' @param base_line_size optional, sets the base line size for lines and borders
#' within the ggplot object. This parameter is passed to the theme_void function,
#' ensuring consistent line thickness across visual elements. Adjust this value to $
#' modify the overall line width for improved visual clarity or stylistic preferences.
#' @param base_rect_size optional, specifies the base size for rectangular elements
#' such as legend boxes or plot backgrounds within the ggplot object. This parameter
#' is passed to the theme_void function, ensuring consistent sizing of rectangular
#' shapes across ggplot elements. Adjust this value to control the thickness of
#' borders for better visual balance or customization.
#'
#' @return a void ggplot theme.
#'
#' @export
#' @import ggplot2 dplyr
#' @importFrom gridExtra arrangeGrob
#' @importFrom grDevices rgb
#' @examples \dontrun{
#' ggplot(...) +
#'   ssz_theme_void()
#' }
ssz_theme_void <- function(base_size = 8.2,
                           base_family = "",
                           base_line_size = base_size / 170,
                           base_rect_size = base_size / 170) {
  # List object

  # Base theme_bw
  theme_void(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%

    # SSZ specific theme components
    theme(

      # Title
      plot.title.position = "plot",
      plot.title = element_text(
        color = "#020304",
        face = "bold",
        size = rel(1.5),
        hjust = 0,
        margin = margin(
          t = 0,
          r = 0,
          b = 2,
          l = 0
        )
      ),

      # Subtitle
      plot.subtitle = element_text(
        color = "#020304",
        size = rel(1),
        hjust = 0,
        margin = margin(
          t = 0,
          r = 0,
          b = 25,
          l = 0
        )
      ),

      # Panel
      panel.spacing = unit(base_size / 11, "lines"),

      # Plot Background
      plot.background = element_rect(fill = "white", color = NA),

      # Legend
      legend.position = "left",
      legend.justification = c(0, 1),
      legend.margin = margin(
        t = -2,
        r = 100,
        b = 0,
        l = 0
      ),
      legend.title = element_blank(),
      legend.text = element_text(
        color = "#020304",
        size = rel(1)
      ),
      legend.key.height = unit(base_size / 11, "line"),
      legend.key.width = unit(base_size / 11, "line"),

      # Facets
      strip.background = element_blank(),
      strip.text = element_text(
        color = "#020304",
        face = "bold",
        size = rel(1),
        hjust = 0,
        margin = margin(
          t = 0,
          r = 0,
          b = 5,
          l = 0
        ),
      ),

      # Caption
      plot.caption.position = "plot",
      plot.caption = element_text(
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
