##### SSZ-ggplot-Theme
# library(ggplot2)
# library(gridExtra)
# library(dplyr)

#' ssz_theme
#'
#' @description Function for styling ggplots according to Stadt Zürich's corporate design
#'
#' @param base_size optional, basic font size, base_size parameter passed to minimal theme
#' @param base_family optional, base_family parameter to be passed to minimal theme
#' @param base_line_size tbd
#' @param base_rect_size tbd
#' @param orientation tbd
#'
#' @export
#' @import ggplot2 dplyr
#' @importFrom gridExtra arrangeGrob
#' @importFrom grDevices rgb
#' @examples \dontrun{
#' ggplot(...) +
#'   ssz_theme()
#' }
ssz_theme <- function(base_size = 11,
                      base_family = "",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170,
                      orientation = "x") {
  # Werte müssen Character sein
  if (!is.character(orientation)) {
    stop("orientation must be a character")

    # Grids, wenn orientation == X
  } else if (orientation == "x") {

    # Minimales Theme anwenden
    theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size
    ) %+replace%

      # SSZ-spezifische Theme-Eigenheiten
      theme(

        # Titel
        plot.title = element_text(
          color = "#020304",
          face = "bold",
          hjust = 0,
          size = rel(0.75)
        ),
        plot.subtitle = element_text(
          color = "#020304",
          face = "bold",
          hjust = 0,
          size = rel(0.55)
        ),

        # Achsenbeschriftung
        axis.title.x = element_text(
        	color = "#020304",
          size = rel(0.55),
          margin = margin(t = 20, r = 0, b = 0, l = 0)
        ),
        axis.title.y = element_text(
        	color = "#020304",
        	size = rel(0.55),
          vjust = rel(1),
          margin = margin(t = 0, r = -20, b = 0, l = 0)
        ),
        axis.text = element_text(
        	color = "#020304",
        	size = rel(0.55),
        ),

        # Achsenlinien
        # axis.line.x = element_line(
        # 	linewidth = rel(0.55),
        # 	linetype = "solid",
        # 	color = "#020304"
        # ),

        # Gitterlinien
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
        	color = "#020304",
          linetype = "solid",
          size = rel(0.55)
        ),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "pink"),

        # Legende
        legend.position = "left",
        legend.justification = c(1,1),
        legend.margin = margin(0, 100, 0, 0),
        legend.title = element_blank(),
        legend.text = element_text(
        	color = "#020304",
        	size = rel(0.55)
        ),

        # Caption
        plot.caption.position =  "plot",
        plot.caption = element_text(
        														size = rel(0.54),
        														margin = margin(15,0,0,0)
        ),

        complete = FALSE
      )

    # Grids, wenn orientation == Y
  } else if (orientation == "y") {

    # Minimales Theme anwenden
  	theme_bw(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size
    ) %+replace%

      # SSZ-spezifische Theme-Eigenheiten
      theme(

        # Titel
        plot.title = element_text(
        	color = "#020304",
        	face = "bold",
        	hjust = 0,
        	size = rel(0.75)
        ),
        plot.subtitle = element_text(
          color = rgb(105, 105, 105, maxColorValue = 255),
          face = "bold",
          size = rel(0.55),
          hjust = 0
        ),

        # Achsenbeschriftung
        axis.title.x = element_text(
        	color = "#020304",
        	size = rel(0.55),
          margin = margin(t = 20, r = 0, b = 0, l = 0)
        ),
        axis.title.y = element_text(
        	color = "#020304",
          size = rel(0.55),
          vjust = rel(1),
          margin = margin(t = 0, r = -20, b = 0, l = 0)
        ),
        axis.text = element_text(
        	color = "#020304",
          size = rel(0.55)
        ),

        # Achsenlinien
        # axis.line.y = element_line(
        # 	linewidth = rel(0.55),
        # 	linetype = "solid",
        # 	color = "#020304"
        # ),

        # Gitterlinien
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(
        	color = "#020304",
          linetype = "solid",
          size = rel(0.55)
        ),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = "pink"),

        # Legende
        legend.position = "left",
        legend.justification = c(1,1),
        legend.margin = margin(0, 100, 0, 0),
        legend.title = element_blank(),
        legend.text = element_text(
        	color = "#020304",
          size = rel(0.55)
        ),

        # Caption
        plot.caption.position =  "plot",
        plot.caption = element_text(
        														size = rel(0.54),
        														margin = margin(15,0,0,0)
        ),

        complete = FALSE
      )
  }
}
