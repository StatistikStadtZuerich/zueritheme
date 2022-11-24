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
#' @param ausrichtung tbd
#'
#' @export
#' @import ggplot2 dplyr
#' @importFrom gridExtra arrangeGrob
#' @importFrom grDevices rgb
#' @examples \dontrun{
#' ggplot(...) +
#' ssz_theme()
#' }
ssz_theme <- function(base_size = 11,
											base_family = "",
											base_line_size = base_size / 170,
											base_rect_size = base_size / 170,
											ausrichtung = "x") {
	# Werte müssen Character sein
	if (!is.character(ausrichtung)) {
		stop("ausrichtung muss character sein")

		# Grids, wenn ausrichtung == X
	} else if (ausrichtung == "x") {

		# Minimales Theme anwenden
		theme_minimal(
			base_size = base_size,
			base_family = base_family,
			base_line_size = base_line_size
		) %+replace%

			# SSZ-spezifische Theme-Eigenheiten
			theme(

				# Titel
				plot.title = element_text(
					color = rgb(25, 43, 65, maxColorValue = 255),
					face = "bold",
					hjust = 0
				),
				plot.subtitle = element_text(
					color = rgb(25, 43, 65, maxColorValue = 255),
					face = "bold",
					size = rel(0.5),
					hjust = 0
				),

				# Achsenbeschriftung
				axis.title.x = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.75),
					margin = margin(t = 20, r = 0, b = 0, l = 0)
				),
				axis.title.y = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.75),
					vjust = rel(1),
					margin = margin(t = 0, r = 20, b = 0, l = 0)
				),
				axis.text = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.55)
				),

				# Gitterlinien
				panel.grid.major.x = element_blank(),
				panel.grid.major.y = element_line(rgb(105, 105, 105, maxColorValue = 255),
																					linetype = "solid",
																					size = rel(0.75)
				),
				panel.grid.minor = element_blank(),

				# Legende
				legend.position = "right",
				legend.title = element_blank(),
				legend.text = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.75)
				),

				complete = TRUE
			)

		# Grids, wenn ausrichtung == Y
	} else if (ausrichtung == "y") {

		# Minimales Theme anwenden
		theme_minimal(
			base_size = base_size,
			base_family = base_family,
			base_line_size = base_line_size
		) %+replace%

			# SSZ-spezifische Theme-Eigenheiten
			theme(

				# Titel
				plot.title = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					face = "bold",
					hjust = 0
				),
				plot.subtitle = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					face = "bold",
					size = rel(0.5),
					hjust = 0
				),

				# Achsenbeschriftung
				axis.title.x = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.75),
					margin = margin(t = 20, r = 0, b = 0, l = 0)
				),
				axis.title.y = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.75),
					vjust = rel(1),
					margin = margin(t = 0, r = 20, b = 0, l = 0)
				),
				axis.text = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.55)
				),

				# Gitterlinien
				panel.grid.major.y = element_blank(),
				panel.grid.major.x = element_line(rgb(105, 105, 105, maxColorValue = 255),
																					linetype = "solid",
																					size = rel(0.75)
				),
				panel.grid.minor = element_blank(),

				# Legende
				legend.position = "right",
				legend.title = element_blank(),
				legend.text = element_text(
					color = rgb(105, 105, 105, maxColorValue = 255),
					size = rel(0.75)
				),

				complete = TRUE
			)
	}
}
