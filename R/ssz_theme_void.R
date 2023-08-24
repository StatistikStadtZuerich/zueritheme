#' ssz_theme_void
#'
#' @description Function for styling void ggplots according to Stadt Zürich's corporate design
#'
#' @param base_size optional, basic font size, parameter passed to minimal theme
#' @param base_family optional, basic font family, parameter to be passed to minimal theme
#' @param base_line_size optional, basic line size, parameter passed to minimal theme
#' @param base_rect_size optional, basic rect size, parameter passed to minimal theme
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
	list(
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

				# Legend
				legend.position = "left",

				legend.justification = c(0, 1),

				legend.margin = margin(t = -2, r = 100, b = 0, l = 0),

				legend.title = element_blank(),

				legend.text = element_text(color = "#020304",
																	 size = rel(1)),

				legend.key.height = unit(base_size / 11, "line"),

				legend.key.width = unit(base_size / 11, "line"),

				legend.spacing.y = unit(base_size / 100, "line"),

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
			),

		guides(fill = guide_legend(byrow = TRUE))
	)
}
