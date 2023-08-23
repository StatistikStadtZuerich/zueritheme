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
#' @param grid_lines axis, on which you want to show the main grid lines
#'
#' @export
#' @import ggplot2 dplyr
#' @importFrom gridExtra arrangeGrob
#' @importFrom grDevices rgb
#' @examples \dontrun{
#' ggplot(...) +
#'   ssz_theme()
#' }
ssz_theme <- function(base_size = 8.2,
											base_family = "",
											base_line_size = base_size / 170,
											base_rect_size = base_size / 170,
											grid_lines) {

	# Orientation value must be character
	if (missing(grid_lines)) {
		warning <- c("You forgot to specify on which axis you want to display the grid lines of the plot.\n  Please provide axis for grid lines. Accepted values:\n  'x'\n  'y'\n  'both'")
		stop(warning)
	}

	# Transform input for x and y for further use
	axis_grid <<- tolower(as.character(substitute(grid_lines)))

	# Return error message if orientation is not x or y
	if (!(axis_grid %in% c("x", "y", "both"))) {
		warning <- c(paste0("\n  ",toupper(axis_grid), " is an unvalid input.\n  Please provide axis for grid lines. Accepted values:\n  'x'\n  'y'\n  'both'"))
		stop(warning)
	}

	# Orientation == x
	if (axis_grid == "x") {

		# List object
		list(

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

					plot.title = element_text(
						color = "#020304",
						face = "bold",
						size = rel(1.5),
						hjust = 0,
						margin = margin(t = 0, r = 0, b = 2, l = 0)
					),

					# Subtitle
					plot.subtitle = element_text(
						color = "#020304",
						size = rel(1),
						hjust = 0,
						margin = margin(t = 0, r = 0, b = 25, l = 0)
					),

					# Axis text and title
					axis.title.x = element_text(
						color = "#020304",
						size = rel(1),
						margin = margin(t = 20, r = 0, b = 0, l = 0)
					),

					axis.title.y = element_text(
						color = "#020304",
						size = rel(1),
						hjust = 0,
						vjust = 1,
						margin = margin(t = 0, r = 0, b = 0, l = 0)
					),

					axis.text = element_text(
						color = "#020304",
						size = rel(1),
					),

					axis.ticks = element_blank(),

					# Grid Lines
					panel.grid.major.y = element_blank(),

					panel.grid.major.x = element_line(
						color = "#020304",
						linetype = "solid",
						size = rel(3)
					),

					panel.grid.minor = element_blank(),

					panel.border = element_blank(),

					# Legend
					legend.position = "left",

					legend.justification = c(0,1),

					legend.margin = margin(-2, 100, 0, 0),

					legend.title = element_blank(),

					legend.text = element_text(
						color = "#020304",
						size = rel(1)
					),

					legend.key.height = unit(base_size/11, "line"),

					legend.key.width = unit(base_size/11, "line"),

					legend.spacing.y = unit(base_size/100, "line"),

					# Caption
					plot.caption.position = "plot",

					plot.caption = element_text(
						size = rel(1),
						margin = margin(t = 15, r = 0, b = 0, l = 0),
						hjust = 0
					),

					complete = FALSE
				),

			guides(fill = guide_legend(byrow = TRUE))
		)

	# Orientation == y
	} else if (axis_grid == "y") {

		# List object
		list(

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

					plot.title = element_text(
						color = "#020304",
						face = "bold",
						size = rel(1.5),
						hjust = 0,
						margin = margin(t = 0, r = 0, b = 2, l = 0)
					),

					# Subtitle
					plot.subtitle = element_text(
						color = "#020304",
						hjust = 0,
						size = rel(1),
						margin = margin(t = 0, r = 0, b = 25, l = 0)
					),

					# Axis text and title
					axis.title.x = element_text(
						color = "#020304",
						size = rel(1),
						margin = margin(t = 20, r = 0, b = 0, l = 0)
					),

					axis.title.y = element_text(
						color = "#020304",
						size = rel(1),
						hjust = 0,
						vjust = 1,
						margin = margin(t = 0, r = 0, b = 0, l = 0)
					),

					axis.text = element_text(
						color = "#020304",
						size = rel(1),
					),

					axis.ticks = element_blank(),

					# Grid Lines
					panel.grid.major.x = element_blank(),

					panel.grid.major.y = element_line(
						color = "#020304",
						linetype = "solid",
						size = rel(3)
					),

					panel.grid.minor = element_blank(),

					panel.border = element_blank(),

					# Legend
					legend.position = "left",

					legend.justification = c(0,1),

					legend.margin = margin(-2, 100, 0, 0),

					legend.title = element_blank(),

					legend.text = element_text(
						color = "#020304",
						size = rel(1)
					),

					legend.key.height = unit(base_size/11, "line"),

					legend.key.width = unit(base_size/11, "line"),

					legend.spacing.y = unit(base_size/100, "line"),

					# Caption
					plot.caption.position = "plot",

					plot.caption = element_text(
						size = rel(1),
						margin = margin(t = 15, r = 0, b = 0, l = 0),
						hjust = 0
					),

					complete = FALSE
				),

			guides(fill = guide_legend(byrow = TRUE))
		)
	}

	# Orientation == both
	else if (axis_grid == "both") {

		# List object
		list(

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

					plot.title = element_text(
						color = "#020304",
						face = "bold",
						size = rel(1.5),
						hjust = 0,
						margin = margin(t = 0, r = 0, b = 2, l = 0)
					),

					# Subtitle
					plot.subtitle = element_text(
						color = "#020304",
						hjust = 0,
						size = rel(1),
						margin = margin(t = 0, r = 0, b = 25, l = 0)
					),

					# Axis text and title
					axis.title.x = element_text(
						color = "#020304",
						size = rel(1),
						margin = margin(t = 20, r = 0, b = 0, l = 0)
					),

					axis.title.y = element_text(
						color = "#020304",
						size = rel(1),
						hjust = 0,
						vjust = 1,
						margin = margin(t = 0, r = 0, b = 0, l = 0)
					),

					axis.text = element_text(
						color = "#020304",
						size = rel(1),
					),

					axis.ticks = element_blank(),

					# Grid Lines
					panel.grid.major.y = element_line(
						color = "#020304",
						linetype = "solid",
						size = rel(3)
					),

					panel.grid.major.x = element_line(
						color = "#020304",
						linetype = "solid",
						size = rel(3)
					),

					panel.grid.minor = element_blank(),

					panel.border = element_blank(),

					# Legend
					legend.position = "left",

					legend.justification = c(0,1),

					legend.margin = margin(-2, 100, 0, 0),

					legend.title = element_blank(),

					legend.text = element_text(
						color = "#020304",
						size = rel(1)
					),

					legend.key.height = unit(base_size/11, "line"),

					legend.key.width = unit(base_size/11, "line"),

					legend.spacing.y = unit(base_size/100, "line"),

					# Caption
					plot.caption.position = "plot",

					plot.caption = element_text(
						size = rel(1),
						margin = margin(t = 15, r = 0, b = 0, l = 0),
						hjust = 0
					),

					complete = FALSE
				),

			guides(fill = guide_legend(byrow = TRUE))
		)

	}
}
