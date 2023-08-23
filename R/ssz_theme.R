##### SSZ-ggplot-Theme
# library(ggplot2)
# library(gridExtra)
# library(dplyr)

#' ssz_theme
#'
#' @description Function for styling ggplots according to Stadt Zürich's corporate design
#'
#' @param base_size optional, basic font size, parameter passed to minimal theme
#' @param base_family optional, basic font family, parameter to be passed to minimal theme
#' @param base_line_size optional, basic line size, parameter passed to minimal theme
#' @param base_rect_size optional, basic rect size, parameter passed to minimal theme
#' @param grid_lines axis, on which to show the main grid lines
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
		warning <-
			paste0(
				"You forgot to specify on which axis you want to display the grid lines of the plot.\n",
				"  Please provide axis for grid lines. Accepted values:\n  'x'\n  'y'\n  'both'"
			)
		stop(warning)
	}

	# Transform input for x and y for further use
	axis_grid <<- tolower(as.character(substitute(grid_lines)))

	# Return error message if orientation is not x or y
	if (!(axis_grid %in% c("x", "y", "both"))) {
		warning <-
			c(
				paste0(
					"\n  ",
					toupper(axis_grid),
					" is not a valid input.\n",
					"  Please provide axis for grid lines. Accepted values:\n  'x'\n  'y'\n  'both'"
				)
			)
		stop(warning)
	}

	# Orientation == x
	if (axis_grid == "x") {
		# List object
		list(
			# Base theme ssz
			ssz_theme_base(
				base_size = base_size,
				base_family = base_family,
				base_line_size = base_line_size,
				base_rect_size = base_rect_size
			) %+replace%

				# SSZ specific theme components
				theme(
					# Grid Lines
					panel.grid.major.y = element_blank(),

					panel.grid.major.x = element_line(
						color = "#020304",
						linetype = "solid",
						size = rel(3)
					)
				),

			guides(fill = guide_legend(byrow = TRUE))
		)

		# Orientation == y
	} else if (axis_grid == "y") {
		# List object
		list(
			# Base theme ssz
			ssz_theme_base(
				base_size = base_size,
				base_family = base_family,
				base_line_size = base_line_size,
				base_rect_size = base_rect_size
			) %+replace%

				# SSZ specific theme components
				theme(
					# Grid Lines
					panel.grid.major.x = element_blank(),

					panel.grid.major.y = element_line(
						color = "#020304",
						linetype = "solid",
						size = rel(3)
					),
				),

			guides(fill = guide_legend(byrow = TRUE))
		)
	}

	# Orientation == both
	else if (axis_grid == "both") {
		# List object
		list(
			# Base theme ssz
			ssz_theme_base(
				base_size = base_size,
				base_family = base_family,
				base_line_size = base_line_size,
				base_rect_size = base_rect_size
			) %+replace%

				# SSZ specific theme components
				theme(
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

					panel.spacing = unit(base_size / 50, "lines"),

				),

			guides(fill = guide_legend(byrow = TRUE))
		)

	}
}
