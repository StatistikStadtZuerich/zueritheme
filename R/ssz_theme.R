#' ssz_theme
#'
#' @description Function for styling ggplots according to Stadt Zürich's corporate design
#'
#' @param grid_lines axis, on which to show the main grid lines. Default is "y".
#' @param base_size optional, basic font size, parameter passed to minimal theme
#' @param base_family optional, basic font family, parameter to be passed to minimal theme
#' @param base_line_size optional, basic line size, parameter passed to minimal theme
#' @param base_rect_size optional, basic rect size, parameter passed to minimal theme
#' @param medium medium, for which the theme is used. Possible options are "print" and "web". Default is "print".
#'
#' @export
#' @import ggplot2 dplyr
#' @importFrom gridExtra arrangeGrob
#' @importFrom grDevices rgb
#' @examples \dontrun{
#' ggplot(...) +
#'   ssz_theme(grid_lines = "x")
#' }
ssz_theme <- function(grid_lines = "y",
                      base_size = 8.2,
                      base_family = "",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170,
                      medium = "print") {
  # Transform input for x and y for further use
  axis_grid <- tolower(as.character(grid_lines))

  # Transform input for medium
  medium <- tolower(as.character(medium))

  # Return error message if medium is wrong
  if (!(medium %in% c("print", "web"))) {
    warning <-
      c(
        paste0(
          "\n  ",
          toupper(medium),
          " is not a valid input.\n",
          "  Please provide a medium for the theme. Accepted values:\n  'print'\n  'web'"
        )
      )
    stop(warning)

    # Check the Medium
  } else if (medium == "print") {
    # Return error message if orientation is not x or y
    if (!(axis_grid %in% c("x", "y", "both", "none"))) {
      warning <-
        c(
          paste0(
            "\n  ",
            toupper(axis_grid),
            " is not a valid input.\n",
            "  Please provide axis for grid lines. Accepted values:\n  'x'\n  'y'\n  'both'\n  'none'"
          )
        )
      stop(warning)
    }

    # Orientation == x
    if (axis_grid == "x") {
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
            linewidth = rel(3)
          )
        )

      # Orientation == y
    } else if (axis_grid == "y") {
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
        )

      # Orientation == both
    } else if (axis_grid == "both") {
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
          )
        )

      # Orientation == none
    } else if (axis_grid == "none") {
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
          panel.grid.major = element_blank()
        )
    }
  } else if (medium == "web") {
    # Return error message if orientation is not x or y
    if (!(axis_grid %in% c("x", "y"))) {
      warning <-
        c(
          paste0(
            "\n  ",
            toupper(axis_grid),
            " is not a valid input.\n",
            "  Please provide axis for grid lines. Accepted values:\n  'x'\n  'y'"
          )
        )
      stop(warning)
    }

    # Orientation == y
    if (axis_grid == "y") {
      # Base theme ssz
      ssz_theme_web_base(
        base_size = base_size,
        base_family = base_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
      ) %+replace%

        # SSZ specific theme components
        theme(

          # Axis Line on X Axis
          axis.line.x = element_line(colour = "#737373", size = 0.5),
          axis.ticks.x = element_line(colour = "#737373", size = 0.5),
          axis.ticks.length.x = unit(0.1, "cm")
        )

      # Orientation == x
    } else if (axis_grid == "x") {
      # Base theme ssz
      ssz_theme_web_base(
        base_size = base_size,
        base_family = base_family,
        base_line_size = base_line_size,
        base_rect_size = base_rect_size
      ) %+replace%

        # SSZ specific theme components
        theme(

          # Axis Line on X Axis
          axis.line.y = element_line(colour = "#737373", size = 0.5),
          axis.ticks.y = element_line(colour = "#737373", size = 0.5),
          axis.ticks.length.y = unit(0.1, "cm")
        )
    }
  }
}
