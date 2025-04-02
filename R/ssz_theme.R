#' ssz_theme
#'
#' @description Function for styling ggplots graphs according to Stadt Zürich's
#' corporate design.
#'
#' @param grid_lines specifies the axis on which to display the primary
#' grid lines or axis lines. "y" is the default value, can be "x" or "y" for
#' publication type "web", and "x", "y", "both", or "none" for publication_type
#' "print".
#' @param base_size optional, defines the base font size used throughout the plot.
#' This parameter is passed to the ssz_theme_base and ssz_theme_base_web function,
#' ensuring consistent typography across ggplot elements. Adjust this value to
#' control the overall text size in the ggplot object for better readability or
#' stylistic preference.
#' @param base_family optional, specifies the base font family for the text elements
#' in the ggplot object. This parameter is passed to the ssz_theme_base and
#' ssz_theme_base_web function, ensuring a consistent font style across ggplot elements.
#' Adjust this value to control the overall font family in the ggplot object.
#' @param base_line_size optional, sets the base line size for lines and borders
#' within the ggplot object. This parameter is passed to the ssz_theme_base and
#' ssz_theme_base_web function, ensuring consistent line thickness across visual
#' elements. Adjust this value to modify the overall line width for improved visual
#' clarity or stylistic preferences.
#' @param base_rect_size optional, specifies the base size for rectangular elements
#' such as legend boxes or plot backgrounds within the ggplot object. This parameter
#' is passed to the ssz_theme_base and ssz_theme_base_web function, ensuring
#' consistent sizing of rectangular shapes across ggplot elements. Adjust this value
#' to control the thickness of borders for better visual balance or customization.
#' @param publication_type specifies the output type for with the theme is
#' tailored. Available options are "print" and "web", with "print" as the default
#' value. This parameter controls how visual elements like grid lines and axis
#' formatting are adjusted to best suit the chosen publication type, ensuring optimal
#' presentation for either print or web-based outputs.
#'
#' @return a ggplot theme.
#'
#' @export
#' @import ggplot2 dplyr
#' @importFrom gridExtra arrangeGrob
#' @importFrom grDevices rgb
#' @examples \dontrun{
#' ggplot(...) +
#'   ssz_theme(grid_lines = "x", publication_type = "print")
#' }
ssz_theme <- function(grid_lines = "y",
                      base_size = 8.2,
                      base_family = "",
                      base_line_size = base_size / 170,
                      base_rect_size = base_size / 170,
                      publication_type = "print") {
  # Transform input for x and y for further use
  axis_grid <- tolower(as.character(grid_lines))

  # Transform input for publication type
  publication_type <- tolower(as.character(publication_type))

  # Return error message if publication_type is wrong
  if (!(publication_type %in% c("print", "web"))) {
    warning <-
      c(
        paste0(
          "\n  ",
          toupper(publication_type),
          " is not a valid input.\n",
          "  Please provide a publication type for the theme. Accepted values:\n  'print'\n  'web'"
        )
      )
    stop(warning)
  }

  # Check the publication_type
  if (publication_type == "print") {
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
  } else if (publication_type == "web") {
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
          axis.line.x = element_line(colour = "#737373", size = 0.25),
          axis.ticks.x = element_line(colour = "#737373", size = 0.25),
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
          axis.line.y = element_line(colour = "#737373", size = 0.25),
          axis.ticks.y = element_line(colour = "#737373", size = 0.25),
          axis.ticks.length.y = unit(0.1, "cm")
        )
    }
  }
}
