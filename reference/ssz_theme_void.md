# ssz_theme_void

Function for styling ggplots graphs according to Stadt Zürich's
corporate design. Used for plots with a void theme, i.e. no grid lines
and no axis texts.

## Usage

``` r
ssz_theme_void(
  base_size = 8.2,
  base_family = "",
  base_line_size = base_size/170,
  base_rect_size = base_size/170
)
```

## Arguments

- base_size:

  optional, defines the base font size used throughout the plot. This
  parameter is passed to the theme_void function, ensuring consistent
  typography across ggplot elements. Adjust this value to control the
  overall text size in the ggplot object for better readability or
  stylistic preference.

- base_family:

  optional, specifies the base font family for the text elements in the
  ggplot object. This parameter is passed to the theme_void function,
  ensuring a consistent font style across ggplot elements. Adjust this
  value to control the overall font family in the ggplot object.

- base_line_size:

  optional, sets the base line size for lines and borders within the
  ggplot object. This parameter is passed to the theme_void function,
  ensuring consistent line thickness across visual elements. Adjust this
  value to \$ modify the overall line width for improved visual clarity
  or stylistic preferences.

- base_rect_size:

  optional, specifies the base size for rectangular elements such as
  legend boxes or plot backgrounds within the ggplot object. This
  parameter is passed to the theme_void function, ensuring consistent
  sizing of rectangular shapes across ggplot elements. Adjust this value
  to control the thickness of borders for better visual balance or
  customization.

## Value

a void ggplot theme.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(...) +
  ssz_theme_void()
} # }
```
