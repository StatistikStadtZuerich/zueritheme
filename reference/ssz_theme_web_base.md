# ssz_theme_base_web

Function that creates a ggplot theme that can be used as a base theme on
top of which to replace the theme's items that depend on the orientation
of the web plot.

## Usage

``` r
ssz_theme_web_base(base_size, base_family, base_line_size, base_rect_size)
```

## Arguments

- base_size:

  optional, defines the base font size used throughout the plot. This
  parameter is passed to the theme_bw function, ensuring consistent
  typography across ggplot elements.

- base_family:

  optional, specifies the base font family for the text elements in the
  ggplot object. This parameter is passed to the theme_bw function,
  ensuring a consistent font style across ggplot elements.

- base_line_size:

  optional, sets the base line size for lines and borders within the
  ggplot object. This parameter is passed to the theme_bw function,
  ensuring consistent line thickness across visual elements.

- base_rect_size:

  optional, specifies the base size for rectangular elements such as
  legend boxes or plot backgrounds within the ggplot object. This
  parameter is passed to the theme_bw function, ensuring consistent
  sizing of rectangular shapes across ggplot elements.

## Value

a ggplot theme
