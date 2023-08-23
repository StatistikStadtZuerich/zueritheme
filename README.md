<img src='pictures/Hexagon_zueritheme.png' align="right" height="138.5" />

# zueritheme

<!-- badges: start -->
<!-- badges: end -->

The goal of zueritheme is to provide a ggplot theme that conforms to the corporate identity/design of the city of Zurich.

Note: this is still under development! A void theme is coming soon.

## Installation

You can install the development version of zueritheme from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("StatistikStadtZuerich/zueritheme")
```

## Example

This is an example which shows you how to set the ssz_theme as your general ggplot_theme:

``` r
library(zueritheme)
library(ggplot2)
theme_set(ssz_theme())
```

Alternatively, you can add the theme to a ggplot directly:
``` r
library(zueritheme)
library(ggplot2)
ggplot(...) +
	... +
	ssz_theme()
```

