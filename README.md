<img src='pictures/Hexagon_zueritheme.png' align="right" height="138.5" />

# zueritheme

<!-- badges: start -->
<!-- badges: end -->

zueritheme is an R-Package, which provides a ggplot2 theme that conforms to the corporate design of the city of Zurich. zueritheme's main function `ssz_theme(...)` provides most of the styling. However, in order for ggplot2 graphs to fully conform to the corporate design, a few manual steps are still necessary. These steps are explained in further detail in the [zueriplots](https://github.com/StatistikStadtZuerich/zueriplots) repository. You can find CI/CD conform color palettes in the [zuericolors](https://github.com/StatistikStadtZuerich/zuericolors) package.

## Installation

The easiest way to get zuericolors is to install it from this repo:

``` r
# install.packages("devtools")
devtools::install_github("StatistikStadtZuerich/zueritheme")
```

Alternatively, download the files (by clicking 'Clone or download' / 'Download Zip'), extract it to any location on your computer, e.g. to your Desktop and then run:

``` r
remotes::install_local("<path_to_location>/zueritheme-main")
```

## Version
To check your version of zueritheme, run:

``` r
packageVersion("zueritheme")
```

## Example

This is an example which shows you how to set the ssz_theme as your general ggplot theme:

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
	ssz_theme(grid_lines = "x")
```

## Getting Help
If you encounter a bug, please contact statistik@zuerich.ch.

