<img src="man/figures/Hexagon_zueritheme.png" align="right" height="138.5" width="138.5"/>

# zueritheme

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/zueritheme)](https://CRAN.R-project.org/package=zueritheme)
[![R-CMD-check](https://github.com/StatistikStadtZuerich/zueritheme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/StatistikStadtZuerich/zueritheme/actions/workflows/R-CMD-check.yaml)
[![GitLab Pipeline](https://cmp-sdlc.stzh.ch/OE-7035/ssz-da/libraries/zueriverse/zueritheme/badges/main/pipeline.svg?key_text=GitlabPipeline&key_width=100)](https://cmp-sdlc.stzh.ch/OE-7035/ssz-da/libraries/zueriverse/zueritheme/badges/main/pipeline.svg?key_text=GitLabPipeline&key_width=100)
<!-- badges: end -->

`zueritheme` is an R-Package, which provides a `ggplot2` theme that conforms to the corporate design of the city of Zurich. `zueritheme`'s main function `ssz_theme(...)` provides most of the styling. However, in order for `ggplot2` graphs to fully conform to the corporate design, a few manual steps are still necessary. These steps are explained in further detail in the [`zueriplots`](https://github.com/StatistikStadtZuerich/zueriplots) repository. You can find CI/CD conform color palettes in the [`zuericolors`](https://github.com/StatistikStadtZuerich/zuericolors) package.

## Installation

The easiest way to get `zueritheme` is to install it from this repo:

``` r
# install.packages("devtools")
devtools::install_github("StatistikStadtZuerich/zueritheme")
```

Alternatively, download the files (by clicking 'Clone or download' / 'Download Zip'), extract it to any location on your computer, e.g. to your Desktop and then run:

``` r
remotes::install_local("<path_to_location>/zueritheme-main")
```

## Version

To check your version of `zueritheme`, run:

``` r
packageVersion("zueritheme")
```

## Example

This is an example which shows you how to set the `ssz_theme` as your general `ggplot` theme:

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
    ssz_theme(publication_type  = "print", grid_lines = "x")
```

With the release of version 0.0.2 of `zueritheme`, a new function parameter named `publication_type` has been added, allowing for different styling options specifically for web-based `ggplots`, such as those generated with `ggiraph`.

For detailed instructions on how to create correctly styled plots (for print), check out the [zueriplots repository](https://github.com/StatistikStadtZuerich/zueriplots).

## Getting Help

If you encounter a bug, please contact [statistik\@zuerich.ch](mailto:statistik@zuerich.ch){.email}.
