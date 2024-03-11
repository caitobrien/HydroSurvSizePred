
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HydroSurvSizePred

<!-- badges: start -->
<!-- badges: end -->

The goal of **HydroSurvSizePred** is to explore spring/summer Chinook
salmon, *Oncorhynchus tshawytscha*, smolt length distributions, predator
risk from Northern Pikeminnow and Pacific Hake, and survival across
years and seasons during downstream migration through the Federal
Columbia River Power System Hydrosystem (FCRPS)

Please note that this Shiny App is dependent on data availability and
serves as an exploratory tool.

## Installation

You can install the development version of **HydroSurvSizePred** from
[GitHub](https://github.com/) with:

``` r
# Install the developmental version of the HydroSurvSizePred package from GitHub
devtools::install_github("caitobrien/HydroSurvSizePred")

# Load the package
library(HydroSurvSizePred)

# Run the app
run_app()
```

\*Since project is currently in development, please rerun
`install_github("caitobrien/HydroSurvSizePred")` to see latest commits.
If no commits since last import, a warning will appear: “Skipping
install of ‘HydroSurvSizePred’ from a github remote, the SHA1 (fdd71350)
has not changed since last install. Use `force = TRUE` to force
installation”

If you are interested in the files that support the development version,
please see:
(<https://github.com/caitobrien/HydroSurvSizePred>)\[<https://github.com/caitobrien/HydroSurvSizePred>\]
for files necessary to run. The app structure follows a Golem framework
described in (Engineering Production-Grade Shiny
Apps)\[<https://engineering-shiny.org/setting-up-for-success.html>\] by
Colin Fay, Sébastien Rochette, Vincent Guyader and Cervan Girard.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(HydroSurvSizePred)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
