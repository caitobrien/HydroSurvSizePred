
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
