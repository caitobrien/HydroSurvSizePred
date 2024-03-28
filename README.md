
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HydroSurvSizePred

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<!-- lastcommit: start -->

[![Last
Commit](https://img.shields.io/github/last-commit/caitobrien/HydroSurvSizePred)](https://github.com/caitobrien/HydroSurvSizePred/commits/main)
<!-- lastcommit: end -->

The goal of **HydroSurvSizePred** is to explore spring/summer Chinook
salmon, *Oncorhynchus tshawytscha*, smolt length distributions, predator
risk from Northern Pikeminnow and Pacific Hake, and survival across
years and seasons during downstream migration through the Federal
Columbia River Power System Hydrosystem (FCRPS), Pacific Northwest, USA.

*Please note that this Shiny App is currently under development and
dependent on data availability. This app serves as an exploratory tool.*

## Installation

**Option 1:** You can install the development version of
*HydroSurvSizePred* from [GitHub](https://github.com/) with:

``` r
# Install the developmental version of the HydroSurvSizePred package from GitHub
devtools::install_github("caitobrien/HydroSurvSizePred")

# Load the package
library(HydroSurvSizePred)

# Run the app
run_app()

#if needed, detach the package workspace and repeat above lines of code
detach("package:HydroSurvSizePred", unload=TRUE)
```

*HydroSurvSizePred* is currently in development and changes are
continuously being made. If you have already imported to R studio,
please rerun `install_github("caitobrien/HydroSurvSizePred")` to see
latest changes in the developmental version. If no changes have been
made since last import, a warning will appear:
`Skipping install of 'HydroSurvSizePred' from a github remote, the SHAX (XXXXXX) has not changed since last install. Use 'force = TRUE' to force installation`
and you have the latest developmental version imported. Use `detach()`
if not working properly and reinstall.

This option will run the Shiny App within you RStudio environment but
will not download the background files necessary to run.

**Option 2:** If you are interested in the files that support the
development version, please see:
<https://github.com/caitobrien/HydroSurvSizePred> to clone the
repository. Alternatively, within Rstudio, use the following steps:

1.  Start a new project with
    `File > New Project > Version Control > Git`

2.  In the repository URL field, paste
    `https://github.com/caitobrien/HydroSurvSizePred.git`

3.  Once project is created, and the repository is cloned, you can run
    the app within R environment by going to the folder:
    `dev > run_dev.R` and loading lines of code, with the final
    `run_app()` to launch the app.

The app structure follows a Golem framework described in [Engineering
Production-Grade Shiny
Apps](https://engineering-shiny.org/setting-up-for-success.html) by
Colin Fay, Sébastien Rochette, Vincent Guyader and Cervan Girard.

## Contact

This app is being developed by Caitlin O’Brien, Research Scientist,
Columbia Basin Research, SAFS, University of Washington. Please reach
out with questions/concerns via <csobrien@uw.edu>.
