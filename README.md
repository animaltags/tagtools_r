
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tagtools

<!-- badges: start -->

    #> ✔ Setting active project to "/Users/sld33/Dropbox/animaltags/tagtools_r".
    #> ✔ Adding "CRAN status badge" to 'README.Rmd'.
    #> ☐ Re-knit 'README.Rmd' with `devtools::build_readme()`.
    #> ✔ Adding "Lifecycle: stable badge" to 'README.Rmd'.
    #> ☐ Re-knit 'README.Rmd' with `devtools::build_readme()`.

[![R-CMD-check](https://github.com/animaltags/tagtools_r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/animaltags/tagtools_r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository contains an R package with the R version of the
animaltag tool kit. High-resolution movement-sensor tags typically
include accelerometers to measure body posture and sudden movements or
changes in speed, magnetometers to measure direction of travel, and
pressure sensors to measure dive depth in aquatic or marine animals. The
sensors in these tags usually sample many times per second. Some tags
include sensors for speed, turning rate (gyroscopes), and sound. This
package provides software tools to facilitate calibration, processing,
and analysis of such data. Tools are provided for: data import/export;
calibration (from raw data to calibrated data in scientific units);
visualization (for example, multi-panel time-series plots); data
processing (such as event detection, calculation of derived metrics like
jerk and dynamic acceleration, dive detection, and dive parameter
calculation); and statistical analysis (for example, track
reconstruction, a rotation test, and Mahalanobis distance analysis).

## Installation

You can install the current CRAN version of tagtools with:

``` r
install.packages("tagtools")
```

You can install the development version of tagtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("animaltags/tagtools_r")
```

## Further background

Documentation and vignettes are available at:
<https://animaltags.github.io/tagtools_r/>

This material is based upon research supported by the United States
Office of Naval Research under Award Number N00014-16-1-3089.
