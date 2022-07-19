
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsviewer

<!-- badges: start -->
<!-- badges: end -->

`tsviewer` is a time series viewer for auditing tag deployments as part
of the heart rate scaling project. One day I’ll convert it to a
general-purpose tool for tag data…

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FlukeAndFeather/tsviewer", ref = "narwhal")
```

## Example

Load narwhal data and launch the tool.

``` r
library(tsviewer)
# The resulting data frame from 10hz_pitch_roll.Rmd
narwhal_10hz <- "your code here"
# Launch the tool
run_tsviewer(narwhal_10hz)
```

Use brushing to zoom in on the top two depth profiles. They’re
hierarchical, so use the first one to roughly isolate the period of
interest and use the second one for additional detail.

Clicking on the detail plots (third depth profile, pitch, and roll)
shows the time at that point in red.

## Problems

Encounter a problem? Open an
[issue](https://github.com/FlukeAndFeather/tsviewer/issues)!
