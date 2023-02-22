
<!-- README.md is generated from README.Rmd. Please edit that file -->

# memprof

<!-- badges: start -->

![R build
status](https://github.com/mrc-ide/memprof/actions/workflows/R-CMD-check.yaml/badge.svg)
<!-- badges: end -->

Evaluate an R expression whilst monitoring your system memory over time.

## Installation

You can install the development version of memprof from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("mrc-ide/memprof")
```

## Example

To monitor system memory whilst executing a function:

``` r
library(memprof)

f <- function(msg) {
  Sys.sleep(1)
  z <- numeric(1e8) ## 800MB
  Sys.sleep(1)
  msg
}

out <- with_monitor(f("hello"))
```

``` r
plot(out)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
