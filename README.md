
<!-- README.md is generated from README.Rmd. Please edit that file -->

# memprof

<!-- badges: start -->

![R build
status](https://github.com/mrc-ide/memprof/actions/workflows/R-CMD-check.yaml/badge.svg)
<!-- badges: end -->

Evaluate an R expression whilst monitoring the process or your system
memory over time.

## Requirements

This requires the development version of `ps` to render the plot with
accurate numbers for memory use if monitoring a process and not the
whole system. At time of writing CRAN version of `ps` reports memory
size from `ps_memory_info` in pages, development version reports in
bytes. Install the development version using

``` r
# install.packages("remotes")
remotes::install_github("r-lib/ps")
```

## Installation

You can install the development version of memprof from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("mrc-ide/memprof")
```

## Example

To monitor process memory including any spawned child processes:

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

To monitor system memory:

``` r
out <- with_monitor(f("hello"), mode = "system")
```

``` r
plot(out)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
