# memprof

Evaluate an R expression whilst monitoring your system memory over time.

## Usage

```r
f <- function(msg) {
  Sys.sleep(1)
  z <- numeric(1e8) ## 800MB
  Sys.sleep(1)
  msg
}

out <- with_monitor(f())
plot(out)
```
