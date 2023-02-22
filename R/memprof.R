#' Run an R expression and monitor memory
#'
#' Evaluate an expression, and monitor the memory every `poll_interval`.
#' This will monitor the memory of your entire system, not just the expression
#' being run.
#'
#' @param expr The expression to evaluate.
#' @param poll_interval Time between each check of memory use.
#' @param monitor_file Path to log memory monitor to, you can use this if
#'   your expr errors to recover the output for later plotting. Uses a
#'   tempfile by default.
#' @param overwrite If TRUE then overwrite `monitor_file` if it exists already.
#' @param gc_first If `TRUE` run a garbage collect before starting monitoring.
#'
#' @return A `memprof_monitor` object containing the `result` as the outcome
#'   from the expression and `memory_use` the memory used
#' @export
with_monitor <- function(expr, poll_interval = 0.1,
                         monitor_file = tempfile(),
                         overwrite = FALSE,
                         gc_first = TRUE) {
  validate_monitor_file(monitor_file, overwrite)
  memory <- monitor$new(monitor_file, poll_interval, gc_first)
  on.exit(memory$stop())
  result <- force(expr)
  used <- memory$finish()
  as_memprof_result(result, as_memprof_use(used))
}


#' Read monitor log file as a `memprof_use` data frame for plotting.
#'
#' @param monitor_file Path to memprof monitor log file.
#'
#' @return A `memprof_use` data frame.
#' @export
monitor_read <- function(monitor_file) {
  as_memprof_use(utils::read.csv(monitor_file))
}

as_memprof_use <- function(obj) {
  structure(obj, class = c("memprof_use", class(obj)))
}

as_memprof_result <- function(result, memory_used) {
  structure(list(
    result = result,
    memory_use = memory_used
  ), class = "memprof_result")
}


monitor <- R6::R6Class(
  "monitor",

  private = list(
    process = NULL,
    filename = NULL,
    interval = NULL
  ),

  public = list(
    initialize = function(filename, interval, gc_first) {
      private$interval <- interval
      private$filename <- filename
      if (isTRUE(gc_first)) {
        gc()
      }
      dir.create(dirname(filename), FALSE, TRUE)
      private$process <- callr::r_bg(
        function(interval) monitor_bg(interval),
        list(interval = private$interval),
        stdout = private$filename,
        package = "memprof")
      while (!file.exists(private$filename)) {
        if (!private$process$is_alive()) {
          stop("Failed to start monitor")
        }
        Sys.sleep(0.01)
      }
      invisible(TRUE)
    },

    read = function() {
      if (!file.exists(private$filename)) {
        message("Monitor log doesn't exist")
        return()
      }
      utils::read.csv(private$filename)
    },

    stop = function(verbose = FALSE) {
      if (!private$process$is_alive()) {
        return()
      }
      if (verbose) {
        message("Stopping monitor process")
      }
      private$process$kill()
    },

    finish = function(verbose = FALSE) {
      self$stop(verbose)
      self$read()
    }
  )
)


monitor_bg <- function(interval) {
  t0 <- as.numeric(Sys.time())
  system_memory <- function() {
    c(list(time = as.numeric(Sys.time()) - t0), ps::ps_system_memory())
  }
  print_header(system_memory())
  repeat {
    print_log(system_memory())
    Sys.sleep(interval)
  }
}


print_header <- function(log) {
  print_csv(names(log))
}


print_log <- function(log) {
  print_csv(unlist(log, FALSE, FALSE))
}


print_csv <- function(log) {
  cat(paste(paste(log, collapse = ","), "\n"))
}


validate_monitor_file <- function(monitor_file, overwrite) {
  if (!dir.exists(dirname(monitor_file))) {
    stop(sprintf(paste0("Containing dir for monitor file '%s' must exist,",
                        " create dir or review path."),
                 monitor_file))
  }
  if (file.exists(monitor_file)) {
    if (isFALSE(overwrite)) {
      stop(sprintf(paste0(
        "Monitor file at '%s' already exists and overwrite",
        " is 'FALSE'. Delete file or set overwrite to 'TRUE'."),
        monitor_file))
    } else {
      unlist(monitor_file)
    }
  }
  invisible(TRUE)
}


#' Plot memprof use data frame
#'
#' @param x The `memprof_use` data frame
#' @param ... Additional arguments passed on to plot
#'
#' @return Nothing, creates a plot.
#' @export
plot.memprof_use <- function(x, ...) {
  x$used <- x$used / 1e6
  op <- graphics::par(mar = c(4, 4, 1, 1))
  on.exit(graphics::par(op))
  plot(x$used,
       x$time,
       xlab = "Time (s)",
       ylab = "System used RAM (MB)",
       type = "l",
       lwd = 2,
       col = "#0055ff")
}

#' Plot memprof monitor result
#'
#' @param x The `memprof_result` object
#' @param ... Additional arguments passed on to plot
#'
#' @return Nothing, creates a plot.
#' @export
plot.memprof_result <- function(x, ...) {
  plot(x$memory_use)
}
