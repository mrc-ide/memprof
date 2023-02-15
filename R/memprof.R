#' Run an R expression and monitor memory
#'
#' Evaluate an expression, and monitor the memory every `poll_interval`.
#' This can monitor either the memory of your R expression and any processes
#' it spawns or it can monitor the memory of your entire system.
#'
#' @param expr The expression to evaluate.
#' @param mode The monitor mode to use, `process` to monitor the function
#'   process and any spawned processes. `system` to monitor the memory
#'   of the entire system.
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
with_monitor <- function(expr,
                         mode = "process",
                         poll_interval = 0.1,
                         monitor_file = tempfile(),
                         overwrite = FALSE,
                         gc_first = TRUE) {
  validate_monitor_file(monitor_file, overwrite)
  match_value(mode, c("process", "system"))
  memory <- monitor$new(monitor_file, mode, poll_interval, gc_first)
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
    mode = NULL,
    filename = NULL,
    interval = NULL
  ),

  public = list(
    initialize = function(filename, mode, interval, gc_first) {
      private$mode <- mode
      private$interval <- interval
      private$filename <- filename
      if (isTRUE(gc_first)) {
        gc()
      }
      dir.create(dirname(filename), FALSE, TRUE)

      private$process <- callr::r_bg(
        function(mode, interval) monitor_bg(mode, interval),
        list(mode = private$mode, interval = private$interval),
        stdout = private$filename,
        package = "memprof")

      wait_for_monitor_start(private$process, private$filename)
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

wait_for_monitor_start <- function(process, filename) {
  while (!file.exists(filename)) {
    if (!process$is_alive()) {
      stop("Failed to start monitor")
    }
    Sys.sleep(0.01)
  }
}


monitor_bg <- function(mode, interval) {
  get_memory <- if (identical(mode, "system")) {
    get_system_memory
  } else {
    get_process_memory
  }

  t0 <- as.numeric(Sys.time())
  memory_with_time <- function() {
    mem <- get_memory()
    time <- as.numeric(Sys.time()) - t0
    lapply(mem, function(x) c(time = time, x))
  }
  print_header(memory_with_time())
  repeat {
    print_log(memory_with_time())
    Sys.sleep(interval)
  }
}


get_system_memory <- function() {
  list(ps::ps_system_memory())
}


get_process_memory <- function() {
  processes <- processes_to_monitor()

  mem_use <- lapply(processes, function(process) {
    tryCatch({
      mem <- as.list(ps::ps_memory_info(process))
      c(list(id = ps::ps_pid(process),
             parent_id = ps::ps_ppid(process)),
        mem)},
      error = function(e) {
        ## If we're here the child process has probably been cleaned up
        ## in between listing it and then trying to get the
        ## memory from it. Ignore these cases
        NULL
      }
    )
  })
  mem_use[vlapply(mem_use, function(x) !is.null(x))]
}

processes_to_monitor <- function() {
  parent_process <- ps::ps_parent()
  all_children <- ps::ps_children(parent_process, recursive = TRUE)
  processes <- c(parent_process, all_children)
  monitor_pid <- ps::ps_pid()
  monitor_process <- vlapply(processes, function(process) {
    ps::ps_pid(process) == monitor_pid
  })
  processes[!monitor_process]
}


print_header <- function(logs) {
  cat(log_to_csv(names(logs[[1]])))
}


print_log <- function(logs) {
  logs <- vcapply(logs, function(log) log_to_csv(unlist(log, FALSE, FALSE)))
  cat(paste0(logs, collapse = ""))
}


log_to_csv <- function(log) {
  paste(paste(log, collapse = ","), "\n")
}


#' Plot memprof use data frame
#'
#' If a `system` memory log this prints the total system memory used.
#' If a `process` memory log, it sums total memory of the process and any
#' child process at each time point and plots the total
#'
#' @param x The `memprof_use` data frame
#' @param ... Additional arguments passed on to plot
#'
#' @return Nothing, creates a plot.
#' @export
plot.memprof_use <- function(x, ...) {
  if ("id" %in% colnames(x)) {
    x <- used_memory_total_by_time(x)
  }
  x$used <- x$used / 1e6
  op <- graphics::par(mar = c(4, 4, 1, 1))
  on.exit(graphics::par(op))
  plot(x$time,
       x$used,
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

used_memory_total_by_time <- function(data) {
  agg <- aggregate(data$rss, by = list(time = data$time), FUN = sum)
  colnames(agg) <- c("time", "used")
  agg
}
