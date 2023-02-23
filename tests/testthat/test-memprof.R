test_that("with_monitor can monitor system memory", {
  f <- function(msg) {
    Sys.sleep(1)
    z <- numeric(1e8) ## 800MB
    Sys.sleep(1)
    msg
  }

  memprof <- with_monitor(f("hello"), mode = "system")

  expect_s3_class(memprof, "memprof_result")
  expect_equal(memprof$result, "hello")
  ## Available metrics depend on OS, so test against ps
  expected_names <- names(ps::ps_system_memory())
  expect_setequal(colnames(memprof$memory_use),
                  c("time", expected_names))
  ## Used must be present as this is used in plotting
  expect_true("used" %in% colnames(memprof$memory_use))

  ## No id in the memory_use i.e. we've not monitored indivdual processes
  expect_false("id" %in% colnames(memprof$memory_use))

  ## Check that memory has increased by "close to" expected amount
  ## there will be some variability here, especially as this is monitoring
  ## the memory on the whole system. Revisit this if it is a bit flaky
  start_mem <- mean(memprof$memory_use[1:8, "used"])
  rows <- nrow(memprof$memory_use)
  end_mem <- mean(memprof$memory_use[(rows - 8):rows, "used"])
  expect_equal(end_mem - start_mem, 8e8, tolerance = 0.2)

  t <- tempfile()
  png(filename = t)
  plot(memprof)
  dev.off()
  ## Test that something has been generated successfully
  expect_true(file.size(t) > 100)
})


test_that("with_monitor can monitor process and child process memory", {
  ## Run function h, which spawns g, which spawns f to check
  ## child memory is monitored
  h <- function() {
    g <- function() {

      f <- function(msg) {
        Sys.sleep(1)
        z <- numeric(1e8) ## 800MB
        Sys.sleep(1)
        msg
      }

      p <- callr::r_bg(f, list(msg = "hello from f"))
      p$wait()
      c("hello from g", p$get_result())
    }

    p <- callr::r_bg(g)
    p$wait()
    c("hello from h", p$get_result())
  }

  memprof <- with_monitor(h(), mode = "process")

  expect_s3_class(memprof, "memprof_result")
  expect_equal(memprof$result,
               c("hello from h", "hello from g", "hello from f"))
  ## Available metrics depend on OS, so test against ps
  expected_names <- names(ps::ps_memory_info())
  expect_setequal(colnames(memprof$memory_use),
                  c("time", "id", "parent_id", "name", expected_names))

  ## All the processes are in the log
  ## We expect 4 processes, 1 for this process (running h), 1 for process
  ## running g and 1 for process running f
  expect_length(unique(memprof$memory_use$id), 3)

  ## Check that memory has increased by "close to" expected amount
  ## there will be some variability here, especially as this is monitoring
  ## the memory on the whole system. Revisit this if it is a bit flaky
  total_mem <- used_memory_total_by_time(memprof$memory_use)
  start_mem <- mean(total_mem[1:8, "used"])
  rows <- nrow(total_mem)
  end_mem <- mean(total_mem[(rows - 8):rows, "used"])
  expect_equal(end_mem - start_mem, 8e8, tolerance = 0.2)

  t <- tempfile()
  png(filename = t)
  plot(memprof)
  dev.off()
  ## Test that something has been generated successfully
  expect_true(file.size(t) > 100)
})


test_that("with_monitor errors if monitor file dir doesn't exist", {
  t <- tempfile()
  file <- file.path(t, "new_file.csv")
  expect_error(
    with_monitor(function() message("hello"), monitor_file = file),
    sprintf(paste0("Containing dir for monitor file '%s' must exist",
                   ", create dir or review path."), file),
    fixed = TRUE)
})

test_that("with_monitor errors if monitor file exists and overwrite FALSE", {
  t <- tempfile()
  dir.create(t)
  file <- file.path(t, "new_file.csv")
  writeLines("some text", file)
  expect_error(
    with_monitor(function() message("hello"), monitor_file = file),
    sprintf(paste0("Monitor file at '%s' already exists and overwrite is ",
                   "'FALSE'. Delete file or set overwrite to 'TRUE'."), file),
    fixed = TRUE)
})


test_that("with_monitor overwrites monitor file", {
  t <- tempfile()
  dir.create(t)
  file <- file.path(t, "new_file.csv")
  writeLines("some text", file)

  f <- function(msg) {
    Sys.sleep(1)
    z <- numeric(1e8) ## 800MB
    Sys.sleep(1)
    msg
  }

  memprof <- with_monitor(f("hello"), mode = "system",
                          monitor_file = file, overwrite = TRUE)
  lines <- readLines(file)
  expect_false(any(grepl("some text", lines)))
})


test_that("memprof can recover profile data from errored code", {
  f <- function(msg) {
    Sys.sleep(1)
    z <- numeric(1e8) ## 800MB
    Sys.sleep(1)
    stop("An error")
    msg
  }

  log_file <- tempfile()
  tryCatch(with_monitor(f(), mode = "system", monitor_file = log_file),
           error = function(e) e)
  memory_use <- monitor_read(log_file)

  expect_s3_class(memory_use, "memprof_use")

  ## Available metrics depend on OS, so test against ps
  expected_names <- names(ps::ps_system_memory())
  expect_setequal(colnames(memory_use),
                  c("time", expected_names))
  ## Used must be present as this is used in plotting
  expect_true("used" %in% colnames(memory_use))

  ## Check that memory has increased by "close to" expected amount
  ## there will be some variability here, especially as this is monitoring
  ## the memory on the whole system. Revisit this if it is a bit flaky
  start_mem <- mean(memory_use[1:8, "used"])
  rows <- nrow(memory_use)
  end_mem <- mean(memory_use[(rows - 8):rows, "used"])
  expect_equal(end_mem - start_mem, 8e8, tolerance = 0.2)

  ## Can plot a memprof_use object
  t <- tempfile()
  png(filename = t)
  plot(memory_use)
  dev.off()
  ## Test that something has been generated successfully
  expect_true(file.size(t) > 100)
})
