match_value <- function(x, choices, name = deparse(substitute(x))) {
  stopifnot(length(x) == 1L, is.character(x), !is.na(x))
  i <- match(x, choices)
  if (is.na(i)) {
    stop(sprintf("%s must be one of {%s}", name,
                 paste(choices, collapse = ", ")),
         call. = FALSE)
  }
  choices[[i]]
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
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
