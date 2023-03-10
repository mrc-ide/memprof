% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memprof.R
\name{plot.memprof_use}
\alias{plot.memprof_use}
\title{Plot memprof use data frame}
\usage{
\method{plot}{memprof_use}(x, ...)
}
\arguments{
\item{x}{The \code{memprof_use} data frame}

\item{...}{Additional arguments passed on to plot}
}
\value{
Nothing, creates a plot.
}
\description{
If a \code{system} memory log this prints the total system memory used.
If a \code{process} memory log, it sums total memory of the process and any
child process at each time point and plots the total
}
