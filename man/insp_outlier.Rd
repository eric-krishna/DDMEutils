% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier.R
\name{insp_outlier}
\alias{insp_outlier}
\alias{insp_outlier.ts}
\alias{insp_outlier.data.frame}
\title{Time series outlier inspection}
\usage{
insp_outlier(x, ...)

\method{insp_outlier}{ts}(x, window_size = 3, anom_method = c("gesd",
  "iqr"))

\method{insp_outlier}{data.frame}(x, margin = 1L, window_size = 3,
  .parallel = FALSE, idcol = NULL, dtcol = NULL,
  out_format = c("wide", "long"), anom_method = c("gesd", "iqr"))
}
\arguments{
\item{x}{ts or data.frame}

\item{window_size}{integer window size. Determines how many observations near an outlier are used to calculate the input value.}

\item{anom_method}{"gesd" or "iqr", argument passed to anomalize::anomalize().}

\item{\code{...}}{Additional arguments for the data.frame case. See details.}
}
\description{
Time series outlier inspection
}
\details{
\itemize{
 \item When \code{margin = 1}, set \code{idcol} to an integer pointing out the index column that identifies the time series;
 \item When \code{margin = 2}, set \code{dtcol} to an integer pointing out the date column;
 \item .parallel TRUE/FALSE if tasks should be parallelized. If \code{TRUE}, uses \code{future} and \code{furrr} packages.
 \item out_format "wide" or "long", indicating output format. If \code{margin = 2} and there are many time series, \code{out_format = "long"} is easier to handle than wide format.
}
}
