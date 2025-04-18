% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{try_map}
\alias{try_map}
\title{Template to create silent version of \code{lapply}}
\usage{
try_map(X, FUN, ..., USE.NAMES = FALSE, OTHERWISE = NULL, SILENT = TRUE)
}
\arguments{
\item{X}{Input to iterate over}

\item{FUN}{Function to apply to each element of \code{X}. Can also be a name (character) or index (integer)}

\item{...}{Arguments passed to \code{FUN}}

\item{USE.NAMES}{If \code{TRUE}, names of \code{X} will be applied to names of output}

\item{OTHERWISE}{Value to be returned if output of \code{FUN} results in an error. Default is \code{NA}}

\item{SILENT}{If \code{TRUE} (default), warnings are suppressed}
}
\value{
List with same length as \code{X}
}
\description{
Template to create silent version of \code{lapply}
}
