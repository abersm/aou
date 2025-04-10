% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/programming.R
\name{as_try_fn}
\alias{as_try_fn}
\title{Wrap a function in \code{tryCatch}}
\usage{
as_try_fn(.f, .otherwise = NULL, .silent = TRUE)
}
\arguments{
\item{.f}{Expression}

\item{.otherwise}{Return value if \code{.f} results in an error. Default is \code{NULL}}

\item{.silent}{If \code{TRUE} (default), warnings are suppressed}
}
\value{
Function
}
\description{
Wrap a function in \code{tryCatch}
}
