% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vectors.R
\name{is_sorted}
\alias{is_sorted}
\alias{is_ordered}
\title{Determine if a vector is orderec (i.e., sorted)}
\usage{
is_sorted(x, na.rm = TRUE)

is_ordered(x, na.rm = TRUE)
}
\arguments{
\item{x}{Vector}

\item{na.rm}{If \code{TRUE} (default), missing values are ignored}
}
\value{
Logical of length 1. If \code{x} is in increasing or decreasing order, output is \code{TRUE}. Otherwise, output is \code{FALSE}
}
\description{
Determine if a vector is orderec (i.e., sorted)

Alias for is_sorted
}
