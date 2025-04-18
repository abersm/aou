% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testing_prop.R
\name{p_prop_test}
\alias{p_prop_test}
\title{P value for proportion test}
\usage{
p_prop_test(
  x,
  n,
  ci = 0.95,
  hypothesis_type = "two.sided",
  yates = FALSE,
  otherwise = NA_real_
)
}
\arguments{
\item{x}{Vector containing number of positive individuals, 1D table with 2 entries, or table (or matrix) with 2 columns containing number positive in 1 column and number negative in other column}

\item{n}{If \code{x} is a vector, \code{n} must be a vector of equal length and contain the corresponding total number of individuals}

\item{ci}{Confidence level. Default is \code{0.95}}

\item{hypothesis_type}{Type of hypothesis test. Options: \code{"two.sided"} (default), \code{"less"}, \code{"greater"}}

\item{yates}{If \code{FALSE} (default), Yates' correction not applied}

\item{otherwise}{Value to return if unable to run test. Default is \code{NA_real_}}
}
\value{
Length 1 numeric vector containing P value
}
\description{
Functionality from \code{stats::prop.test}
}
