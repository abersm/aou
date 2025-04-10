% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats_transformation.R
\name{percentile}
\alias{percentile}
\alias{z_score}
\title{Percentile}
\usage{
percentile(x, inclusive = FALSE)

z_score(x)
}
\arguments{
\item{x}{Numeric vector}

\item{inclusive}{If \code{TRUE}, percentile calculated as percentage at OR below a given score (values can range from 0-1). If \code{FALSE} (default), percentile calculated as percentage below a given score (values can't be 0 or 1)}
}
\value{
Numeric vector with same length as input. Values range from 0-1. Missing values in \code{x} will also be missing in output. Percentile calculated after removing all missing values from \code{x}. Percentile only calculated when \code{x} contains more than 2 unique values. Otherwise, output values will be \code{NA_real_}
}
\description{
Functionality from \code{stats::ecdf}
}
