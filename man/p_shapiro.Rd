% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testing_normality.R
\name{p_shapiro}
\alias{p_shapiro}
\alias{p_dagostino}
\alias{p_anderson}
\alias{p_ks}
\title{Shapiro-Wilk test}
\usage{
p_shapiro(x, otherwise = 0)

p_dagostino(x, otherwise = 0)

p_anderson(x, otherwise = 0)

p_ks(x, otherwise = 0)
}
\arguments{
\item{x}{Numeric vector. Missing values allowed. Need at least 3 values to get a reliable result for \code{p_shapiro} (8 for \code{p_dagostino})}

\item{otherwise}{Output if normality test fails. Default is \code{0}}
}
\value{
Length 1 numeric vector containing P value
}
\description{
\code{stats::shapiro.test} can't be simplified

Functionality from fBasics package

Functionality from nortest package

\code{stats::ks.test} can't be simplified
Input can include missing values
}
