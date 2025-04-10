% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survival.R
\name{p_log_rank}
\alias{p_log_rank}
\alias{p_surv}
\title{Log-rank p value}
\usage{
p_log_rank(
  df,
  predictor_var,
  time_var = "time",
  outcome_var = "death",
  method = "MH",
  otherwise = NA_real_
)

p_surv(x)
}
\arguments{
\item{df}{Data frame}

\item{predictor_var}{Variable(s) to use as covariates in model. Enter as character vector}

\item{time_var}{Variable containing follow up time. Enter as length 1 character vector. Default is \code{"time"}}

\item{outcome_var}{Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as length 1 character vector. Default is \code{"death"}}

\item{method}{Method for determining P value. Options: \code{"MH"} (default, Mantel-Haenszel test), \code{"PP"} (Peto and Peto modification of Gehan-Wilcoxon test)}

\item{otherwise}{Value to return if unable to perform log-rank test}

\item{x}{coph or survdiff object}
}
\value{
Length 1 numeric vector containing P value
}
\description{
Log-rank p value

P value for log rank test
}
