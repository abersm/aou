% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survival.R
\name{surv_tidy}
\alias{surv_tidy}
\title{Summary of coxph object}
\usage{
surv_tidy(x, ci = 0.95, clean_predictor_names = TRUE)
}
\arguments{
\item{x}{coxph object}

\item{ci}{Confidence interval. Enter as numeric 0-1. Default is \code{0.95}}

\item{clean_predictor_names}{If \code{TRUE} (default), predictor column of output is cleaned and output contains separate columns for predictor_var ("variable" in output) and level of predictor variable ("predictor" in output)}
}
\value{
Data frame with columns "outcome_var", "time_var", "predictor_var", "predictor", "hr", "hr_lower", "hr_upper", "p_wald", "n", "n_events", "univariate", "covariates"
}
\description{
Inspired by \code{broom::tidy}
}
