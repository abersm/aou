% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{col_prop}
\alias{col_prop}
\alias{row_prop}
\alias{col_perc}
\alias{row_perc}
\title{Convert counts in contingency table to column proportions}
\usage{
col_prop(x, ..., incl_na = FALSE)

row_prop(x, ..., incl_na = FALSE)

col_perc(x, ..., incl_na = FALSE, digits = 1)

row_perc(x, ..., incl_na = FALSE, digits = 1)
}
\arguments{
\item{x}{Contingency table or data frame}

\item{...}{If \code{x} is a data frame, enter column names using tidyselect syntax}

\item{incl_na}{If \code{x} is a data frame, \code{incl_na} determines whether missing values form distinct groups}

\item{digits}{Number of digits to include after decimal. Default is \code{1}}
}
\value{
Table containing column proportions (i.e., column proportions sum to 1)

Table containing row proportions (i.e., row proportions sum to 1)

Table containing column percentages (i.e., column percentages sum to 100)

Table containing row percentages (i.e., row percentages sum to 100)
}
\description{
Convert counts in contingency table to column proportions

Convert counts in contingency table to row proportions

Convert counts in contingency table to column percentages

Convert counts in contingency table to row percentages
}
