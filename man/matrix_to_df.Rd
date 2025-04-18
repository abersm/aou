% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_coerce.R
\name{matrix_to_df}
\alias{matrix_to_df}
\title{Convert a matrix to a data frame}
\usage{
matrix_to_df(
  x,
  rownames_to_col = FALSE,
  colname_rownames = "rowname",
  prefix = "V"
)
}
\arguments{
\item{x}{Matrix}

\item{rownames_to_col}{If \code{TRUE}, rownames of \code{x} are used to create a new column in output data frame. Default is \code{FALSE}}

\item{colname_rownames}{Name of column in output data frame containing rownames of \code{x}. Only relevant when \code{rownames_to_col = TRUE}. Default is \code{"rowname"}}

\item{prefix}{Prefix for newly created column names. Default is \code{"V"}}
}
\value{
Data frame
}
\description{
Convert a matrix to a data frame
}
