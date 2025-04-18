% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_subset.R
\name{remove_na}
\alias{remove_na}
\title{Remove rows with missing values}
\usage{
remove_na(df, cols = NULL)
}
\arguments{
\item{df}{Data frame}

\item{cols}{Columns to search for missing values. Enter as character vector. If \code{NULL} (default), all columns are used}
}
\value{
Data frame
}
\description{
Similar to \code{tidyr::drop_na}
}
