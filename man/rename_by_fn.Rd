% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_manipulation.R
\name{rename_by_fn}
\alias{rename_by_fn}
\title{Rename variables in data frame, list, or vector, using a transformation function}
\usage{
rename_by_fn(.x, .fn = function(x) tolower(clean_names(x)), ...)
}
\arguments{
\item{.x}{Data frame, list, or vector}

\item{.fn}{Function to transform \code{names(.x)}}

\item{...}{Column names in \code{.x} to transform. Enter using tidyselect syntax. Default uses all columns in \code{.x}. Only relevant when \code{.x} is a data frame}
}
\value{
Same class as input
}
\description{
Rename variables in data frame, list, or vector, using a transformation function
}
