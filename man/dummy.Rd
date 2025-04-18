% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_manipulation.R
\name{dummy}
\alias{dummy}
\title{Add binary variables for each level of grouping variable}
\usage{
dummy(df, ..., .sep = "_")
}
\arguments{
\item{df}{Data frame}

\item{...}{Categorical columns to create binary variables. Enter using tidyselect syntax. Default uses all columns in \code{df}}

\item{.sep}{Separator between column name and value for categorical variables. Default is \code{"_"}}
}
\value{
Data frame with new binary variables coded as 0 (feature absent) or 1 (feature present).
}
\description{
Add binary variables for each level of grouping variable
}
