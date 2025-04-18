% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_axis.R
\name{log_transform}
\alias{log_transform}
\alias{lognp_transform}
\alias{log_nonzero_transform}
\title{log transformer}
\usage{
log_transform(base = 10)

lognp_transform(base = 10, n = 1)

log_nonzero_transform(base = 10)
}
\arguments{
\item{base}{Log base. Enter as length 1 numeric. Default is \code{10}}

\item{n}{Constant added to raw values prior to log transformation. Enter as length 1 numeric. Default is \code{1}}
}
\value{
Enter as input to \code{transform} argument of \verb{scale_(x|y)_continuous} function

Enter as input to \code{transform} argument of \verb{scale_(x|y)_continuous} function

Enter as input to \code{transform} argument of \verb{scale_(x|y)_continuous} function
}
\description{
log transformer

lognp transformer

lognp transformer
}
