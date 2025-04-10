% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_axis.R
\name{axis_label_log}
\alias{axis_label_log}
\title{Create axis labels with "base^x" format (i.e., without "multipler x" prior to base^x)}
\usage{
axis_label_log(base = 10)
}
\arguments{
\item{base}{Logarithm base. Enter as length 1 numeric. Default is \code{10}}
}
\value{
Function that returns an expression. Enter as \verb{scale_(x|y)_continuous(labels = axis_label_log(10))}
}
\description{
Functionality from \code{scales::label_log}
}
