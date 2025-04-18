% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_multi.R
\name{multislide_plot_list}
\alias{multislide_plot_list}
\title{List of multipanel plots for printing}
\usage{
multislide_plot_list(
  plot_list,
  plots_per_slide = 6,
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  align = "hv",
  axis = "trbl",
  ...
)
}
\arguments{
\item{plot_list}{List of ggplot objects}

\item{plots_per_slide}{Number of plots per slide. Default is \code{6}}

\item{nrow, ncol}{Number of rows and column of plots per slide}

\item{byrow}{If \code{TRUE} (default), plots added rowwise. If \code{FALSE}, plots added columnwise}

\item{align}{Alignment of plots. Options: \code{"hv"} (default. Align columns of plots by their x axis components and rows of plots by their y axis components), \code{"h"} (align a column of plots by their x axis components), \code{"v"} (align a row of plots by their y axis components). Enter as length 1 character vector}

\item{axis}{Axis to align plots. Options: combination of \code{"t"}, \code{"r"}, \code{"b"}, \code{"l"}. Enter as length 1 character vector with no spaces between letters}

\item{...}{Arguments passed to \code{plot_grid}}
}
\value{
List of multipanel plots that can be passed to \code{ppt()}
}
\description{
List of multipanel plots for printing
}
