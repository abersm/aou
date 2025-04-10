% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_incidence.R
\name{plot_incidence}
\alias{plot_incidence}
\title{Frequency plot showing incidence over time}
\usage{
plot_incidence(
  df,
  date = NULL,
  bar_color_var = NULL,
  colors = c("#14B3EC", "#A284BA"),
  bar_colors = colors,
  alpha = 0.8,
  bar_border_color = "#000000",
  bar_border_thickness = 0.5,
  month_format = c("number", "name", "abbreviation"),
  leading_zero = FALSE,
  full_year = FALSE,
  date_sep = "-",
  n_bins = 20,
  x_axis_title = NULL,
  expand_x = if (n_bins > 10) 0.1 else 0.15,
  x_angle = 45,
  y_axis_title = "n",
  show_legend = FALSE,
  annotation_prefix = "N = ",
  ...
)
}
\arguments{
\item{df}{Data frame or vector of dates}

\item{date}{Column containing dates if df is a data frame. Enter as quoted or unquoted variable name}

\item{bar_color_var}{Variable used to color histogram bars. Enter as string}

\item{colors, bar_colors}{Color of bars. Default is \code{"#328EC3"}}

\item{alpha}{Histogram bar opacity. Default is \code{0.8}}

\item{bar_border_color}{Color of lines around histogram bars. Default is black}

\item{bar_border_thickness}{Thickness of lines around histogram bars. Default is \code{0.5}}

\item{month_format}{Options: \code{"number"} (default), \code{"name"}, \code{"abbreviation"}}

\item{leading_zero}{If \code{FALSE} (default), leading 0s are removed from month and day if 1-9}

\item{full_year}{If \code{FALSE} (default), last 2 digits are used. If \code{TRUE}, year is displayed using 4 digits}

\item{date_sep}{Separator between month, day, and year. Default is \code{"-"}}

\item{n_bins}{Number of bins for x axis. Default is \code{20}}

\item{x_axis_title}{Title for x axis}

\item{expand_x}{Expansion multiplier for x axis. Default is \code{0.1} if n_bins > 10 and \code{0.15} if n_bins <= 10. Only relevant if x is treated as numeric}

\item{x_angle}{Angle for x axis labels. Default is \code{45}}

\item{y_axis_title}{Title for y axis. Default is \code{"n"}}

\item{show_legend}{If \code{TRUE}, legend is displayed}

\item{...}{Arguments passed to \code{theme_custom}}
}
\value{
ggplot
}
\description{
Frequency plot showing incidence over time
}
