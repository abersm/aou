% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_spaghetti.R
\name{plot_spaghetti}
\alias{plot_spaghetti}
\title{Spaghetti plot}
\usage{
plot_spaghetti(
  df,
  formula,
  id,
  line_color_var = NULL,
  line_color_categorical = TRUE,
  colors = c("#333333", "#0072B5", "#BC3C29", "#999999", "#CC79A7", "#56B4E9", "#E69F00",
    "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58",
    "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  line_color = colors,
  line_thickness = 0.7,
  alpha = 0.7,
  line_alpha = alpha,
  y_scale = "regular",
  n_breaks = 3,
  y_axis_breaks = NULL,
  y_min = NULL,
  y_max = NULL,
  expand_y = 0.1,
  y_axis_labels = NULL,
  y_axis_title = waiver(),
  x_axis_title = waiver(),
  x_axis_breaks = waiver(),
  n_x_axis_breaks = 6,
  expand_x = 0.05,
  plot_title = NULL,
  show_legend = FALSE,
  censor_fn = rescale_none,
  aspect_ratio = 0.75,
  ...
)
}
\arguments{
\item{df}{Data frame in long format with columns for id, y (continuous variable), x (continuous variable)}

\item{formula}{y ~ x format}

\item{id}{Variable to identify subjects. Enter as quoted variable name}

\item{line_color_var}{Variable used to determine line color. Enter as quoted variable name}

\item{line_color_categorical}{If \code{TRUE} (default), line color variable determined by \code{line_color_var} is set to factor (which uses a categorical palette). If \code{FALSE}, \code{line_color_var} will be converted to a continuous variable}

\item{colors}{Alias for \code{line_color}}

\item{line_color}{Line colors. Enter as character vector of hexadecimal codes and/or quoted color names}

\item{line_thickness}{Thickness of line in mm Default is \code{0.7}}

\item{alpha}{Alias for \code{line_alpha}}

\item{line_alpha}{Transparency of lines. Default is \code{0.7}}

\item{y_scale}{Scaling for y-axis. Options: \code{"regular"} (default), \code{"log"}, \code{"scientific"}}

\item{n_breaks}{Number of tick marks for y axis. Default is \code{3}}

\item{y_min}{Minimum value to use for y axis Default is \code{NULL}}

\item{y_max}{Maximum value to use for y axis Default is \code{NULL}}

\item{expand_y}{Expansion of y axis around y = 0. Default is \code{0.1}}

\item{y_axis_labels}{Vector or function specifying y axis tick labels}

\item{x_axis_title, y_axis_title}{Titles for x and y axis, respectively. Default is \code{NULL}}

\item{x_axis_breaks, y_axis_breaks}{Numeric vector or function specifying location of ticks along x and y axis, respectively}

\item{n_x_axis_breaks}{Number of x axis breaks. Only relevant when \code{x_axis_breaks = waiver()}}

\item{expand_x}{Expansion of x axis at ends. Enter as length 1 or 2 (lower, upper) numeric vector. Default is \code{0.05}}

\item{plot_title}{Text to be used as main plot title. Enter as quoted string}

\item{show_legend}{If \code{FALSE} (default), legend is not displayed. If \code{TRUE}, legend for lines is shown}

\item{censor_fn}{Function used to transform data outside y axis limits. Default is \code{rescale_none.} Alternative: \code{scales::censor}}

\item{aspect_ratio}{Aspect ratio. Passed to \code{theme_custom()}. Enter as length 1 numeric. Default is \code{0.75}}

\item{...}{Arguments passed to \code{theme_custom()}}
}
\description{
Spaghetti plot
}
