% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_histogram.R
\name{plot_histogram}
\alias{plot_histogram}
\title{Histogram}
\usage{
plot_histogram(
  df,
  col = NULL,
  n_bins = 20,
  bar_color_var = NULL,
  colors = c("#328EC3", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00",
    "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58",
    "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  bar_colors = colors,
  bar_border_color = "black",
  rev_x_order = FALSE,
  x_axis_breaks = NULL,
  x_axis_labels = waiver(),
  x_axis_title = NULL,
  expand_x = 0.1,
  x_axis_label_angle = 45,
  width = 0.7,
  bar_width = width,
  y_axis_title = "n",
  n_breaks_x = 4L,
  n_breaks_y = 4L,
  bar_alpha = 1,
  show_legend = FALSE,
  legend_title = NULL,
  show_n = TRUE,
  n_anno_font_size = 12,
  n_anno_font_color = "black",
  n_anno_x = NULL,
  n_anno_x_nudge = 0,
  n_anno_y_nudge = 0,
  n_prefix = "N = ",
  y_axis_title_angle = 0,
  y_axis_title_margin = 10,
  ...
)
}
\arguments{
\item{df}{Data frame or vector}

\item{col}{Continuous variable. Enter as quoted or unquoted variable name}

\item{n_bins}{Number of bins for x axis. Default is \code{20}}

\item{bar_color_var}{Variable in df used to color bars. Enter as quoted or unquoted variable name}

\item{colors}{Color of bars. Default is \code{"#328EC3"}}

\item{bar_colors}{Alias for colors}

\item{bar_border_color}{Color of bar borders. Default is \code{"black"}}

\item{rev_x_order}{If \code{FALSE} (default), order of x axis variable is not reversed}

\item{x_axis_breaks, x_axis_labels, x_axis_title}{x axis breaks, labels, and title}

\item{expand_x}{Expansion to add to x axis. Default is \code{0.6}}

\item{x_axis_label_angle}{Angle of x axis labels. Default is \code{45} degrees}

\item{width}{Width of bars. Default is \code{0.7}}

\item{bar_width}{Alias for width}

\item{y_axis_title}{Title for y axis. Default is \code{"n"}}

\item{n_breaks_x}{Number of breaks desired for x axis. Default is \code{4L}}

\item{n_breaks_y}{Number of breaks desired for y axis. Default is \code{4L}}

\item{bar_alpha}{Alpha for bars. Default is \code{1}}

\item{show_legend}{If \code{FALSE} (default), legend is not displayed}

\item{legend_title}{Title of legend. Default is blank}

\item{show_n}{If \code{TRUE} (default), total sample size is displayed on plot}

\item{n_anno_font_size}{Font size for sample size annotation in pts. Default is \code{12}}

\item{n_anno_font_color}{Font color for sample size annotation. Default is \code{"black"}}

\item{n_anno_x}{Position of sample size annotation along x axis. Options: \code{NULL} (default), \code{"right"}, \code{"left"}}

\item{n_anno_x_nudge}{Amount to nudge sample size annotation along x axis. Default is \code{0}}

\item{n_anno_y_nudge}{Amount to nudge sample size annotation along y axis. Default is \code{0}}

\item{n_prefix}{Prefix for sample size annotation. Default is \code{"N = "}}

\item{y_axis_title_angle}{Angle for y axis title. Default is \code{0}}

\item{y_axis_title_margin}{Margin between y axis title and labels. Default is \code{10}}

\item{...}{Arguments passed to \code{theme_custom()}}
}
\description{
Histogram
}
