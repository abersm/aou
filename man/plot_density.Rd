% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_density.R
\name{plot_density}
\alias{plot_density}
\title{Plot distribution of values using density function}
\usage{
plot_density(
  df,
  col = NULL,
  grouping_var = NULL,
  colors = c("#333333", "#00A1D5", "#D75725", "#6761A8", "#009872", "#EFC000", "#003C67",
    "#CCCCCC", "#FFFFFF", "#8B2323"),
  alpha = NULL,
  line_color = colors,
  line_thickness = 1,
  show_data = FALSE,
  symbol = "none",
  breaks = waiver(),
  x_axis_breaks = breaks,
  bounds = c(-Inf, Inf),
  x_min = NULL,
  x_max = NULL,
  x_title = NULL,
  x_axis_title = x_title,
  axis_labels = axis_label_numeric,
  show_legend = NULL,
  scaling_fn = identity,
  expand_y = 0.02,
  plot_title = NULL,
  ...
)
}
\arguments{
\item{df}{Data frame or numeric vector}

\item{col}{Continuous variable. Enter as quoted or unquoted variable name}

\item{grouping_var}{Grouping variable. Enter as quoted or unquoted variable name}

\item{colors}{Colors used to fill density plot. Default is blue and red}

\item{alpha}{Alpha for density color. Default is \code{0.6}}

\item{line_color}{Line color. Default is \code{black}}

\item{line_thickness}{Line thickness. Default is \code{1}}

\item{show_data}{If \code{TRUE}, a symbol is plotted for each observation}

\item{symbol}{Type of symbol used to show data. Options: \code{"none"} (default), \code{"point"}, \code{"rug"}}

\item{breaks}{Alias for x_axis_breaks}

\item{x_axis_breaks}{x axis breaks. Enter as numeric vector}

\item{bounds}{Bounds for density estimate. Enter as length 2 numeric. Default is \code{c(-Inf, Inf)}}

\item{x_min, x_max}{Minimum and maximum x axis values}

\item{x_title}{Alias for \code{x_axis_title}}

\item{x_axis_title}{x axis title}

\item{axis_labels}{Function used to generate axis labels}

\item{show_legend}{If \code{FALSE} (default), legend is not displayed}

\item{scaling_fn}{Scaling function used to transform variable. Default performs no transformation}

\item{expand_y}{Expansion to add to y axis. Default is \code{0.2}}

\item{plot_title}{Title for plot. Default is \code{NULL}}

\item{...}{Arguments passed to \code{theme_custom()}}
}
\value{
ggplot object
}
\description{
Plot distribution of values using density function
}
