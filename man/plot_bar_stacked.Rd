% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bar_stacked.R
\name{plot_bar_stacked}
\alias{plot_bar_stacked}
\title{Stacked bar plot with error bars}
\usage{
plot_bar_stacked(
  df,
  formula = NULL,
  grouping_var = NULL,
  grouping_var_order = NULL,
  rev_grouping_var_order = FALSE,
  facet_var = NULL,
  x = NULL,
  y = NULL,
  x_order = NULL,
  rev_x_order = FALSE,
  facet_var_order = NULL,
  rev_facet_var_order = FALSE,
  colors = c("#0072B5", "#BC3C29", "#868686", "#2A2D34", "#CC79A7", "#56B4E9", "#E69F00",
    "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58",
    "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  bar_colors = colors,
  alpha = 1,
  bar_alpha = alpha,
  bar_width = 0.6,
  bar_border_thickness = 0.75,
  bar_border_colors = "black",
  show_legend = FALSE,
  show_errorbar = TRUE,
  summary_fn = Mean,
  error_fn = SE,
  incl_lower_errorbar = FALSE,
  errorbar_width_multiplier = 0.5,
  errorbar_colors = "black",
  errorbar_thickness = bar_border_thickness,
  legend_title = "",
  y_title = waiver(),
  y_axis_title = y_title,
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  x_title = NULL,
  x_axis_title = x_title,
  x_axis_labels = NULL,
  x_angle = 45,
  x_axis_label_angle = x_angle,
  expand_x = waiver(),
  plot_title = NULL,
  theme_fn = theme_plain,
  nrow = NULL,
  ncol = NULL,
  ...
)
}
\arguments{
\item{df}{Data frame in long format containing continuous variable, categorical or integer variable, and grouping variable}

\item{formula}{y ~ x format}

\item{grouping_var}{Variable to group by. Enter as quoted or unquoted variable name}

\item{grouping_var_order}{Order for levels of \code{grouping_var} along x axis. Enter as character vector}

\item{rev_grouping_var_order}{If \code{TRUE}, levels of \code{grouping_var} are reversed}

\item{facet_var}{Column in \code{df} to generate plot facets. Enter as string}

\item{x, y}{Variables for x  and y axis, respectively. Enter as quoted or unquoted variable names}

\item{x_order}{Order of x values. Enter as character vector. Default is \code{NULL}}

\item{rev_x_order}{If \code{FALSE} (default), order of x axis variable is not reversed}

\item{facet_var_order}{Order for levels of \code{facet_var}. Enter as character vector}

\item{rev_facet_var_order}{If \code{TRUE}, levels of \code{facet_var} are reversed}

\item{colors}{Alias for \code{point_colors}}

\item{bar_colors}{Bar color. Enter as character vector of color names or hexadecimal codes}

\item{alpha}{Alias for \code{point_alpha}}

\item{bar_alpha}{Alpha value for bar fill. Enter as length 1 numeric. Default is \code{0}}

\item{bar_border_thickness}{Thickness of line surrounding bars in pts. Enter as length 1 numeric. Default is \code{0.75}}

\item{bar_border_colors}{Bar border color. Default is \code{"black"}}

\item{show_legend}{If \code{FALSE} (default), legend is not displayed}

\item{show_errorbar}{If \code{TRUE}, error bars are displayed}

\item{summary_fn}{Function used to determine midpoint of error bars. Default is \code{Mean}}

\item{error_fn}{Function used to determine ends of error bars. Default is \code{SE}}

\item{incl_lower_errorbar}{If \code{TRUE} and \code{show_errorbar = TRUE}, lower error bar is displayed. Default is \code{FALSE}}

\item{errorbar_width_multiplier}{Multiplied by \code{bar_width} to determine width of error bars. Enter as length 1 numeric. Default is \code{0.5}}

\item{errorbar_colors}{Color of error bars. Default is \code{"black"}}

\item{errorbar_thickness}{Line thickness for error bars. Default uses value for \code{bar_thickness} (unless \code{bar_thickness} is 0)}

\item{legend_title}{Legend title. Default is blank}

\item{y_title}{Alias for \code{y_axis_title}}

\item{y_axis_title}{Enter as quoted string}

\item{y_axis_breaks}{Numeric vector or function specifying location of ticks along y axis}

\item{y_axis_labels}{Vector or function specifying y axis tick labels}

\item{x_title}{Alias for \code{x_axis_title}}

\item{x_axis_title}{Enter as quoted string}

\item{x_axis_labels}{Vector or function specifying x axis tick labels}

\item{x_axis_label_angle, x_angle}{Angle for x axis labels. Default is \code{45}. Enter as length 1 integer vector}

\item{expand_x}{Argument entered for expand argument of \code{scale_x_continuous}. Default is \code{waiver()}}

\item{plot_title}{Text to be used as main plot title. Enter as quoted string. Default is \code{""} (i.e. no title)}

\item{theme_fn}{Theme function. Default is \code{theme_custom}}

\item{nrow, ncol}{Number of facet rows and columns respectively. Enter as length 1 integer vectors}

\item{...}{Arguments passed to \code{theme_fn()}}
}
\value{
ggplot
}
\description{
Only use when displaying \% of total on y axis and groups sum to 100\%
}
