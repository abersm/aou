% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_survival.R
\name{plot_km}
\alias{plot_km}
\title{Kaplan-Meier plot}
\usage{
plot_km(
  df,
  predictor_var,
  time_var = "time",
  outcome_var = "death",
  show_censor = FALSE,
  show_error = FALSE,
  colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00",
    "#20854E", "#FFD500", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58",
    "#DA3978", "#61D04F", "#FBA27D", "#00C5CD"),
  line_thickness = 1,
  linetype = 1,
  x_axis_title = "Weeks since onset",
  y_axis_title = if (y_axis_format == "percent") "Survival (\%)" else "Survival",
  show_p = TRUE,
  show_hr = TRUE,
  hr_prefix = "HR ",
  hr_anno_x = 0,
  hr_anno_y = 0.12,
  plot_title = NULL,
  x_axis_limits = NULL,
  y_axis_limits = c(0, 1),
  break_time_by = 1,
  x_axis_breaks = NULL,
  base_size = 16,
  expand_y = 0,
  hr_anno_size = base_size/4.5,
  y_axis_breaks = seq.int(from = 0, to = 1, by = 0.25),
  legend = "none",
  y_axis_format = "percent",
  cdf = FALSE,
  show_curve_labels = TRUE,
  labels = NULL,
  show_error_labels = FALSE,
  error_colors = colors,
  error_alpha = 0.2,
  show_legend = FALSE,
  expand_time = TRUE,
  ...
)
}
\arguments{
\item{df}{Data frame. May contain missing values}

\item{predictor_var}{Variable(s) to use as covariates in model. Enter as comma separated list of quoted variables names wrapped in \code{c()}}

\item{time_var}{Variable containing follow up time. Enter as quoted variable name. Default is \code{"time"}}

\item{outcome_var}{Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is \code{"death"}}

\item{show_censor}{If \code{FALSE} (default), censored events not shown on plot. If \code{TRUE}, vertical line appears on plot at censored events}

\item{show_error}{If \code{FALSE} (default), error estimates of survival probability not displayed on plot. If \code{TRUE}, error estimate for survival probability displayed}

\item{colors}{Colors for groups. Enter as quoted color names or quoted hexadecimal codes. Default is blue and red}

\item{line_thickness}{Line thickness for KM curves. Default is \code{1}}

\item{linetype}{Linetype for KM curves. Default is \code{1} (\code{"solid"}). Enter as numeric or quoted linetype}

\item{x_axis_title}{Title for x axis. Enter as quoted text. Default is \code{"Weeks since onset"}}

\item{y_axis_title}{Title for y axis. Enter as quoted text. Default is \code{"Survival probability"}}

\item{show_p}{If \code{TRUE} (default), p value displayed on plot. If \code{FALSE}, p value not displayed on plot}

\item{show_hr}{If \code{TRUE} (default), HR annotation displayed on plot. If \code{FALSE}, not displayed on plot}

\item{hr_prefix}{Prefix for HR annotation. Default is \code{"HR "}. Enter as quoted text}

\item{hr_anno_x}{Location of HR annotation along x axis. Default is \code{0}. Enter as numeric}

\item{hr_anno_y}{Location of HR annotation along y axis. Default is \code{0.1}. Enter as numeric}

\item{plot_title}{Title of plot. Enter as quoted text. Default is \code{NULL}}

\item{x_axis_limits}{Limits for x axis. Enter as numeric vector of length 2. Default from 0 to max time}

\item{y_axis_limits}{Limits for x axis. Enter as numeric vector of length 2. Default is \code{c(0, 1)}}

\item{break_time_by}{Interval for breaks along x axis. Enter as numeric. Default is \code{1}}

\item{x_axis_breaks}{Breaks for x axis. Enter as numeric vector}

\item{base_size}{Size of text passed to theme_custom. Default is \code{16}}

\item{expand_y}{Expansion applied to lower end of y axis. Enter as numeric. Default is \code{0}}

\item{hr_anno_size}{Size of text for HR annotation. Enter as numeric. Default is \code{base_size/4.5}}

\item{y_axis_breaks}{Breaks along y axis. Default \code{seq.int(0, 1, by = 0.25)}. Enter as numeric vector}

\item{legend}{Location of legend. Default shows no legend. Enter as quoted position}

\item{y_axis_format}{Options: \code{"frequency"} (default, proportion used to create y axis tick labels) or \code{"percent"} (\% used to create y axis tick labels). Enter as quoted text}

\item{cdf}{If \code{FALSE} (default) KM curve plotted. If \code{TRUE}, cumulative distribution function (incidence) plotted}

\item{show_curve_labels}{If \code{TRUE} (default), KM curve labels are displayed at rightmost aspect of plot}

\item{labels}{Labels for KM curves. Enter as character vector}

\item{show_error_labels}{If \code{FALSE} (default), shaded error regions are not displayed around curves}

\item{error_colors}{Colors used for error region around survival curves. Enter as quoted color names or quoted hexadecimal codes}

\item{error_alpha}{Alpha for shaded error region. Default is \code{0.2}. Enter as numeric}

\item{show_legend}{If \code{FALSE} (default), legend is not shown}

\item{expand_time}{If \code{TRUE} (default), output expanded to include all times within \code{x_axis_limits}}

\item{...}{Arguments passed to \code{theme_custom()}}
}
\description{
Kaplan-Meier plot
}
