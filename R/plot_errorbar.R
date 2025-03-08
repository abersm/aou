#' Error bars
#'
#' @param summary_fn Function used to generated summary estimate for y. Default is `Mean`
#' @param error_fn Function used to determine error limits for y. Default is `SE`
#' @param error_limits Options: `"upper"` (default), `"lower"`, `"both"`
#' @param line_thickness Thickness of error bar lines in pts units. Enter as length 1 numeric. Default is `1`
#' @param width Width of error bar. Enter as length 1 numeric. Default is `0.1`
#' @param dodge Dodge width. Enter as length 1 numeric. Default is `0.9`
#' @param ... Arguments passed to `stat_summary`
#' @returns ggproto object. Add error bars to plot using `plot + errorbar()`
#' @export
errorbar <- function(
    summary_fn = Mean,
    error_fn = SE,
    error_limits = "upper",
    line_thickness = 1,
    width = 0.1,
    dodge = NULL,
    ...) {
  ggplot2::stat_summary(
    geom = "errorbar",
    fun.data = .prep_error_limits,
    width = width,
    linewidth = line_thickness,
    position = ggplot2::position_dodge(width = dodge),
    fun.args = list(
      summary_fn = summary_fn,
      error_fn = error_fn,
      error_limits = error_limits
    ),
    ...
  )
}

#' ggproto for `geom_errorupper`
#'
#' @returns ggproto object
#' @export
GeomErrorUpper <- ggplot2::ggproto(
  "GeomErrorUpper",
  ggplot2::Geom,
  default_aes = ggplot2::aes(colour = "black", size = 0.5, linetype = 1, width = 0.5, alpha = NA),
  required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),
  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, range_is_orthogonal = TRUE)
    params
  },
  extra_params = c("na.rm", "orientation"),
  setup_data = function(data, params) {
    # Check for flipped axes
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)

    # Error bar width
    if (is.null(data$width)) {
      width <- if (is.null(params$width)) {
        if (is.integer(data$x) || .has_zero_range(range(data$x, na.rm = TRUE))) {
          0.9
        } else {
          min(Diff(sort(unique(as.numeric(data$x)))))*0.9
        }
      } else {
        params$width
      }
    } else {
      width <- data$width
      data$width <- NULL
    }
    width <- width/2

    # Use width to calculate xmin and xmax
    z <- data$x
    data$xmin <- z - width
    data$xmin <- z + width
    ggplot2::flip_data(data, params$flipped_aes)
  },
  draw_panel = function(data, panel_params, coord, width = NULL, flipped_aes = FALSE) {
    # Check for flipped axes
    data <- ggplot2::flip_data(data, flipped_aes)

    # Data frame with draw features for error bars
    n <- Nrow(data)
    data <- vec_to_df(
      x = as.vector(rbind(data$xmin, data$xmax, NA, data$x, data$x)),
      y = as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$ymin)),
      colour = rep(data$colour, each = 5),
      alpha = rep(data$alpha, each = 5),
      size = rep(data$size, each = 5),
      linetype = rep(data$linetype, each = 5),
      group = rep(seq_len(n), each = 5),
      row.names = seq_len(n*5)
    )
    data <- ggplot2::flip_data(data, flipped_aes)
    ggplot2::GeomPath$draw_panel(data, panel_params, coord)
  },
  draw_key = function(...) grid::nullGrob()
)

#' Upper portion of error bar
#'
#' @inheritParams ggplot2::geom_errorbar
#' @returns ggproto object
#' @export
geom_errorupper <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    na.rm = TRUE,
    orientation = NA,
    show.legend = NA,
    inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomErrorUpper,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    ))
}

# Helpers -----------------------------------------------------------------

#' Mean and standard deviation bar for plots
#'
#' @param x Numeric vector
#' @param error_limits Limits for error bar on plots. Options: `"both"` (default, range: mean-sd to mean+sd), `"upper"` (range: mean to mean+sd), or `"lower" `(range: mean-sd to mean)
#' @returns Data frame with columns "y" (center), "ymin" (lower), "ymax" (upper)
#' @noRd
mean_sd <- function(x, error_limits = "both") {
  .prep_error_limits(x, summary_fn = Mean, error_fn = SD, error_limits = error_limits)
}

#' Mean and standard error bar for plots
#'
#' @param x Numeric vector
#' @param error_limits Limits for error bar on plots. Options: `"both"` (default, range: mean-se to mean+se), `"upper"` (range: mean to mean+se), or `"lower"` (range: mean-se to mean)
#' @returns Data frame with columns "y" (center), "ymin" (lower), "ymax" (upper)
#' @noRd
mean_se <- function(x, error_limits = "both") {
  .prep_error_limits(x, summary_fn = Mean, error_fn = SE, error_limits = error_limits)
}

#' Mean and CI bar for plots
#'
#' @param x Numeric vector
#' @param error_limits Limits for CI on plots. Options: `"both"` (default), `"upper"`, or `"lower"`
#' @param ci Confidence interval. Default is `0.95`
#' @returns Data frame with columns "y" (center), "ymin" (lower), "ymax" (upper)
#' @noRd
mean_ci <- function(x, error_limits = "both", ci = 0.95) {
  .prep_error_limits(x, summary_fn = Mean, error_fn = CI, error_limits = error_limits, ci = ci)
}

#' Median and IQR for plots
#'
#' @param x Numeric vector
#' @returns Data frame with columns "y" (center), "ymin" (lower), "ymax" (upper)
#' @noRd
median_iqr <- function(x) {
  z <- Quantile(x, probs = c(0.25, 0.5, 0.75))
  vec_to_df(y = z[2L], ymin = z[1L], ymax = z[3L])
}

#' Geometric mean and error bar for plots
#'
#' @param x Numeric vector
#' @param error_fn Statistical function used to make calculations. Options: `mean_ci` (default), `mean_se`, `mean_sd`, `median_iqr`
#' @param error_limits Limits for error bar on plots. Options: `"both"` (default), `"upper"`, or `"lower"`
#' @param ... Arguments passed to `error_fn`
#' @returns Data frame with columns "y" (center), "ymin" (lower), "ymax" (upper)
#' @noRd
geo_mean_plot <- function(x, error_fn = mean_ci, error_limits = "both", ...) {
  error_fn <- match.fun(error_fn)
  x_log <- log1p(x[!is.na(x)])
  gm_range <- error_fn(x_log, error_limits = error_limits, ...)
  expm1(gm_range)
}

#' Helper function to obtain summary statistic and limits of error bar
#'
#' @param x Numeric vector
#' @param summary_fn Function used to calculate midpoint of error bar. Must take a numeric vector as input and return a length 1 numeric as output
#' @param error_fn Function to generate upper and lower limits of error bar. Must take a numeric vector as input and return a length 1 numeric as output
#' @param error_limits Limits for error bar on plots. Options: `"upper"`, `"lower"`, `"both"`
#' @param ... Not used
#' @returns Data frame with columns "y" (center), "ymin" (lower), "ymax" (upper)
#' @noRd
.prep_error_limits <- function(x, summary_fn, error_fn, error_limits, ...) {
  m <- summary_fn(x)
  s <- error_fn(x)
  switch(error_limits,
         both = vec_to_df(y = m, ymin = m - s, ymax = m + s),
         upper = vec_to_df(y = m, ymin = m, ymax = m + s),
         lower = vec_to_df(y = m, ymin = m - s, ymax = m),
         vec_to_df(y = m, ymin = m - s, ymax = m + s))
}
