#' Spaghetti plot
#'
#' @param df Data frame in long format with columns for id, y (continuous variable), x (continuous variable)
#' @param formula y ~ x format
#' @param id Variable to identify subjects. Enter as quoted variable name
#' @param colors Alias for `line_color`
#' @param line_color Line colors. Enter as character vector of hexadecimal codes and/or quoted color names
#' @param line_color_var Variable used to determine line color. Enter as quoted variable name
#' @param line_color_categorical If `TRUE` (default), line color variable determined by `line_color_var` is set to factor (which uses a categorical palette). If `FALSE`, `line_color_var` will be converted to a continuous variable
#' @param line_thickness Thickness of line in mm Default is `0.7`
#' @param line_alpha Transparency of lines. Default is `0.7`
#' @param alpha Alias for `line_alpha`
#' @param y_scale Scaling for y-axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param n_breaks Number of tick marks for y axis. Default is `3`
#' @param x_axis_breaks,y_axis_breaks Numeric vector or function specifying location of ticks along x and y axis, respectively
#' @param y_min Minimum value to use for y axis Default is `NULL`
#' @param y_max Maximum value to use for y axis Default is `NULL`
#' @param expand_y Expansion of y axis around y = 0. Default is `0.1`
#' @param y_axis_labels Vector or function specifying y axis tick labels
#' @param x_axis_title,y_axis_title Titles for x and y axis, respectively. Default is `NULL`
#' @param n_x_axis_breaks Number of x axis breaks. Only relevant when `x_axis_breaks = waiver()`
#' @param expand_x Expansion of x axis at ends. Enter as length 1 or 2 (lower, upper) numeric vector. Default is `0.05`
#' @param plot_title Text to be used as main plot title. Enter as quoted string
#' @param show_legend If `FALSE` (default), legend is not displayed. If `TRUE`, legend for lines is shown
#' @param censor_fn Function used to transform data outside y axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param aspect_ratio Aspect ratio. Passed to `theme_custom()`. Enter as length 1 numeric. Default is `0.75`
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_spaghetti <- function(
  df,
  formula,
  id,
  line_color_var = NULL,
  line_color_categorical = TRUE,
  colors = c("#333333", "#0072B5", "#BC3C29", "#999999", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
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
  ...) {
  # Data frame
  df <- .create_plot_df(df, formula)

  vars <- formula2vars(formula = formula)
  x <- vars$x
  y <- vars$y

  # Line colors
  if (is.null(line_color_var)) {
    df$line_color_var <- factor("a")
  } else {
    df$line_color_var <- df[[line_color_var]]
    if (line_color_categorical) {
      line_color_levels <- create_levels(df$line_color_var)
      df$line_color_var <- factor(df$line_color_var, levels = line_color_levels)
    } else {
      if (all(can_be_numeric(df$line_color_var))) {
        df$line_color_var <- as.numeric(df$line_color_var)
      } else {
        line_color_levels <- create_levels(df$line_color_var)
        df$line_color_var <- factor(df$line_color_var, levels = line_color_levels)
      }
    }
  }
  n_line_colors <- n_unique(df$line_color_var, na.rm = FALSE)
  if (length(line_color) < n_line_colors) {
    if (is.numeric(df$line_color_var)) {
      low <- grDevices::col2rgb("#CCCCCC")
      high <- grDevices::col2rgb(line_color[[1L]])
      red <- seq.int(low[1L, 1L], high[1L, 1L], length.out = n_line_colors)/255
      green <- seq.int(low[3L, 1L], high[3L, 1L], length.out = n_line_colors)/255
      blue <- seq.int(low[2L, 1L], high[2L, 1L], length.out = n_line_colors)/255
      line_color <- grDevices::rgb(red, blue, green)
    } else {
      line_color <- rep(line_color, length.out = n_line_colors)
    }
  }

  # Axes
  if (is.null(y_axis_breaks)) {
    y_max <- y_max %||% Max(df[[y]])
    y_min <- y_min %||% Min(df[[y]])
  } else {
    y_max <- Max(y_axis_breaks)
    y_min <- Min(y_axis_breaks)
  }
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  expand_x <- rep(expand_x, length.out = 2)
  x_axis_breaks <- x_axis_breaks %W% breaks_extended(Range(df[[x]]), n = n_x_axis_breaks)

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(.data[[x]], .data[[y]])) +
    ggplot2::geom_line(ggplot2::aes(group = .data[[id]], color = line_color_var), linewidth = line_thickness, alpha = line_alpha, show.legend = show_legend) +
    ggplot2::scale_color_manual(values = line_color, name = NULL) +
    ggplot2::scale_x_continuous(name = x_axis_title, expand = c(expand_x[1L], 0, expand_x[2L], 0), breaks = x_axis_breaks, n.breaks = n_x_axis_breaks) +
    scale_continuous(axis = "y", limits = c(y_min, y_max), title = y_axis_title, expand_lower = expand_y, scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, censor_fn = censor_fn) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_custom(aspect_ratio = aspect_ratio, ...)
  p
}

#' Faceted spaghetti plot by slope
#'
#' @inheritParams plot_spaghetti
#' @param y Continuous variable used for y axis. Enter as quoted or unquoted variable name
#' @param color_by Variable used to color lines. Enter as quoted or unquoted variable name
#' @param time Index variable containing time. Enter as quoted or unquoted variable name. Only required if `date_t0` and `date_t1` are not entered
#' @param date_t0 Initial date (t = 0) used to calculate time. Enter as quoted or unquoted variable name. If provided, must also provide `date_t1`
#' @param date_t1 Final date used to calculate time. Enter as quoted or unquoted variable name. If provided, must also provide `date_t0`
#' @param units Units for time variable. Enter as quoted or unquoted unit. Default is `"days"`
#' @param min_time,max_time Minimum and maximum values for time. Enter as numeric
#' @param n_strata Number of strata. Enter as numeric. Default is `4`
#' @param min_n_obs Minimum number of observations in each strata. Enter as numeric. Default is `1`
#' @param show_facet_labels If `TRUE` (default), facet labels are displayed
#' @returns Spaghetti plot facetted by slope
#' @export
plot_spag_facet_strata <- function(
  df,
  y,
  color_by = NULL,
  time = NULL,
  date_t0 = NULL,
  date_t1 = NULL,
  id = "id",
  units = "days",
  n_strata = 4,
  min_time = -Inf,
  max_time = Inf,
  colors = c("#005E7A", "#EE1E7A", "#E69F00", "#00C5CD", "#7D5FA7", "#999999", "#333333", "#20854E", "#F0E442", "#97572B", "#FBA27D", "#F7F7F7"),
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
  censor_fn = rescale_none,
  min_n_obs = 1,
  show_legend = FALSE,
  plot_title = NULL,
  aspect_ratio = 0.75,
  theme_fn = theme_custom,
  show_facet_labels = TRUE,
  ...) {
  # Inputs
  pkg_required("brolgar")
  plot_fn <- "plot_spag_facet_strata"

  # Variables
  y <- get_input(y)
  color_by <- get_input(color_by)
  date_t0 <- get_input(date_t0)
  date_t1 <- get_input(date_t1)
  time <- get_input(time)
  id <- get_input(id)
  if (is.null(color_by)) {
    df$color_by <- "a"
    color_by <- "color_by"
  }

  # tsibble
  df <- remove_na(df, c(y, id, time, date_t0, date_t1))
  df_ts <- tsibble(df, id = id, time = time, t0 = date_t0, t1 = date_t1, min_time = min_time, max_time = max_time, units = units, new_time_col = "TIME")
  f <- create_formula(y, "TIME")
  # Steps below from brolgar::key_slope
  key_vars <- names(attr(df_ts, "key", exact = TRUE))
  n <- length(key_vars)
  key_vars <- key_vars[-n]
  otherwise <- matrix(NA_real_, ncol = n)
  slope_per_id <- do.call(rbind, lapply(split_df(df_ts, key_vars), function(x) {
    z <- tryCatch(t.default(Lm(x, f)$coefficients), error = function(e) otherwise)
    cbind(x[1L, key_vars, drop = FALSE], z, Nrow(x))
  }))
  #slope_cols <- if (length(slope_cols <- all.vars(f)[-1L]) == 1L) "SLOPE" else paste0("SLOPE", "_", slope_cols)
  names(slope_per_id) <- c(key_vars, "INTERCEPT", "SLOPE", "n_obs")
  df_ts <- dplyr::left_join(df_ts, slope_per_id, by = id)
  df_ts <- df_ts[df_ts$n_obs >= min_n_obs, , drop = FALSE]

  # Axes
  if (is.null(y_axis_breaks)) {
    y_max <- y_max %||% Max(df_ts[[y]])
    y_min <- y_min %||% Min(df_ts[[y]])
  } else {
    y_max <- Max(y_axis_breaks)
    y_min <- Min(y_axis_breaks)
  }
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  x_axis_title <- x_axis_title %W% gsub("_", " ", sprintf("Time (%s)", units), fixed = TRUE)
  expand_x <- rep(expand_x, length.out = 2)

  # Plot
  plot_theme <- if ("aspect_ratio" %in% names(formals(theme_fn))) {
    theme_fn(aspect_ratio = aspect_ratio, ...)
  } else {
    theme_fn(...)
  }
  p <- ggplot2::ggplot(df_ts, ggplot2::aes(TIME, .data[[y]], group = .data[[id]])) +
    ggplot2::geom_line(ggplot2::aes(group = .data[[id]], color = .data[[color_by]]), linewidth = line_thickness, alpha = line_alpha, show.legend = show_legend, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = colors) +
    brolgar::facet_strata(along = SLOPE, n_strata = n_strata) +
    ggplot2::scale_x_continuous(name = x_axis_title, expand = c(expand_x[1L], 0, expand_x[2L], 0), breaks = x_axis_breaks, n.breaks = n_x_axis_breaks) +
    scale_continuous(axis = "y", limits = c(y_min, y_max), title = y_axis_title, expand_lower = expand_y, scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, censor_fn = censor_fn) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::coord_cartesian(clip = "off") +
    plot_theme
  if (!show_facet_labels) {
    p <- p + ggplot2::theme(strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank())
  }
  p
}
