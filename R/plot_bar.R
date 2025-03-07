#' Bar plot
#'
#' @inheritParams plot_point
#' @param bar_alpha Alpha value for bar fill. Enter as length 1 numeric. Default is `0`
#' @param bar_border_thickness Thickness of line surrounding bars in pts. Enter as length 1 numeric. Default is `0.75`
#' @param width,bar_width Width of bars. Default is 0.2*number of x values
#' @param bar_color_var Variable used to determine bar color. Enter as quoted or unquoted variable name
#' @param bar_border_colors Bar border color. Default is `"black"`
#' @param errorbar_width_multiplier Multiplied by `bar_width` to determine width of error bars. Enter as length 1 numeric. Default is `0.5`
#' @param bar_colors Bar color. Enter as character vector of color names or hexadecimal codes
#' @param show_all If `TRUE` (default), all significance bars are displayed
#' @param free_y_scale If `TRUE` (default), y scaling allows for free facet scales for y axis
#' @returns ggplot
#' @export
plot_bar <- function(
  df,
  formula = NULL,
  grouping_var = NULL,
  grouping_var_order = NULL,
  rev_grouping_var_order = FALSE,
  x_order = NULL,
  rev_x_order = FALSE,
  x = NULL,
  y = NULL,
  bar_color_var = NULL,
  colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  bar_colors = colors,
  bar_alpha = 1,
  bar_border_thickness = 0.75,
  bar_border_colors = "black",
  width = NULL,
  bar_width = width,
  dodge = 0.7,
  show_errorbar = TRUE,
  summary_fn = Mean,
  error_fn = SE,
  errorbar_width_multiplier = 0.5,
  errorbar_thickness = bar_border_thickness,
  error_limits = "upper",
  show_legend = FALSE,
  legend_title = "",
  y_scale = "regular",
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  x_axis_breaks = NULL,
  x_axis_labels = NULL,
  y_title = waiver(),
  y_axis_title = y_title,
  x_title = waiver(),
  x_axis_title = x_title,
  plot_title = NULL,
  show_sig = TRUE,
  stars = TRUE,
  sig_font_color = "black",
  sig_bar_color = "black",
  sig_bar_thickness = 0.6,
  show_ns = TRUE,
  ns_symbol = "ns",
  sig_star_size = 20,
  sig_text_size = 12,
  sig_bar_nudge = 0.06,
  sig_star_nudge = 0.05,
  sig_text_nudge = 0.09,
  step_increase = if (y_scale == "regular") sig_bar_nudge + 0.05 else 0,
  sig_method = "p_by_normality",
  p_case = "upper",
  p_spaces = TRUE,
  n_breaks = 3,
  breaks_fn = pretty,
  y_max = NULL,
  expand_x = waiver(),
  censor_fn = rescale_none,
  show_all = TRUE,
  free_y_scale = TRUE,
  ...) {
  plot_fn <- "plot_bar"
  dots <- list(...)
  if (!is.null(angle <- c(dots$x_axis_label_angle, dots$x_angle)) && angle[1L] != 0 && missing(x_axis_title) && missing(x_title)) {
    x_axis_title <- NULL
  }

  # Data setup
  grouping_var <- get_input(grouping_var)
  bar_color_var <- get_input(bar_color_var)
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_bar")
  formula <- vars$formula
  x <- vars$x
  y <- vars$y
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c(grouping_var, bar_color_var))
  x_levels <- x_order %||% create_levels(df$x, reverse = rev_x_order)
  df$x_numeric <- match(df$x, x_levels, incomparables = NA_integer_)

  # grouping_var
  df$grouping_var <- .new_cat_var(df, var = grouping_var, levels = grouping_var_order, reverse = rev_grouping_var_order)

  # bar_color_var
  df$bar_color_var <- .new_cat_var(df, var = bar_color_var, if_null = df$grouping_var)
  bar_colors <- rep(bar_colors, length.out = n_unique(df$bar_color_var, na.rm = FALSE))

  # Inputs to geom/stat functions: mapped vs. set variables
  if (is.null(bar_width)) {
    bar_width <- if (is.null(grouping_var)) 0.4 else (0.64-0.03*n_unique(df$x, na.rm = FALSE))
  }
  set_args <- list(geom = "bar", fun = summary_fn, size = bar_border_thickness, position = ggplot2::position_dodge(width = dodge), color = bar_border_colors, show.legend = show_legend, alpha = bar_alpha, width = bar_width)
  mapped_args <- ggplot2::aes(x = x_numeric, y = y, fill = bar_color_var, group = grouping_var)

  # Build plot
  set_args$mapping <- mapped_args <- do.call("aes", mapped_args)
  p <- ggplot2::ggplot(df, mapped_args)

  # Bars
  bar <- do.call("stat_summary", set_args)
  p <- p + bar + ggplot2::scale_fill_manual(name = legend_title, values = bar_colors)

  # Error bars
  if (show_errorbar) {
    if (bar_border_thickness == 0) {
      if (missing(errorbar_thickness)) {
        errorbar_thickness <- 0.75
      }
      if (missing(error_limits)) {
        error_limits <- "both"
      }
    }
    p <- p + errorbar(summary_fn = summary_fn, error_fn = error_fn, line_thickness = errorbar_thickness, width = errorbar_width_multiplier*bar_width, dodge = dodge, error_limits = error_limits)
  }

  # x axis
  x_breaks <- x_axis_breaks %||% sort.int(unique.default(df$x_numeric))
  x_labels <- x_axis_labels %||% x_levels
  if (!missing(x_axis_labels) && is.null(x_breaks)) {
    x_labels <- NULL
  }
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  p <- p + ggplot2::scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Significance annotation
  if (show_sig) {
    y_max_type <- if (show_errorbar) "error" else "summary"
    if (startsWith(y_scale, "log")) {
      sig_bar_nudge <- 1
      sig_star_nudge <- 0.75
      sig_text_nudge <- 1.75
    }
    fn_sig_anno <- if (n_unique(df$grouping_var, na.rm = FALSE) > 1L) .plot_sig_anno_grouped else .plot_sig_anno
    p <- p + fn_sig_anno(
      df = df,
      method = sig_method,
      dodge = dodge,
      y_max_type = y_max_type,
      summary_fn = summary_fn,
      error_fn = error_fn,
      bar_nudge = sig_bar_nudge,
      star_nudge = sig_star_nudge,
      text_nudge = sig_text_nudge,
      star_size = sig_star_size,
      text_size = sig_text_size,
      p_case = p_case,
      p_spaces = p_spaces,
      bar_thickness = sig_bar_thickness,
      bar_color = sig_bar_color,
      font_color = sig_font_color,
      show_ns = show_ns,
      stars = stars,
      ns_symbol = ns_symbol,
      step_increase = step_increase,
      breaks_fn = breaks_fn
    )
  }

  # y axis
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  if (is.null(y_max)) {
    if (is.null(y_axis_breaks)) {
      df_by_x <- dplyr::group_by(df, x, grouping_var)
      fn <- if (show_errorbar) {
        function(x) summary_fn(x) + error_fn(x)
      } else {
        summary_fn
      }
      df_by_x <- dplyr::summarize(df_by_x, y = fn(y), .groups = "drop")
      y_max <- Max(df_by_x$y)
    } else {
      y_max <- Max(y_axis_breaks)
    }
  }
  if (free_y_scale) {
    if (y_scale == "regular") {
      p <- p +
        ggplot2::scale_y_continuous(
          name = y_axis_title,
          labels = y_axis_labels %||% ggplot2::waiver(),
          #guide = ggplot2::guide_axis(cap = "upper"),
          expand = c(0, 0, 0, 0),
          oob = censor_fn
        )
    } else {
      p <- p + ggplot2::scale_continuous(axis = "y", scale = y_scale, labels = y_axis_labels, title = y_axis_title, censor_fn = censor_fn)
    }
  } else if (show_all) {
    y_axis_breaks <- y_axis_breaks %||% .create_axis_breaks(.limits = c(0, y_max), .scale = y_scale, .breaks_fn = breaks_fn, .n = n_breaks)
    plot_limits <- get_plot_data_limits(p, axis = "y")
    p <- p + scale_axis_clean(axis = "y", scale = y_scale, plot_limits = plot_limits, axis_limits = c(0, y_max), n_breaks = n_breaks, labels = y_axis_labels, title = y_axis_title, censor_fn = censor_fn, breaks_fn = breaks_fn, guide = guide_clean_axis())
  } else {
    p <- p + scale_continuous(axis = "y", scale = y_scale, limits = c(0, y_max), n_breaks = n_breaks, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, censor_fn = censor_fn)
  }

  # Plot title and theme
  p <- p + ggplot2::coord_cartesian(clip = "off", default = TRUE) + ggplot2::ggtitle(plot_title) + theme_custom(...)
  p
}
