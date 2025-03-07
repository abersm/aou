#' Plot statistical summary value +/- error of a continuous variable for each group
#'
#' @inheritParams plot_point
#' @returns ggplot object
#' @export
plot_summary <- function(
    df,
    formula = NULL,
    grouping_var = NULL,
    x = NULL,
    y = NULL,
    dodge = 0.7,
    x_order = NULL,
    rev_x_order = FALSE,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    point_shapes = "square",
    point_shape_var = NULL,
    point_color_var = NULL,
    colors = c("#0072B5", "#BC3C29", "#868686", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#333333", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    point_colors = colors,
    point_size = 3,
    point_border_thickness = 1,
    point_border_colors = "black",
    show_errorbar = TRUE,
    summary_fn = Mean,
    error_fn = SE,
    errorbar_width = 0.25,
    errorbar_thickness = 0.75,
    show_legend = FALSE,
    legend_title = "",
    y_title = waiver(),
    y_axis_title = y_title,
    y_scale = "regular",
    y_axis_breaks = NULL,
    y_axis_labels = NULL,
    breaks_fn = pretty,
    n_breaks = 4,
    y_min = NULL,
    y_max = NULL,
    expand_y = 0.1,
    censor_fn = rescale_none,
    x_title = NULL,
    x_axis_label_angle = 45,
    x_angle = x_axis_label_angle,
    x_axis_title = x_title,
    x_axis_breaks = NULL,
    x_axis_labels = NULL,
    expand_x = waiver(),
    plot_title = NULL,
    show_sig = FALSE,
    sig_method = "p_by_normality",
    stars = TRUE,
    show_ns = TRUE,
    ns_symbol = "ns",
    sig_bar_nudge = if (y_scale == "log") 0.9 else 0.06,
    sig_star_nudge = if (y_scale == "log") -0.45 else sig_bar_nudge - 0.015,
    sig_text_nudge = if (y_scale == "log") 0.5 else sig_bar_nudge + 0.03,
    step_increase = if (y_scale == "log") 0 else sig_bar_nudge + 0.05,
    sig_bar_thickness = 0.6,
    sig_bar_color = "black",
    sig_font_color = "black",
    sig_star_size = 20,
    sig_text_size = 12,
    p_case = "upper",
    p_spaces = TRUE,
    theme_fn = theme_custom,
    ...) {
  # Plotting function
  plot_fn <- "plot_summary"

  # Data
  ## Add new variables rather than renaming variables to preserve original variable names (needed if facets are used)
  grouping_var <- get_input(grouping_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_summary")
  formula <- vars$formula
  x <- vars$x
  y <- vars$y
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c(grouping_var, point_color_var, point_shape_var))
  x_levels <- x_order %||% create_levels(df$x, reverse = rev_x_order)
  df$x_numeric <- match(df$x, x_levels, incomparables = NA_integer_)

  # grouping_var
  df$grouping_var <- .new_cat_var(df, var = grouping_var, levels = grouping_var_order, reverse = rev_grouping_var_order)
  n_groups <- length(unique.default(df$grouping_var))

  # point_color_var
  df$point_color_var <- .new_cat_var(df, var = point_color_var, if_null = df$grouping_var)
  point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))

  # point_shape_var
  df$point_shape_var <- .new_cat_var(df, var = point_shape_var)
  point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
  if (is.character(point_shapes)) {
    look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
    point_shapes <- look_up_point_shape[point_shapes]
    names(point_shapes) <- NULL
  }
  unique_shapes <- unique(point_shapes)
  if (length(unique_shapes) == 1L) {
    point_args <- list(mapping = ggplot2::aes(fill = point_color_var), shape = unique_shapes)
    scale_shape <- NULL
  } else {
    point_args <- list(mapping = ggplot2::aes(fill = point_color_var, shape = point_shape_var))
    scale_shape <- ggplot2::scale_shape_manual(name = NULL, values = point_shapes)
  }

  # y axis
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  if (startsWith(y_scale, "log")) {
    df$y <- ifelse(df$y <= 0, 1, df$y)
  }
  error_fn <- if (show_errorbar) error_fn else function(x) 0
  df_by_x <- dplyr::group_by(df, x, grouping_var)
  df_by_x <- dplyr::summarize(df_by_x, error = error_fn(y), summary = summary_fn(y), .groups = "drop")
  df_by_x$min <- df_by_x$summary - df_by_x$error
  df_by_x$max <- df_by_x$summary + df_by_x$error
  y_limits <- c(Min(df_by_x$min), Max(df_by_x$max))
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = y_limits)

  # Core plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x_numeric, y = y, group = grouping_var))

  # Error bars
  if (show_errorbar) {
    y_max_type <- "error"
    p <- p +
      errorbar(summary_fn = summary_fn, error_fn = error_fn, line_thickness = errorbar_thickness, width = errorbar_width, dodge = dodge, error_limits = "both")
  } else {
    y_max_type <- "summary"
  }

  # Point geom
  point <- do.call(
    ggplot2::stat_summary,
    c(point_args,
      list(
        geom = "point",
        fun = summary_fn,
        color = point_border_colors,
        size = point_size,
        alpha = 1,
        stroke = point_border_thickness,
        position = if (n_groups > 1L) ggplot2::position_dodge(width = dodge) else "identity",
        show.legend = show_legend
      )
    )
  )
  p <- p +
    point +
    ggplot2::scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape

  # x axis
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  x_breaks <- x_axis_breaks %||% sort.int(unique(df$x_numeric))
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  p <- p + ggplot2::scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Plot title/theme
  p <- p +
    ggplot2::coord_cartesian(clip = "off", default = TRUE) +
    ggplot2::ggtitle(plot_title) +
    theme_fn(..., x_angle = x_angle)

  # Significance annotation
  if (show_sig) {
    if (startsWith(y_scale, "log")) {
      sig_bar_nudge <- 1
      sig_star_nudge <- 0.75
      sig_text_nudge <- 1.75
    }
    fn_sig_anno <- if (n_unique(df$grouping_var, na.rm = FALSE) > 1L) .plot_sig_anno_grouped else .plot_sig_anno
    p <- p + fn_sig_anno(df = df, method = sig_method, dodge = dodge, y_max_type = y_max_type, summary_fn = summary_fn, error_fn = error_fn, bar_nudge = sig_bar_nudge, star_nudge = sig_star_nudge, text_nudge = sig_text_nudge, star_size = sig_star_size, text_size = sig_text_size, p_case = p_case, p_spaces = p_spaces, bar_thickness = sig_bar_thickness, bar_color = sig_bar_color, font_color = sig_font_color, show_ns = show_ns, stars = stars, ns_symbol = ns_symbol, step_increase = step_increase, breaks_fn = breaks_fn)
    y_plot_limits <- get_plot_data_limits(p, axis = "y")
    p <- p + scale_axis_clean(axis = "y", scale = y_scale, plot_limits = y_plot_limits, axis_limits = y_limits, labels = y_axis_labels, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, title = y_axis_title, guide = guide_axis(cap = "upper"))
  } else {
    p <- p + scale_continuous(axis = "y", scale = y_scale, limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
  }
  p
}
