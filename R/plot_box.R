#' Box plot
#'
#' @inheritParams plot_bar_point
#' @inheritParams plot_point
#' @param color_var Variable used to color boxes. Enter as quoted or unquoted variable name
#' @param colors Hexadecimal or quoted color names. Enter using `c()`. Default uses black/red
#' @param alpha Alpha value for box fill. Default is `1`
#' @param border_thickness Thickness of line surrounding boxes in pt units. Default is `0.75`
#' @param border_colors Color of line surrounding boxes. Default is `"black"`
#' @param width Width of boxes. Default is `0.5`
#' @param errorbar_thickness Thickness of errorbar lines. Default is `border_thickness`
#' @param show_points If `TRUE`, points are displayed. Default is `FALSE`
#' @param base_size Font size used in theme
#' @export
plot_box <- function(
  df,
  formula,
  grouping_var = NULL,
  x = NULL,
  y = NULL,
  grouping_var_order = NULL,
  rev_grouping_var_order = FALSE,
  x_order = NULL,
  color_var = NULL,
  colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  alpha = 1,
  border_thickness = 0.75,
  border_colors = "black",
  width = NULL,
  errorbar_thickness = border_thickness,
  errorbar_width_multiplier = 0.5,
  show_points = FALSE,
  point_shapes = "circle",
  point_shape_var = NULL,
  point_color_var = NULL,
  point_colors = "#2A2D34",
  point_border_color = "black",
  point_border_thickness = 0,
  point_size = 0.5,
  jitter_width = 0.2,
  show_legend = FALSE,
  legend_title = "",
  y_scale = "regular",
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  y_axis_title = "",
  x_title = NULL,
  x_axis_title = x_title,
  plot_title = "",
  dodge = 0.7,
  n_breaks = 3,
  y_max = NULL,
  y_min = NULL,
  expand_y = 0.1,
  x_axis_labels = waiver(),
  censor_fn = rescale_none,
  base_size = 14,
  theme_fn = theme_custom,
  ...) {
  plot_fn <- "plot_box"
  dots <- list(...)
  if (!is.null(angle <- c(dots$x_axis_label_angle, dots$x_angle)) && angle[1L] != 0 && missing(x_axis_title) && missing(x_title)) {
    x_axis_title <- NULL
  }

  # Data
  grouping_var <- get_input(grouping_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  color_var <- get_input(color_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  x <- vars$x
  y <- vars$y
  if (is.null(y)) {
    if (is.atomic(df)) {
      df <- vec_to_df(y = df, x = "")
      y <- "y"
      x <- "x"
      x_axis_title <- x_axis_title %W% NULL
      y_axis_title <- y_axis_title %W% NULL
    } else {
      y <- vars_numeric(df)
      if (length(y) != 1L) {
        Stop(sprintf("In %s, must specify y axis variable using 'y' or 'formula' (y ~ x)", plot_fn))
      }
    }
  }
  if (is.null(x)) {
    Warning(sprintf("In %s, 'x' not specified", plot_fn))
    x_axis_title <- x_axis_title %W% NULL
    df$x <- ""
    x <- "x"
  }
  df <- df[stats::complete.cases(df[c(x, y)]), , drop = FALSE]
  x_vals <- .subset2(df, x)
  if (is.numeric(x_vals) || is.logical(x_vals)) {
    x_vals <- as.character(x_vals)
  }
  df[[x]] <- x_vals <- as_fct(x_vals, levels = x_order)
  y_vals <- .subset2(df, y)

  # Grouping/box color variables
  df$grouping_var <- grouping_vals <- .new_cat_var(df, var = grouping_var, if_null = "", levels = grouping_var_order, reverse = rev_grouping_var_order)
  n_groups <- n_unique(grouping_vals, na.rm = FALSE)
  if (n_groups == 1L) {
    width <- width %||% 0.4
    df$grouping_var <- x_vals
  } else {
    width <- width %||% (0.64-0.03*n_unique(x_vals, na.rm = FALSE))
  }
  colors <- rep(colors, length.out = n_unique(df$grouping_var, na.rm = FALSE))

  # Points
  if (show_points) {
    # Point color
    df$point_color_var <- .new_cat_var(df, var = point_color_var, if_null = df$grouping_var)
    point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))
    scale_color <- scale_color_manual(name = NULL, values = point_colors)

    # Point shape
    df$point_shape_var <- .new_cat_var(df, var = point_shape_var, if_null = "")
    point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
    if (is.character(point_shapes)) {
      look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
      point_shapes <- look_up_point_shape[point_shapes]
      names(point_shapes) <- NULL
    }
    # Point geom
    unique_shapes <- unique(point_shapes)
    if (length(unique_shapes) == 1L) {
      points <- ggplot2::geom_point(mapping = ggplot2::aes(fill = point_color_var), shape = unique_shapes, color = point_border_color, size = point_size, stroke = point_border_thickness, ggplot2::position_jitter(width = jitter_width, height = 0), show.legend = FALSE)
      scale_shape <- NULL
    } else {
      points <- ggplot2::geom_point(mapping = ggplot2::aes(fill = point_color_var, shape = point_shape_var), color = point_border_color, size = point_size, stroke = point_border_thickness, ggplot2::position_jitter(width = jitter_width, height = 0), show.legend = FALSE)
      scale_shape <- ggplot2::scale_shape_manual(name = NULL, values = point_shapes)
    }
  } else {
    points <- scale_shape <- scale_color <- NULL
  }

  # Whiskers for box plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]], group = grouping_var, fill = grouping_var)) +
    ggplot2::geom_boxplot(
      color = "black",
      size = border_thickness,
      coef = 1.5,
      width = width,
      alpha = alpha,
      position = ggplot2::position_dodge(width = dodge),
      show.legend = show_legend,
      outlier.size = NA,
      outlier.shape = NA,
      staplewidth = errorbar_width_multiplier) +
    ggplot2::scale_fill_manual(NULL, values = colors) +
    ggplot2::scale_x_discrete(name = x_axis_title, labels = x_axis_labels)
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = y_vals)
  p <- p + scale_continuous(axis = "y", limits = y_limits, scale = y_scale, n_breaks = n_breaks, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, censor_fn = censor_fn)
  p <- p +
    ggplot2::ggtitle(plot_title) +
    ggplot2::coord_cartesian(clip = "off", default = TRUE) +
    theme_fn(base_size = base_size, ...)
  p
}
