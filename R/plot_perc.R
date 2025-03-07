#' Plot percentage +/- CI for each group
#'
#' @inheritParams plot_point
#' @param invert If `TRUE`, 1 - proportion (or 100 - percentage) plotted along y axis
#' @param as_perc If `TRUE` (default), y axis is plotted as percentage (0-100). If `FALSE`, y axis plotted as proportion (0-1)
#' @param geom geom function to plot percent. Default is `"point"`
#' @returns ggplot object
#' @export
plot_perc <- function(
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
    colors = c("#333333", "#0072B5", "#BC3C29", "#868686", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    point_colors = colors,
    point_size = 3,
    point_border_thickness = 1,
    point_border_colors = "black",
    show_errorbar = TRUE,
    errorbar_width = 0.25,
    errorbar_thickness = 0.75,
    show_legend = FALSE,
    legend_title = "",
    as_perc = TRUE,
    y_title = waiver(),
    y_axis_title = y_title,
    invert = FALSE,
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
    x_axis_labels = waiver(),
    expand_x = waiver(),
    geom = "point",
    plot_title = NULL,
    theme_fn = theme_custom,
    ...) {
  # Plotting function
  plot_fn <- "plot_perc"

  # Data
  ## Add new variables rather than renaming variables to preserve original variable names (needed if facets are used)
  grouping_var <- get_input(grouping_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_perc")
  formula <- vars$formula
  x <- vars$x
  y <- vars$y
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c(grouping_var, point_color_var, point_shape_var))

  # Proportion/percent
  y_vals <- df$y
  if (n_unique(y_vals) > 2L) stop("In 'plot_perc', 'y' variable must be a binary variable")
  if (is.numeric(y_vals)) {
    if (!all(unique.default(y_vals) %in% c(0, 1))) stop("In 'plot_perc', 'y' variable must be a binary variable. If 'y' is numeric, values must be either '0' or '1'")
    y_vals <- as.logical(y_vals)
  } else if (!is.logical(y_vals)) {
    z <- create_levels(y_vals, reverse = TRUE)
    y_vals <- y_vals == z[1L]
    if (is_waiver(y_axis_title)) {
      y_axis_title <- paste(if (invert) z[2L] else z[1L], "(%)")
    }
  }
  if (invert) {
    y_vals <- !y_vals
  }
  df$y <- as.integer(y_vals)

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
  unique_shapes <- unique.default(point_shapes)
  if (length(unique_shapes) == 1L) {
    point_args <- list(mapping = ggplot2::aes(fill = point_color_var), shape = unique_shapes, size = point_size, stroke = point_border_thickness)
    scale_shape <- NULL
  } else {
    point_args <- list(mapping = ggplot2::aes(fill = point_color_var, shape = point_shape_var), size = point_size, stroke = point_border_thickness)
    scale_shape <- ggplot2::scale_shape_manual(name = NULL, values = point_shapes)
  }

  if (geom != "point") {
    point_args$size <- point_args$shape <- point_args$stroke <- NULL
    point_args$width <- errorbar_width*2
  }

  # y axis
  y_axis_title <- y_axis_title %W% paste(gsub("_", " ", str_capitalize(y), fixed = TRUE), "(%)")

  # Error bars/y axis limits
  if (show_errorbar) {
    #y_max_type <- "error"
    error_data <- function(x) {
      total <- length(x)
      n <- sum(x)
      ci <- CI_prop(n, total)
      vec_to_df(y = ci[1L], ymin = ci[2L], ymax = ci[3L])
    }
    y_limits <- do.call(rbind, lapply(split_df(df, c("x", "grouping_var")), function(x) error_data(x$y)))
    y_limits <- Range(c(y_limits$ymin, y_limits$ymax))
    error <- ggplot2::stat_summary(geom = "errorbar", fun.data = error_data, width = errorbar_width, linewidth = errorbar_thickness, position = ggplot2::position_dodge(width = dodge))
  } else {
    y_limits <- dplyr::group_by(df, x, grouping_var)
    y_limits <- dplyr::summarize(y_limits, y = Mean(y), .groups = "drop")
    y_limits <- Range(y_limits$y)
    #y_max_type <- "summary"
    error <- NULL
  }
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = y_limits)

  # Point geom
  point <- do.call(
    ggplot2::stat_summary,
    c(point_args,
      list(
        geom = geom,
        fun = Mean,
        color = point_border_colors,
        alpha = 1,
        position = if (n_groups > 1L) ggplot2::position_dodge(width = dodge) else "identity",
        show.legend = show_legend
      )
    )
  )

  # x axis
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  if (is.null(x_order)) {
    if (is.factor(df$x)) {
      x_order <- attr(df$x, "levels")
    } else {
      x_order <- attr(fct_reorder(df$x, df$y, .increasing = FALSE), "levels")
    }
  }
  if (rev_x_order) {
    x_order <- Rev(x_order)
  }
  df$x <- factor(df$x, levels = x_order)
  if (as_perc && is.null(y_axis_labels)) {
    y_axis_labels <- function(x) x*100
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = grouping_var)) +
    error +
    point +
    ggplot2::scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape +
    ggplot2::scale_x_discrete(name = x_axis_title, labels = x_axis_labels, expand = expand_x) +
    ggplot2::coord_cartesian(clip = "off", default = TRUE) +
    ggplot2::ggtitle(plot_title) +
    theme_fn(..., x_angle = x_angle) +
    scale_continuous(axis = "y", scale = "regular", limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
  p
}
