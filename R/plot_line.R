#' Plot lines
#'
#' @inheritParams plot_line_mean
#' @export
plot_line <- function(
    df,
    formula = NULL,
    grouping_var = NULL,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    x = NULL, y = NULL,
    colors = c("#0072B5", "#BC3C29", "#868686", "#2A2D34", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    line_type = "solid",
    line_type_var = NULL,
    line_thickness = 1,
    line_color_var = NULL,
    line_colors = colors,
    show_points = TRUE,
    point_color_var = line_color_var,
    point_shape_var = NULL,
    point_shapes = "circle",
    point_size = 3,
    point_colors = colors,
    point_border_color = "black",
    point_border_thickness = 0,
    show_legend = FALSE,
    y_scale = "regular",
    x_axis_title = waiver(),
    y_axis_breaks = NULL,
    y_axis_labels = NULL,
    y_axis_title = waiver(),
    x_axis_breaks = waiver(),
    x_axis_labels = waiver(),
    expand_y = 0.05,
    expand_x = waiver(),
    plot_title = NULL,
    n_breaks = 3,
    y_min = NULL,
    y_max = NULL,
    censor_fn = rescale_none,
    legend_title = NULL,
    theme_fn = theme_custom,
    ...) {
  # Plotting function
  plot_fn <- "plot_line"

  # Data
  grouping_var <- get_input(grouping_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  line_color_var <- get_input(line_color_var)
  line_type_var <- get_input(line_type_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_line")
  x <- vars$x
  y <- vars$y
  df <- df[stats::complete.cases(df[c(x, y)]), , drop = FALSE]
  y_vals <- .subset2(df, y)

  # Grouping variable
  df$grouping_var <- .new_cat_var(df, var = grouping_var, if_null = "", levels = grouping_var_order, reverse = rev_grouping_var_order)

  # Line color
  df$line_color_var <- .new_cat_var(df, var = line_color_var, if_null = df$grouping_var)
  line_colors <- rep(line_colors, length.out = n_unique(df$line_color_var, na.rm = FALSE))

  # Line type
  df$line_type_var <- .new_cat_var(df, var = line_type_var, if_null = df$line_color_var)
  line_type <- rep(line_type, length.out = n_unique(df$line_type_var, na.rm = FALSE))

  # Points
  if (show_points) {
    # Point color
    df$point_color_var <- .new_cat_var(df, var = point_color_var, if_null = df$grouping_var)
    point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))

    # Point shape
    df$point_shape_var <- .new_cat_var(df, var = point_shape_var, if_null = df$point_color_var)
    point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
    if (is.character(point_shapes)) {
      look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
      point_shapes <- look_up_point_shape[point_shapes]
      names(point_shapes) <- NULL
    }

    # Point geom
    unique_shapes <- unique.default(point_shapes)
    if (length(unique_shapes) == 1L) {
      points <- ggplot2::geom_point(mapping = ggplot2::aes(fill = point_color_var), shape = unique_shapes, color = point_border_color, size = point_size, stroke = point_border_thickness, show.legend = show_legend)
      scale_shape <- NULL
    } else {
      points <- ggplot2::geom_point(mapping = ggplot2::aes(fill = point_color_var, shape = point_shape_var), color = point_border_color, size = point_size, stroke = point_border_thickness, show.legend = show_legend)
      scale_shape <- ggplot2::scale_shape_manual(name = NULL, values = point_shapes)
    }
  } else {
    points <- NULL
    scale_shape <- NULL
  }

  # y axis
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = y_vals)
  # Change y values if log scale is used
  if (startsWith(y_scale, "log")) {
    minimum_y_value <- min(y_vals)
    if (minimum_y_value < 1) {
      if (minimum_y_value < 0) {
        min_rounded <- round_up(minimum_y_value, 2)
        Warning(sprintf("Some y values are < 0 (min = %s). Will add %s + 1 to all values", min_rounded, abs(min_rounded)))
        df[[y]] <- y_vals + abs(minimum_y_value) + 1
      } else if (minimum_y_value == 0) {
        z <- y_vals[y_vals > 0 & y_vals < 1]
        if (length(z) > 0L) {
          y_limits[1L] <- min(z)
        } else {
          df[[y]] <- y_vals + 1
          y_limits[1L] <- 1
        }
      }
    }
  }

  # x axis
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(.data[[x]], .data[[y]], group = grouping_var)) +
    ggplot2::geom_line(ggplot2::aes(color = line_color_var, linetype = line_type_var), linewidth = line_thickness, show.legend = show_legend) +
    ggplot2::scale_color_manual(name = legend_title, values = line_colors) +
    ggplot2::scale_linetype_manual(name = legend_title, values = line_type) +
    points +
    ggplot2::scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape +
    ggplot2::scale_x_continuous(name = x_axis_title, breaks = x_axis_breaks, labels = x_axis_labels, expand = expand_x) +
    scale_continuous(axis = "y", limits = y_limits, scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::coord_cartesian(clip = "off", default = TRUE) +
    theme_fn(...)
  p
}
