#' Plot regression line or loess smoothed mean per group
#'
#' @inheritParams plot_point
#' @param smoothing_fn Function used for smoothing. Options: `"loess"` (default) `"lm"`, `"glm"`
#' @param x_range_data Range of x values for input data. Default is -Inf to Inf. Enter as length 2 numeric vector
#' @param line_type Type of line. Options include `"solid"` (default), `"dashed"`, `"longdash"`, `"twodash"`, `"dotdash"`, `"dotted"`. Enter as quoted linetype
#' @param line_colors Line colors. Hexadecimal or quoted color names. Default is `"black"`
#' @param line_thickness Line thickness in mm. Default is `1`
#' @param plot_formula Formula for smoothing function Default is `y ~ x`
#' @param show_error If `TRUE` (default), error estimate for curve displayed as shaded region
#' @param ci Confidence interval for error region. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param error_fill_color Color of shaded error region. Default is same as `colors`. Alternative: `"grey60"`
#' @param show_points If `FALSE` (default), points not displayed. If `TRUE`, raw data points displayed
#' @param points_in_front If `FALSE` (default), points are displayed behind smoothed curve
#' @param x_min,x_max Minimum and maximum values to use for x axis
#' @returns ggplot object
#' @export
plot_smooth <- function(
    df,
    formula = NULL,
    grouping_var = NULL,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    y = NULL,
    x = NULL,
    smoothing_fn = stats::loess,
    plot_formula = y ~ x,
    line_type = "solid",
    colors = c("#2A2D34", "#BC3C29", "#00A1D5", "#6761A8", "#009872"),
    line_colors = colors,
    line_color_var = NULL,
    line_thickness = 1,
    show_error = TRUE,
    ci = 0.95,
    error_fill_color = line_colors,
    show_points = FALSE,
    points_in_front = FALSE,
    point_colors = line_colors,
    point_color_var = NULL,
    point_shapes = 21,
    point_shape_var = NULL,
    point_size = 2,
    point_border_thickness = 0,
    point_border_colors = "black",
    point_alpha = 0.9,
    y_axis_title = waiver(),
    y_scale = "regular",
    expand_y = NULL,
    y_axis_labels = NULL,
    y_axis_breaks = NULL,
    y_min = NULL,
    y_max = NULL,
    x_axis_title = waiver(),
    x_axis_breaks = NULL,
    x_range_data = c(-Inf, Inf),
    x_min = NULL,
    x_max = NULL,
    n_breaks = 4,
    breaks_fn = pretty,
    censor_fn = rescale_none,
    plot_title = NULL,
    show_legend = FALSE,
    legend_title = NULL,
    ...) {
  # Plot function
  plot_fn <- "plot_smooth"

  # Get input variables
  grouping_var <- get_input(grouping_var)
  line_color_var <- get_input(line_color_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y))
  x <- vars$x
  y <- vars$y

  # grouping_var
  df$grouping_var <- .new_cat_var(df, var = grouping_var, if_null = "a", levels = grouping_var_order, reverse = rev_grouping_var_order)

  # line_color_var
  df$line_color_var <- .new_cat_var(df, var = line_color_var, if_null = df$grouping_var)
  n_colors <- n_unique(df$line_color_var, na.rm = FALSE)
  line_colors <- rep(line_colors, length.out = n_colors)

  # Error fill color
  if (length(error_fill_color) > 1L) {
    error_fill_color <- rep(error_fill_color, length.out = n_colors)
    error_fill_scale <- ggplot2::scale_fill_manual(name = NULL, values = error_fill_color)
  } else {
    error_fill_scale <- NULL
  }

  # Point variables
  if (show_points) {
    # Arguments for geom_point
    point_args <- list(
      color = point_border_colors[1L],
      size = point_size,
      alpha = point_alpha,
      stroke = point_border_thickness,
      show.legend = show_legend,
      mapping = ggplot2::aes(fill = point_color_var, shape = point_shape_var)
    )

    # point_color_var
    if (is.null(point_color_var) || length(unique.default(point_colors)) == 1L) {
      point_args$fill <- point_colors[1L]
      point_args$mapping$fill <- scale_point_colors <- NULL
    } else {
      df$point_color_var <- .new_cat_var(df, var = point_color_var, if_null = "a", as_fct = FALSE)
      if (is_categorical(df$point_color_var)) {
        df$point_color_var <- factor(df$point_color_var, levels = create_levels(df$point_color_var))
        point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))
      } else if (length(point_colors) < 5L) {
        point_colors <- clr_continuous(point_colors)
      }
      scale_point_colors <- ggplot2::scale_fill_manual(name = legend_title, values = point_colors)
    }

    # point_shape_var
    if (is.character(point_shapes)) {
      look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
      point_shapes <- look_up_point_shape[point_shapes]
      names(point_shapes) <- NULL
    } else {
      point_shapes <- dplyr::case_when(
        point_shapes %in% c(21, 22, 23, 24, 25) ~ point_shapes,
        point_shapes %in% c(0, 15) ~ 22,
        point_shapes %in% c(5, 18) ~ 23,
        point_shapes %in% c(2, 17) ~ 24,
        point_shapes == 6 ~ 25,
        .default = 21
      )
    }
    if (is.null(point_shape_var) || length(unique.default(point_shapes)) == 1L) {
      point_args$shape <- point_shapes[1L]
      point_args$mapping$shape <- scale_point_shape <- NULL
    } else {
      df$point_shape_var <- .new_cat_var(df, var = point_shape_var, if_null = "a")
      point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
      scale_point_shape <- ggplot2::scale_shape_manual(name = legend_title, values = point_shapes)
    }
    if (length(point_args$mapping) == 0L) {
      point_args$mapping <- NULL
    }

    # Point geom
    points_geom <- list(do.call("geom_point", point_args), scale_point_colors, scale_point_shape)
  } else {
    points_geom <- NULL
  }

  # DATA
  df <- remove_na(df, c(x, y, "grouping_var", "line_color_var"))
  df <- df[is_between(df[[x]], x_range_data[1L], x_range_data[2L]), , drop = FALSE]
  y_log <- y_scale %in% c("log", "log10")
  if (y_log) {
    df[[y]] <- log10(df[[y]] + 1)
  }

  # CORE PLOT
  p <- ggplot2::ggplot(df, ggplot2::aes(.data[[x]], .data[[y]], group = grouping_var))
  if (is.null(error_fill_scale)) {
    smooth_geom <- list(ggplot2::geom_smooth(ggplot2::aes(color = line_color_var), method = smoothing_fn, formula = plot_formula, se = show_error, fill = error_fill_color, level = ci, linewidth = line_thickness, show.legend = show_legend), ggplot2::scale_color_manual(name = legend_title, values = line_colors))
  } else {
    smooth_geom <- list(ggplot2::geom_smooth(ggplot2::aes(color = line_color_var, fill = line_color_var), method = smoothing_fn, formula = plot_formula, se = show_error, level = ci, linewidth = line_thickness, show.legend = show_legend), ggplot2::scale_color_manual(name = legend_title, values = line_colors), error_fill_scale)
  }

  # POINTS
  p <- if (points_in_front) {
    if (is.null(error_fill_color)) {
      p + smooth_geom + points_geom
    } else {
      p + smooth_geom + ggnewscale::new_scale_fill() + points_geom
    }
  } else {
    if (is.null(error_fill_color)) {
      p + points_geom + smooth_geom
    } else {
      p + points_geom + ggnewscale::new_scale_fill() + smooth_geom
    }
  }

  # X AXIS
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  x_min <- x_min %||% min(df[[x]])
  x_max <- x_max %||% max(df[[x]])
  x_breaks <- x_axis_breaks %||% breaks_extended(c(x_min, x_max), n = 4)

  # Y AXIS
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  y_plot_limits <- get_plot_data_limits(p, axis = "y")
  if (!is.null(y_min)) {
    y_plot_limits[1L] <- y_min
  }
  if (!is.null(y_max)) {
    y_plot_limits[2L] <- y_max
  }
  y_axis_breaks <- y_axis_breaks %||% .create_axis_breaks(.limits = y_plot_limits, .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)
  y_plot_limits <- range(y_plot_limits, y_axis_breaks)

  # PLOT
  if (is.null(y_axis_labels)) {
    if (y_log) {
      y_axis_labels <- axis_label_numeric
    } else {
      y_axis_labels <- function(x) {
        x_max <- abs(Max(x))
        if (x_max < 1e-4 || x_max >= 10000) {
          has_decimal <- any(grepl(".", format(x, scientific = TRUE), fixed = TRUE))
          axis_labels <- function(j) {
            if (is.na(j) || j < 0) {
              return("")
            } else if (j == 0) {
              return(0)
            }
            j <- unlist(strsplit(format(j, scientific = TRUE), "e", fixed = TRUE), use.names = FALSE)
            j1 <- j[1L]
            if (has_decimal && !grepl(".", j1, fixed = TRUE)) {
              j1 <- paste0(j1, ".0")
            }
            bquote(.(paste(j1, "×", "10"))^.(as.numeric(j[2L])))
          }
          as.expression(lapply(x, axis_labels))
        } else {
          labels <- format(x, scientific = FALSE)
          labels[is.na(x) | x < 0] <- ""
          labels
        }
      }
    }
  }
  if (min(y_axis_breaks) < 0 && y_axis_breaks[2L] >= 0) {
    y_axis_breaks <- y_axis_breaks[y_axis_breaks >= 0]
    if (is.null(expand_y)) {
      delta <- y_axis_breaks[1L] + (y_axis_breaks[1L] - y_axis_breaks[2L])/2
      if (delta < 0 && y_plot_limits[1L] < delta) {
        expand_y <- -0.1
      }
    }
  }
  p <- p +
    ggplot2::ggtitle(plot_title) +
    ggplot2::scale_x_continuous(name = x_axis_title, limits = range(x_min, x_max, x_breaks), breaks = x_breaks, oob = censor_fn) +
    ggplot2::scale_y_continuous(name = y_axis_title, limits = y_plot_limits, expand = c(expand_y %||% 0, 0, 0, 0), oob = censor_fn, breaks = y_axis_breaks, labels = y_axis_labels) +
    theme_custom(...)
  p
}
