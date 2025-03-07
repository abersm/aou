#' Scatter plot
#'
#' @param x,y Continuous variables variables. Enter as quoted or unquoted variable names
#' @param point_border_color_var Variable used to determine border color of points. Enter as quoted or unquoted variable name
#' @param point_size_var Variable used to determine point size. Enter as quoted or unquoted variable name
#' @param border_color Alias for `point_border_color`
#' @param point_shape Point shape Options: `"circle"` (default), `"square"`, `"triangle"`, `"diamond"`
#' @param shape,size,stroke Alias for `point_shape`, `point_size`, `point_border_thickness` respectively
#' @param x_scale Scale used for x axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param x_min Minimum value for x axis. Default is `NULL`
#' @param x_max Maximum value for x axis. Default is `NULL`
#' @param scale Alias for `y_scale` and `x_scale` that sets both simultaneously
#' @param expand_y,expand_x Expansion to add to y and x axis, respectively. Default is `0.1`
#' @param expand Alias for `expand_y` and `expand_x.` Sets both simultaneously
#' @inheritParams plot_cor
#' @inheritParams plot_point
#' @returns ggplot
#' @export
plot_scatter <- function(
    df,
    formula = NULL,
    x = NULL,
    y = NULL,
    size = 3,
    point_size = size,
    point_size_var = NULL,
    shape = c("circle", "square", "triangle", "diamond"),
    point_shape = shape,
    point_shape_var = NULL,
    colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    point_colors = colors,
    point_color_var = NULL,
    border_color = "black",
    point_border_color = border_color,
    point_border_color_var = NULL,
    stroke = 0.75,
    point_border_thickness = stroke,
    alpha = 0.75,
    point_alpha = alpha,
    n_breaks = 3,
    y_max = NULL,
    y_min = NULL,
    x_max = NULL,
    x_min = NULL,
    y_axis_title = waiver(),
    y_axis_breaks = NULL,
    y_axis_labels = NULL,
    x_axis_title = waiver(),
    x_axis_breaks = NULL,
    x_axis_labels = NULL,
    expand = 0.1,
    expand_y = expand,
    expand_x = expand,
    scale = "regular",
    x_scale = scale,
    y_scale = scale,
    show_legend = FALSE,
    censor_fn = rescale_none,
    plot_title = NULL,
    legend_title = "",
    ...) {
  plot_fn <- "plot_scatter"
  # Data setup
  point_color_var <- get_input(point_color_var)
  point_size_var <- get_input(point_size_var)
  point_shape_var <- get_input(point_shape_var)
  point_border_color_var <- get_input(point_border_color_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  x <- vars$x
  y <- vars$y
  df <- remove_na(df, c(x, y))

  # Build point components
  set_args <- list(show.legend = show_legend, alpha = point_alpha, stroke = point_border_thickness)
  mapped_args <- list()

  ## Shape
  look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
  point_shapes <- look_up_point_shape[point_shape]
  names(point_shapes) <- NULL
  if (is.null(point_shape_var) || is.null(z <- .subset2(df, point_shape_var)) || (n <- length(unique(z))) == 1L) {
    set_args$shape <- point_shapes[1L]
  } else {
    point_shapes <- rep(point_shapes, length.out = n)
    mapped_args <- alist(shape = .data[[point_shape_var]])
    if (is_categorical(z)) {
      df[[point_shape_var]] <- as_fct(z)
    }
  }

  ## Size
  if (is.null(point_size_var) || is.null(z <- .subset2(df, point_size_var)) || (n <- length(unique(z))) == 1L) {
    set_args$size <- point_size[1L]
  } else {
    point_size <- rep(point_size, length.out = n)
    mapped_args <- c(mapped_args, alist(size = .data[[point_size_var]]))
  }

  ## Color
  if (is.null(point_color_var) || is.null(z <- .subset2(df, point_color_var)) || (n <- length(unique(z))) == 1L) {
    set_args$fill <- point_colors[1L]
    point_fill <- NULL
  } else {
    mapped_args <- c(mapped_args, alist(fill = .data[[point_color_var]]))
    if (is_categorical(z)) {
      df[[point_color_var]] <- as_fct(z)
      point_colors <- rep(point_colors, length.out = n)
      point_fill <- ggplot2::scale_fill_manual(name = legend_title, values = point_colors)
    } else {
      point_fill <- ggplot2::scale_fill_gradient(name = legend_title, low = point_colors[1L], high = point_colors[[length(point_colors)]])
    }
  }

  ## Border color
  if (is.null(point_border_color_var) || is.null(z <- .subset2(df, point_border_color_var)) || (n <- length(unique(z))) == 1L) {
    set_args$color <- point_border_color[1L]
  } else {
    point_border_color <- rep(point_border_color, length.out = n)
    mapped_args <- c(mapped_args, alist(color = .data[[point_border_color_var]]))
    if (is_categorical(z)) {
      df$point_border_color_var <- as_fct(z)
    }
  }

  ## Geom
  set_args$mapping <- if (length(mapped_args) == 0L) NULL else do.call("aes", mapped_args)
  point <- do.call("geom_point", set_args)

  # Axes
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df[[y]])
  x_limits <- .set_axis_limits(.min = x_min, .max = x_max, .breaks = x_axis_breaks, .values = df[[x]])
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    point +
    point_fill +
    ggplot2::scale_color_manual(name = NULL, values = point_border_color) +
    ggplot2::scale_shape_manual(name = NULL, values = point_shapes) +
    ggplot2::scale_size() +
    scale_continuous(axis = "y", scale = y_scale, limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn) +
    scale_continuous(axis = "x", scale = x_scale, limits = x_limits, breaks = x_axis_breaks, labels = x_axis_labels, title = x_axis_title, expand_lower = expand_x, n_breaks = n_breaks, censor_fn = censor_fn) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::ggtitle(plot_title) +
    theme_custom(...)
  p
}
