#' Plot distribution of values using density function
#'
#' @param df Data frame or numeric vector
#' @param col Continuous variable. Enter as quoted or unquoted variable name
#' @param grouping_var Grouping variable. Enter as quoted or unquoted variable name
#' @param colors Colors used to fill density plot. Default is blue and red
#' @param alpha Alpha for density color. Default is `0.6`
#' @param line_color Line color. Default is `black`
#' @param line_thickness Line thickness. Default is `1`
#' @param show_data If `TRUE`, a symbol is plotted for each observation
#' @param symbol Type of symbol used to show data. Options: `"none"` (default), `"point"`, `"rug"`
#' @param x_axis_breaks x axis breaks. Enter as numeric vector
#' @param breaks Alias for x_axis_breaks
#' @param x_min,x_max Minimum and maximum x axis values
#' @param bounds Bounds for density estimate. Enter as length 2 numeric. Default is `c(-Inf, Inf)`
#' @param x_axis_title x axis title
#' @param x_title Alias for `x_axis_title`
#' @param axis_labels Function used to generate axis labels
#' @param show_legend If `FALSE` (default), legend is not displayed
#' @param scaling_fn Scaling function used to transform variable. Default performs no transformation
#' @param expand_y Expansion to add to y axis. Default is `0.2`
#' @param plot_title Title for plot. Default is `NULL`
#' @param ... Arguments passed to `theme_custom()`
#' @returns ggplot object
#' @export
plot_density <- function(
    df,
    col = NULL,
    grouping_var = NULL,
    colors = c("#333333", "#00A1D5", "#D75725", "#6761A8", "#009872", "#EFC000", "#003C67", "#CCCCCC", "#FFFFFF", "#8B2323"),
    alpha = NULL,
    line_color = colors,
    line_thickness = 1,
    show_data = FALSE,
    symbol = "none",
    breaks = waiver(),
    x_axis_breaks = breaks,
    bounds = c(-Inf, Inf),
    x_min = NULL,
    x_max = NULL,
    x_title = NULL,
    x_axis_title = x_title,
    axis_labels = axis_label_numeric,
    show_legend = NULL,
    scaling_fn = identity,
    expand_y = 0.02,
    plot_title = NULL,
    ...) {
  # Plotting function
  plot_fn <- "plot_density"
  col <- get_input(col)
  grouping_var <- get_input(grouping_var)
  if (!is.data.frame(df)) {
    df <- vec_to_df(x_var = df)
    col <- "x_var"
  }
  df <- remove_na(df, col)
  df[[col]] <- x_vals <- scaling_fn(df[[col]])
  if (is.null(grouping_var)) {
    alpha <- alpha %||% 0.6
    df$grouping_var <- "a"
    grouping_var <- "grouping_var"
    show_legend <- show_legend %||% FALSE
  } else {
    df[[grouping_var]] <- as_fct(df[[grouping_var]])
    n_groups <- length(levels(df[[grouping_var]]))
    colors <- rep(colors, length.out = n_groups)
    line_color <- rep(line_color, length.out = n_groups)
    alpha <- alpha %||% if (length(line_color) == 1L) 0.6 else 0.3
    show_legend <- show_legend %||% TRUE
  }

  # x axis
  if (inherits(breaks, "waiver")) {
    breaks <- .create_axis_breaks(x_vals)
  }
  x_limits <- Range(c(breaks, x_vals))
  if (!is.null(x_min)) {
    x_limits[1L] <- x_min
  }
  if (!is.null(x_max)) {
    x_limits[2L] <- x_max
  }

  # Plot
  blank <- ggplot2::element_blank()
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[col]], group = .data[[grouping_var]], fill = .data[[grouping_var]], color = .data[[grouping_var]])) +
    ggplot2::geom_density(
      size = line_thickness,
      alpha = alpha,
      bounds = bounds,
      na.rm = TRUE,
      show.legend = show_legend) +
    ggplot2::scale_fill_manual(NULL, values = colors) +
    ggplot2::scale_color_manual(NULL, values = line_color) +
    ggplot2::scale_x_continuous(name = x_axis_title, limits = x_limits, breaks = breaks, expand = c(0, 0, 0, 0), guide = guide_axis(cap = "both")) +
    ggplot2::scale_y_continuous(expand = c(expand_y, 0, 0, 0)) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_custom(...) +
    ggplot2::theme(
      axis.title.y = blank,
      axis.text.y = blank,
      axis.line.y = blank,
      axis.ticks.y = blank,
      legend.key.height = if (show_legend) grid::unit(5, "pt") else NULL,
      legend.key.width = if (show_legend) grid::unit(15, "pt") else NULL,
      legend.text = ggplot2::element_text(margin = ggplot2::margin(l = grid::unit(4, "mm")))
    )
  p
}
