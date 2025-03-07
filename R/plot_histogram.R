#' Histogram
#'
#' @param df Data frame or vector
#' @param col Continuous variable. Enter as quoted or unquoted variable name
#' @param n_bins Number of bins for x axis. Default is `20`
#' @param bar_color_var Variable in df used to color bars. Enter as quoted or unquoted variable name
#' @param colors Color of bars. Default is `"#328EC3"`
#' @param bar_colors Alias for colors
#' @param bar_border_color Color of bar borders. Default is `"black"`
#' @param x_axis_breaks,x_axis_labels,x_axis_title x axis breaks, labels, and title
#' @param expand_x Expansion to add to x axis. Default is `0.6`
#' @param x_axis_label_angle Angle of x axis labels. Default is `45` degrees
#' @param width Width of bars. Default is `0.7`
#' @param bar_width Alias for width
#' @param y_axis_title Title for y axis. Default is `"n"`
#' @param n_breaks_x Number of breaks desired for x axis. Default is `4L`
#' @param n_breaks_y Number of breaks desired for y axis. Default is `4L`
#' @param bar_alpha Alpha for bars. Default is `1`
#' @param show_legend If `FALSE` (default), legend is not displayed
#' @param legend_title Title of legend. Default is blank
#' @param show_n If `TRUE` (default), total sample size is displayed on plot
#' @param n_anno_font_size Font size for sample size annotation in pts. Default is `12`
#' @param n_anno_font_color Font color for sample size annotation. Default is `"black"`
#' @param n_anno_x Position of sample size annotation along x axis. Options: `NULL` (default), `"right"`, `"left"`
#' @param n_anno_x_nudge Amount to nudge sample size annotation along x axis. Default is `0`
#' @param n_anno_y_nudge Amount to nudge sample size annotation along y axis. Default is `0`
#' @param n_prefix Prefix for sample size annotation. Default is `"N = "`
#' @param y_axis_title_angle Angle for y axis title. Default is `0`
#' @param y_axis_title_margin Margin between y axis title and labels. Default is `10`
#' @param ... Arguments passed to `theme_custom()`
#' @inheritParams plot_point
#' @export
plot_histogram <- function(
    df,
    col = NULL,
    n_bins = 20,
    bar_color_var = NULL,
    colors = c("#328EC3", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    bar_colors = colors,
    bar_border_color = "black",
    rev_x_order = FALSE,
    x_axis_breaks = NULL,
    x_axis_labels = waiver(),
    x_axis_title = NULL,
    expand_x = 0.1,
    x_axis_label_angle = 45,
    width = 0.7,
    bar_width = width,
    y_axis_title = "n",
    n_breaks_x = 4L,
    n_breaks_y = 4L,
    bar_alpha = 1,
    show_legend = FALSE,
    legend_title = NULL,
    show_n = TRUE,
    n_anno_font_size = 12,
    n_anno_font_color = "black",
    n_anno_x = NULL,
    n_anno_x_nudge = 0,
    n_anno_y_nudge = 0,
    n_prefix = "N = ",
    y_axis_title_angle = 0,
    y_axis_title_margin = 10,
    ...) {
  plot_fn <- "plot_histogram"
  col <- get_input(col)
  bar_color_var <- get_input(bar_color_var)
  if (is.data.frame(df)) {
    df <- df[names(df) != "y"]
    df$y <- df[[col]]
    df <- df[!is.na(df$y), ]
    df$color <- if (is.null(df[[bar_color_var]])) {
      "a"
    } else {
      factor(df[[bar_color_var]])
    }
  } else {
    df <- vec_to_df(y = df[!is.na(df)])
    df$color <- "a"
  }
  x_axis_breaks <- x_axis_breaks %||% .create_axis_breaks(.limits = range(df$y), .scale = "regular", .n = n_breaks_x)
  x_axis_limits <- range(x_axis_breaks)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = y)) +
    ggplot2::geom_histogram(ggplot2::aes(fill = color), alpha = bar_alpha, color = bar_border_color, bins = n_bins, show.legend = show_legend) +
    ggplot2::scale_fill_manual(legend_title, values = bar_colors) +
    ggplot2::scale_x_continuous(x_axis_title, limits = x_axis_limits, breaks = x_axis_breaks, expand = ggplot2::expansion(mult = c(expand_x, expand_x)), oob = rescale_none) +
    theme_custom(...) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle = y_axis_title_angle, margin = ggplot2::margin(r = y_axis_title_margin)))
  plot_limits <- get_plot_data_limits(p)
  y_axis_breaks <- pretty(plot_limits$y, n = n_breaks_y)
  y_axis_breaks <- y_axis_breaks[y_axis_breaks %% 1 == 0]
  y_axis_limits <- range(y_axis_breaks)
  p <- p + ggplot2::scale_y_continuous(y_axis_title, limits = y_axis_limits, breaks = y_axis_breaks, expand = c(0, 0, 0, 0))
  if (show_n) {
    n_anno_x <- n_anno_x %||% if (mean.default(df$y) > Median(df$y)) "right" else "left"
    if (n_anno_x == "right") {
      x_anno <- plot_limits$x[2L]
      hjust <- 1
    } else {
      x_anno <- plot_limits$x[1L]
      hjust <- 0
    }
    x_nudge <- Diff(plot_limits$x)*n_anno_x_nudge
    y_nudge <- Diff(y_axis_limits)*n_anno_y_nudge
    n_anno_font_size <- convert(n_anno_font_size, "pt", "mm")
    p <- p + ggplot2::annotate("text", label = paste0(n_prefix, nrow(df)), x = x_anno + x_nudge, y = y_axis_limits[2L] + y_nudge, hjust = hjust, vjust = 1, color = n_anno_font_color, size = n_anno_font_size)
  }
  suppress(p)
}
