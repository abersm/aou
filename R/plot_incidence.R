#' Frequency plot showing incidence over time
#'
#' @param df Data frame or vector of dates
#' @param date Column containing dates if df is a data frame. Enter as quoted or unquoted variable name
#' @param bar_color_var Variable used to color histogram bars. Enter as string
#' @param colors,bar_colors Color of bars. Default is `"#328EC3"`
#' @param alpha Histogram bar opacity. Default is `0.8`
#' @param bar_border_color Color of lines around histogram bars. Default is black
#' @param bar_border_thickness Thickness of lines around histogram bars. Default is `0.5`
#' @param month_format Options: `"number"` (default), `"name"`, `"abbreviation"`
#' @param date_sep Separator between month, day, and year. Default is `"-"`
#' @param leading_zero If `FALSE` (default), leading 0s are removed from month and day if 1-9
#' @param full_year If `FALSE` (default), last 2 digits are used. If `TRUE`, year is displayed using 4 digits
#' @param n_bins Number of bins for x axis. Default is `20`
#' @param x_axis_title Title for x axis
#' @param expand_x Expansion multiplier for x axis. Default is `0.1` if n_bins > 10 and `0.15` if n_bins <= 10. Only relevant if x is treated as numeric
#' @param x_angle Angle for x axis labels. Default is `45`
#' @param y_axis_title Title for y axis. Default is `"n"`
#' @param show_legend If `TRUE`, legend is displayed
#' @param ... Arguments passed to `theme_custom`
#' @returns ggplot
#' @export
plot_incidence <- function(
    df,
    date = NULL,
    bar_color_var = NULL,
    colors = c("#14B3EC", "#A284BA"),
    bar_colors = colors,
    alpha = 0.8,
    bar_border_color = "#000000",
    bar_border_thickness = 0.5,
    month_format = c("number", "name", "abbreviation"),
    leading_zero = FALSE,
    full_year = FALSE,
    date_sep = "-",
    n_bins = 20,
    x_axis_title = NULL,
    expand_x = if (n_bins > 10) 0.1 else 0.15,
    x_angle = 45,
    y_axis_title = "n",
    show_legend = FALSE,
    annotation_prefix = "N = ",
    ...) {
  plot_fn <- "plot_incidence"
  date <- get_input(date)
  if (!is.data.frame(df)) {
    df <- vec_to_df(date = df[!is.na(df)])
    date <- "date"
  }
  df <- remove_na(df, date)
  month_format <- match.arg(month_format, choices = c("number", "name", "abbreviation"))
  month_format <- switch(month_format, number = "%m", name = "%B", "%b")
  year_format <- if (full_year) "%Y" else "%y"
  date_format <- paste(month_format, if (!leading_zero && month_format != "%m") "%e" else "%d", year_format, sep = date_sep)
  if (!is.null(bar_color_var) && (n_colors <- n_unique(.subset2(df, bar_color_var))) > 1L) {
    colors <- rep(colors, length.out = n_colors)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[date]], fill = .data[[bar_color_var]])) +
      ggplot2::geom_histogram(fill = bar_colors, alpha = alpha, color = bar_border_color, linewidth = bar_border_thickness, bins = n_bins, show.legend = show_legend) +
      ggplot2::scale_fill_manual(name = NULL, values = colors)
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[date]])) +
      ggplot2::geom_histogram(fill = bar_colors[1L], alpha = alpha, color = bar_border_color, linewidth = bar_border_thickness, bins = n_bins, show.legend = show_legend)
  }
  p <- p +
    ggplot2::scale_x_date(x_axis_title, date_labels = date_format, expand = ggplot2::expansion(mult = c(expand_x, expand_x)), oob = rescale_none) +
    theme_custom(x_axis_label_angle = x_angle, ...) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0, margin = ggplot2::margin(r = 15)))
  y <- ggplot2::ggplot_build(p)$data[[1L]]$count
  y_axis_breaks <- pretty(c(0, y))
  y_axis_breaks <- y_axis_breaks[y_axis_breaks %% 1 == 0]
  y_axis_limits <- range(y_axis_breaks)
  p <- p + ggplot2::scale_y_continuous(y_axis_title, limits = y_axis_limits, breaks = y_axis_breaks, expand = c(0, 0, 0, 0))
  p <- p + ggplot2::annotate("text", label = paste0(annotation_prefix, Nrow(df)), x = max(df[[date]]), y = y_axis_limits[2L], hjust = 1, vjust = 1)
  suppress(print(p))
}
