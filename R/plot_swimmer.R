#' Swimmer plot
#'
#' @inheritParams plot_spaghetti
#' @param time_var Column in `df` containing time variable. Enter as quoted variable name. Default is `"time"`
#' @param point_size,point_color,point_border_thickness,point_border_color Point size, color, border thickness, border color
#' @param show_gridlines If `TRUE` (default), grid lines are displayed
#' @param gridline_color,gridline_thickness Line color and thickness for grid lines
#' @param show_y_axis_labels,show_y_axis_ticks If `TRUE`, y axis labels or ticks (indicating values of `id`) are displayed. Default is `FALSE` for both
#' @param theme Plot theme
#' @returns ggplot object
#' @export
plot_swimmer <- function(
    df,
    time_var = "time",
    id = "id",
    point_size = 2,
    point_shape = 21,
    point_color = "#BC3C29",
    point_border_thickness = 0,
    point_border_color = "black",
    line_color = "grey40",
    line_thickness = 0.3,
    show_gridlines = TRUE,
    gridline_thickness = 0.4,
    gridline_color = "grey60",
    gridline_linetype = "dashed",
    x_axis_title = waiver(),
    y_axis_labels = NULL,
    show_y_axis_labels = FALSE,
    show_y_axis_ticks = FALSE,
    expand_x = ggplot2::expansion(c(0.02, 0.02)),
    expand_y = ggplot2::expansion(c(0.02, 0.02)),
    theme = theme_plain(base_size = 14, ...),
    ...) {
  gridlines <- if (show_gridlines) {
    ggplot2::geom_vline(xintercept = seq.int(from = 0, to = 75, by = 15), linetype = gridline_linetype, color = gridline_color, size = gridline_thickness)
  } else {
    NULL
  }
  id <- get_input(id)
  time_var <- get_input(time_var)
  df[[id]] <- fct_reorder(df[[id]], df[[time_var]], Min, .increasing = FALSE)
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(time_var), fixed = TRUE)
  ggplot2::ggplot(df, ggplot2::aes(.data[[time_var]], .data[[id]], group = .data[[id]])) +
    ggplot2::geom_line(size = line_thickness, color = line_color) +
    ggplot2::geom_point(
      shape = point_shape,
      fill = point_color,
      size = point_size,
      stroke = point_border_thickness,
      color = point_border_color) +
    scale_continuous(
      axis = "x",
      title = x_axis_title,
      guide = guide_clean_axis()) +
    ggplot2::scale_y_discrete(
      name = NULL,
      labels = y_axis_labels,
      expand = expand_y) +
    remove_plot_axis(axis = "y", component = c("lines", "title", if (!show_y_axis_labels) "text", if (!show_y_axis_ticks) "ticks")) +
    ggplot2::coord_cartesian(clip = "off") +
    theme
}
