#' Custom ggplot2 theme
#'
#' @param base_size Size of text in pts. Default is `16`
#' @param font_family Font used in plots. Default uses helvetica
#' @param aspect_ratio Aspect ratio, y/x. Default is `1`
#' @param ratio Alias for `aspect_ratio`
#' @param axis_tick_length Axis tick length in pts. Default is `base_size/2.5`
#' @param axis_line_thickness Axis and tick thickness in mm. Default is `0.7`
#' @param plot_margin_top Extra space added above plot in pts. Default is `base_size/2`
#' @param plot_margin_bottom Extra space added below plot in pts. Default is `base_size/2`
#' @param plot_margin_right Extra space added to right of plot in pts. Default is `base_size/2`
#' @param plot_margin_left Extra space added to left of plot in pts. Default is `base_size/2`
#' @param plot_title_font_face Title font face. Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param plot_title_font_color Title font color. Default is `"black"`
#' @param plot_title_font_size Title font size in pts. Default is `base_size + 2`
#' @param x_axis_title_font_size,y_axis_title_font_size Default is `base_size + 2`
#' @param x_axis_title_margin_top Distance from x axis to x axis title in pts. Default is `0.5*base_size`
#' @param x_axis_title_hjust,y_axis_title_hjust Horizontal justification for axis titles. Enter as numeric. Default is `0.5`
#' @param x_axis_label_font_face,y_axis_label_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param x_axis_label_angle,y_axis_label_angle Angle of x and y axis tick labels. Default is `0`
#' @param x_angle,y_angle Alias for `x_axis_label_angle` and `y_axis_label_angle`, respectively
#' @param x_axis_label_hjust,x_axis_label_vjust Horizontal and vertical justification of x axis text. Default is `0.5`
#' @param x_axis_label_font_size,y_axis_label_font_size Axis tick label font size in pts. Default is `18`
#' @param x_axis_label_margin_top Margin between x axis tick labels to tick mark in pts. Default is `0.3*base_size`
#' @param x_axis_label_margin_right Margin to the right of x axis tick labels. Default is `0`
#' @param y_axis_title_angle Angle of text used for y axis title. Default is 90
#' @param y_axis_title_margin_right Distance from y axis to x axis title in pts. Default is `0.5*base_size`
#' @param y_axis_label_margin_right Margin between y axis tick labels to tick mark. Units in pts. Default is `0.3*base_size`
#' @param legend_position Legend position. Default is `"right"`
#' @param legend_direction Legend direction. Options: `"vertical"` (default), `"horizontal"`
#' @param legend_title_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param legend_title_font_color Font color of legend title. Default is `"black"`
#' @param legend_title_font_size Units in pts. Default is `base_size`
#' @param legend_label_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param legend_label_font_color Font color of legend labels Default is `"black"`
#' @param legend_label_font_size Units in pts. Default is `base_size`
#' @param legend_margin Space between text labels in legend. Units in pts. Default is `base_size/2`
#' @param legend_symbol_size Size of symbols in legend. Units in pts. Default is `base_size*0.75`
#' @param legend_spacing Spacing between legend labels. Units in pts. Default is `1`
#' @param facet_font_face Options: `"plain"` (default), `"italic"`, `"bold"`, `"bold.italic"`
#' @param facet_font_color Default is black
#' @param facet_fill_color Facet box fill color
#' @param facet_border_thickness Line thickness for facet border
#' @param facet_spacing_x,facet_spacing_y Horizontal and vertical spacing between facets. Units in pts. Default is `base_size/2`
#' @export
theme_custom <- function(
  base_size = 16,
  font_family = "",
  aspect_ratio = 1,
  ratio = aspect_ratio,
  axis_tick_length = 0.4*base_size,
  axis_line_thickness = 0.7,
  plot_margin_top = 1.25*base_size,
  plot_margin_bottom= 0.5*base_size,
  plot_margin_left = 0.5*base_size,
  plot_margin_right = 0.5*base_size,
  plot_title_font_face = "plain",
  plot_title_font_color = "black",
  plot_title_font_size = base_size + 2,
  x_axis_title_font_size = base_size + 2,
  x_axis_label_font_size = base_size,
  y_axis_title_font_size = base_size + 2,
  y_axis_label_font_size = base_size,
  x_axis_title_margin_top = 0.5*base_size,
  x_axis_title_hjust = 0.5,
  x_axis_label_margin_top = 0.3*base_size,
  x_axis_label_margin_right = 0,
  y_axis_title_angle = 90,
  y_axis_title_margin_right = 0.5*base_size,
  y_axis_title_hjust = 0.5,
  y_axis_label_margin_right = 0.3*base_size,
  x_axis_label_font_face = "plain",
  x_axis_label_angle = 0,
  x_angle = x_axis_label_angle,
  x_axis_label_vjust = 0.5,
  x_axis_label_hjust = 0.5,
  y_axis_label_font_face = "plain",
  y_axis_label_angle = 0,
  y_angle = y_axis_label_angle,
  legend_position = "right",
  legend_title_font_face = "plain",
  legend_title_font_color = "black",
  legend_title_font_size = base_size,
  legend_label_font_face = "plain",
  legend_label_font_color = "black",
  legend_label_font_size = base_size,
  legend_direction = "vertical",
  legend_margin = 0.5*base_size,
  legend_symbol_size = 0.75*base_size,
  legend_spacing = 1,
  facet_font_face = "plain",
  facet_font_color = "#000000",
  facet_fill_color = "#DEDEDE",
  facet_spacing_x = 0.5*base_size,
  facet_spacing_y = 0.5*base_size,
  facet_border_thickness = axis_line_thickness) {
  if (x_angle > 0) {
    x_axis_label_hjust <- 1
    x_axis_label_vjust <- 1
  }
  blank <- ggplot2::element_blank()
  zero_margin <- ggplot2::margin()

  ggplot2::theme(
    # Lines
    line = ggplot2::element_line(
      color = "black",
      linewidth = axis_line_thickness,
      linetype = 1,
      lineend = "square"
    ),

    # Rectangles
    rect = ggplot2::element_rect(
      fill = "transparent",
      color = "black",
      linewidth = axis_line_thickness,
      linetype = 1
    ),

    # Text
    text = ggplot2::element_text(
      family = font_family,
      face = "plain",
      color = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = zero_margin,
      debug = FALSE
    ),

    # Axis
    # Axis lines
    axis.line = ggplot2::element_line(
      color = "black",
      linewidth = axis_line_thickness,
      linetype = 1,
      lineend = "square"
    ),

    # Axis ticks
    axis.ticks = ggplot2::element_line(
      color = "black",
      linewidth = axis_line_thickness,
      linetype = 1,
      lineend = "square"
    ),
    axis.ticks.length = grid::unit(axis_tick_length, "pt"),

    # Axis text
    axis.text.x = ggplot2::element_text(
      face = x_axis_label_font_face,
      color = "black",
      angle = x_angle,
      size = x_axis_label_font_size,
      margin = ggplot2::margin(t = x_axis_label_margin_top, r = x_axis_label_margin_right, unit = "pt"),
      vjust = x_axis_label_vjust,
      hjust = x_axis_label_hjust
    ),
    axis.text.y = ggplot2::element_text(
      face = y_axis_label_font_face,
      color = "black",
      angle = y_angle,
      size = y_axis_label_font_size,
      margin = ggplot2::margin(r = y_axis_label_margin_right, unit = "pt"),
      hjust = 1
    ),

    # Axis title
    axis.title.x = ggplot2::element_text(
      color = "black",
      size = x_axis_title_font_size,
      angle = 0,
      margin = ggplot2::margin(t = x_axis_title_margin_top, unit = "pt"),
      vjust = 0.5,
      hjust = x_axis_title_hjust
    ),
    axis.title.y = ggplot2::element_text(
      color = "black",
      size = y_axis_title_font_size,
      angle = y_axis_title_angle,
      margin = ggplot2::margin(r = y_axis_title_margin_right, unit = "pt"),
      vjust = 0.5,
      hjust = y_axis_title_hjust
    ),

    # Legend
    legend.position = legend_position,
    legend.direction = legend_direction,
    legend.justification = "center",
    legend.background = blank,
    # Space between legends
    legend.spacing = grid::unit(legend_spacing, "pt"),
    # Margin around each legend
    legend.margin = ggplot2::margin(legend_margin, legend_margin, legend_margin, legend_margin, unit = "pt"),
    legend.key = blank,
    legend.key.height = grid::unit(legend_symbol_size, unit = "pt"),
    legend.key.width = grid::unit(legend_symbol_size, unit = "pt"),
    legend.title = ggplot2::element_text(
      face = legend_title_font_face,
      color = "black",
      size = legend_title_font_size,
      hjust = 0,
      margin = zero_margin
    ),
    legend.text = ggplot2::element_text(
      color = legend_label_font_color,
      face = legend_label_font_face,
      size = legend_label_font_size,
      hjust = 0,
      margin = zero_margin
    ),
    # Space between plot area and box around legend
    legend.box.spacing = grid::unit(base_size, "pt"),
    legend.box = NULL,
    legend.box.margin = zero_margin,
    legend.box.background = blank,

    # Panel (plotting area bounded by axes)
    panel.spacing.x = grid::unit(facet_spacing_x, "pt"),
    panel.spacing.y = grid::unit(facet_spacing_y, "pt"),
    panel.background = blank,
    panel.border = blank,
    panel.grid = blank,
    panel.ontop = FALSE,

    # Strip
    strip.background = ggplot2::element_rect(
      fill = facet_fill_color,
      color = "black",
      linewidth = facet_border_thickness
    ),
    strip.text = ggplot2::element_text(
      face = facet_font_face,
      color = facet_font_color,
      size = base_size,
      margin = ggplot2::margin(0.4*base_size, 0.4*base_size, 0.4*base_size, 0.4*base_size, unit = "pt")
    ),
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.switch.pad.grid = grid::unit(0.25*base_size, "pt"),
    strip.switch.pad.wrap = grid::unit(0.25*base_size, "pt"),
    strip.placement = "inside",

    # Plot (area including panel area, plot title, axis title, legend)
    plot.background = blank,
    plot.title = ggplot2::element_text(
      face = plot_title_font_face,
      color = plot_title_font_color,
      size = plot_title_font_size,
      hjust = 0.5,
      vjust = 1,
      margin = ggplot2::margin(b = 0.5*base_size, unit = "pt")
    ),
    plot.margin = ggplot2::margin(
      t = plot_margin_top,
      r = plot_margin_right,
      b = plot_margin_bottom,
      l = plot_margin_left,
      unit = "pt"
    ),
    complete = TRUE
  ) +
    ggplot2::theme(aspect.ratio = ratio)
}

#' Theme with transparent background and no grid lines
#'
#' @inheritParams theme_custom
#' @param x_axis_label_angle Angle for x axis tick labels
#' @param x_angle Alias for `x_axis_label_angle`
#' @param ... Arguments passed to `theme()`
#' @export
theme_plain <- function(
    base_size = 14,
    axis_line_thickness = 0.7,
    aspect_ratio = NULL,
    y_axis_title_angle = 90,
    x_axis_label_angle = 0,
    x_angle = x_axis_label_angle,
    x_axis_label_hjust = 0.5,
    x_axis_label_vjust = 0.5,
    x_axis_label_margin_right = 0,
    x_axis_title_font_size = base_size + 2,
    y_axis_title_font_size = base_size + 2,
    ...) {
  if (x_angle > 0) {
    x_axis_label_hjust <- 1
    x_axis_label_vjust <- 1
  }
  ggplot2::theme(
    text = ggplot2::element_text(size = base_size, color = "black"),
    rect = ggplot2::element_blank(),
    line = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    plot.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = base_size, color = "black"),
    axis.line = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    axis.ticks = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    axis.ticks.length = grid::unit(0.4*base_size, "pt"),
    axis.text.x = ggplot2::element_text(
      face = "plain",
      color = "black",
      angle = x_angle,
      size = base_size,
      margin = ggplot2::margin(t = 0.3*base_size, r = x_axis_label_margin_right, unit = "pt"),
      vjust = x_axis_label_vjust,
      hjust = x_axis_label_hjust
    ),
    axis.text.y = ggplot2::element_text(
      face = "plain",
      color = "black",
      angle = 0,
      size = base_size,
      margin = ggplot2::margin(r = 0.3*base_size, unit = "pt"),
      hjust = 1
    ),
    axis.title.x = ggplot2::element_text(
      color = "black",
      size = x_axis_title_font_size,
      angle = 0,
      margin = ggplot2::margin(t = base_size/3, unit = "pt"),
      vjust = 0.5
    ),
    axis.title.y = ggplot2::element_text(
      color = "black",
      size = y_axis_title_font_size,
      angle = 90,
      margin = ggplot2::margin(r = 0.5*base_size, unit = "pt"),
      vjust = 0.5
    ),
    aspect.ratio = aspect_ratio,
    complete = TRUE,
    ...
  )
}

#' Alias for `theme_plain`
#'
#' @rdname theme_plain
#' @export
theme_basic <- theme_plain

#' Blank/empty theme
#'
#' No grid lines, axis lines/ticks, axis labels/titles, background color, border, or legend
#' @param ... Arguments passed to `ggplot2::theme`
#' @export
theme_blank <- function(...) {
  blank <- ggplot2::element_blank()
  ggplot2::theme(
    line = blank,
    text = blank,
    rect = blank,
    panel.border = blank,
    legend.position = "none",
    ...
  )
}

#' Alias for theme_blank
#'
#' @rdname theme_blank
#' @export
theme_empty <- theme_blank

#' Plot theme for FACS data
#'
#' Slightly modified version of `theme_plain`
#' @inheritParams theme_plain
#' @param boxed If `TRUE` (default), rectangle drawn around plotting area
#' @param plot_margin Plot margin. Enter as `margin()`. Default is `margin(t = 10, r = 20)`
#' @param facet_fill_color Color of facet boxes. Default is `"#D9D9D9"`
#' @param facet_line_color Color of facet box border. Default is `"black"`
#' @param facet_line_type Line type of facet box border. Default is `1` (solid line)
#' @param facet_line_thickness Line thickness of facet box border. Default uses `axis_line_thickness`
#' @param facet_text_size,facet_text_color,facet_text_face Text style for facet labels
#' @param facet_text_margin Margin around facet labels. Enter as `margin()`. Default is `margin(t = 5, b = 5)`
#' @export
theme_facs <- function(
    base_size = 14,
    boxed = TRUE,
    axis_line_thickness = 0.4,
    aspect_ratio = 1,
    y_axis_title_angle = 90,
    x_axis_label_angle = 0,
    x_angle = x_axis_label_angle,
    x_axis_label_hjust = 0.5,
    x_axis_label_vjust = 0.5,
    x_axis_label_margin_right = 0,
    x_axis_title_font_size = base_size + 2,
    y_axis_title_font_size = base_size + 2,
    plot_margin = ggplot2::margin(t = 10, r = 20),
    facet_fill_color = "#D9D9D9",
    facet_line_color = "black",
    facet_line_type = 1,
    facet_line_thickness = axis_line_thickness,
    facet_text_size = base_size,
    facet_text_color = NULL,
    facet_text_face = "plain",
    facet_text_margin = ggplot2::margin(t = 5, b = 5),
    ...) {
  if (x_angle > 0) {
    x_axis_label_hjust <- 1
    x_axis_label_vjust <- 1
  }
  blank <- ggplot2::element_blank()
  boxed <- if (boxed) {
    ggplot2::element_rect(fill = NA, color = "black", linewidth = axis_line_thickness, linetype = 1)
  } else {
    blank
  }
  if (is.null(facet_text_color)) {
    facet_text_color <- if (is.na(facet_fill_color) || facet_fill_color == "transparent") "black" else clr_text(facet_fill_color)
  }
  ggplot2::theme(
    text = ggplot2::element_text(size = base_size, color = "black"),
    line = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    plot.background = blank,
    panel.grid = blank,
    panel.background = boxed,
    axis.text = ggplot2::element_text(size = base_size, color = "black"),
    axis.line = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    axis.ticks = ggplot2::element_line(color = "black", linewidth = axis_line_thickness, linetype = 1, lineend = "square"),
    axis.ticks.length = grid::unit(0.4*base_size, "pt"),
    axis.text.x = ggplot2::element_text(
      face = "plain",
      color = "black",
      angle = x_angle,
      size = base_size,
      margin = ggplot2::margin(t = 0.3*base_size, r = x_axis_label_margin_right, unit = "pt"),
      vjust = x_axis_label_vjust,
      hjust = x_axis_label_hjust
    ),
    axis.text.y = ggplot2::element_text(
      face = "plain",
      color = "black",
      angle = 0,
      size = base_size,
      margin = ggplot2::margin(r = 0.3*base_size, unit = "pt"),
      hjust = 1
    ),
    axis.title.x = ggplot2::element_text(
      color = "black",
      size = x_axis_title_font_size,
      angle = 0,
      margin = ggplot2::margin(t = base_size/3, unit = "pt"),
      vjust = 0.5
    ),
    axis.title.y = ggplot2::element_text(
      color = "black",
      size = y_axis_title_font_size,
      angle = 90,
      margin = ggplot2::margin(r = 0.5*base_size, unit = "pt"),
      vjust = 0.5
    ),
    strip.background = ggplot2::element_rect(
      fill = facet_fill_color,
      linewidth = facet_line_thickness,
      linetype = facet_line_type,
      color = facet_line_color
    ),
    strip.text = ggplot2::element_text(
      size = facet_text_size,
      color = facet_text_color,
      face = facet_text_face,
      margin = facet_text_margin
    ),
    aspect.ratio = aspect_ratio,
    plot.margin = plot_margin,
    complete = TRUE,
    ...
  )
}
