#' Plot line
#'
#' @param df Data frame in long format containing continuous variable (y), categorical or integer variable (x), and grouping variable (grouping_var)
#' @param formula y ~ x format
#' @param grouping_var Variable to group by. Enter as quoted or unquoted variable name
#' @param rev_grouping_var_order If `FALSE` (default), levels of grouping_var ordered alphabetically. If `TRUE`, order of grouping_var levels is reversed
#' @param x,y Variables for x and y axis respectively. Enter as quoted or unquoted variable names
#' @param show_errorbar If `TRUE` (default), error bars are displayed. If `FALSE`, error bars are not displayed
#' @param summary_fn Statistical function used to as summary estimate. Default is `Mean`
#' @param error_fn Statistical function used to calculate error. Default is `SE`
#' @param colors Color used for points and lines
#' @param show_points If `TRUE` (default), points are displayed
#' @param point_shape Shape of points. Options include `"square"`, `"circle"`, `"triangle"`, `"diamond"`, or list of shapes for each group. Enter as quoted or unquoted shape or list of shapes using `c()`
#' @param point_size Point size. Default is `3`
#' @param point_colors Fill color for points. Hexadecimal or quoted color names. Enter using `c()`
#' @param point_alpha Alpha value for point fill. Default is `1`
#' @param alpha Alias for point_alpha
#' @param point_border_thickness Thickness of line surrounding points in pt units. Default is `1`
#' @param point_border_colors Color for line surrounding points. Default is `"black"`
#' @param line_type Type of line. Options include `"solid"` (default), `"dashed"`, `"longdash"`, `"twodash"`, `"dotdash"`, `"dotted"`. Enter as quoted linetype
#' @param line_colors Color for lines. Hexadecimal or quoted color names. Default is `"black"`
#' @param line_thickness Thickness of line in mm. Default is `0.75`
#' @param errorbar_width Width of error bars. Default is `point_size/6`
#' @param errorbar_thickness Thickness of error bars. Default is `0.75`
#' @param errorbar_color Color of error bars. Default is `"black"`
#' @param y_scale Scaling for y-axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param y_axis_breaks Numeric vector or function specifying location of ticks along y axis
#' @param x_axis_breaks Numeric vector or function specifying location of ticks along x axis
#' @param y_min Minimum value to use for y axis Default is `0`
#' @param y_max Maximum y axis value. Default is `max(df$y)`
#' @param expand_y Expansion of y axis around y = 0. Default is `0`
#' @param expand_x Argument entered for `expand` argument of `scale_x_continuous`
#' @param y_axis_labels Vector or function specifying y axis tick labels
#' @param x_axis_labels Vector or function specifying x axis tick labels
#' @param y_axis_title Enter as quoted string
#' @param x_axis_title Enter as quoted string
#' @param plot_title Text to be used as main plot title. Enter as quoted string. Default is `""` (i.e. no title)
#' @param n_breaks Number of tick marks for y axis. Default is `3`
#' @param show_legend If `TRUE`, legend displayed. If `FALSE`, legend not displayed
#' @param show_sig If `TRUE` (default), significance annotation displayed. If `FALSE`, no significance annotation displayed
#' @param stars If `TRUE` (default), significance stars are displayed rather than actual P values
#' @param sig_method Options: `"p_by_normality"` (default), `"mann_whitney"`, `"t_test_welch"`, `"t_test_no_welch"`. Enter as quoted test type
#' @param show_ns If `FALSE` (default), no annotation for P > 0.05. If `TRUE`, non-significant comparisons displayed
#' @param ns_symbol Symbol to use when P > 0.05. Only used if show_ns is TRUE. default is `"ns"`
#' @param p_case Case of p used in significance annotations when stars is FALSE. Options: `"upper"` (default) or `"lower"`. Enter as quoted text
#' @param p_spaces If `TRUE` (default), spaces placed between p, equality symbol, and p-value in significance annotation. If `FALSE`, no spaces placed between p, equality symbol, and p-value in significance annotation. Only relevant when stars is `FALSE`
#' @param sig_star_nudge Nudge factor along y axis for significance stars. Distance between point or bar and significance stars is `sig_star_nudge` x maximum y value on plot. Default is `0.05`
#' @param sig_text_nudge Nudge factor along y axis for significance annotation text. Distance between point or bar and significance annotation text is sig_text_nudge*maximum y value on plot. Default is `0.09`
#' @param sig_text_size Font size for P values in pts. Only used when stars is `FALSE`. Default is `12`
#' @param sig_star_size Size of significance stars. Only used when stars is `TRUE.` Default is `20`
#' @param censor_fn Function used to transform data outside y axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_line_mean <- function(
  df,
  formula = NULL,
  grouping_var = NULL,
  rev_grouping_var_order = FALSE,
  x = NULL, y = NULL,
  show_errorbar = TRUE,
  summary_fn = Mean,
  error_fn = SE,
  colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  show_points = TRUE,
  point_colors = colors,
  point_shape = "circle",
  point_size = 3.5,
  point_border_thickness = 1,
  point_border_colors = "black",
  alpha = 1,
  point_alpha = alpha,
  line_colors = point_colors,
  line_thickness = 0.75,
  line_type = "solid",
  errorbar_color = "black",
  errorbar_thickness = 0.7,
  errorbar_width = point_size/20,
  show_legend = FALSE,
  y_scale = "regular",
  x_axis_title = waiver(),
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  x_axis_labels = waiver(),
  y_axis_title = waiver(),
  x_axis_breaks = waiver(),
  expand_y = 0,
  expand_x = waiver(),
  plot_title = NULL,
  n_breaks = 3,
  show_sig = TRUE,
  stars = TRUE,
  show_ns = TRUE,
  ns_symbol = "ns",
  sig_star_size = 20,
  sig_text_size = 12,
  sig_star_nudge = 0.05,
  sig_text_nudge = 0.09,
  sig_method = "p_by_normality",
  p_case = "upper",
  p_spaces = TRUE,
  y_min = NULL,
  y_max = NULL,
  censor_fn = rescale_none,
  ...) {
  plot_fn <- "plot_line_mean"

  # Data setup
  df <- dplyr::select(df, grouping_var = {{grouping_var}}, dplyr::everything())
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = "grouping_var")
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_line")
  x <- vars$x
  y <- vars$y
  df$grouping_var <- df[["grouping_var"]] %||% "a"
  df <- df[order(df$x, df$grouping_var), ]
  grouping_var_levels <- create_levels(df$grouping_var, reverse = rev_grouping_var_order)
  df$grouping_var <- as.character(df$grouping_var)
  df$grouping_var <- factor(df$grouping_var, levels = grouping_var_levels)
  n_groups <- n_unique(df$grouping_var, na.rm = FALSE)
  #errorbar_width <- errorbar_width*(0.64 - 0.03*n_unique(df$x, na.rm = FALSE))
  line_thickness <- line_thickness/0.75

  # Axis titles
  x_axis_title <- x_axis_title %W% gsub(pattern = "_", replacement = " ", x = str_capitalize(x), fixed = TRUE)
  y_axis_title <- y_axis_title %W% gsub(pattern = "_", replacement = " ", x = str_capitalize(y), fixed = TRUE)

  # Determine if df consists of 1 value for each x (i.e., a simple line plot)
  n_time_pts <- length(unique.default(df$x))
  time_pts_per_group <- vapply(split.default(df$x, df$grouping_var), any_dupes, logical(1))
  if (!any(time_pts_per_group)) {
    p <- ggplot2::ggplot(df, aes(x = x, y = y, group = grouping_var))
    # Line
    line_colors <- rep(line_colors, length.out = n_groups)
    p <- p +
      ggplot2::geom_line(
        mapping = ggplot2::aes(color = grouping_var),
        linewidth = line_thickness,
        linetype = line_type,
        show.legend = show_legend) +
      ggplot2::scale_color_manual(name = NULL, values = line_colors)

    # Points
    if (show_points) {
      point_colors <- rep(point_colors, length.out = n_groups)
      point_shape <- switch(point_shape, circle = 21, square = 22, triangle = 24, diamond = 23)
      point_shape <- rep(point_shape, length.out = n_groups)
      p <- p +
        ggplot2::geom_point(
          mapping = ggplot2::aes(fill = grouping_var, shape = grouping_var),
          size = point_size,
          stroke = point_border_thickness,
          show.legend = FALSE) +
        ggplot2::scale_fill_manual(name = NULL, values = point_colors) +
        ggplot2::scale_shape_manual(name = NULL, values = point_shape)
    }

    # x axis
    x_max <- max(df$x, x_axis_breaks %W% NULL, na.rm = TRUE)
    p <- p + ggplot2::scale_x_continuous(name = x_axis_title, limits = c(NA, x_max), breaks = x_axis_breaks, labels = x_axis_labels, expand = expand_x, n.breaks = 6)

    # y axis
    y_limits <- range(df$y)
    p <- p +
      scale_continuous(axis = "y", limits = c(y_min %||% y_limits[1L], y_max %||% y_limits[2L]), scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn) +
      ggplot2::ggtitle(plot_title) +
      ggplot2::coord_cartesian(clip = "off") +
      theme_custom(...)
    return(p)
  }

  # Data for plotting
  if (show_sig && n_groups > 1) {
    df_pvalues <- tryCatch(.create_df_sig_anno_grouped(df, x = "x", show_ns = show_ns, ns_symbol = ns_symbol), error = function(e) empty_df())
    if (length(df_pvalues) == 0) {
      show_sig <- FALSE
    }
  } else {
    show_sig <- FALSE
  }
  df_summary <- dplyr::group_by(df, x, grouping_var)
  df_summary <- dplyr::summarize(df_summary, s = error_fn(y), y = summary_fn(y))
  df_summary <- dplyr::mutate(df_summary, s = ifelse(is.na(s), 0, s), ymin = y - s, ymax = y + s)

  # Plot
  p <- ggplot2::ggplot(df_summary, aes(x = x, y = y, group = grouping_var))

  # Error bar
  if (show_errorbar) {
    p <- p + ggplot2::geom_errorbar(mapping = ggplot2::aes(x = x, ymin = ymin, ymax = ymax, group = grouping_var), width = errorbar_width, show.legend = FALSE, size = errorbar_thickness, color = errorbar_color)
  }

  # Line
  line_colors <- rep(line_colors, length.out = n_groups)
  p <- p +
    ggplot2::geom_line(
      data = df_summary,
      mapping = ggplot2::aes(group = grouping_var, color = grouping_var),
      linewidth = line_thickness,
      linetype = line_type,
      show.legend = show_legend) +
    ggplot2::scale_color_manual(name = NULL, values = line_colors)

  # Points
  if (show_points) {
    point_colors <- rep(point_colors, length.out = n_groups)
    point_shape <- switch(point_shape, circle = 21, square = 22, triangle = 24, diamond = 23)
    point_shape <- rep(point_shape, length.out = n_groups)
    p <- p +
      ggplot2::geom_point(
        mapping = ggplot2::aes(fill = grouping_var, shape = grouping_var),
        size = point_size,
        stroke = point_border_thickness,
        show.legend = FALSE) +
      ggplot2::scale_fill_manual(name = NULL, values = point_colors) +
      ggplot2::scale_shape_manual(name = NULL, values = point_shape)
  }

  # x axis
  p <- p + ggplot2::scale_x_continuous(name = x_axis_title, breaks = x_axis_breaks, expand = expand_x, labels = x_axis_labels, n.breaks = 6)

  # y axis
  df_summary$ymin <- if (show_errorbar) df_summary$ymin else df_summary$y
  df_summary$ymax <- if (show_errorbar) df_summary$ymax else df_summary$y
  df_summary <- dplyr::group_by(df_summary, x)
  df_summary <- dplyr::summarize(df_summary, min = Min(ymin), max = Max(ymax))
  y_limits <- .set_axis_limits(y_min, y_max, y_axis_breaks, c(df_summary$min, df_summary$max))
  y_max <- max(y_limits)
  p <- p +
    scale_continuous(axis = "y", limits = y_limits, scale = y_scale, n_breaks = n_breaks, title = y_axis_title, breaks = y_axis_breaks, labels = y_axis_labels, expand_lower = expand_y, censor_fn = censor_fn) +
    theme_custom(...)

  # Significance annotation
  if (show_sig) {
    if (!show_ns) {
      df_pvalues <- df_pvalues[df_pvalues$p < 0.05, ]
    }
    if (nrow(df_pvalues) >= 0) {
      df_pvalues <- dplyr::left_join(df_pvalues[, c("x", "p", "label")], df_summary, by = "x")
      if (startsWith(y_scale, "log")) {
        sig_star_nudge <- 0.75
        sig_text_nudge <- 1.75
      }
      df_pvalues$y_label <- ifelse(grepl("*", df_pvalues$label, fixed = TRUE), df_pvalues$max + y_max*sig_star_nudge, df_pvalues$max + y_max*sig_text_nudge)

      if (any(df_pvalues$p < 0.05)) {
        df_pvalues_signif <- df_pvalues[df_pvalues$p < 0.05, ]
        df_pvalues <- df_pvalues[df_pvalues$p >= 0.05, ]
        p <- p + ggplot2::geom_text(data = df_pvalues_signif, mapping = ggplot2::aes(x = x, y = y_label, label = label), size = sig_star_size, size.unit = "pt", color = "#000000", na.rm = TRUE, show.legend = FALSE, vjust = 0, hjust = 0.5, inherit.aes = FALSE)
      }
      if (nrow(df_pvalues) > 0) {
        p <- p + ggplot2::geom_text(data = df_pvalues, mapping = ggplot2::aes(x = x, y = y_label, label = label), size = sig_text_size, size.unit = "pt", color = "#000000", na.rm = TRUE, show.legend = FALSE, vjust = 0, hjust = 0.5, inherit.aes = FALSE)
      }
    }
  }
  p +
    ggplot2::ggtitle(plot_title) +
    ggplot2::coord_cartesian(clip = "off")
}
