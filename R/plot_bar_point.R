#' Bar plot with overlying points
#'
#' @inheritParams plot_bar
#' @inheritParams plot_point
#' @param color_var Variable used to color points. Default uses `grouping_var.` Enter as quoted or unquoted variable name
#' @param bar_border_color Color for bar border. Default is `"black"`
#' @param show_legend_points If `FALSE` (default) legend for points is not displayed
#' @param show_legend_bars If `FALSE` (default) legend for bars is not displayed
#' @param small_points If `TRUE`, small points are plotted in 1 color for all groups. If `FALSE`, plots are pointed as usual
#' @param show_points If `TRUE` (default), points are displayed. If `FALSE`, points not displayed in plot
#' @returns ggplot object
#' @export
plot_bar_point <- function(
  df,
  formula = NULL,
  grouping_var = NULL,
  grouping_var_order = NULL,
  rev_grouping_var_order = FALSE,
  x_order = NULL,
  rev_x_order = FALSE,
  x = NULL,
  y = NULL,
  beeswarm = FALSE,
  beeswarm_method = "smiley",
  width = 0.2,
  beeswarm_width = width,
  n_bins = NULL,
  band_width = 1 + n_bins/5,
  dodge = NULL,
  jitter_width = width,
  colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  color_var = NULL,
  point_shapes = c("circle", "circle"),
  point_size = 5,
  show_points = TRUE,
  small_points = point_size == 0,
  point_alpha = 0.9,
  bar_alpha = 0,
  bar_border_thickness = 0.75,
  point_border_thickness = 1,
  bar_border_color = "black",
  point_border_color = "black",
  bar_width = NULL,
  point_shape_var = NULL,
  show_errorbar = TRUE,
  summary_fn = Mean,
  error_fn = SE,
  errorbar_width_multiplier = 0.5,
  show_legend = FALSE,
  show_legend_points = show_legend,
  show_legend_bars = show_legend,
  legend_title = "",
  y_scale = "regular",
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  x_axis_breaks = NULL,
  x_axis_labels = NULL,
  y_title = waiver(),
  y_axis_title = y_title,
  x_title = waiver(),
  x_axis_title = x_title,
  plot_title = NULL,
  show_sig = TRUE,
  stars = TRUE,
  sig_font_color = "black",
  sig_bar_color = "black",
  sig_bar_thickness = 0.6,
  show_ns = TRUE,
  ns_symbol = "ns",
  sig_star_size = 20,
  sig_text_size = 12,
  sig_bar_nudge = 0.06,
  sig_star_nudge = sig_bar_nudge*0.75,
  sig_text_nudge = sig_bar_nudge*1.35,
  step_increase = if (grepl("^log", y_scale)) 0 else sig_bar_nudge + 0.05,
  sig_method = "p_by_normality",
  p_case = "upper",
  p_spaces = TRUE,
  n_breaks = 4,
  breaks_fn = pretty,
  y_max = NULL,
  y_min = 0,
  expand_y = 0,
  expand_x = waiver(),
  censor_fn = rescale_none,
  show_all = TRUE,
  seed = 1234,
  ...) {
  dots <- list(...)
  if (!is.null(angle <- c(dots$x_axis_label_angle, dots$x_angle)) && angle[1L] != 0 && missing(x_axis_title) && missing(x_title)) {
    x_axis_title <- NULL
  }
  plot_fn <- "plot_bar_point"
  grouping_var <- get_input(grouping_var)
  color_var <- get_input(color_var)
  point_shape_var <- get_input(point_shape_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  x <- vars$x
  y <- vars$y
  if (small_points && missing(bar_alpha)) {
    bar_alpha <- 1
  }

  # Plot data
  if (is.null(y)) {
    if (is.atomic(df)) {
      df <- vec_to_df(y = df, x = "")
      y <- "y"
      x <- "x"
      x_axis_title <- x_axis_title %W% NULL
      y_axis_title <- y_axis_title %W% NULL
    } else {
      y <- vars_numeric(df)
      if (length(y) != 1L) {
        Stop(sprintf("In %s, must specify y axis variable using 'y' or 'formula' (y ~ x)", plot_fn))
      }
    }
  }
  if (is.null(x)) {
    Warning(sprintf("In %s, 'x' not specified", plot_fn))
    x_axis_title <- x_axis_title %W% NULL
    df$x <- ""
    x <- "x"
  }
  df <- df[stats::complete.cases(df[c(x, y)]), , drop = FALSE]
  x_vals <- .subset2(df, x)
  x_levels <- x_order %||% create_levels(x_vals, reverse = rev_x_order)
  df$x_numeric <- match(x_vals, x_levels, incomparables = NA_integer_)
  x_unique <- unique(df$x_numeric)

  # Grouping variable
  df$grouping_var <- .new_cat_var(df, var = grouping_var, levels = grouping_var_order, reverse = rev_grouping_var_order)
  # Don't use length(levels(df$grouping_var)) in next step in case df$grouping_var contains NAs
  n_groups <- length(unique(df$grouping_var))
  if (n_groups == 1L) {
    beeswarm <- FALSE
    bar_width <- bar_width %||% 0.4
  }

  # color_var, colors, point_shape_var
  df$color_var <- .new_cat_var(df, var = color_var, if_null = df$grouping_var)
  # Don't use length(levels(df$color_var)) in next step in case df$color_var contains NAs
  n_colors <- length(unique(df$color_var))
  colors <- rep(colors, length.out = n_colors)
  df$point_shape_var <- .new_cat_var(df, var = point_shape_var)

  # Change y values if log scale is used
  y_vals <- .subset2(df, y)
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = y_vals)
  if (startsWith(y_scale, "log")) {
    df[[y]] <- y_vals <- ifelse(y_vals == 0, 1, y_vals)
    if (Min(y_limits) <= 0) {
      y_limits[1L] <- 1
    }
    sig_bar_nudge <- 1
    sig_star_nudge <- 0.75
    sig_text_nudge <- 1.75
  }

  # Bars
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_numeric, y = .data[[y]], group = grouping_var))

  ## Bar legend
  if (!show_points && missing(bar_alpha)) {
    bar_alpha <- 1L
  }
  if (show_legend_bars && bar_alpha == 0) {
    show_legend_bars <- FALSE
  }
  ## Bar width/dodge
  dodge <- dodge %||% if (beeswarm) 0.8 else 0.7
  bar_width <- bar_width %||% (0.64 - 0.03*length(x_unique))
  p <- p +
    ggplot2::stat_summary(
      mapping = ggplot2::aes(fill = color_var, group = grouping_var),
      fun = summary_fn,
      geom = "bar",
      position = ggplot2::position_dodge(width = dodge),
      width = bar_width,
      size = bar_border_thickness,
      color = bar_border_color,
      alpha = bar_alpha,
      show.legend = show_legend_bars
    ) +
    ggplot2::scale_fill_manual(name = legend_title, values = colors)

  # Error bars
  if (show_errorbar) {
    p <- p + errorbar(summary_fn = summary_fn, error_fn = error_fn, line_thickness = bar_border_thickness, width = errorbar_width_multiplier*bar_width, dodge = dodge)
  }

  # Points
  if (show_points) {
    point_args <- list(
      size = point_size,
      alpha = point_alpha,
      stroke = point_border_thickness,
      show.legend = show_legend_points
    )

    ## Point shape
    if (small_points) {
      if (show_legend_points && missing(show_legend_points)) {
        point_args$show.legend <- FALSE
      }
      point_border_thickness <- 0
      if (missing(point_size)) {
        point_args$size <- 1
      }
      if (missing(point_alpha)) {
        point_args$alpha <- 1
      }
      point_args$color <- "#000000"
      look_up_point_shape <- c(circle = 19, square = 15, triangle = 17, diamond = 18)
    } else {
      look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
    }
    point_shapes <- look_up_point_shape[point_shapes]
    names(point_shapes) <- NULL
    unique_shapes <- unique(point_shapes)

    ## Point mapping
    if (length(unique_shapes) == 1L) {
      point_args$shape <- unique_shapes
      point_args$mapping <- ggplot2::aes(fill = color_var)
      scale_shape <- NULL
    } else {
      point_args$mapping <- ggplot2::aes(fill = color_var, shape = point_shape_var)
      scale_shape <- ggplot2::scale_shape_manual(name = NULL, values = point_shapes)
    }

    ## Point geom
    if (beeswarm) {
      n_bins <- n_bins %||% (30/nrow(df))
      point_geom <- do.call(ggbeeswarm::geom_quasirandom, c(point_args, method = beeswarm_method, width = beeswarm_width, dodge.width = dodge, nbins = n_bins, bandwidth = band_width))
    } else {
      point_geom <- do.call(ggplot2::geom_point, c(point_args, position = ggplot2::position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge, jitter.height = 0, seed = seed)))
    }
    p <- p + point_geom + scale_shape
  }

  # Axis titles
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)

  # y axis
  if (startsWith(y_scale, "log")) {
    minimum_y_value <- min(y_vals)
    if (minimum_y_value < 1) {
      if (minimum_y_value < 0) {
        Warning(sprintf("Some values are < 0 (min = %.3f). Will add %.3f to all values", minimum_y_value, 1 + minimum_y_value))
        df[[y]] <- y_vals + minimum_y_value + 1
      } else if (minimum_y_value == 0) {
        z <- y_vals[y_vals > 0 & y_vals < 1]
        if (length(z) > 0L) {
          y_limits[1L] <- min(z)
        } else {
          df[[y]] <- y_vals + 1
          y_limits[1L] <- min(df[[y]])
        }
      }
    }
  }

  # x axis
  x_breaks <- x_axis_breaks %||% sort.int(x_unique)
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  p <- p + ggplot2::scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Plot title and theme
  p <- p + ggplot2::coord_cartesian(clip = "off", default = TRUE) + ggplot2::ggtitle(plot_title) + theme_custom(...)

  # Significance annotation
  if (show_sig) {
    if (n_groups > 1L) {
      fn_sig_anno <- .plot_sig_anno_grouped
      if (startsWith(y_scale, "log")) {
        if (missing(sig_bar_nudge)) {
          y_breaks <- .create_axis_breaks(.limits = c(if (y_limits[1L] == 0) 1 else y_limits[1L], y_limits[2L]), .scale = y_scale, .n = n_breaks, .breaks_fn = breaks_fn)
          y_breaks <- if (y_scale == "log2") log2(y_breaks) else log10(y_breaks)
          sig_bar_nudge <- 0.25*(Max(y_breaks) - Min(y_breaks)) - 0.05*length(y_breaks) + 0.06*point_size - 0.18
        }
        if (missing(sig_text_nudge)) {
          sig_text_nudge <- 1.61*sig_bar_nudge - 0.09
        }
        if (missing(sig_star_nudge)) {
          sig_star_nudge <- 0.66*sig_bar_nudge + 0.05
        }
      }
    } else {
      fn_sig_anno <- .plot_sig_anno
    }
    p <- p + fn_sig_anno(df = df, x = "x_numeric", y = y, method = sig_method, dodge = dodge, y_max_type = "raw", y_scale = y_scale, bar_nudge = sig_bar_nudge, star_nudge = sig_star_nudge, text_nudge = sig_text_nudge, star_size = sig_star_size, text_size = sig_text_size, p_case = p_case, p_spaces = p_spaces, bar_thickness = sig_bar_thickness, bar_color = sig_bar_color, font_color = sig_font_color, show_ns = show_ns, stars = stars, ns_symbol = ns_symbol, step_increase = step_increase, breaks_fn = breaks_fn)
    if (show_all) {
      y_plot_limits <- get_plot_data_limits(p, axis = "y")
      p <- p + scale_axis_clean(axis = "y", scale = y_scale, plot_limits = y_plot_limits, axis_limits = y_limits, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_clean_axis())
      return(p)
    }
  }

  # Plot without significance annotation
  p <- p + scale_continuous(limits = y_limits, axis = "y", scale = y_scale, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
  p
}
