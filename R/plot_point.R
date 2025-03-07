#' Point plot
#'
#' @param df Data frame in long format containing continuous variable, categorical or integer variable, and grouping variable
#' @param formula y ~ x format
#' @param grouping_var Variable to group by. Enter as quoted or unquoted variable name
#' @param x,y Variables for x  and y axis, respectively. Enter as quoted or unquoted variable names
#' @param x_order Order of x values. Enter as character vector. Default is `NULL`
#' @param rev_x_order If `FALSE` (default), order of x axis variable is not reversed
#' @param grouping_var_order Order for levels of `grouping_var` along x axis. Enter as character vector
#' @param rev_grouping_var_order If `TRUE`, levels of `grouping_var` are reversed
#' @param beeswarm If `FALSE` (default), jittered points. If `TRUE`, beeswarm plot
#' @param beeswarm_method Method for `geom_quasirandom` function. Default is `"smiley"`
#' @param width Alias for either `jitter_width` or `beeswarm_width`
#' @param beeswarm_width Width argument for beeswarm points. Default is `0.15`
#' @param n_bins Number of bins. Used in `ggbeeswarm::geom_quasirandom` function. Default uses 30/number of points
#' @param band_width Bandwidth. Used in `ggbeeswarm::geom_quasirandom` function. Default is `1 + n_bins/5`
#' @param dodge Entry for position_dodge. Default is `0.7`
#' @param jitter_width Width between groups of points. Default is `0.2`
#' @param point_shapes Point shapes. Options: `"circle"` (default), `"square"`, `"triangle"`, `"diamond"`. Enter as quoted shape
#' @param point_shape_var Variable used to determine point shapes. Default uses `grouping_var.` Enter as quoted or unquoted variable name
#' @param point_color_var Variable used to determine point colors. Enter as quoted or unquoted variable name. Default uses `grouping_var`
#' @param colors Alias for `point_colors`
#' @param point_colors Point fill colors. Enter as character vector of color names or hexadecimal codes
#' @param alpha Alias for `point_alpha`
#' @param point_alpha Transparency of point colors. Enter as numeric 0-1. Default is `0.9`
#' @param point_size Point size in pts units. Default is `5`
#' @param point_border_thickness Thickness of line surrounding points in pts units. Default is `1`
#' @param point_border_colors Color of line surrounding points. Default is `"black"`
#' @param show_errorbar If `TRUE`, error bars are displayed
#' @param summary_fn Function used to determine midpoint of error bars. Default is `Mean`
#' @param error_fn Function used to determine ends of error bars. Default is `SE`
#' @param errorbar_width Width of error bars. Default is `n_unique(df$x)/10`
#' @param errorbar_thickness Line thickness of error bar. Default is `0.75`
#' @param error_limits Component of error bar to display. Only relevant when `show_errorbar = TRUE`. Options: `"both"` (default), `"upper"` (upper half of error bar only), `"lower"` (lower error bar only)
#' @param show_legend If `FALSE` (default), legend is not displayed
#' @param legend_title Legend title. Default is blank
#' @param y_axis_title Enter as quoted string
#' @param y_title Alias for `y_axis_title`
#' @param y_scale Scaling for y axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param y_axis_breaks Numeric vector or function specifying location of ticks along y axis
#' @param y_axis_labels Vector or function specifying y axis tick labels
#' @param breaks_fn Function used to generate y axis breaks. Passed to `.create_axis_breaks`. Default is `pretty`
#' @param n_breaks Number of tick marks for y axis. Default is `3`
#' @param y_min Minimum value to use for y axis. Default is `0`
#' @param y_max Maximum y axis value. Default is `NULL`
#' @param expand_y Expansion of y axis around y = 0. Default is `0`
#' @param censor_fn Function used to transform data outside y axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param x_title Alias for `x_axis_title`
#' @param x_axis_title Enter as quoted string
#' @param x_axis_breaks Numeric vector or function specifying location of ticks along x axis
#' @param x_axis_labels Vector or function specifying x axis tick labels
#' @param expand_x Argument entered for expand argument of `scale_x_continuous`. Default is `waiver()`
#' @param plot_title Text to be used as main plot title. Enter as quoted string. Default is `""` (i.e. no title)
#' @param show_sig If `TRUE` (default), significance annotation displayed. If `FALSE`, no significance annotation displayed
#' @param sig_method Options: `"p_by_normality"` (default), `"mann_whitney"`, `"t_test_welch"`, `"t_test_no_welch"`. Enter as quoted test type
#' @param stars If `TRUE` (default), significance stars are displayed. If `FALSE`, actual P values are displayed
#' @param show_ns If `TRUE` (default), non-significant annotations are displayed. If `FALSE`, no annotation is displayed comparisons in which P >= 0.05
#' @param ns_symbol Symbol to use when P >= 0.05. Only relevant when `show_ns = TRUE`. Default is `"ns"`
#' @param sig_bar_nudge Nudge factor along y axis for significance bars. Distance between point or bar and significance bar is sig_bar_nudge x maximum y value on plot. Default is `0.08`
#' @param sig_star_nudge Nudge factor along y axis for significance stars. Distance between point or bar and significance stars is `sig_star_nudge`*maximum y value on plot. Default is `0.05`
#' @param sig_text_nudge Nudge factor along y axis for significance annotation text. Distance between point or bar and significance annotation text is `sig_text_nudge`*maximum y value on plot. Default is `0.09`
#' @param step_increase Step increase as proportion of view port. Default is `0.11`
#' @param sig_bar_thickness Line thickness for significance bars in linewidth units. Default is `0.6`
#' @param sig_bar_color Color for significance bars and annotations. Default is `"black"`
#' @param sig_font_color Color for text in significance annotations. Default is `"black"`
#' @param sig_star_size Size of significance stars in pts. Only relevant when `stars = TRUE`. Default is `20`
#' @param sig_text_size Font size for P values in pts. Only relevant when `stars = FALSE`. Default is `12`
#' @param p_case Case of "p" used in significance annotations when `stats = FALSE.` Options: `"upper"` ("P", default) or `"lower"` ("p"). Enter as quoted text
#' @param p_spaces If `TRUE` (default), spaces placed between p, equality symbol, and P-value in significance annotation. If `FALSE`, no space between "p", equality symbol, and P-value in significance annotation. Only relevant when `stars = FALSE`
#' @param seed Length 1 integer vector to make point jittering reproducible. Only relevant when `beeswarm = FALSE`. Default is `1234`
#' @param theme_fn Theme function. Default is `theme_custom`
#' @param ... Arguments passed to `theme_fn()`
#' @returns ggplot object
#' @import ggplot2
#' @export
plot_point <- function(
  df,
  formula = NULL,
  grouping_var = NULL,
  x = NULL,
  y = NULL,
  beeswarm = FALSE,
  beeswarm_method = "smiley",
  width = if (beeswarm) 0.15 else 0.2,
  beeswarm_width = width,
  n_bins = 25,
  band_width = 1 + n_bins/5,
  dodge = if (beeswarm) 0.85 else 0.7,
  jitter_width = width,
  x_order = NULL,
  rev_x_order = FALSE,
  grouping_var_order = NULL,
  rev_grouping_var_order = FALSE,
  point_shapes = c("circle", "circle"),
  point_shape_var = NULL,
  point_color_var = NULL,
  colors = c("#0072B5", "#BC3C29", "#868686", "#2A2D34", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
  point_colors = colors,
  alpha = 0.9,
  point_alpha = alpha,
  point_size = 5,
  point_border_thickness = 1,
  point_border_colors = "black",
  show_errorbar = FALSE,
  summary_fn = Mean,
  error_fn = SE,
  errorbar_width = NULL,
  errorbar_thickness = 0.75,
  error_limits = "both",
  show_legend = FALSE,
  legend_title = "",
  y_title = waiver(),
  y_axis_title = y_title,
  y_scale = "regular",
  y_axis_breaks = NULL,
  y_axis_labels = NULL,
  breaks_fn = pretty,
  n_breaks = 4,
  y_min = NULL,
  y_max = NULL,
  expand_y = 0.1,
  censor_fn = rescale_none,
  x_title = waiver(),
  x_axis_title = x_title,
  x_axis_breaks = NULL,
  x_axis_labels = NULL,
  expand_x = waiver(),
  plot_title = NULL,
  show_sig = TRUE,
  sig_method = "p_by_normality",
  stars = TRUE,
  show_ns = TRUE,
  ns_symbol = "ns",
  sig_bar_nudge = if (grepl("^log", y_scale)) 0.9 else 0.08,
  #sig_star_nudge = if (grepl("^log", y_scale)) -0.45 else sig_bar_nudge - 0.015,
  sig_star_nudge = if (grepl("^log", y_scale)) -0.45 else sig_bar_nudge*0.75,
  #sig_text_nudge = if (grepl("^log", y_scale)) 0.5 else sig_bar_nudge + 0.03,
  sig_text_nudge = if (grepl("^log", y_scale)) 0.5 else sig_bar_nudge*1.35,
  step_increase = if (grepl("^log", y_scale)) 0 else sig_bar_nudge + 0.05,
  sig_bar_thickness = 0.6,
  sig_bar_color = "black",
  sig_font_color = "black",
  sig_star_size = 20,
  sig_text_size = 12,
  p_case = "upper",
  p_spaces = TRUE,
  seed = 1234,
  theme_fn = theme_custom,
  ...) {
  plot_fn <- "plot_point"
  dots <- list(...)
  if (!is.null(angle <- c(dots$x_axis_label_angle, dots$x_angle)) && angle[1L] != 0 && missing(x_axis_title) && missing(x_title)) {
    x_axis_title <- NULL
  }
  # Set seed
  #if (exists(".Random.seed", where = globalenv())) {
  #  random_seed <- .Random.seed
  #  on.exit(assign(".Random.seed", random_seed, pos = globalenv()), add = TRUE)
  #}
  #set.seed(seed)

  # Data
  ## Add new variables rather than renaming variables to preserve original variable names (needed if facets are used)
  grouping_var <- get_input(grouping_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  x <- vars$x
  y <- vars$y
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
  df <- remove_na(df, c(x, y))
  x_vals <- .subset2(df, x)
  x_levels <- x_order %||% create_levels(x_vals, reverse = rev_x_order)
  df$x_numeric <- match(x_vals, x_levels, incomparables = NA_integer_)
  y_vals <- .subset2(df, y)

  # Grouping variable
  df$grouping_var <- .new_cat_var(df, var = grouping_var, levels = grouping_var_order, reverse = rev_grouping_var_order)

  # Point color
  df$point_color_var <- .new_cat_var(df, var = point_color_var, if_null = df$grouping_var)
  point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))

  # Point shape
  df$point_shape_var <- .new_cat_var(df, var = point_shape_var)
  point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
  if (is.character(point_shapes)) {
    look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
    point_shapes <- look_up_point_shape[point_shapes]
    names(point_shapes) <- NULL
  }
  unique_shapes <- unique.default(point_shapes)
  if (length(unique_shapes) == 1L) {
    point_args <- list(mapping = aes(fill = point_color_var), shape = unique_shapes, color = point_border_colors, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- NULL
  } else {
    point_args <- list(mapping = aes(fill = point_color_var, shape = point_shape_var), color = point_border_colors, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- scale_shape_manual(name = NULL, values = point_shapes)
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

  # Points
  p <- if (beeswarm) {
    n_bins <- n_bins %||% (30/Nrow(df))
    do.call(ggbeeswarm::geom_quasirandom, c(point_args, method = beeswarm_method, width = beeswarm_width, dodge.width = dodge, nbins = n_bins, bandwidth = band_width))
  } else {
    do.call(ggplot2::geom_point, c(point_args, position = position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge, jitter.height = 0, seed = seed)))
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x_numeric, y = .data[[y]], group = grouping_var)) +
    p +
    ggplot2::scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape

  # Error bars
  if (show_errorbar) {
    errorbar_width <- errorbar_width %||% (n_unique(df$x_numeric)/10)
    p <- p +
      errorbar(summary_fn = summary_fn, error_fn = error_fn, line_thickness = errorbar_thickness, width = errorbar_width, dodge = dodge, error_limits = error_limits) +
      ggplot2::stat_summary(geom = "errorbar", fun = summary_fn, fun.min = summary_fn, fun.max = summary_fn, position = ggplot2::position_dodge(width = dodge), linewidth = errorbar_thickness, width = errorbar_width*2)
  }

  # x axis
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  x_breaks <- x_axis_breaks %||% sort.int(unique.default(df$x_numeric))
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  p <- p + ggplot2::scale_x_continuous(name = x_axis_title, breaks = x_breaks, labels = x_labels, expand = expand_x)

  # Plot title/theme
  p <- p +
    ggplot2::coord_cartesian(clip = "off", default = TRUE) +
    ggplot2::ggtitle(plot_title) +
    theme_fn(...)

  # Significance annotation
  n_groups <- length(unique.default(df$grouping_var))
  if (missing(show_sig)) {
    n_x_vals <- n_unique(df$x_numeric, na.rm = FALSE)
    show_sig <- if (n_groups == 1L) {
      n_x_vals <= 5L
    } else {
      n_groups <= 4L
    }
  }
  if (show_sig) {
    if (n_groups > 1) {
      fn_sig_anno <- .plot_sig_anno_grouped
      if (startsWith(y_scale, "log")) {
        if (missing(sig_bar_nudge)) {
          y_breaks <- .create_axis_breaks(
            .limits = c(if (y_limits[1L] == 0) 1 else y_limits[1L], y_limits[2L]),
            .scale = y_scale,
            .n = n_breaks,
            .breaks_fn = breaks_fn
          )
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
    p <- p + fn_sig_anno(
      df = df,
      x = "x_numeric",
      y = y,
      method = sig_method,
      dodge = dodge,
      y_max_type = "raw",
      y_scale = y_scale,
      bar_nudge = sig_bar_nudge,
      star_nudge = sig_star_nudge,
      text_nudge = sig_text_nudge,
      star_size = sig_star_size,
      text_size = sig_text_size,
      p_case = p_case,
      p_spaces = p_spaces,
      bar_thickness = sig_bar_thickness,
      bar_color = sig_bar_color,
      font_color = sig_font_color,
      show_ns = show_ns,
      stars = stars,
      ns_symbol = ns_symbol,
      step_increase = step_increase,
      breaks_fn = breaks_fn
    )
    y_plot_limits <- get_plot_data_limits(p, axis = "y")
    #p <- p + scale_axis_clean(axis = "y", scale = y_scale, plot_limits = y_plot_limits, axis_limits = y_limits, labels = y_axis_labels, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_clean_axis(title = y_axis_title))
    #p <- p + scale_axis_clean(axis = "y", scale = y_scale, plot_limits = y_plot_limits, axis_limits = y_limits, labels = y_axis_labels, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_axis(cap = "upper", title = y_axis_title))
    p <- p + scale_axis_clean(axis = "y", scale = y_scale, plot_limits = y_plot_limits, axis_limits = y_limits, labels = y_axis_labels, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, title = y_axis_title, guide = guide_axis(cap = "upper"))
  } else {
    p <- p + scale_continuous(axis = "y", scale = y_scale, limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
  }
  p
}
