#' Stacked bar plot with error bars
#'
#' Only use when displaying % of total on y axis and groups sum to 100%
#' @inheritParams plot_paired
#' @inheritParams plot_bar
#' @param errorbar_colors Color of error bars. Default is `"black"`
#' @param errorbar_thickness Line thickness for error bars. Default uses value for `bar_thickness` (unless `bar_thickness` is 0)
#' @param incl_lower_errorbar If `TRUE` and `show_errorbar = TRUE`, lower error bar is displayed. Default is `FALSE`
#' @returns ggplot
#' @export
plot_bar_stacked <- function(
    df,
    formula = NULL,
    grouping_var = NULL,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    facet_var = NULL,
    x = NULL,
    y = NULL,
    x_order = NULL,
    rev_x_order = FALSE,
    facet_var_order = NULL,
    rev_facet_var_order = FALSE,
    colors = c("#0072B5", "#BC3C29", "#868686", "#2A2D34", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    bar_colors = colors,
    alpha = 1,
    bar_alpha = alpha,
    bar_width = 0.6,
    bar_border_thickness = 0.75,
    bar_border_colors = "black",
    show_legend = FALSE,
    show_errorbar = TRUE,
    summary_fn = Mean,
    error_fn = SE,
    incl_lower_errorbar = FALSE,
    errorbar_width_multiplier = 0.5,
    errorbar_colors = "black",
    errorbar_thickness = bar_border_thickness,
    legend_title = "",
    y_title = waiver(),
    y_axis_title = y_title,
    y_axis_breaks = NULL,
    y_axis_labels = NULL,
    x_title = NULL,
    x_axis_title = x_title,
    x_axis_labels = NULL,
    x_angle = 45,
    x_axis_label_angle = x_angle,
    expand_x = waiver(),
    plot_title = NULL,
    theme_fn = theme_plain,
    nrow = NULL,
    ncol = NULL,
    ...) {
  plot_fn <- "plot_bar_stacked"

  # Variables
  ## y contains %
  ## x contains groups (y sums to 100 for each)
  facet_var <- get_input(facet_var)
  no_facet <- is.null(facet_var)
  grouping_var <- get_input(grouping_var)
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  formula <- vars$formula
  x <- vars$x
  y <- vars$y

  # Data
  df <- remove_na(df, y)
  df[[x]] <- .new_cat_var(df, var = x, levels = x_order, reverse = rev_x_order)
  x_levels <- attr(df[[x]], "levels")
  df$facet_var <- .new_cat_var(df, var = facet_var, levels = facet_var_order, reverse = rev_facet_var_order)
  df$grouping_var <- .new_cat_var(df, var = grouping_var, levels = grouping_var_order, reverse = rev_grouping_var_order)

  # Bar colors
  bar_colors <- rep(bar_colors, length.out = n_unique(df$grouping_var, na.rm = FALSE))

  # x axis
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  x_scale <- ggplot2::scale_x_discrete(name = x_axis_title, labels = x_labels, expand = expand_x, guide = guide_axis(angle = x_axis_label_angle))

  # y axis
  if (is.null(y_axis_breaks)) {
    is_perc <- max(df[[y]], na.rm = TRUE) <= 1
    if (is_perc) {
      y_axis_breaks <- seq(0, 1, by = 0.25)
      if (is_waiver(y_axis_title)) {
        y_axis_title <- "%"
      }
    } else {
      y_axis_breaks <- seq(0, 100, by = 25)

    }
  }
  y_axis_title <- y_axis_title %W% if (max(y_axis_breaks, na.rm = TRUE) <= 1) "Proportion" else "%"
  y_scale <- ggplot2::scale_y_continuous(name = y_axis_title, breaks = y_axis_breaks, expand = c(0, 0, 0, 0), guide = guide_axis(cap = TRUE))

  # Plot data
  df_plot <- dplyr::group_by(df, !!!rlang::syms(c("grouping_var", x, "facet_var")))
  df_plot <- dplyr::summarise(df_plot, .summary = summary_fn(.data[[y]]), .error = error_fn(.data[[y]]), .groups = "drop")

  # Errorbar data. Calculate even if show_errorbar is FALSE so that below warning can be displayed
  df_error <- df_plot[order(df_plot$grouping_var, decreasing = TRUE), , drop = FALSE]
  df_error <- dplyr::group_by(df_error, !!!rlang::syms(c(x, "facet_var")))
  df_error <- dplyr::mutate(df_error, .summary_adj = cumsum(.summary), .total = sum(.summary, na.rm = TRUE))
  df_error <- dplyr::ungroup(df_error)

  # Warn if any bars have height > 105%
  if (max(df_error$.total) > 105) {
    idx <- df_error[df_error$.total > 105, , drop = FALSE]
    idx <- unique(idx[c("facet_var", x)])
    idx <- paste(sprintf("Facet = %s, x = %s", idx[[1L]], idx[[2L]]), collapse = "\n")
    Warning(sprintf("In '%s', the following facets/'x' values had total percent > 105:\n\n%s. \n\nReview data by reviewing the 'y' column in the output from the following command:\n\nabers::split_df(ggplot2::ggplot_build(last_plot())$data[[2L]], c('PANEL', 'x'))", plot_fn, idx))
  }

  # Error bars
  if (show_errorbar) {
    aes_error <- if (incl_lower_errorbar) {
      ggplot2::aes(x = .data[[x]], ymax = .summary_adj + .error, ymin = .summary_adj - .error)
    } else {
      ggplot2::aes(x = .data[[x]], ymax = .summary_adj + .error, ymin = .summary_adj)
    }
    if (bar_border_thickness == 0 && missing(errorbar_thickness)) {
      errorbar_thickness <- 0.5
    }
    error <- ggplot2::geom_errorbar(
      data = df_error,
      mapping = aes_error,
      width = errorbar_width_multiplier*bar_width,
      color = errorbar_colors,
      linewidth = errorbar_thickness
    )
  } else {
    error <- NULL
  }

  # Facets
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- if (n_unique(df$facet_var) < 6) 1 else 2
  }

  p <- ggplot2::ggplot(data = df_plot, mapping = ggplot2::aes(x = .data[[x]], y = .summary)) +
    ggplot2::geom_bar(ggplot2::aes(fill = grouping_var), stat = "identity", width = bar_width, color = bar_border_colors, linewidth = bar_border_thickness, show.legend = show_legend) +
    ggplot2::scale_fill_manual(name = legend_title, values = bar_colors) +
    error +
    y_scale +
    x_scale +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::ggtitle(plot_title) +
    theme_fn(...)
  if (!no_facet) {
    p <- p + ggplot2::facet_wrap(facets = dplyr::vars(facet_var), nrow = nrow, ncol = ncol, scales = "fixed")
  }
  p
}
