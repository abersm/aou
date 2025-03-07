#' Paired plot
#'
#' Plots pre and post values as points with lines connecting paired values
#' @inheritParams plot_point
#' @param facet_var Column in `df` to generate plot facets. Enter as string
#' @param facet_var_order Order for levels of `facet_var`. Enter as character vector
#' @param rev_facet_var_order If `TRUE`, levels of `facet_var` are reversed
#' @param nrow,ncol Number of facet rows and columns respectively. Enter as length 1 integer vectors
#' @param y_free If `FALSE` (default), all facets share same y axis. If `TRUE`, y axis can be different in each facet
#' @param x_axis_label_angle,x_angle Angle for x axis labels. Default is `45`. Enter as length 1 integer vector
#' @returns ggplot object
#' @export
plot_paired <- function(
    df,
    formula = NULL,
    id_var = "id",
    facet_var = NULL,
    x = NULL,
    y = NULL,
    x_order = NULL,
    rev_x_order = FALSE,
    facet_var_order = NULL,
    rev_facet_var_order = FALSE,
    point_shapes = c("circle", "circle"),
    point_shape_var = NULL,
    point_color_var = NULL,
    colors = c("#0072B5", "#BC3C29", "#868686", "#2A2D34", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    point_colors = colors,
    alpha = 1,
    point_alpha = alpha,
    point_size = 3.5,
    point_border_thickness = 0,
    point_border_colors = "black",
    line_color_var = NULL,
    line_colors = "black",
    line_thickness = 0.5,
    line_type = "solid",
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
    x_title = NULL,
    x_axis_title = x_title,
    x_axis_labels = NULL,
    x_angle = 45,
    x_axis_label_angle = x_angle,
    expand_x = waiver(),
    plot_title = NULL,
    show_sig = TRUE,
    sig_method = "p_by_normality",
    stars = TRUE,
    show_ns = TRUE,
    ns_symbol = "ns",
    sig_bar_nudge = if (y_scale == "log") 0.9 else 0.05,
    sig_star_nudge = if (y_scale == "log") -0.45 else sig_bar_nudge - 0.015,
    sig_text_nudge = if (y_scale == "log") 0.5 else sig_bar_nudge + 0.03,
    step_increase = if (y_scale == "log") 0 else sig_bar_nudge + 0.05,
    sig_bar_thickness = 0.6,
    sig_bar_color = "black",
    sig_font_color = "black",
    sig_star_size = 20,
    sig_text_size = 12,
    p_case = "upper",
    p_spaces = TRUE,
    theme_fn = theme_plain,
    nrow = NULL,
    ncol = NULL,
    y_free = FALSE,
    facet_border_thickness = 0.7,
    byrow = TRUE,
    ...) {
  plot_fn <- "plot_paired"

  # Variables
  ## x contains groups (pre, post)
  id_var <- get_input(id_var)
  facet_var <- get_input(facet_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  line_color_var <- get_input(line_color_var)
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  formula <- vars$formula
  x <- vars$x
  y <- vars$y

  # Data
  df <- .create_plot_df(.df = df, .formula = formula, .vars_remove_na = c(facet_var, id_var, point_color_var, point_shape_var, line_color_var))
  if (is_continuous(df$x) && is_continuous(df$y)) {
    # formula entered as pre ~ post
    df$id_var <- seq_nrow(df)
    id_var <- "id_var"
    df <- tidyr::pivot_longer(df, cols = c(x, y), names_to = "x", values_to = "y", values_drop_na = TRUE)
    df <- remove_na(df, cols = "x")
    x_levels <- x_order %||% c(x, y)
    if (rev_x_order) {
      x_levels <- Rev(x_levels)
    }
    df$x <- factor(df$x, levels = x_levels)
  } else {
    df$x <- .new_cat_var(df, var = "x", levels = x_order, reverse = rev_x_order)
    x_levels <- attr(df$x, "levels")
  }
  df$facet_var <- .new_cat_var(df, var = facet_var, levels = facet_var_order, reverse = rev_facet_var_order)
  df$point_color_var <- .new_cat_var(df, var = point_color_var)
  df$point_shape_var <- .new_cat_var(df, var = point_shape_var)
  df$line_color_var <- .new_cat_var(df, var = line_color_var)

  # ID variable
  if (missing(id_var)) {
    if (is.null(.subset2(df, id_var))) {
      Warning(sprintf("In '%s', 'id_var' not specified and 'df' does not contain a column named 'id'.\nWill assume rows of 'df' are ordered by sample ID (i.e., identical sample order for pre values and post values)", plot_fn))
      df <- Reduce(rbind, lapply(split.default(seq_nrow(df), f = as.list(df[c("facet_var", "x")]), drop = FALSE), function(z) {
        df_new <- df[z, , drop = FALSE]
        df_new$id_var <- seq_nrow(df_new)
        df_new
      }))
      id_var <- "id_var"
    } else {
      message(sprintf("In '%s', 'id_var' not specified.\nWill use 'id' column in 'df' as 'id_var'", plot_fn))
    }
  } else if (is.null(.subset2(df, id_var))) {
    message(sprintf("In '%s', 'id_var = NULL'.\nWill assume rows of 'df' are ordered by sample ID (i.e., identical sample order for pre values and post values)", plot_fn))
    df <- Reduce(rbind, lapply(split.default(seq_nrow(df), f = as.list(df[c("facet_var", "x")]), drop = FALSE), function(z) {
      df_new <- df[z, , drop = FALSE]
      df_new$id_var <- seq_nrow(df_new)
      df_new
    }))
    id_var <- "id_var"
  } else {
    df$id_var <- .subset2(df, id_var)
  }

  # Line geom
  line_colors <- rep(line_colors, length.out = n_unique(df$line_color_var, na.rm = FALSE))
  if (length(line_colors) == 1L) {
    line_geom <- ggplot2::geom_line(color = line_colors, linetype = line_type, linewidth = line_thickness, show.legend = FALSE)
    scale_line_color <- NULL
  } else {
    line_geom <- ggplot2::geom_line(mapping = ggplot2::aes(color = line_color_var), linetype = line_type, linewidth = line_thickness, show.legend = FALSE)
    scale_line_color <- scale_color_manual(name = NULL, values = line_colors)
  }

  # Point geom
  point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))
  point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
  if (is.character(point_shapes)) {
    look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
    point_shapes <- look_up_point_shape[point_shapes]
    names(point_shapes) <- NULL
  }
  unique_shapes <- unique.default(point_shapes)
  if (length(unique_shapes) == 1L) {
    point_geom <- ggplot2::geom_point(mapping = ggplot2::aes(fill = point_color_var), shape = unique_shapes, color = point_border_colors, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- NULL
  } else {
    point_geom <- ggplot2::geom_point(mapping = ggplot2::aes(fill = point_color_var, shape = point_shape_var), color = point_border_colors, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- ggplot2::scale_shape_manual(name = NULL, values = point_shapes)
  }

  # x axis
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  x_scale <- scale_x_discrete(name = x_axis_title, labels = x_labels, expand = expand_x)

  # y axis
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df$y)

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = id_var)) +
    line_geom +
    scale_line_color +
    point_geom +
    ggplot2::scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape +
    x_scale +
    theme_custom(facet_border_thickness = facet_border_thickness)

  # Significance annotation
  if (show_sig) {
    df_sig <- compare_means(df, y ~ x, "facet_var", paired = TRUE)
    sig_method <- if (sig_method == "p_by_normality") "p" else paste0("p_", sig_method)
    df_sig$p <- .subset2(df_sig, sig_method)
    df_sig <- df_sig[!is.na(df_sig$p), , drop = FALSE]
    if (!show_ns) {
      df_sig <- df_sig[df_sig$p < 0.05, , drop = FALSE]
    }
    if (Nrow(df_sig) == 0) {
      sig_stars_plot <- sig_text_plot <- sig_bars_plot <- NULL
    } else {
      df_sig$label <- sig_stars(df_sig$p, symbols = c("****", "***", "**", "*",  ns_symbol))
      df_sig$y <- pmax(df_sig$max_1, df_sig$max_2)
      df_sig$ymin <- pmin(df_sig$min_1, df_sig$min_2)
      df_sig <- df_sig[c("facet_var", "Group1", "Group2", "p", "label", "y", "ymin")]
      names(df_sig)[c(2, 3)] <- c("x", "xend")
      is_sig <- df_sig$p < 0.05
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
        df_sig$y_sig_label <- df_sig$y*(1 + ifelse(stars & is_sig, sig_star_nudge + 1, sig_text_nudge + 1))
      } else {
        if (y_free) {
          df_sig$delta_y <- vapply(seq_nrow(df_sig), function(z) {
            df_row <- df_sig[z, , drop = FALSE]
            diff(range(.create_axis_breaks(.limits = c(df_row$ymin, df_row$y), .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)))
          }, numeric(1), USE.NAMES = FALSE)
        } else {
          df_sig$delta_y <- diff(range(.create_axis_breaks(.limits = c(df_sig$ymin, df_sig$y), .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)))
        }
        df_sig$y_sig_bar <- df_sig$y + df_sig$delta_y*sig_bar_nudge
        df_sig$y_sig_label <- df_sig$y + ifelse(stars & is_sig, df_sig$delta_y*sig_star_nudge, df_sig$delta_y*sig_text_nudge)
      }

      # Bars geom
      sig_bars_plot <- ggplot2::geom_segment(data = df_sig, ggplot2::aes(x = x, xend = xend, y = y_sig_bar, yend = y_sig_bar), linewidth = sig_bar_thickness, color = sig_bar_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)

      # Stars geom
      if (stars) {
        df_stars <- df_sig[is_sig, , drop = FALSE]
        if (Nrow(df_stars) > 0) {
          sig_stars_plot <- ggplot2::geom_text(data = df_stars, ggplot2::aes(x = 1.5, y = y_sig_label, label = label), size = sig_star_size, size.unit = "pt", vjust = 0, hjust = 0.5, color = sig_font_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)
        } else {
          sig_stars_plot <- NULL
        }
        df_text <- df_sig[!is_sig, , drop = FALSE]
      } else {
        df_sig$label <- format_p_value(df_sig$p, trim_ws = !p_spaces, prefix = if (p_case == "upper") "P" else "p")
        df_text <- df_sig
        sig_stars_plot <- NULL
      }

      # Text geom
      if (Nrow(df_text) > 0) {
        sig_text_plot <- ggplot2::geom_text(data = df_text, ggplot2::aes(x = 1.5, y = y_sig_label, label = label), size = sig_text_size, size.unit = "pt", vjust = 0, hjust = 0.5, color = sig_font_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)
      } else {
        sig_text_plot <- NULL
      }
    }
    p <- p +
      sig_bars_plot +
      sig_stars_plot +
      sig_text_plot
    y_plot_limits <- get_plot_data_limits(p, axis = "y")
    y_scale <- scale_axis_clean(axis = "y", scale = y_scale, plot_limits = y_plot_limits, axis_limits = y_limits, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_clean_axis())
  } else {
    #y_scale <- scale_continuous(axis = "y", scale = y_scale, limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
    y_scale <- scale_axis(axis = "y", scale = y_scale, title = y_axis_title, expand_lower = expand_y, breaks = y_axis_breaks, censor_fn = censor_fn)
  }

  # Facets
  facet_scales <- if (y_free) "free_y" else "fixed"
  if (is.null(nrow) && is.null(ncol)) {
    nrow <- if (n_unique(df$facet_var) < 6) 1 else 2
  }

  # Final plot
  p <- p +
    ggplot2::coord_cartesian(clip = "off", default = TRUE) +
    y_scale +
    ggplot2::facet_wrap(facets = dplyr::vars(facet_var), nrow = nrow, ncol = ncol, scales = facet_scales) +
    ggplot2::ggtitle(plot_title) +
    theme_fn(x_axis_label_angle = x_axis_label_angle, ...)
  p
}
