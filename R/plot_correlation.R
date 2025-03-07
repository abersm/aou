#' Plot correlation coefficient, p value, linear regression line
#'
#' @param show_error If `TRUE` (default), error region displayed with color as determined by `error_colors`. If `FALSE`, error region not displayed
#' @param error_colors Color for error region around trend line. Only relevant when `show_error = TRUE`
#' @param show_corr_coef If `TRUE` (default), correlation coefficient and P value are displayed on plot
#' @param anno_position Position of correlation coefficient and P value annotation. Can enter as numeric vector of length 2 containing x and y coordinates, `"best"`, a length 1 character vector with some combination of the following letters: `"t"`, `"b"`, `"l"`, `"r"`
#' @param method Method for performing correlation test. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param corr_prefix Prefix for correlation coefficient annotation. If `NULL`, rho used for Spearman, R for Pearson, tau for Kendall
#' @param p_prefix Prefix for P values. Enter as length 1 character vector. Default is `"P"`
#' @param show_n If `TRUE` (default), number of observations displayed as component of correlation annotation
#' @param n_prefix Prefix for number of observations. Enter as length 1 character vector. Default is `"n"`
#' @param show_rsq If `TRUE` (default), R squared and P value are displayed
#' @param point_geom_fn Geom function to generate points. Default is `ggplot2::geom_point`. Alternative: `scattermore::geom_scattermore`
#' @inheritParams plot_scatter
#' @inheritParams plot_line
#' @returns ggplot object
#' @export
plot_cor <- function(
    df,
    formula = NULL,
    x = NULL,
    y = NULL,
    grouping_var = NULL,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    point_color_var = NULL,
    colors = c("#0072B5", "#BC3C29", "#868686", "#2A2D34", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    point_colors = colors,
    point_alpha = 0.9,
    point_shape_var = NULL,
    point_shapes = "circle",
    point_size = 4,
    point_border_thickness = 0.5,
    point_border_colors = "#333333",
    line_color_var = NULL,
    line_colors = point_colors,
    line_thickness = 1,
    show_error = TRUE,
    error_colors = "#999999",
    show_legend = FALSE,
    y_scale = "regular",
    y_axis_breaks = NULL,
    y_max = NULL,
    y_min = NULL,
    expand_y = 0.1,
    y_axis_labels =  waiver(),
    y_axis_title = waiver(),
    x_scale = "regular",
    x_axis_breaks = NULL,
    x_max = NULL,
    x_min = NULL,
    expand_x = 0.1,
    x_axis_labels = waiver(),
    x_axis_title = waiver(),
    plot_title = NULL,
    show_corr_coef = TRUE,
    anno_position = "best",
    method = "spearman",
    corr_prefix = NULL,
    p_prefix = "P",
    show_n = TRUE,
    n_prefix = "n",
    show_rsq = TRUE,
    point_geom_fn = ggplot2::geom_point,
    ...) {
  plot_fn <- "plot_cor"
  grouping_var <- get_input(grouping_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  line_color_var <- get_input(line_color_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  x <- vars$x
  y <- vars$y
  df <- remove_na(df, c(x, y))
  x_vals <- .subset2(df, x)
  y_vals <- .subset2(df, y)

  # Grouping variable
  df$grouping_var <- .new_cat_var(df, var = grouping_var, levels = grouping_var_order, reverse = rev_grouping_var_order)

  # Point color
  df$point_color_var <- .new_cat_var(df, var = point_color_var, if_null = df$grouping_var)
  point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))

  # Point border color
  if (length(point_border_colors) > 1L) {
    Warning(sprintf("In '%s', 'point_border_colors' must be length 1", plot_fn))
  }

  # Point shape
  df$point_shape_var <- .new_cat_var(df, var = point_shape_var)
  point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
  if (is.character(point_shapes)) {
    look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
    point_shapes <- look_up_point_shape[point_shapes]
    names(point_shapes) <- NULL
  }
  unique_shapes <- unique(point_shapes)
  if (length(unique_shapes) == 1L) {
    point_args <- list(mapping = ggplot2::aes(fill = .data$point_color_var), shape = unique_shapes, color = point_border_colors, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    point_shape <- NULL
  } else {
    point_args <- list(mapping = ggplot2::aes(fill = .data$point_color_var, shape = .data$point_shape_var), color = point_border_colors, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    point_shape <- ggplot2::scale_shape_manual(name = NULL, values = point_shapes)
  }

  # Point geom
  if ("pointsize" %in% names(formals(point_geom_fn))) {
    point_args$pointsize <- point_args$size*1.0235
    point_args$pixels <- c(1000, 1000)
    point_args$size <- NULL
  }
  point <- do.call(point_geom_fn, point_args)

  # Line color
  df$line_color_var <- .new_cat_var(df, var = line_color_var, if_null = df$point_color_var)
  n_line_colors <- n_unique(df$line_color_var, na.rm = FALSE)
  line_colors <- rep(line_colors, length.out = n_line_colors)

  # Line/error geom
  if (show_error && length(error_colors) > 1L) {
    error_colors <- rep(error_colors, length.out = n_line_colors)
    line <- list(
      ggplot2::geom_smooth(ggplot2::aes(group = .data$grouping_var, color = .data$line_color_var, fill = .data$line_color_var), method = lm, se = show_error, formula = y ~ x, size = line_thickness, show.legend = FALSE),
      scale_fill_new(),
      ggplot2::scale_fill_manual(name = NULL, values = error_colors)
    )
  } else {
    line <- ggplot2::geom_smooth(ggplot2::aes(group = .data$grouping_var, color = .data$line_color_var), method = lm, se = show_error, formula = y ~ x, size = line_thickness, fill = error_colors, show.legend = FALSE)
  }

  # Axes
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = y_vals)
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  x_limits <- .set_axis_limits(.min = x_min, .max = x_max, .breaks = x_axis_breaks, .values = x_vals)
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]], group = grouping_var)) +
    point +
    point_shape +
    ggplot2::scale_fill_manual(name = NULL, values = point_colors) +
    line +
    ggplot2::scale_color_manual(name = NULL, values = line_colors) +
    scale_continuous(axis = "y", title = y_axis_title, scale = y_scale, limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, expand_lower = expand_y) +
    scale_continuous(axis = "x", title = x_axis_title, scale = x_scale, limits = x_limits, breaks = x_axis_breaks, labels = x_axis_labels, expand_lower = expand_x) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_custom(...)
  if (!show_corr_coef && !show_rsq && !show_n) return(p)
  if (x_scale == "log") {
    df[[x]] <- log10_1(x_vals)
  }
  if (y_scale == "log") {
    df[[y]] <- log10_1(y_vals)
  }

  # Annotation position
  if (all(is.character(anno_position))) {
    anno_position <- tolower(anno_position)
    if (anno_position == "best") {
      y_delta <- diff(y_limits)
      plot_data <- ggplot2::ggplot_build(p)
      plot_data <- plot_data$data[[2L]]
      y_left <- plot_data$y[which.min(plot_data$x)]
      delta_left <- abs(y_left/y_delta)
      delta_left <- Max(c(delta_left, 1 - delta_left))
      y_right <- plot_data$y[which.max(plot_data$x)]
      delta_right <- abs(y_right/y_delta)
      delta_right <- Max(c(delta_right, 1 - delta_right))
      anno_slope <- if (length(y_left) != 0L && length(y_right) != 0L) {
        if (!is.na(y_left) && !is.na(y_right) && y_left > y_right) "negative" else "positive"
      } else {
        "positive"
      }
      anno_horizontal <- if (length(delta_left) != 0L && length(delta_right) != 0L) {
        if (is.finite(delta_left) && !is.na(delta_right) && delta_left > delta_right) "l" else "r"
      } else {
        "r"
      }
      anno_vertical <- if (anno_slope == "negative") {
        if (anno_horizontal == "l") "b" else "t"
      } else {
        if (anno_horizontal == "l") "t" else "b"
      }
      anno_position <- paste0(anno_vertical, anno_horizontal)
    }
    label_y_pos <- get_plot_axis_breaks(p)
    label_y_pos <- label_y_pos$y
    label_y_pos <- range(label_y_pos, na.rm = TRUE)
    label_y_pos <- if (grepl("b", anno_position, fixed = TRUE)) {
      label_y_pos[1L] + 0.2*Diff(label_y_pos) - expand_y*Diff(label_y_pos)
    } else {
      label_y_pos[1L] + 0.9*Diff(label_y_pos)
    }
    label_x_pos <- get_plot_axis_breaks(p)
    label_x_pos <- range(label_x_pos$x, na.rm = TRUE)
    label_x_pos <- if (grepl("l", anno_position, fixed = TRUE)) {
      label_x_pos[1L] + 0.1*Diff(label_x_pos) - expand_x*Diff(label_x_pos)
    } else {
      label_x_pos[1L] + 0.57*Diff(label_x_pos)
    }
    if (y_scale == "log") {
      label_y_pos <- 10^label_y_pos
    }
    if (x_scale == "log") {
      label_x_pos <- 10^label_x_pos
    }
  }

  # Correlation coefficient annotation
  separator <- if (show_corr_coef && show_rsq) ", " else "\n"
  if (show_corr_coef) {
    # TODO: step below performed on possibly log transformed values
    corr_p <- .cor_test(df[[x]], df[[y]], method = method)$p
    corr_prefix <- paste0(corr_prefix %||% switch(method, spearman = "\u03c1", kendall = "\u03c4", "R"), " = ")
    corr_r <- suppress(stats::cor(df[[x]], df[[y]], use = "pairwise.complete.obs", method = method))
    corr_annotation <- paste0(corr_prefix, sprintf("%.2f", corr_r))
    corr_p <- format_p_value(corr_p, prefix = p_prefix)
    corr_annotation <- paste(corr_annotation, corr_p, sep = separator)
  }

  # R squared annotation
  if (show_rsq) {
    # TODO: step below performed on possibly log transformed values
    fit <- Lm(df, y = y, x = x)
    z <- lm_glance(fit)
    rsq_annotation <- paste0("R", "\u00b2", " = ", sprintf("%.2f", z$r_sq))
    rsq_p_annotation <- format_p_value(z$p, prefix = p_prefix)
    rsq_annotation <- paste(rsq_annotation, rsq_p_annotation, sep = separator)
  }

  # Paste annotations for correlation coefficient and R squared
  if (show_corr_coef && show_rsq) {
    corr_annotation <- paste(corr_annotation, rsq_annotation, sep = "\n")
  } else if (show_rsq) {
    corr_annotation <- rsq_annotation
  }

  # N annotation
  if (show_n) {
    n_prefix <- sprintf("%s = ", n_prefix)
    n_annotation <- paste0(n_prefix, nrow(df))
    corr_annotation <- paste(corr_annotation, n_annotation, sep = "\n")
  }
  p <- p +
    ggplot2::annotate(x = label_x_pos, y = label_y_pos, hjust = 0, label = corr_annotation, geom = "text", parse = FALSE)
  p
}

#' Correlation heatmap
#'
#' @param df Data frame in wide form (columns will be correlated with one another)
#' @param ... Variables to be included in heatmap
#' @param colors Color for heatmap. Default from blue (low) to red (high)
#' @param method Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param min_variance Minimum variance allowed for variable above which variable will be excluded from heatmap. Default is `1` (includes all columns)
#' @export
plot_cor_heatmap <- function(
    df,
    ...,
    colors = clr_continuous(c("#0072B5", "#FFFFFF", "#BC3C29"), n = 100),
    method = c("spearman", "pearson", "kendall"),
    min_variance = 1) {
  pkg_required("corrplot")
  df <- Select(df, ...)
  method <- match.arg(method, choices = c("spearman", "pearson", "kendall"))
  df <- df[vars_numeric(df)]
  incl_vars <- vapply(df, Var, numeric(1), USE.NAMES = TRUE)
  incl_vars <- incl_vars[!is.na(incl_vars)]
  df <- df[names(incl_vars)[incl_vars > min_variance]]
  corrplot::corrplot(
    corr = suppress(stats::cor(df, method = method, use = "pairwise.complete.obs")),
    method = "color",
    outline = TRUE,
    type = "lower",
    order = "AOE",
    tl.col = "black",
    tl.srt = 45,
    tl.cex = 0.5,
    diag = FALSE,
    col = colors
  )
}

# stat_cor_anno -----------------------------------------------------------

#' Add correlation coefficient and P value to scatter plot
#'
#' Functionality from Alboukadel Kassambara's excellent package ggpubr
#' @inheritParams ggplot2::geom_text
#' @param method Correlation test method. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param ci Confidence interval for correlation coefficient. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param hypothesis_type Alternative hypothesis. Options: `"two.sided"` (default), `"greater"`, `"less"`
#' @param auto_label_pos If `TRUE` (default), x and y coordinates for annotation automatically determined
#' @param label_x_npc,label_y_npc Annotation coordinates in npc units. Can be numeric (between 0-1) or character ("center", "middle", "top", "bottom", "right", "left"). Length should match number of groups/panels, but will be recycled if needed
#' @param label_x,label_y Label coordinates in raw data units
#' @param label_type Label type. Options: `"expression"` (default), `"latex"`, `"tex"`, `"text"`
#' @param coef_format,p_format Functions to format correlation coefficient and P value, respectively. Should accept numeric vector as input and return character vector as output
#' @param cor_coef_p_sep String that separates correlation coefficient and P value in annotation. Default is `", "`
#' @param ... Arguments passed to `geom_text` or `geom_label`
#' @returns ggproto object
#' @export
stat_cor_anno <- function(
    mapping = NULL,
    data = NULL,
    method = "spearman",
    ci = 0.95,
    hypothesis_type = "two.sided",
    auto_label_pos = TRUE,
    label_x_npc = "left",
    label_y_npc = "top",
    label_x = NULL,
    label_y = NULL,
    label_type = "expression",
    coef_format = format_number,
    p_format = format_p_value,
    cor_coef_p_sep = ", ",
    geom = "text",
    position = "identity",
    na.rm = TRUE,
    show.legend = FALSE,
    inherit.aes = TRUE,
    ...) {
  ggplot2::layer(
    stat = StatCorAnno,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      auto_label_pos = auto_label_pos,
      label_x_npc = label_x_npc,
      label_y_npc = label_y_npc,
      label_x = label_x,
      label_y = label_y,
      cor_coef_p_sep = cor_coef_p_sep,
      method = method,
      ci = ci,
      hypothesis_type = hypothesis_type,
      label_type = label_type,
      coef_format = format_number,
      p_format = format_p_value,
      parse = label_type == "expression",
      na.rm = na.rm,
      ...
    )
  )
}

# StatCorAnno -------------------------------------------------------------

#' Stat ggproto for `stat_cor_anno`
#'
#' @rdname stat_cor_anno
#' @returns ggproto object
#' @export
StatCorAnno <- ggplot2::ggproto(
  "StatCorAnno",
  ggplot2::Stat,
  required_ggplot2 = c("x", "y"),
  default_aes = ggplot2::aes(hjust = ggplot2::after_stat(hjust), vjust = ggplot2::after_stat(vjust)),
  setup_params = function(data, params) {
    if (!params$auto_label_pos) return(params)
    n_groups <- length(unique(data$group))
    label_coords <- rect_min_density(x = data$x, y = data$y, n_x = 2, n_y = 20/n_groups)
    label_x <- label_coords$x
    label_y <- label_coords$y
    if (length(label_y) > 1L) {
      r <- cor_test(data, x = "x", y = "y", ci = params$ci, method = params$method, hypothesis_type = params$hypothesis_type)
      y_fn <- if (r$cor_coeff > 0L) which.min else which.max
      idx <- y_fn(label_y)
      label_x <- label_x[idx]
      label_y <- label_y[idx]
    }
    params$label_x <- label_x
    params$label_y <- label_y
    params$label_x_npc <- params$label_y_npc <- numeric(0)
    params
  },
  compute_group = function(
    data,
    scales,
    method,
    ci,
    hypothesis_type,
    auto_label_pos,
    label_x_npc,
    label_y_npc,
    label_x,
    label_y,
    cor_coef_p_sep,
    label_type,
    coef_format,
    p_format) {
    if (length(unique(data$x)) < 2L) return(empty_df())
    expression_label <- label_type == "expression"
    # Create data frame with columns "r" (correlation coefficient), "p", "label", "method"
    z <- cor_test(data, x = "x", y = "y", ci = ci, method = method, hypothesis_type = hypothesis_type)
    z <- vec_to_df(r = z$cor_coeff, rr = z$cor_coeff*z$cor_coeff, p = z$p, method = method)

    # P value formatting
    z$p_label <- p_format(z$p)
    if (expression_label) {
      z$p_label <- gsub("P = ", "italic(P)~`=`~", z$p_label, fixed = TRUE)
      z$p_label <- gsub("P < ", "italic(P)~`<`~", z$p_label, fixed = TRUE)
    }

    # Correlation coefficient formatting
    cor_coef_symbol <- switch(method, pearson = "R", spearman = "rho", kendall = "tau")
    coef_label <- function(x, prefix = "R") {
      x <- paste0(prefix, " = ", shQuote(coef_format(x)))
      if (expression_label) {
        x <- gsub("R2 = ", "italic(R)^2~`=`~", x, fixed = TRUE)
        x <- gsub("R = ", "italic(R)~`=`~", x, fixed = TRUE)
      }
      gsub("R", cor_coef_symbol, x, fixed = TRUE)
    }
    z$r_label <- coef_label(z$r, prefix = "R")
    z$rr_label <- coef_label(z$rr, prefix = "R2")
    group_idx <- abs(data$group[1L])
    n_groups <- length(group_idx)
    #cor_coef_p_sep <- cor_coef_p_sep %||% if (n_groups > 1L) ", " else "\n"
    z$label <- if (expression_label) {
      if (cor_coef_p_sep == "\n") {
        # Line break at each comma
        paste0("atop(", z$r_label, ",", z$p_label, ")")
      } else {
        cor_coef_p_sep <- trimws(cor_coef_p_sep)
        cor_coef_p_sep <- if (cor_coef_p_sep == "") "~" else paste0("*`", cor_coef_p_sep, "`~")
        paste0(z$r_label, cor_coef_p_sep, z$p_label)
      }
    } else if (label_type %in% c("latex", "tex", "text")) {
      paste0(z$r_label, cor_coef_p_sep, z$p_label)
    } else {
      ""
    }

    # Label coordinates
    #if (auto_label_pos) {
    #  label_coords <- rect_min_density(x = data$x, y = data$y, n_x = 2, n_y = 20/n_groups)
    #  label_x <- label_coords$x
    #  label_y <- label_coords$y
    #  if (length(label_x) > 1L) {
    #    y_fn <- if (Mean(z$r) > 0L) which.min else which.max
    #    idx <- y_fn(label_y)
    #    label_x <- label_x[idx]
    #    label_y <- label_y[idx]
    #  }
    #  label_x_npc <- label_y_npc <- numeric(0)
    #}

    if (n_groups != 0L) {
      idx <- n_groups >= group_idx
      label_x_npc <- ifelse(idx, label_x_npc[group_idx], label_x_npc[1L])
      label_y_npc <- ifelse(idx, label_y_npc[group_idx], label_y_npc[1L])
      if (!is.null(label_x)) {
        label_x <- ifelse(idx, label_x[group_idx], label_x[1L])
      }
      if (!is.null(label_y)) {
        label_y <- ifelse(idx, label_y[group_idx], label_y[1L])
      }
    }

    # Label x coordinate
    if (length(label_x) != 0L) {
      x <- label_x
    } else if (length(label_x_npc) != 0L) {
      x <- scales$x$dimension()
      x <- if (is.numeric(label_x_npc)) {
        x[1L] + label_x_npc*Diff(x)
      } else {
        switch(label_x_npc,
               right = x[2L],
               middle = ,
               center = mean(x),
               left = x[1L]
        )
      }
    }

    # Label y coordinate
    if (length(label_y) != 0L) {
      y <- label_y
      #vjust <- 0.5
      vjust <- 1.4*group_idx
    } else if (length(label_y_npc) != 0L) {
      vjust <- 1.4*group_idx
      y <- scales$y$dimension()
      if (is.numeric(label_y_npc)) {
        y <- y[1L] + label_y_npc*Diff(y)
        vjust <- vjust - 0.7*n_groups
      } else {
        switch(label_y_npc,
               bottom = {
                 y <- y[1L]
                 vjust <- -vjust
               },
               middle = ,
               center = {
                 y <- mean(y)
                 vjust <- vjust - 0.7*n_groups
               },
               top = {
                 y <- y[2L]
               }
        )
      }
    }
    cbind(z, vec_to_df(x = x, y = y, hjust = 0, vjust = vjust))
  }
)


# geom_cor_anno -----------------------------------------------------------

#' Geom for `stat_cor_anno`
#'
#' @rdname stat_cor_anno
#' @inheritParams ggplot2::geom_text
#' @returns ggproto object
#' @export
geom_cor_anno <- function(
    mapping = NULL,
    data = NULL,
    stat = "cor_anno",
    geom = "text",
    position = "identity",
    method = "spearman",
    ci = 0.95,
    hypothesis_type = "two.sided",
    label_type = "expression",
    coef_format = format_number,
    p_format = format_p_value,
    cor_coef_p_sep = ", ",
    auto_label_pos = TRUE,
    label_x_npc = "left",
    label_y_npc = "top",
    label_x = NULL,
    label_y = NULL,
    na.rm = TRUE,
    show.legend = FALSE,
    inherit.aes = TRUE,
    ...) {
  ggplot2::layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      auto_label_pos = auto_label_pos,
      label_x_npc = label_x_npc,
      label_y_npc = label_y_npc,
      label_x = label_x,
      label_y = label_y,
      cor_coef_p_sep = cor_coef_p_sep,
      method = method,
      ci = ci,
      hypothesis_type = hypothesis_type,
      label_type = label_type,
      coef_format = format_number,
      p_format = format_p_value,
      parse = label_type == "expression",
      na.rm = na.rm,
      ...
    )
  )
}
