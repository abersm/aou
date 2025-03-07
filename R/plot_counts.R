#' Plot counts
#'
#' @inheritParams plot_bar
#' @inheritParams plot_freq
#' @returns ggplot object
#' @export
plot_counts <- function(
    df,
    formula = NULL,
    x = NULL,
    y = NULL,
    grouping_var = NULL,
    order_by_freq = c("both", "x", "grouping_var", "none"),
    x_order = NULL,
    rev_x_order = FALSE,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    bar_color_var = NULL,
    colors = c("#333333", "#0072B5", "#BC3C29", "#999999", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    bar_border_thickness = 0.75,
    bar_width = NULL,
    width = bar_width,
    dodge = 0.7,
    show_legend = FALSE,
    legend_title = "",
    y_max = NULL,
    x_max = NULL,
    y_axis_breaks = NULL,
    y_axis_labels = NULL,
    x_axis_breaks = waiver(),
    x_axis_labels = waiver(),
    x_axis_label_angle = NULL,
    y_axis_title_angle = 0,
    y_axis_title = "N",
    x_axis_title = NULL,
    plot_title = NULL,
    n_breaks = 3,
    breaks_fn = pretty,
    censor_fn = rescale_none,
    aspect_ratio = NULL,
    show_anno = TRUE,
    anno_type = c("counts", "percent"),
    anno_font_size = NULL,
    anno_font_color = "black",
    anno_nudge = 0.03,
    anno_y_nudge = 0.03,
    show_n = FALSE,
    n_anno_font_size = 12,
    n_anno_font_color = "black",
    n_anno_x = "right",
    n_anno_x_nudge = 0.2,
    n_prefix = "N = ",
    flip = FALSE,
    nudge = 0,
    ...) {
  plot_fn <- "plot_counts"
  df_names <- names(df)
  if (is.logical(order_by_freq)) {
    order_by_freq <- if (order_by_freq) "both" else "none"
  }
  order_by_freq <- match.arg(order_by_freq, choices = c("both", "x", "grouping_var", "none"))
  anno_type <- match.arg(anno_type, choices = c("counts", "percent"))

  # Variables
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  x <- vars$x
  y <- vars$y

  # Order along x axis
  if (!is.factor(df[[x]])) {
    df[[x]] <- as.character(df[[x]])
  }
  x_levels <- x_order %||% if (order_by_freq %in% c("none", "grouping_var")) {
    create_levels(df[[x]], reverse = rev_x_order)
  } else {
    df[[x]][order(df[[y]], decreasing = !rev_x_order)]
  }
  df[[x]] <- factor(df[[x]], levels = x_levels)

  # Set width (must keep before ordering of grouping_var)
  if (is.null(grouping_var) || grouping_var %!in% names(df)) {
    df$grouping_var <- "a"
    grouping_var <- "grouping_var"
    width <- width %||% 0.5
  } else {
    width <- width %||% (0.2*n_unique(df[[grouping_var]], na.rm = FALSE))
  }

  # Order of grouping_var along x axis
  if (!is.factor(df[[grouping_var]])) {
    df[[grouping_var]] <- as.character(df[[grouping_var]])
  }
  grouping_var_levels <- grouping_var_order %||% if (order_by_freq %in% c("none", "x")) {
    create_levels(df[[grouping_var]], reverse = rev_grouping_var_order)
  } else {
    grouping_var_levels <- names(n_per_group(df[[grouping_var]]))
    if (rev_grouping_var_order) {
      Rev(grouping_var_levels)
    } else {
      grouping_var_levels
    }
  }
  df[[grouping_var]] <- factor(df[[grouping_var]], levels = grouping_var_levels)

  # Bar color
  if (is.null(bar_color_var) || bar_color_var %!in% names(df)) {
    df$bar_color_var <- if (length(colors) == 1L) "a" else df[[grouping_var]]
    bar_color_var <- "bar_color_var"
  }

  # Clean variables for plotting
  df <- remove_na(df, y)
  n_bars <- n_unique(df[[x]], na.rm = FALSE)*n_unique(df[[grouping_var]], na.rm = FALSE)

  # Flipped
  if (flip) {
    mapping <- ggplot2::aes(x = .data[[y]], y = .data[[x]], group = .data[[grouping_var]])
    counts_annotation <- ggplot2::geom_text(
      ggplot2::aes(
        x = label_pos,
        y = .data[[x]],
        label = label
      ),
      size = anno_font_size %||% if (n_bars < 5) 12 else 6,
      size.unit = "pt",
      color = anno_font_color,
      inherit.aes = FALSE,
      vjust = 0.5,
      hjust = 0,
      nudge_x = nudge
    )
    aspect_ratio <- aspect_ratio %||% if (n_bars < 4L) 2 + 2/sqrt(5) else 1

    x_title <- if (missing(x_axis_title)) y_axis_title else x_axis_title
    y_title <- if (missing(y_axis_title)) NULL else y_axis_title

    x_max <- x_max %||% Max(df[[y]])

    x_breaks <- if (missing(x_axis_breaks)) y_axis_breaks else x_axis_breaks
    x_breaks <- x_breaks %||% .create_axis_breaks(.limits = c(0, x_max), .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)

    x_limits <- Range(x_breaks)

    x_labels <- if (missing(x_axis_labels)) y_axis_labels else x_axis_labels
    x_labels <- x_labels %||% function(x) format(x, big.mark = ",", scientific = FALSE)

    y_labels <- if (missing(y_axis_labels)) x_axis_labels else y_axis_labels
    y_labels <- y_labels %||% function(x) format(x, big.mark = ",", scientific = FALSE)

    y_breaks <- if (missing(y_axis_breaks)) x_axis_breaks else y_axis_breaks

    x_axis_label_angle <- if (missing(x_axis_label_angle)) 0 else x_axis_label_angle

    axis_limits <- x_limits
    axes <- list(ggplot2::scale_x_continuous(name = x_title, limits = x_limits, labels = x_labels, expand = c(0, 0, 0, 0), oob = censor_fn), ggplot2::scale_y_discrete(name = y_title, breaks = y_breaks, labels = y_labels), theme_custom(aspect_ratio = aspect_ratio, x_axis_label_angle = x_axis_label_angle, ...), ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 1)))
  } else {
    mapping <- ggplot2::aes(x = .data[[x]], y = .data[[y]], group = .data[[grouping_var]])
    counts_annotation <- ggplot2::geom_text(
      ggplot2::aes(
      x = .data[[x]],
      y = label_pos,
      label = label
    ),
      size = anno_font_size %||% if (n_bars < 5) 12 else 6,
      size.unit = "pt",
      color = anno_font_color,
      inherit.aes = FALSE,
      vjust = 0,
      hjust = 0.5,
      nudge_y = nudge
    )
    aspect_ratio <- aspect_ratio %||% if (n_bars < 4L) 0.5 + sqrt(5)/2 else 1

    y_max <- y_max %||% Max(df[[y]])
    y_axis_breaks <- y_axis_breaks %||% .create_axis_breaks(.limits = c(0, y_max), .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)
    y_axis_limits <- Range(y_axis_breaks)
    y_axis_labels <- y_axis_labels %||% function(x) format(x, big.mark = ",", scientific = FALSE)
    if (is.null(x_axis_label_angle)) {
      x_axis_label_angle <- if (max(nchar(attr(df[[x]], "levels")), na.rm = TRUE) > 5) 45 else 0
    }

    axis_limits <- y_axis_limits
    axes <- list(ggplot2::scale_y_continuous(name = y_axis_title, limits = y_axis_limits, labels = y_axis_labels, expand = c(0, 0, 0, 0), oob = censor_fn), ggplot2::scale_x_discrete(name = x_axis_title, breaks = x_axis_breaks, labels = x_axis_labels), theme_custom(aspect_ratio = aspect_ratio, x_axis_label_angle = x_axis_label_angle, y_axis_title_angle = y_axis_title_angle, ...))
  }

  # Count annotations
  if (show_anno) {
    #df$label_pos <- df[[y]]
    df$label_pos <- df[[y]] + Diff(axis_limits)*anno_nudge
    n <- sum(df[[y]])
    df$label <- if (anno_type == "counts") df[[y]] else paste0(round_up(df[[y]]/n*100, digits = 1), "%")
  } else {
    counts_annotation <- NULL
  }

  # Plot
  p <- ggplot2::ggplot(df, mapping) +
    ggplot2::geom_col(ggplot2::aes(fill = .data[[bar_color_var]]), show.legend = show_legend, position = ggplot2::position_dodge(width = dodge), color = "#000000", width = width, linewidth = bar_border_thickness) +
    scale_fill_manual(name = legend_title, values = colors) +
    ggplot2::coord_cartesian(clip = "off", default = TRUE) +
    counts_annotation +
    axes +
    ggplot2::ggtitle(plot_title)

  if (!show_n) return(p)
  n_anno_font_size <- convert(n_anno_font_size, "pt", "mm")
  n_anno_x <- match.arg(n_anno_x, choices = c("right", "left"))
  x_pos <- if (n_anno_x == "right") n_bars else 1
  p <- p + ggplot2::annotate("text", label = paste0(n_prefix, sum(df[[y]])), x = x_pos + n_anno_x_nudge, y = y_axis_limits[2L], hjust = 1, vjust = 1, size = n_anno_font_size, color = n_anno_font_color)
  suppress(p)
}
