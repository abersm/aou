# Pie chart ---------------------------------------------------------------

#' Pie chart
#'
#' @param df Data frame
#' @param categorical_var Variable in `df` containing categories. Enter as quoted column name
#' @param count_var Variable in `df` containing counts (or proportions or percentages) for each `categorical_var`. Enter as quoted column name. Default is `"n"`
#' @param order_by Variable in `df` to order unique values in `df[[categorical_var]]`. Enter as quoted column column name. Default is `count_var`. If `NULL`, values appear in piechart in same order as row order in `df`. In this case, `rev_order` is ignored.
#' @param rev_order If `TRUE`, order of levels in pie chart are reversed so that levels are plotted from most to least frequent. Default is `FALSE` (levels are plotted from least frequent to most frequent)
#' @param fill_colors Pie colors for each unique value in `df[[categorical_var]]`
#' @param border_color Line color for pie borders. Default is black
#' @param show_legend If `TRUE` (default), legend is displayed
#' @param legend_title Legend title. Default is blank
#' @returns ggplot object
#' @export
plot_piechart <- function(
    df,
    categorical_var,
    count_var = "n",
    rev_order = FALSE,
    order_by = count_var,
    fill_colors = c("#333333", "#00A1D5", "#E41A1C", "#6761A8", "#009872", "#EFC000", "#045A8D", "#FF7F00", "#BABABA", "#FFFFFF"),
    colors = fill_colors,
    border_color = "#000000",
    show_legend = TRUE,
    legend_title = "") {
  pkg_required("ggforce")
  plot_fn <- "plot_piechart"
  categorical_var <- get_input(categorical_var)
  count_var <- get_input(count_var)
  order_by <- get_input(order_by)
  df <- remove_na(df, c(categorical_var, count_var))
  if (!is.null(order_by)) {
    df <- df[order(.subset2(df, order_by), decreasing = rev_order), , drop = FALSE]
  }
  colors <- rep(colors, length.out = length(unique(.subset2(df, categorical_var))))
  p <- ggplot2::ggplot(df) +
    ggforce::geom_arc_bar(
      mapping = ggplot2::aes(
        x0 = 0,
        y0 = 0,
        r0 = 0,
        r = 1,
        amount = .data[[count_var]],
        fill = .data[[categorical_var]]
      ),
      stat = "pie",
      color = border_color,
      show.legend = show_legend
    ) +
    ggplot2::scale_fill_manual(legend_title, values = colors) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void()
  p
}

# Map ---------------------------------------------------------------------

#' Plot of world map
#'
#' @param countries Countries to highlight on map. Enter as character vector
#' @param color_for_countries Color used to fill countries mentioned in countries argument
#' @param color_other_countries Color used to fill countries not mentioned in countries argument. Default is `"grey"`
#' @param line_color Line color for country borders. Default is `"black"`
#' @param line_thickness Line thickness for country borders. Default is `0.3`
#' @param ... Arguments passed to `theme_blank`
#' @returns ggplot object
#' @export
plot_map <- function(
    countries = NULL,
    color_for_countries = "#C8543B",
    color_other_countries = "#F7F7F7",
    line_color = "#333333",
    line_thickness = 0.3,
    ...) {
  pkg_required("ggfocus")
  if (is.null(countries)) {
    color_for_countries <- color_other_countries
    countries <- "USA"
  }
  ggplot2::ggplot(ggplot2::map_data("world"), ggplot2::aes(x = long, y = lat, group = group, fill = region)) +
    ggplot2::geom_polygon(color = line_color, linewidth = line_thickness) +
    ggfocus::scale_fill_focus(focus_levels = countries, color_focus = color_for_countries, color_other = color_other_countries) +
    theme_blank(...)
}

# Legend ------------------------------------------------------------------

#' Create a plot legend
#'
#' @param labels Symbol labels. Enter as character vector (can include text to be parsed)
#' @param colors Symbol fill colors. Enter as character vector hexadecimal codes or color names. Length can be 1 or same length as `labels`
#' @param alpha Symbol fill transparency. Enter as length 1 numeric between 0-1. Default is `1`
#' @param shapes Symbol shapes. Enter as numeric or character vector of shapes. Length can be 1 or same length as `labels`
#' @param size Symbol size in inches. Enter as length 1 numeric. Default is `0.25`
#' @param border_thickness Symbol border thickness in pts. Enter as length 1 numeric. Set to `0` to remove symbol borders. Default is `1`
#' @param border_colors Symbol border colors. Enter as character vector hexadecimal codes or color names. Default is black
#' @param font_size Font size for symbol labels in pts. Enter as length 1 numeric. Default calculates font size from symbol size (`size`)
#' @param font_color Font color for symbol labels. Enter as length 1 character vector hexadecimal codes or color names. Default is black
#' @param font_face Font face for symbol labels.  Enter as length 1 character vector. Options: `"plain"` (default), `"italic"`, `"bold"`
#' @param labels_from_axis If `TRUE` (default), labels are derived from y axis text. If `FALSE`, labels are plotted using `geom_text`
#' @param nudge Horizontal space between symbol and label. Only relevant when `labels_from_axis = FALSE`. Enter as length 1 numeric. Default is `3.5`
#' @param parse If `TRUE`, symbol labels are generated by parsing input to `labels`. If `FALSE` (default), symbol labels appear exactly as specified in `labels`
#' @returns ggplot object
#' @noRd
manual_plot_legend <- function(
    labels,
    colors,
    alpha = 1,
    shapes = "square",
    size = 0.25,
    border_thickness = 1,
    border_colors = "#333333",
    font_size = NULL,
    font_color = "#333333",
    font_face = "plain",
    labels_from_axis = TRUE,
    nudge = 3.5,
    parse = FALSE) {
  if (is.character(shapes)) {
    look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
    shapes <- look_up_point_shape[shapes]
    names(shapes) <- NULL
  }
  #font_size <- font_size %||% (79.5*size + 0.11)
  font_size <- font_size %||% (70*size)
  vert_gap <- size/0.6
  size <- 1/3*(100*size - 2)
  n <- length(labels)
  df <- vec_to_df(
    x = 0,
    fill = rep(colors, length.out = n),
    color = rep(border_colors, length.out = n),
    shape = rep(shapes, length.out = n)
  )

  if (labels_from_axis) {
    # Labels from axis text
    #df$y <- factor(labels, levels = rev(labels))
    #y_axis <- ggplot2::scale_y_discrete(position = "right")
    df$y <- breaks <- seq(1, by = vert_gap, length.out = n)
    y_axis <- ggplot2::scale_y_reverse(breaks = breaks, labels = labels, position = "right", expand = c(0, 5/n, 0, 5/n))
    text <- x_axis <- NULL
    axis.text.y.right <- ggplot2::element_text(size = font_size, color = font_color, face = font_face, vjust = 0.5, hjust = 0)
    aspect_ratio <- if (n > 5 && missing(nudge)) n else n*nudge
  } else {
    # Labels as plot component
    if (font_face != "plain") {
      labels <- sprintf("%s('%s')", font_face, labels)
      parse <- TRUE
    }
    df$y <- seq_len(n)
    df$label <- labels
    df$x_label <- nudge
    text <- ggplot2::geom_text(ggplot2::aes(x = x_label, label = label), size = font_size, size.unit = "pt", color = font_color, hjust = 0, vjust = 0.5, parse = parse)
    x_axis <- ggplot2::scale_x_continuous(limits = c(0, nudge*1.5))
    axis.text.y.right <- y_axis <- NULL
    aspect_ratio <- n
  }

  # Plot
  blank <- ggplot2::element_blank()
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = fill, color = color, shape = shape)) +
    ggplot2::geom_point(size = size, alpha = alpha, stroke = border_thickness) +
    text +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_shape_identity() +
    x_axis +
    y_axis +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      rect = blank,
      line = blank,
      axis.title = blank,
      axis.text = blank,
      axis.text.y.right = axis.text.y.right,
      legend.position = "none",
      aspect.ratio = aspect_ratio
    )
}

# Other -------------------------------------------------------------------

#' Convert static plot to interactive plot
#'
#' @param x ggplot object. Default is `last_plot()`
#' @param tooltip_vars Variables to display in tool tips. Enter as character vector
#' @param show_legend If `TRUE`, legend is displayed. If `FALSE` (default), no legend is displayed
#' @param dynamic_ticks If `TRUE` (default), dynamic ticks are shown. If `FALSE`, dynamic ticks are not shown
#' @returns plotly object
#' @noRd
plot_interactive <- function(x = last_plot(), tooltip_vars = NULL, show_legend = FALSE, dynamic_ticks = TRUE) {
  pkg_required("plotly")
  if (!is.null(tooltip_vars)) {
    x <- x + aes(text = .data[[tooltip_vars]])
  }
  if (!show_legend) {
    x <- x + theme(legend.position = "none")
  }
  tryCatch(suppress(plotly::ggplotly(x, dynamicTicks = dynamic_ticks)), error = function(e) suppress(plotly::ggplotly(x, dynamicTicks = FALSE)))
}

#' Alias for `plot_interactive`
#'
#' @param x ggplot object. Default is `last_plot()`
#' @param tooltip_vars Variables to display in tool tips. Enter as character vector
#' @param show_legend If `TRUE`, legend is displayed. If `FALSE` (default), no legend is displayed
#' @param dynamic_ticks If `TRUE` (default), dynamic ticks are shown. If `FALSE`, dynamic ticks are not shown
#' @returns plotly object
#' @noRd
plotly <- plot_interactive

#' Display all shapes for ggplot2
#'
#' @returns ggplot object
#' @noRd
show_point_options <- function() {
  ggplot2::ggplot(vec_to_df(p = seq(0, 25, by = 1L))) +
    ggplot2::scale_x_continuous(name = "") +
    ggplot2::scale_shape_identity() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = p %% 6, y = p %/% 6, shape = p), size = 5, fill = "#BC3C29") +
    ggplot2::geom_text(mapping = ggplot2::aes(x = p %% 6, y = p %/% 6 + 0.25, label = p), size = 3)+
    ggplot2::scale_y_reverse(name = "") +
    ggplot2::theme(rect = ggplot2::element_blank(), axis.title =  ggplot2::element_blank(), axis.line = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
}

#' Display all linetypes for ggplot2
#'
#' @returns ggplot object
#' @noRd
show_linetype_options <- function() {
  ggplot2::ggplot(vec_to_df(x = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678"))) +
    ggplot2::scale_x_continuous(name = "", limits = c(0, 1), breaks = NULL) +
    ggplot2::scale_y_discrete(name = "") +
    ggplot2::scale_linetype_identity() +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = 0, xend = 1, y = x, yend = x, linetype = x)) +
    ggplot2::theme(text = ggplot2::element_text(size = 14), line = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -10, unit = "pt")), panel.background = ggplot2::element_rect(fill = "white"))
}

#' Display ggproto function(s)
#'
#' @param x ggproto object (GeomX) or ggproto method (GeomX$setup_data)
#' @param fn Functions within `x` to return. Enter quoted or unquoted function names. If `NULL` (default), all functions returned
#' @param copy If `TRUE` (default) output is copied to clipboard
#' @returns Function or list of functions
#' @noRd
ggproto_fn <- function(x, fn = NULL, copy = TRUE) {
  x_name <- get_input(x)
  format_output <- function(z, fn_name) {
    if (is.function(z)) {
      args <- deparse(args(z), width.cutoff = 500L)
      args <- args[args != "NULL"]
      args <- sub("^[ \t\r\n]+", "", args, perl = TRUE)
      args <- paste0(args, collapse = "")
      body <- paste(deparse(body(z), width.cutoff = 500L), collapse = "\n")
      z <- paste0(args, body)
      z <- gsub("\\}\\s+else", "} else", z)
      z <- paste0(fn_name, " = ", z)
      z <- paste(z, collapse = "\n")
      z <- gsub("if\\(", "if (", z)
      z <- gsub("\\)\\{", ") {", z)
      z <- gsub("function \\(", "function(", z)
      z <- gsub(" \\* ", "*", z)
      z <- gsub("/", "/", z)
      paste0(z, collapse = "\n")
    } else if (inherits(z, "uneval")) {
      z <- vapply(unclass(z), function(x) paste(deparse(x, width.cutoff = 500L), collapse = " "), character(1))
      z <- paste(paste0(names(z), " = ", z), collapse = ",\n")
      paste0(fn_name, " = aes(\n", z, "\n)")
    } else {
      paste0(fn_name, " = ", paste(deparse(z, width.cutoff = 500L), collapse = " "))
    }
  }
  if (!inherits(x, "ggproto")) {
    if (inherits(x, "ggproto_method")) {
      x <- environment(x)$f
    }
    if (copy) {
      fn_string <- format_output(x, fn_name = if (is.character(x_name)) x_name else "fn")
      fn_string <- gsub("^fn = ", "", fn_string)
      tryNULL(suppress(copy(paste0(fn_string, "\n"))))
    }
    return(x)
  }
  x_list <- as.list(x)
  fn <- get_input(fn)
  x_names <- names(x_list)
  fn <- x_names[match(fn, x_names, nomatch = 0L)]
  n <- length(fn)
  out <- if (n == 0L) {
    fn <- x_names
    x_list
  } else if (n == 1L) {
    .subset2(x_list, fn)
  } else {
    .subset(x_list, fn)
  }
  if (copy) {
    fn_string <- if (n == 1L) {
      format_output(out, fn)
    } else {
      paste(vapply(seq_along(out), function(i) format_output(out[[i]], fn[i]), character(1), USE.NAMES = FALSE), collapse = "\n\n")
    }
    tryNULL(suppress(copy(paste0(fn_string, "\n"))))
  }
  out
}

# Example plot ------------------------------------------------------------

#' Example plot - y vs. x
#'
#' @param fn Plot function. Default is `plot_bar_point`
#' @param formula formula. Default is `re ~ day`
#' @param group Grouping variable. Default is `"genotype"`
#' @param ... Arguments passed to `fn`
#' @returns ggplot object
#' @export
plot_ex <- function(fn = "plot_bar_point", ...) {
  fn <- get_input(fn)
  if (!startsWith(fn, "plot_") && fn %!in% pkg_fns("abers")) {
    fn <- paste0("plot_", fn)
  }
  args <- switch(
    fn,
    plot_bar = ,
    plot_box = ,
    plot_line = ,
    plot_line_mean = ,
    plot_point = ,
    plot_bar_point = list(df = dplyr::filter(dock8, gene == "Il17f"), formula = re ~ day, grouping_var = "genotype", y_axis_title = "Relative expression", ...),
    plot_freq = list(df = covid, col = "severity_death", grouping_var = "gender", x_axis_label_angle = 45, ...),
    plot_violin = list(df = covid, formula = wbc ~ gender, y_axis_title = "WBC", x_axis_label_angle = 45, ...),
    plot_cor = list(df = covid, formula = wbc ~ age, y_axis_title = "WBC", x_axis_title = "Age"),
    plot_cor_heatmap = list(covid, ...),
    plot_heatmap = ,
    plot_histogram = ,
    plot_density = list(df = covid, col = "age", grouping_var = "gender", ...),
    plot_gating_tree = list(gs_gate_paths(gs_example()), ...),
    plot_gsea = ,
    plot_smooth = list(df = dock8[dock8$gene == "Il17a", , drop = FALSE], formula = re ~ day, grouping_var = "genotype", ...),
    plot_paired = {
      df <- dock8[dock8$gene %in% c("Il17a", "Il17f") & dock8$day < 5, , drop = FALSE]
      df <- df[order(df$genotype), , drop = FALSE]
      df <- dplyr::group_by(df, gene, day)
      df <- dplyr::mutate(df, id = seq_len(dplyr::n()))
      df <- dplyr::ungroup(df)
      list(df = df, formula = re ~ day, facet_var = "gene", ...)
    },
    plot_spag_facet_strata = list(df = covid, y = "wbc", strata_var = "age", time = "time", ...),
    plot_spaghetti = list(df = dock8[dock8$gene == "Il17a", , drop = FALSE], formula = re ~ day, id = "id", ...),
    plot_scatter = list(df = covid, formula = wbc ~ hgb, ...),
    plot_roc = list(df = covid, outcome_var = "death", predictor_var = "age", ...),
    plot_km = list(df = set_max_follow_up_time(covid, max_time = 40), predictor_var = "gender", ...),
    plot_forest = ,
    plot_incidence = list(df = covid, date = "date_admission", ...),
    plot_piechart = list(df = counts(xtab(covid, severity_death, gender)), ...),
    plot_volcano = list(df = Rename(compare_means(dock8[dock8$day == 1, , drop = FALSE], re ~ genotype, "gene"), fc_median = "fc"), ...),
    plot_swimmer = list(covid, time_var = "days_admission", ...),
    plot_map = list("USA", ...),
    plot_96_well_plate = list(...),
    plot_interactive = list(x = plot_point(df = dock8[dock8$gene == "Il17a", , drop = FALSE], formula = re ~ day, grouping_var = "genotype", ...)),
    plot_tsne = list(df = downsample(facs_df(ff_example()), 1000), ...),
    plot_signal_time = list(df = downsample(facs_df(ff_example()), 1000), ...),
    plot_n_x_n = list(df = downsample(facs_df(ff_example()), 1000), ...),
    plot_backgate = list(gs = gs_example(), gate_path = "/Cells/Singlets/Singlets/Live/CD45/Non-PMN/Mac", ...),
    plot_comp_n_x_n = list(x = gs_example(), ...),
    plot_histogram_facs = list(df = downsample(facs_df(ff_example()), 5000), x = "PE-Cy5-A", offset = FALSE, ...),
    plot_contour = list(df = downsample(facs_df(ff_example()), 50000), x = "PE-Cy5-A", y = "BUV395-A", ...),
    plot_dot = ,
    plot_pseudocolor = list(df = downsample(facs_df(ff_example()), 5000), x = "PE-Cy5-A", y = "BUV395-A", ...),
    list(df = covid, formula = age ~ gender, ...)
  )
  do.call(fn, args)
}
