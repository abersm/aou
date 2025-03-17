# Limits ------------------------------------------------------------------

#' Data limits (in raw data units) for all data frames used to generate plot
#'
#' @param x ggplot object
#' @param axis Options: `"both"` (default), `"x"`, `"y"`
#' @returns List containing `x` and `y`, each a length 2 numeric vector with limits in raw data units
#' @noRd
get_plot_data_limits <- function(x, axis = "both") {
  plot_data <- ggplot2::ggplot_build(x)$data
  switch(axis,
         y = {
           y_vars <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0")
           y_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% y_vars
             z[idx]
           }), use.names = FALSE)
           if (length(y_axis) == 0L) return(NULL) else Range(y_axis)
         },
         x = {
           x_vars <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0")
           x_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% x_vars
             z[idx]
           }), use.names = FALSE)
           if (length(x_axis) == 0L) return(NULL) else Range(x_axis)
         },
         both = {
           y_vars <- c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0")
           x_vars <- c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0")
           x_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% x_vars
             z[idx]
           }), use.names = FALSE)
           y_axis <- unlist(lapply(plot_data, function(z) {
             if (length(z) == 0L) return(NULL)
             idx <- vapply(z, is.numeric, logical(1), USE.NAMES = FALSE) & names(z) %in% y_vars
             z[idx]
           }), use.names = FALSE)
           x_axis <- if (length(x_axis) == 0L) NULL else Range(x_axis)
           y_axis <- if (length(y_axis) == 0L) NULL else Range(y_axis)
           list(x = x_axis, y = y_axis)
         })
}

#' Axis limits
#'
#' Limits for axis line. When clip = "off", data limits can extend past axis limits
#'
#' @rdname get_plot_data_limits
#' @param units Units for output. Options: `"data"` (default, raw data), `"npc"`
#' @returns If `axis = "x"` or `axis = "y"`, output is a length 2 numeric vector containing limits. If `axis = "both"`, output is a list containing `x` and `y`, each a length 2 numeric vector containing axis limits
#' @noRd
get_plot_axis_limits <- function(x, axis = "both", units = "data") {
  if (units == "npc") return(get_plot_limits(x))
  plot_scales <- ggplot2::layer_scales(x)
  switch(
    axis,
    x = {
      plot_scales$x$range$range %||% ggplot2::ggplot_build(x)$layout$panel_scales_x[[1L]]$break_info()$range
    },
    y = {
      #unlist(remove_null(lapply(seq_along(x$scales$scales), function(i) x$scales$scales[[i]]$limits)), use.names = FALSE)
      #max(ggplot2::ggplot_build(x)$layout$panel_scales_y[[1L]]$range$range)
      plot_scales$y$limits %||% ggplot2::ggplot_build(x)$layout$panel_scales_y[[1L]]$break_info()$range
    },
    both = ,
    {
      if (is.null(plot_scales$x$range$range) || is.null(plot_scales$y$limits)) {
        plot_build <- ggplot2::ggplot_build(x)
        list(
          x = plot_scales$x$range$range %||% plot_build$layout$panel_scales_x[[1L]]$break_info()$range,
          y = plot_scales$y$limits %||% plot_build$layout$panel_scales_y[[1L]]$break_info()$range
        )
      } else {
        list(
          x = plot_scales$x$range$range,
          y = plot_scales$y$limits
        )
      }
    })
}

#' Overall limits for plot (data + axis)
#'
#' @rdname get_plot_axis_limits
#' @param by_facet If `TRUE` and `x` contains facets, output will be a data frame with 1 row per facet and columns "panel", "row", "col", "vars" (facet variables), "xmin", "xmax", "ymin", "ymax". If `FALSE` (default), output is a list containing "x" and "y", each a length 2 numeric vector representing the x and y axis limits across all facets. Only relevant when `x` contains facets
#' @returns If `x` doesn't contain facets and/or `by_facet = FALSE`, will be a list containing `x` and `y` (each a length 2 numeric vector containing min and max across all facets). If plot has multiple facets and `by_facet = TRUE`, output will be a data frame with 1 row per facet and columns "panel", "row", "col", "vars" (facet variables), "xmin", "xmax", "ymin", "ymax"
#' @noRd
get_plot_limits <- function(x, by_facet = FALSE) {
  l <- ggplot2::ggplot_build(x)
  l <- l$layout
  plot_layout <- l$layout
  n <- length(plot_layout$PANEL)
  if (n == 1L) {
    l$coord$range(l$panel_params[[1L]])
  } else {
    df <- vec_to_df(
      panel = plot_layout$PANEL,
      row = plot_layout$ROW,
      col = plot_layout$COL
    )
    idx <- seq_len(n)
    facet_vars <- l$facet$vars()
    df$vars <- lapply(idx, function(i) {
      out <- lapply(facet_vars, function(j) plot_layout[[j]][i])
      names(out) <- facet_vars
      out
    })
    xy <- lapply(l$panel_params, l$coord$range)
    df$xmin <- vapply(xy, function(z) z$x[[1L]], numeric(1), USE.NAMES = FALSE)
    df$xmax <- vapply(xy, function(z) z$x[[2L]], numeric(1), USE.NAMES = FALSE)
    df$ymin <- vapply(xy, function(z) z$y[[1L]], numeric(1), USE.NAMES = FALSE)
    df$ymax <- vapply(xy, function(z) z$y[[2L]], numeric(1), USE.NAMES = FALSE)
    df
  }
}

# geoms -------------------------------------------------------------------

#' Get names of geoms used by plot
#'
#' @param x ggplot object
#' @returns Geom names as character vector in lower case
#' @noRd
get_plot_geom_names <- function(x) {
  tolower(vapply(x$layers, function(z) gsub("New|Geom", "", class(z$geom)[[1L]]), character(1), USE.NAMES = FALSE))
}

# Plot components ---------------------------------------------------------

#' Get axis title
#'
#' @noRd
get_plot_axis_title <- function(x, axis = "both") {
  plot_labels <- x$labels
  plot_build <- ggplot2::ggplot_build(x)$layout
  switch(axis,
         x = plot_build$panel_scales_x[[1L]]$name %W% plot_labels$x,
         y = plot_build$panel_scales_y[[1L]]$name %W% plot_labels$y,
         both = list(
           x = plot_build$panel_scales_x[[1L]]$name %W% plot_labels$x,
           y = plot_build$panel_scales_y[[1L]]$name %W% plot_labels$y
         )
  )
}

#' Get axis tick labels
#'
#' @returns Character vector containing axis tick labels
#' @noRd
get_plot_axis_labels <- function(x, axis = "both") {
  axis_labels <- ggplot2::ggplot_build(x)$layout$panel_params[[1L]]
  switch(axis,
         x = as.character(axis_labels$x$get_labels()),
         y = as.character(axis_labels$y$get_labels()),
         both = list(
           x = as.character(axis_labels$x$get_labels()),
           y = as.character(axis_labels$y$get_labels())))
}

#' Get axis breaks
#'
#' @param units Options: `"data"` (default, raw data units), `"npc"`
#' @noRd
get_plot_axis_breaks <- function(x, axis = "both", units = "data") {
  plot_layout <- ggplot2::ggplot_build(x)$layout
  if (units == "npc") {
    #x_breaks <- plot_layout$panel_scales_x[[1L]]$break_info()$major
    #y_breaks <- plot_layout$panel_scales_y[[1L]]$break_info()$major
    x_breaks <- plot_layout$panel_params[[1L]]$x$break_positions()
    y_breaks <- plot_layout$panel_params[[1L]]$y$break_positions()
  } else {
    #x_breaks <- plot_layout$panel_scales_x[[1L]]$get_breaks()
    #y_breaks <- plot_layout$panel_scales_y[[1L]]$get_breaks()
    #x_breaks <- layer_scales(x)$x$breaks
    #y_breaks <- layer_scales(x)$y$breaks
    x_breaks <- plot_layout$panel_params[[1L]]$x$breaks
    y_breaks <- plot_layout$panel_params[[1L]]$y$breaks
  }
  switch(axis,
         y = y_breaks,
         x = x_breaks,
         both = list(y = y_breaks, x = x_breaks))
}

#' Determine whether object is a waiver object
#'
#' Code from ggplot2
#' @param x Object to test
#' @returns Length 1 logical
#' @noRd
is_waiver <- function(x) inherits(x, "waiver")
