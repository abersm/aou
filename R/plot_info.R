#' Get npc coordinates of plot objects
#'
#' Functionality from https://stackoverflow.com/questions/60803424/npc-coordinates-of-geom-point-in-ggplot2
#' @param x ggplot object
#' @param object Pattern in object name to search for. Use `"point"` (default) for `geom_point`, `"rect"` for `geom_col`, `"line"` for `geom_line`. To include all plot objects, enter `object = "."`
#' @param idx Index of object if multiple grobs have the name specified in `object`. Default is `1`
#' @returns Data frame containing x and y values in npc units
#' @noRd
get_plot_object_npc_coords <- function(x, object = "point", idx = 1) {
  plot_dim <- grDevices::dev.size("cm")*10
  plot_width <- plot_dim[1L]
  plot_height <- plot_dim[2L]
  plot_grob <- ggplot2::ggplotGrob(x)
  to_mm <- function(x) grid::convertUnit(x, unitTo = "mm", valueOnly = TRUE)
  panel_row <- plot_grob$layout[plot_grob$layout$name == "panel", ]
  from_top <- sum(to_mm(plot_grob$heights[seq_len(panel_row$t - 1)]))
  from_left <- sum(to_mm(plot_grob$widths[seq_len(panel_row$l - 1)]))
  from_right <- sum(to_mm(plot_grob$widths[-seq_len(panel_row$l)]))
  from_bottom <- sum(to_mm(plot_grob$heights[-seq_len(panel_row$t)]))
  panel_height <- plot_height - from_top - from_bottom
  panel_width <- plot_width - from_left - from_right
  z <- gtable::gtable_filter(plot_grob, pattern = "panel", fixed = TRUE)$grobs[[1L]]$children
  grobs <- z[grep(object, names(z))]
  n_grobs <- length(grobs)
  if (n_grobs == 1L) {
    grobs <- .subset2(grobs, 1)
    y <- as.numeric(grobs$y)*panel_height + from_bottom
    x <- as.numeric(grobs$x)*panel_width + from_left
    vec_to_df(x = x/plot_width, y = y/plot_height)
  } else if (n_grobs == 0L) {
    NULL
  } else {
    idx <- seq_along(grobs)
    grob_names <- paste0(object, idx)
    rbind_list(grobs, grob_names, function(z, z_name) {
      y <- as.numeric(z$y)*panel_height + from_bottom
      x <- as.numeric(z$x)*panel_width + from_left
      vec_to_df(object = z_name, x = x/plot_width, y = y/plot_height)
    })
  }
}

#' grob height
#'
#' @param x grob
#' @param units Units for height. Default is `"mm"`
#' @noRd
grob_height <- function(x, units = "mm") {
  grid::convertUnit(grid::heightDetails(x), unitTo = units, valueOnly = TRUE)
}

#' grob width
#'
#' @param x grob
#' @param units Units for width. Default is `"mm"`
#' @noRd
grob_width <- function(x, units = "mm") {
  grid::convertUnit(grid::widthDetails(x), unitTo = units, valueOnly = TRUE)
}

# Limits ------------------------------------------------------------------

#' Data limits (in raw data units) for all data frames used to generate plot
#'
#' @param x ggplot object
#' @param axis Options: `"both"` (default), `"x"`, `"y"`
#' @returns List containing `x` and `y`, each a length 2 numeric vector with limits in raw data units
#' @export
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
#' @export
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
#' @export
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
#' @export
get_plot_geom_names <- function(x) {
  tolower(vapply(x$layers, function(z) gsub("New|Geom", "", class(z$geom)[[1L]]), character(1), USE.NAMES = FALSE))
}

#' List stat functions used by a plot
#'
#' @rdname get_plot_geom_names
#' @returns Stat names as character vector in lower case
#' @export
get_plot_stat_names <- function(x) {
  tolower(vapply(x$layers, function(z) gsub("New|Stat", "", class(z$geom)[[1L]]), character(1), USE.NAMES = FALSE))
}

#' Determine whether ggplot object contains a given type of geom
#'
#' @param x ggplot object
#' @param geom_class Class of geom used by plot ("GeomErrorbar", "GeomSegment", "GeomText", etc.)
#' @returns LEngth 1 logical vector
#' @noRd
.plot_contains_geom <- function(x, geom_class) {
  if (geom_class %in% c("Bar", "bar", "GeomBar")) {
    geom_class <- "GeomRect"
  } else {
    if (grepl("_", geom_class, fixed = TRUE)) {
      geom_class <- unlist(strsplit(geom_class, "_", fixed = TRUE), use.names = FALSE)
      geom_class <- paste(str_capitalize(geom_class), collapse = "")
    }
    if (grepl("geom", geom_class, fixed = TRUE)) {
      geom_class <- gsub("geom", "Geom", geom_class, fixed = TRUE)
    }
    if (!grepl("Geom", geom_class, fixed = TRUE)) {
      geom_class <- paste0("Geom", str_capitalize(geom_class))
    }
  }
  any(vapply(seq_along(x$layers), function(z) inherits(x$layers[[z]]$geom, geom_class), logical(1), USE.NAMES = FALSE))
}

# Plot components ---------------------------------------------------------

#' Get plot title
#'
#' @param x ggplot object
#' @returns Plot title as string
#' @export
get_plot_title <- function(x) x$labels$title

#' Extract plot variables
#'
#' @rdname get_plot_aes
#' @export
get_plot_vars <- get_plot_aes

#' Get variables used to generate x and y axes
#'
#' @param x ggplot object
#' @param axis Options: `"both"` (default), `"x"`, `"y"`
#' @returns Raw data input variables used to generate x and/or y axis
#' @export
get_plot_axis_vars <- function(x, axis = "both") {
  vars <- get_plot_aes(x)
  if (axis == "both") {
    # Use .subset2 rather than $ to avoid partial matching issues (though highly unlikely to be a problem)
    list(x = .subset2(vars, "x"), y = .subset2(vars, "y"))
  } else {
    .subset2(vars, axis)
  }
}

#' Get axis title
#'
#' @rdname get_plot_axis_labels
#' @export
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
#' @rdname get_plot_axis_vars
#' @returns Character vector containing axis tick labels
#' @export
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
#' @rdname get_plot_axis_vars
#' @param units Options: `"data"` (default, raw data units), `"npc"`
#' @export
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

#' Get plot aspect ratio
#'
#' @rdname get_plot_title
#' @returns List containing entry into theme function for aspect ratio and actual aspect ratio in data units (if set)
#' @export
get_plot_aspect_ratio <- function(x) {
  z <- get_plot_axis_limits(x)
  list(
    theme = x$theme$aspect.ratio,
    data_units_y_to_x = ggplot2::ggplot_build(x)$layout$coord$ratio,
    axis = (z$y[2L] - z$y[1L])/(z$x[2L] - z$x[1L]))
}

#' Determine number of ggplot plots in an object
#'
#' @param x Object
#' @returns Length 1 integer
#' @noRd
n_ggplots <- function(x) if (inherits(x, "ggplot")) 1L else length(x)

#' Determine whether object is a waiver object
#'
#' Code from ggplot2
#' @param x Object to test
#' @returns Length 1 logical
#' @noRd
is_waiver <- function(x) inherits(x, "waiver")

#' Extract information about x and y axis for each panel
#'
#' Rewritten version of `ggplot2::summarise_layout`
#' @param x ggplot object
#' @returns If `x` has no facets, output is a list with "x" and "y" (x and y axis limits in data units), and "x_scale" and "y_scale" (ggplot2 scales). If `x` has facets, output is a data frame with 1 row per facet with columns "panel", "row", "col", "vars" (facet variables), "xmin", "xmax", "ymin", "ymax", "xscale", "yscale"
#' @noRd
.ggplot_layout <- function(x) {
  x <- ggplot2::ggplot_build(x)
  l <- x$layout
  layout <- l$layout
  n <- length(layout$PANEL)
  if (n == 1L) {
    xy_limits <- l$coord$range(l$panel_params[[1L]])
    z <- l$get_scales(1)
    xy_limits$x_scale <- z$x
    xy_limits$y_scale <- z$y
    xy_limits
  } else {
    df <- vec_to_df(
      panel = layout$PANEL,
      row = layout$ROW,
      col = layout$COL
    )
    idx <- seq_len(n)
    facet_vars <- l$facet$vars()
    df$vars <- lapply(idx, function(i) {
      out <- lapply(facet_vars, function(j) layout[[j]][i])
      names(out) <- facet_vars
      out
    })
    xy <- lapply(l$panel_params, l$coord$range)
    df$xmin <- vapply(xy, function(z) z$x[[1L]], numeric(1), USE.NAMES = FALSE)
    df$xmax <- vapply(xy, function(z) z$x[[2L]], numeric(1), USE.NAMES = FALSE)
    df$ymin <- vapply(xy, function(z) z$y[[1L]], numeric(1), USE.NAMES = FALSE)
    df$ymax <- vapply(xy, function(z) z$y[[2L]], numeric(1), USE.NAMES = FALSE)
    #idx <- lapply(idx, function(n) l$get_scales(n))
    idx <- lapply(idx, l$get_scales)
    df$xscale <- lapply(idx, function(n) n$x)
    df$yscale <- lapply(idx, function(n) n$y)
    df
  }
}

#' Viewport settings
#'
#' @param units Units for output. Enter as string
#' @returns List containing viewport width, height, length, x, y
#' @noRd
viewport_settings <- function(units = "bigpts") {
  unit_1 <- grid::unit(1, "npc")
  unit_half <- grid::unit(0.5, "npc")
  width <- grid::convertWidth(unit_1, units, valueOnly = TRUE)
  height <- grid::convertHeight(unit_1, units, valueOnly = TRUE)
  list(
    width = width,
    height = height,
    length = max(width, height),
    x = grid::convertX(unit_half, units, valueOnly = TRUE),
    y = grid::convertY(unit_half, units, valueOnly = TRUE)
  )
}
