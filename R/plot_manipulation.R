# Modify plot components --------------------------------------------------

#' Edit labels for 1 or more plots
#'
#' Functionality from package kwb.plot
#' @param plot_list List of ggplot objects
#' @param ... Enter name-value pair where names refer to plot component to relabel ("x", "y", "title") and values are new labels to be added entered as quoted string(s). If number of new labels is less than number of plots, terms will be recycled
#' @param indices Plots to apply new new labels to entered as number index. Default includes all plots
#' @param action Method for creating new plot label. Options: `"replace"` (default), `"paste_after"`, `"paste_before"`. Enter as quoted or unquoted action
#' @param sep Text used to separate prior label from new label. Default is `" "`
#' @returns List of ggplot objects with updated labels
#' @export
set_plot_labels <- function(plot_list, ..., indices = seq_along(plot_list), action = "replace", sep = " ") {
  args <- list(...)
  if (length(args)) {
    plot_labels <- lapply(args, rep, length.out = length(indices))
    plots[indices] <- lapply(seq_along(indices), function(i) {
      p <- plot_list[[indices[i]]]
      new_labels <- lapply(plot_labels, "[", i)
      if (action != "replace") {
        new_labels <- lapply(names(new_labels), function(j) {
          old_label <- ggplot2::labs(p)$labels[[j]]
          if (action == "paste_after") {
            paste(old_label, new_labels[[j]], sep = sep)
          } else {
            paste(new_labels[[j]], old_label, sep = sep)
          }
        })
        names(new_labels) <- names(plot_labels)
      }
      p + do.call(ggplot2::labs, new_labels)
    })
  }
  plots
}

#' Rotate x axis labels
#'
#' @inheritParams ggplot_add.axis_clean
#' @returns Not called directly by user
#' @export
ggplot_add.axis_x_rotated <- function(object, plot, object_name) {
  axis <- plot$theme$axis.text.x
  axis$hjust <- object$hjust
  axis$vjust <- object$vjust
  axis$angle <- object$angle
  blank <- ggplot2::element_blank()
  if (object$remove_title) {
    plot + ggplot2::theme(axis.text.x = axis, axis.title.x.bottom = blank, axis.title.x.top = blank)
  } else {
    plot + ggplot2::theme(axis.text.x = axis)
  }
}

#' Rotate x axis labels
#'
#' @param angle Angle to rotate x axis labels. Default is `45`
#' @param remove_title If `TRUE` (default), x axis title also removed
#' @returns ggplot object with x axis labels rotated `angle` degrees. Enter as `plot + rot_x_labs()`
#' @export
rot_x_labs <- function(angle = 45, remove_title = TRUE) {
  args <- list(angle = angle, remove_title = remove_title)
  if (angle > 0) {
    args$hjust <- 1
    args$vjust <- 1
  } else {
    args$hjust <- 0.5
    args$vjust <- 0.5
  }
  structure(args, class = "axis_x_rotated")
}

#' Adjust aspect ratio
#'
#' @param x ggplot object to resize. Default is last plot
#' @param ratio New aspect ratio. Defaults to golden ratio
#' @export
resize_plot <- function(x = last_plot(), ratio = 1.618034) {
  x + ggplot2::theme(aspect.ratio = ratio)
}

# Remove plot components --------------------------------------------------

#' Remove main geoms from plot
#'
#' @param x ggplot object
#' @param geom_pattern Pattern in geom class to search in layers. If `NULL` (default), all geoms are removed
#' @param nth_instance Integer vector indicating indices of layers with `geom_pattern` that should be removed. If `NULL` (default), all instances are removed
#' @export
remove_plot_geoms <- function(x, geom_pattern = NULL, nth_instance = NULL) {
  layers <- x$layers
  if (is.null(geom_pattern)) {
    layers <- list()
  } else {
    nth_layer <- grep(geom_pattern, lapply(layers, function(z) class(z$geom)), ignore.case = TRUE)
    n <- length(nth_layer)
    if (n == 0L) return(x)
    if (!is.null(nth_instance)) {
      nth_layer <- nth_layer[n]
    }
    layers <- layers[-nth_layer]
  }
  x$layers <- layers
  x
}

#' Edit theme components on a plot
#'
#' @param call Modified theme. Enter as `theme()`
#' @param plot ggplot object. Default is `NULL`
#' @returns ggplot object. Enter using `plot + call` or `call(plot)` syntax
#' @noRd
.modify_theme_component <- function(call, plot = NULL) {
  if (inherits(plot, "ggplot")) {
    plot + call
  } else {
    call
  }
}

#' Set theme element to `element_blank()`
#'
#' @param args Character vector of `ggplot2::theme()` arguments to remove from plot
#' @returns theme object with `x` removed. Enter as plot + remove_plot_element("axis.line.x")
#' @noRd
.set_theme_element_blank <- function(args) {
  new_theme <- ggplot2::theme()
  blank <- ggplot2::element_blank()
  for (i in args) {
    new_theme[[i]] <- blank
  }
  new_theme
}

#' Remove plot margins
#'
#' @param x ggplot object. If `NULL` (default), enter as `plot + remove_plot_margins()` to remove margins
#' @returns ggplot object
#' @export
remove_plot_margins <- function(x = NULL) {
  .modify_theme_component(theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "pt")), x)
}

#' Remove legend
#'
#' @rdname remove_plot_margins
#' @export
remove_plot_legend <- function(x = NULL) .modify_theme_component(ggplot2::theme(legend.position = "none"), x)

#' Remove axis
#'
#' @param x ggplot object. If `NULL` (default), enter as `plot + remove_plot_axis()` to remove axis components
#' @param axis Options: `"x"`, `"y"`, or both (default)
#' @param component Components of axis to remove. Options: `"lines"`, `"ticks"`, `"text"`, `"title"`. Default includes all. Enter as character vector. Enter `NULL` to keep all elements
#' @returns theme object with axis removed
#' @export
remove_plot_axis <- function(x = NULL, axis = c("x", "y"), component = c("lines", "ticks", "text", "title")) {
  if (is.null(component)) return(x)
  line <- any(grepl("line", component, fixed = TRUE))
  tick <- any(grepl("tick", component, fixed = TRUE))
  text <- any(grepl("text|label", component))
  title <- any(grepl("title", component, fixed = TRUE))
  component <- c(if (line) "axis.line", if (tick) "axis.ticks", if (text) "axis.text", if (title) "axis.title")
  axis <- c(if ("x" %in% axis) c("x.bottom", "x.top"), if ("y" %in% axis) c("y.left", "y.right"))
  component <- rep(component, each = length(axis))
  component <- paste(component, axis, sep = ".")
  x <- .modify_theme_component(.set_theme_element_blank(component), x)
  x
}

#' Remove axis lines
#'
#' @rdname remove_plot_axis
#' @export
remove_plot_axis_lines <- function(x = NULL, axis = c("x", "y")) {
  remove_plot_axis(x, axis = axis, component = "line")
}

#' Remove axis tick marks
#'
#' @rdname remove_plot_axis
#' @export
remove_plot_axis_ticks <- function(x = NULL, axis = c("x", "y")) {
  remove_plot_axis(x, axis = axis, component = "tick")
}

#' Remove axis labels
#'
#' @rdname remove_plot_axis
#' @export
remove_plot_axis_text <- function(x = NULL, axis = c("x", "y")) {
  remove_plot_axis(x, axis = axis, component = "text")
}

#' Remove axis title
#'
#' @rdname remove_plot_axis
#' @export
remove_plot_axis_title <- function(x = NULL, axis = c("x", "y")) {
  #.remove_grobs(x, "ylab|xlab")
  remove_plot_axis(x, axis = axis, component = "title")
}

#' Remove plot title
#'
#' @rdname remove_plot_axis
#' @export
remove_plot_title <- function(x = NULL) {
  #.remove_grobs(x, "\\btitle\\b")
  .modify_theme_component(.set_theme_element_blank("plot.title"), x)
}

#' Remove plot grid lines
#'
#' @rdname remove_plot_axis
#' @param major,minor If `TRUE` (default), component removed from plot
#' @export
remove_plot_gridlines <- function(x = NULL, axis = c("x", "y"), major = TRUE, minor = TRUE) {
  components <- sprintf("panel.grid.%s", c(if (major) "major", if (minor) "minor"))
  components <- paste(components, rep(axis, each = length(components)), sep = ".")
  .modify_theme_component(.set_theme_element_blank(components), x)
}

#' Keep certain grobs in ggplot object
#'
#' @param x ggplot object
#' @param pattern Substring pattern of grob names to include in plot. Enter as character vector. If multiple terms entered, grobs matching ANY values `pattern` will be included in output. Grob names: `ggplotGrob(x)$layout$name`
#' @returns ggplot object
#' @noRd
.keep_grobs <- function(x, pattern = NULL) {
  z <- ggplot2::ggplotGrob(x)
  grob_names <- z$layout$name
  idx <- grepl(paste(pattern, collapse = "|"), grob_names)
  if (all(idx)) return(x)
  z$layout <- z$layout[idx, , drop = FALSE]
  z$grobs <- z$grobs[idx]
  z <- gtable::gtable_trim(z)
  as_ggplot(z)
}

#' Remove grobs from ggplot object
#'
#' @param x ggplot object
#' @param pattern Substring pattern of grob names to remove from plot. Enter as character vector. If multiple terms entered, grobs matching ANY values `pattern` will be removed from plot. Grob names: `ggplotGrob(x)$layout$name`
#' @returns ggplot object
#' @noRd
.remove_grobs <- function(x, pattern = NULL) {
  z <- ggplot2::ggplotGrob(x)
  grob_names <- z$layout$name
  if (is.null(pattern) || all(idx <- !grepl(paste(pattern, collapse = "|"), grob_names))) return(x)
  z$layout <- z$layout[idx, , drop = FALSE]
  z$grobs <- z$grobs[idx]
  z <- gtable::gtable_trim(z)
  as_ggplot(z)
}

#' Remove plot clipping
#'
#' @param x ggplot object
#' @returns ggplot object
#' @export
unclip <- function(x) {
  ggplot2::ggplot() +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::theme_clean() +
    draw_grob(as_grob(x), x = 0, y = 0, width = 1, height = 1, scale = 1, hjust = 0, vjust = 0, halign = 0.5, valign = 0.5)
}
