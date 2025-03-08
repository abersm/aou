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
