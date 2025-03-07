#' Apply uniform plot dimensions to a list of plots
#'
#' @param plot_list List of ggplot objects
#' @returns List of plots with identical dimensions which can be passed to `plot_grid()`
#' @export
align_plot_dim <- function(plot_list) {
  pkg_required("patchwork")
  # Alternative: max_plot_dim <- patchwork::align_patches(plot_list); lapply(plot_list, function(x) patchwork::set_dim(plot = x, dim = max_plot_dim))
  patchwork::align_patches(plot_list)
}

#' List of multipanel plots for printing
#'
#' @param plot_list List of ggplot objects
#' @param plots_per_slide Number of plots per slide. Default is `6`
#' @param nrow,ncol Number of rows and column of plots per slide
#' @param byrow If `TRUE` (default), plots added rowwise. If `FALSE`, plots added columnwise
#' @param align Alignment of plots. Options: `"hv"` (default. Align columns of plots by their x axis components and rows of plots by their y axis components), `"h"` (align a column of plots by their x axis components), `"v"` (align a row of plots by their y axis components). Enter as length 1 character vector
#' @param axis Axis to align plots. Options: combination of `"t"`, `"r"`, `"b"`, `"l"`. Enter as length 1 character vector with no spaces between letters
#' @param ... Arguments passed to `plot_grid`
#' @returns List of multipanel plots that can be passed to `ppt()`
#' @export
multislide_plot_list <- function(
    plot_list,
    plots_per_slide = 6,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    align = "hv",
    axis = "trbl",
    ...) {
  if (inherits(plot_list, "ggplot") || plots_per_slide == 1L) return(plot_list)
  pkg_required("cowplot")
  #plot_list <- remove_null(plot_list)
  lapply(seq.int(from = 1, to = length(plot_list), by = plots_per_slide), function(i) {
    plots <- plot_list[seq.int(from = i, to = i + plots_per_slide - 1)]
    #plots <- plots[vapply(plots, function(z) any(!is.na(z)), logical(1), USE.NAMES = FALSE)]
    #plots <- plots[vapply(plots, function(z) length(z[!is.na(z)]) != 0L, logical(1), USE.NAMES = FALSE)]
    cowplot::plot_grid(plotlist = plots, nrow = nrow, ncol = ncol, byrow = byrow, align = align, axis = axis, ...)
  })
}

#' Determine number of rows and columns in plot grid
#'
#' Modified version of `ggplot2::wrap_dims` that allows for specification of `aspect_ratio`
#' @param n Number of plots
#' @param nrow,ncol Desired number of rows and columns in plot grid
#' @param aspect_ratio Desired aspect ratio (number of plot rows/number of plot columns) for plot grid. Only relevant when both `nrow` and `ncol` are missing. Default is `1` (i.e., tries to create a square grid). Values < 1 create wide layouts. Values > 1 create tall layouts
#' @returns Length 2 numeric vector containing number of rows and columns for plot grid. By default plot grid will be wide (i.e., when number of rows and columns are not equal, the greater value will be assigned to the number of columns)
#' @noRd
.plot_grid_dims <- function(n, nrow = NULL, ncol = NULL, aspect_ratio = 1) {
  nrow_missing <- is.null(nrow)
  ncol_missing <- is.null(ncol)
  if (nrow_missing && ncol_missing) {
    dims <- grDevices::n2mfrow(nr.plots = n, asp = aspect_ratio)
    nrow <- dims[2L]
    ncol <- dims[1L]
  } else if (ncol_missing) {
    ncol <- ceiling(n/nrow)
  } else if (nrow_missing) {
    nrow <- ceiling(n/ncol)
  }
  c(nrow, ncol)
}

#' Determine layout for an n x n plot matrix
#'
#' @param n Number of continuous variables to be plotted
#' @param upper_tri If `TRUE` (default), plots will form upper triangle of plot matrix. If `FALSE`, plots will form lower triangle of plot matrix
#' @param rowwise If `TRUE` (default), plots will be added row wise to plot matrix. If `FALSE`, plots will be added to plot matrix column wise
#' @param incl_blank_diag If `TRUE`, output will include diagonal containing `NA`. If `FALSE` (default), plot will not contain a blank diagonal
#' @returns Matrix containing index for each plot
#' @noRd
.plot_layout_n_x_n <- function(n, upper_tri = TRUE, rowwise = TRUE, incl_blank_diag = FALSE) {
  idx <- seq_len(sum(seq_len(n - 1)))
  out <- matrix(NA_integer_, ncol = n, nrow = n)
  if (rowwise) {
    # Row wise
    if (upper_tri) {
      # Upper triangle
      out[lower.tri(out)] <- idx
      out <- t.default(out)
      if (incl_blank_diag) out else out[-n, -1L]
    } else {
      # Lower triangle
      out[upper.tri(out)] <- idx
      out <- t.default(out)
      if (incl_blank_diag) out else out[-1L, -n]
    }
  } else {
    # Column wise
    if (upper_tri) {
      # Upper triangle
      out[upper.tri(out)] <- idx
      if (incl_blank_diag) out else out[-n, -1L]
    } else {
      # Lower triangle
      out[lower.tri(out)] <- idx
      if (incl_blank_diag) out else out[-1L, -n]
    }
  }
}
