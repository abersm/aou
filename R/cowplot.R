#' Draw grob
#'
#' Functionality from Claus Wilke's excellent package cowplot
#' @param grob grob object
#' @param x,y x and y starting positions Enter as numeric 0-1. Default is `1` for both
#' @param height,width Height and width of grob Enter as numeric 0-1. Default is `0` for both
#' @param scale Scaling factor for plot. Default is `1`
#' @param clip Clipping for grob. Default is `"inherit"` (inherits from input grob)
#' @param hjust Adjust horizontal position of each label. More negative values move the label further to the right on the plot canvas. Can be a single value (applied to all labels) or a vector of values (one for each label). Default is `-0.5`
#' @param vjust Adjust vertical position of each label. More positive values move the label further down on the plot canvas. Can be a single value (applied to all labels) or a vector of values (one for each label). Default is `1.5`
#' @param halign,valign Horizontal and vertical alignment, respectively. Enter as numeric 0-1. Default is `0.5` for both
#' @returns ggproto
#' @noRd
draw_grob <- function(
    grob,
    x = 0,
    y = 0,
    width = 1,
    height = 1,
    scale = 1,
    clip = "inherit",
    hjust = 0,
    vjust = 0,
    halign = 0.5,
    valign = 0.5) {
  ggplot2::layer(
    data = vec_to_df(x = NA),
    stat = ggplot2::StatIdentity,
    position = ggplot2::PositionIdentity,
    geom = GeomDrawGrob,
    inherit.aes = FALSE,
    params = list(
      grob = grob,
      xmin = x - hjust*width,
      xmax = x + (1 - hjust)*width,
      ymin = y - vjust*height,
      ymax = y + (1 - vjust)*height,
      scale = scale,
      clip = clip,
      halign = halign,
      valign = valign
    )
  )
}

#' Geom for draw_grob
#'
#' @returns ggproto
#' @export
GeomDrawGrob <- ggplot2::ggproto(
  "GeomDrawGrob",
  ggplot2::GeomCustomAnn,
  draw_panel = function(self, data, panel_params, coord, grob, xmin, xmax, ymin, ymax, scale = 1, clip = "inherit", halign = 0.5, valign = 0.5) {
    corners <- vec_to_df(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord$transform(corners, panel_params)
    x_rng <- range(data$x, na.rm = TRUE)
    x_min <- x_rng[1L]
    x_delta <- x_rng[2L] - x_min
    y_rng <- range(data$y, na.rm = TRUE)
    y_min <- y_rng[1L]
    y_delta <- y_rng[2L] - y_min
    vp_outer <- grid::viewport(
      x = x_min + halign*x_delta,
      y = y_min + valign*y_delta,
      width = x_delta,
      height = y_delta,
      just = c(halign, valign),
      clip = clip
    )
    vp_inner <- grid::viewport(
      x = halign,
      y = valign,
      width = scale,
      height = scale,
      just = c(halign, valign)
    )
    id <- local({
      i <- 1
      function() {
        i <<- i + 1
        i
      }
    })()
    inner_grob <- grid::grobTree(grob, vp = vp_inner, name = paste(grob$name, id))
    grid::grobTree(inner_grob, vp = vp_outer, name = paste("GeomDrawGrob", id))
  }
)

#' Arrange multiple plots into a grid
#'
#' @param ... Comma separated list of plots
#' @param plotlist List of plots
#' @param align Options: `"none"` (default), `"horizontal"`, `"vertical"`, `"hv"` (both)
#' @param axis Options: `"none"` (default), `"left"`, `"top"`, `"bottom"`, `"right"`, or any combination (i.e. "tbrl")
#' @param greedy Adjustment method for plot alignment
#' @param nrow,ncol Number of rows and columns, respectively, in plot grid
#' @param byrow If `TRUE` (default), plots arranged by row. If `FALSE`, plots arranged by column
#' @param scale Scaling factor for plot. Default is `1`
#' @param rel_heights,rel_widths Relative column heights and widths, respectively, in plot grid. `rel_widths = c(2, 1)` will make the 1st column 2x width of 2nd column
#' @param labels List of labels to be added to plots. Enter `labels = "AUTO"` for upper case and `labels = "auto"` for lower case
#' @param label_size Font size for labels. Default is `14`
#' @param label_fontfamily Font family for labels
#' @param label_fontface Font face for labels
#' @param label_colour Font color for labels
#' @param label_x,label_y x and y positions respectively for labels relative to the given subplot. Numeric 0-1
#' @param hjust Adjust horizontal position of each label. More negative values move the label further to the right on the plot canvas. Can be a single value (applied to all labels) or a vector of values (one for each label). Default is `-0.5`
#' @param vjust Adjust vertical position of each label. More positive values move the label further down on the plot canvas. Can be a single value (applied to all labels) or a vector of values (one for each label). Default is `1.5`
#' @returns ggplot
#' @export
plot_grid <- function(
  ...,
  plotlist = NULL,
  align = c("none", "h", "v", "hv"),
  axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
  nrow = NULL,
  ncol = NULL,
  byrow = TRUE,
  scale = 1,
  rel_widths = 1,
  rel_heights = 1,
  labels = NULL,
  label_size = 14,
  label_fontfamily = NULL,
  label_fontface = "bold",
  label_colour = NULL,
  label_x = 0,
  label_y = 1,
  hjust = -0.5,
  vjust = 1.5,
  greedy = TRUE) {
  # List of plots
  plots <- c(list(...), plotlist)
  n_plots <- length(plots)

  # ncol, nrow
  #ncol <- ncol %||% if (length(ncol <- ceiling(n_plots/nrow)) != 0L) ncol else ceiling(sqrt(n_plots))
  #nrow <- nrow %||% ceiling(n_plots/ncol)
  dims <- .plot_grid_dims(n = n_plots, nrow = nrow, ncol = ncol)
  nrow <- dims[1L]
  ncol <- dims[2L]

  # Plot order
  m <- ncol*nrow
  if (!byrow) {
    plots <- plots[c(t.default(matrix(c(seq_len(n_plots), rep(NA, m - n_plots)), nrow = nrow, byrow = FALSE)))]
  }

  # Alignment
  grobs <- align_plots(plotlist = plots, align = align, axis = axis, greedy = greedy)

  # Labels
  if (any(labels == "AUTO")) {
    labels <- LETTERS[seq_len(n_plots)]
  } else if (any(labels == "auto")) {
    labels <- letters[seq_len(n_plots)]
  }

  # Label position
  n_labels <- length(labels)
  hjust <- rep_len(hjust, n_labels)
  vjust <- rep_len(vjust, n_labels)
  label_x <- rep_len(label_x, n_labels)
  label_y <- rep_len(label_y, n_labels)

  # rel_heights, rel_widths
  rel_heights <- rep(rel_heights, length.out = nrow)
  rel_widths <- rep(rel_widths, length.out = ncol)
  sum_rel_width <- sum(rel_widths)
  sum_rel_height <- sum(rel_heights)
  x_deltas <- rel_widths/sum_rel_width
  y_deltas <- rel_heights/sum_rel_height
  xs <- cumsum(rel_widths)/sum_rel_width - x_deltas
  ys <- 1 - cumsum(rel_heights)/sum_rel_height

  # Plot layout
  p <- ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    theme_clean()
  col_count <- 0
  row_count <- 1
  scale <- rep(scale, length.out = m)
  for (i in seq_len(m)) {
    if (i > n_plots) break
    n_cols_1 <- col_count + 1
    x_delta <- x_deltas[n_cols_1]
    y_delta <- y_deltas[row_count]
    x <- xs[n_cols_1]
    y <- ys[row_count]
    p_next <- grobs[[i]]
    if (!is.null(p_next)) {
      p <- p + draw_grob(p_next, x, y, x_delta, y_delta, scale[i])
    }
    if (i <= n_labels) {
      # p <- p + cowplot::draw_plot_label(labels[i], x + label_x[i]*x_delta, y + label_y[i]*y_delta, size = label_size, family = label_fontfamily, fontface = label_fontface, colour = label_colour, hjust = hjust[i], vjust = vjust[i])
      p <- p +
        ggplot2::geom_text(
          data = vec_to_df(text = labels[i], x = x + label_x[i]*x_delta, y = y + label_y[i]*y_delta),
          text = labels[i],
          x = x,
          y = y,
          ggplot2::aes(x = x, y = y, label = text),
          hjust = hjust[i],
          vjust = vjust[i],
          size = label_size,
          size.unit = "pt",
          fontface = label_fontface,
          family = label_fontfamily %||% ggplot2::theme_get()$text$family,
          color = label_colour %||% ggplot2::theme_get()$text$colour,
          inherit.aes = FALSE
        )
    }
    col_count <- col_count + 1
    if (col_count >= ncol) {
      col_count <- 0
      row_count <- row_count + 1
    }
  }
  p
}

#' Align plots
#'
#' Used by `plot_grid`
#' @rdname plot_grid
#' @export
align_plots <- function(
    ...,
    plotlist = NULL,
    align = c("none", "h", "v", "hv"),
    axis = c("none", "l", "r", "t", "b", "lr", "tb", "tblr"),
    greedy = TRUE) {
  plots <- c(list(...), plotlist)
  num_plots <- length(plots)
  one_null <- grid::unit(1, "null")
  grobs <- lapply(plots, function(x) {
    if (!is.null(x)) {
      x <- as_grob(x)
      if (inherits(x, "gtable")) {
        x
      } else {
        x <- gtable::gtable_col(NULL, list(x), one_null, one_null)
        x$layout$clip <- "inherit"
        x
      }
    } else {
      NULL
    }
  })
  halign <- switch(align[1L], h = , vh = , hv = TRUE, FALSE)
  valign <- switch(align[1L], v = , vh = , hv = TRUE, FALSE)
  vcomplex_align <- hcomplex_align <- FALSE
  if (valign) {
    num_widths <- unique(lapply(grobs, function(z) length(z$widths)))
    num_widths[num_widths == 0] <- NULL
    if (length(num_widths) > 1L || any(grepl("l|r", axis[1L]))) {
      vcomplex_align <- TRUE
      if (axis[1L] == "none") {
        Warning("Plots cannot be vertically aligned unless 'axis' parameter is set")
        valign <- FALSE
      }
      max_widths <- lapply(grobs, function(z) z$widths)
      if (any(grepl("l", axis[1L], fixed = TRUE))) {
        max_widths <- align_margin(max_widths, "first", greedy = greedy)
      }
      if (any(grepl("r", axis[1L], fixed = TRUE))) {
        max_widths <- align_margin(max_widths, "last", greedy = greedy)
      }
    } else {
      max_widths <- list(do.call(grid::unit.pmax, lapply(grobs, function(z) z$widths)))
    }
  }
  if (halign) {
    num_heights <- unique(lapply(grobs, function(z) length(z$heights)))
    num_heights[num_heights == 0] <- NULL
    if (length(num_heights) > 1L || any(grepl("t|b", axis[1L]))) {
      hcomplex_align <- TRUE
      if (axis[1L] == "none") {
        Warning("Plots cannot be horizontally aligned unless 'axis' parameter is set")
        halign <- FALSE
      }
      max_heights <- lapply(grobs, function(z) z$heights)
      if (any(grepl("t", axis[1L], fixed = TRUE))) {
        max_heights <- align_margin(max_heights, "first", greedy = greedy)
      }
      if (any(grepl("b", axis[1L], fixed = TRUE))) {
        max_heights <- align_margin(max_heights, "last", greedy = greedy)
      }
    } else {
      max_heights <- list(do.call(grid::unit.pmax, lapply(grobs, function(z) z$heights)))
    }
  }
  for (i in seq_len(num_plots)) {
    if (!is.null(grobs[[i]])) {
      if (valign) {
        grobs[[i]]$widths <- if (vcomplex_align) max_widths[[i]] else max_widths[[1L]]
      }
      if (halign) {
        grobs[[i]]$heights <- if (hcomplex_align) max_heights[[i]] else max_heights[[1L]]
      }
    }
  }
  grobs
}

#' Align margins
#'
#' Used by `align_plots`
#' @param sizes Size of plots
#' @param margin_to_align Margin used to align plots. Options: `"first"`, `"last"`
#' @param greedy Adjustment method for plot alignment
#' @noRd
align_margin <- function(sizes, margin_to_align, greedy = TRUE) {
  switch(margin_to_align,
    first = {
      list_indices <- lapply(sizes, function(x) {
        null_idx <- grep("null", x, fixed = TRUE)
        if (length(null_idx) == 0L) return(NULL)
        first_null_idx <- null_idx[1L]
        if (first_null_idx < 2) return(NULL)
        seq_len(first_null_idx - 1)
      })
      extreme_margin <- lapply(sizes, function(x) 1)
    },
    last = {
      list_indices <- lapply(sizes, function(x) {
        null_idx <- grep("null", x, fixed = TRUE)
        n_null <- length(null_idx)
        if (n_null == 0L) return(NULL)
        last_null_idx <- null_idx[n_null]
        n <- length(x)
        if (last_null_idx == n) return(NULL)
        seq.int(from = last_null_idx + 1, to = n)
      })
      extreme_margin <- lengths(sizes, use.names = FALSE)
    }
  )
  grob_seq <- seq_along(list_indices)
  grob_exclude <- which(vapply(list_indices, is.null, logical(1), USE.NAMES = FALSE))
  num <- unique.default(lengths(list_indices, use.names = FALSE))
  num <- num[num != 0]
  zero_units <- grid::unit(0, "pt")
  if (greedy || length(num) > 1L) {
    margins <- lapply(grob_seq, function(x) {
      if (x %!in% grob_exclude) {
        sum(sizes[[x]][list_indices[[x]]])
      } else {
        zero_units
      }
    })
    largest_margin <- max(do.call(grid::unit.c, margins))
    lapply(grob_seq, function(x) {
      if (x %!in% grob_exclude) {
        sizes[[x]][extreme_margin[[x]]] <- largest_margin - sum(sizes[[x]][list_indices[[x]][which(list_indices[[x]] != extreme_margin[[x]])]])
      }
      sizes[[x]]
    })
  } else {
    grob_seq_nonex <- grob_seq[grob_seq %!in% grob_exclude]
    max_margins <- do.call(grid::unit.pmax, lapply(grob_seq_nonex, function(x) sizes[[x]][list_indices[[x]]]))
    lapply(grob_seq, function(x) {
      if (x %!in% grob_exclude) {
        sizes[[x]][list_indices[[x]]] <- max_margins
      }
      sizes[[x]]
    })
  }
}

#' Convert object to grob
#'
#' @param x grob, gList, ggplot, recordedplot, patchwork
#' @returns grob object
#' @noRd
as_grob <- function(x) {
  if (inherits(x, "grob")) {
    x
  } else if (inherits(x, "gList")) {
    grid::grobTree(x)
  } else if (inherits(x, c("ggplot", "patchwork"))) {
    cur_dev <- grDevices::dev.cur()
    grDevices::pdf(NULL, width = 6, height = 6)
    grDevices::dev.control("enable")
    null_dev <- grDevices::dev.cur()
    on.exit({
      grDevices::dev.off(null_dev)
      if (cur_dev > 1) grDevices::dev.set(cur_dev)
    })
    if (inherits(x, "patchwork")) {
      pkg_required("patchwork")
      patchwork::patchworkGrob(x)
    } else {
      ggplot2::ggplotGrob(x)
    }
  } else if (inherits(x, "recordedplot")) {
    tryCatch({
      grDevices::replayPlot(x)
      grid::grid.grab()
    }, error = function(e) {
      pkg_required("gridGraphics")
      gridGraphics::echoGrob(x, device = function(width, height) {
        grDevices::pdf(NULL, width = width, height = height)
        grDevices::dev.control("enable")
      })
    })
  } else {
    grid::nullGrob()
  }
}

#' Convert input to gtable object
#'
#' Functionality from Claus Wilke's excellent package cowplot
#' @param x gtable, grob, gList, ggplot, recordedplot, patchwork
#' @returns gtable
#' @noRd
as_gtable <- function(x) {
  if (inherits(x, "gtable")) return(x)
  x <- as_grob(x)
  z <- grid::unit(1, "null")
  z <- gtable::gtable_col(NULL, list(x), z, z)
  z$layout$clip <- "inherit"
  z
}

#' Theme for cowplot functions
#'
#' @rdname theme_custom
#' @export
theme_clean <- function(base_size = 14, font_family = "") {
  blank <- ggplot2::element_blank()
  zero_cm <- grid::unit(0, "cm")
  base_pt <- grid::unit(base_size, "pt")
  zero_margin <- ggplot2::margin()
  ggplot2::theme_void(base_size = base_size, base_family = font_family) %+replace%
    ggplot2::theme(
      line = blank,
      rect = blank,
      text = ggplot2::element_text(family = font_family, face = "plain", color = "black", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = zero_margin, debug = FALSE),
      axis.line = blank,
      axis.line.x = NULL,
      axis.line.y = NULL,
      axis.text = blank,
      axis.text.x = NULL,
      axis.text.x.top = NULL,
      axis.text.y = NULL,
      axis.text.y.right = NULL,
      axis.ticks = blank,
      axis.ticks.length = grid::unit(0, "pt"),
      axis.title = blank,
      axis.title.x = NULL,
      axis.title.x.top = NULL,
      axis.title.y = NULL,
      axis.title.y.right = NULL,
      legend.background = blank,
      legend.spacing = base_pt,
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.margin = zero_margin,
      legend.key = blank,
      legend.key.size = grid::unit(1.1*base_size, "pt"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text.align = NULL,
      legend.title = ggplot2::element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position = "none",
      legend.direction = NULL,
      legend.justification = "center",
      legend.box = NULL,
      legend.box.margin = zero_margin,
      legend.box.background = blank,
      legend.box.spacing = base_pt,
      panel.background = blank,
      panel.border = blank,
      panel.grid = blank,
      panel.grid.major = NULL,
      panel.grid.minor = NULL,
      panel.spacing = grid::unit(base_size/2, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.ontop = FALSE,
      strip.background = blank,
      strip.text = blank,
      strip.text.x = NULL,
      strip.text.y = NULL,
      strip.placement = "inside",
      strip.placement.x = NULL,
      strip.placement.y = NULL,
      strip.switch.pad.grid = zero_cm,
      strip.switch.pad.wrap = zero_cm,
      plot.background = blank,
      plot.title = blank,
      plot.subtitle = blank,
      plot.caption = blank,
      plot.tag = ggplot2::element_text(face = "bold", hjust = 0, vjust = 0.7),
      plot.tag.position = c(0, 1),
      plot.margin = zero_margin,
      complete = TRUE
    )
}
