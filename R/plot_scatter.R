#' Scatter plot
#'
#' @param x,y Continuous variables variables. Enter as quoted or unquoted variable names
#' @param point_border_color_var Variable used to determine border color of points. Enter as quoted or unquoted variable name
#' @param point_size_var Variable used to determine point size. Enter as quoted or unquoted variable name
#' @param border_color Alias for `point_border_color`
#' @param point_shape Point shape Options: `"circle"` (default), `"square"`, `"triangle"`, `"diamond"`
#' @param shape,size,stroke Alias for `point_shape`, `point_size`, `point_border_thickness` respectively
#' @param x_scale Scale used for x axis. Options: `"regular"` (default), `"log"`, `"scientific"`
#' @param x_min Minimum value for x axis. Default is `NULL`
#' @param x_max Maximum value for x axis. Default is `NULL`
#' @param scale Alias for `y_scale` and `x_scale` that sets both simultaneously
#' @param expand_y,expand_x Expansion to add to y and x axis, respectively. Default is `0.1`
#' @param expand Alias for `expand_y` and `expand_x.` Sets both simultaneously
#' @inheritParams plot_cor
#' @inheritParams plot_point
#' @returns ggplot
#' @export
plot_scatter <- function(
    df,
    formula = NULL,
    x = NULL,
    y = NULL,
    size = 3,
    point_size = size,
    point_size_var = NULL,
    shape = c("circle", "square", "triangle", "diamond"),
    point_shape = shape,
    point_shape_var = NULL,
    colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    point_colors = colors,
    point_color_var = NULL,
    border_color = "black",
    point_border_color = border_color,
    point_border_color_var = NULL,
    stroke = 0.75,
    point_border_thickness = stroke,
    alpha = 0.75,
    point_alpha = alpha,
    n_breaks = 3,
    y_max = NULL,
    y_min = NULL,
    x_max = NULL,
    x_min = NULL,
    y_axis_title = waiver(),
    y_axis_breaks = NULL,
    y_axis_labels = NULL,
    x_axis_title = waiver(),
    x_axis_breaks = NULL,
    x_axis_labels = NULL,
    expand = 0.1,
    expand_y = expand,
    expand_x = expand,
    scale = "regular",
    x_scale = scale,
    y_scale = scale,
    show_legend = FALSE,
    censor_fn = rescale_none,
    plot_title = NULL,
    legend_title = "",
    ...) {
  plot_fn <- "plot_scatter"
  # Data setup
  point_color_var <- get_input(point_color_var)
  point_size_var <- get_input(point_size_var)
  point_shape_var <- get_input(point_shape_var)
  point_border_color_var <- get_input(point_border_color_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = plot_fn)
  x <- vars$x
  y <- vars$y
  df <- remove_na(df, c(x, y))

  # Build point components
  set_args <- list(show.legend = show_legend, alpha = point_alpha, stroke = point_border_thickness)
  mapped_args <- list()

  ## Shape
  look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
  point_shapes <- look_up_point_shape[point_shape]
  names(point_shapes) <- NULL
  if (is.null(point_shape_var) || is.null(z <- .subset2(df, point_shape_var)) || (n <- length(unique(z))) == 1L) {
    set_args$shape <- point_shapes[1L]
  } else {
    point_shapes <- rep(point_shapes, length.out = n)
    mapped_args <- alist(shape = .data[[point_shape_var]])
    if (is_categorical(z)) {
      df[[point_shape_var]] <- as_fct(z)
    }
  }

  ## Size
  if (is.null(point_size_var) || is.null(z <- .subset2(df, point_size_var)) || (n <- length(unique(z))) == 1L) {
    set_args$size <- point_size[1L]
  } else {
    point_size <- rep(point_size, length.out = n)
    mapped_args <- c(mapped_args, alist(size = .data[[point_size_var]]))
  }

  ## Color
  if (is.null(point_color_var) || is.null(z <- .subset2(df, point_color_var)) || (n <- length(unique(z))) == 1L) {
    set_args$fill <- point_colors[1L]
    point_fill <- NULL
  } else {
    mapped_args <- c(mapped_args, alist(fill = .data[[point_color_var]]))
    if (is_categorical(z)) {
      df[[point_color_var]] <- as_fct(z)
      point_colors <- rep(point_colors, length.out = n)
      point_fill <- ggplot2::scale_fill_manual(name = legend_title, values = point_colors)
    } else {
      point_fill <- ggplot2::scale_fill_gradient(name = legend_title, low = point_colors[1L], high = point_colors[[length(point_colors)]])
    }
  }

  ## Border color
  if (is.null(point_border_color_var) || is.null(z <- .subset2(df, point_border_color_var)) || (n <- length(unique(z))) == 1L) {
    set_args$color <- point_border_color[1L]
  } else {
    point_border_color <- rep(point_border_color, length.out = n)
    mapped_args <- c(mapped_args, alist(color = .data[[point_border_color_var]]))
    if (is_categorical(z)) {
      df$point_border_color_var <- as_fct(z)
    }
  }

  ## Geom
  set_args$mapping <- if (length(mapped_args) == 0L) NULL else do.call("aes", mapped_args)
  point <- do.call("geom_point", set_args)

  # Axes
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = df[[y]])
  x_limits <- .set_axis_limits(.min = x_min, .max = x_max, .breaks = x_axis_breaks, .values = df[[x]])
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)

  # Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    point +
    point_fill +
    ggplot2::scale_color_manual(name = NULL, values = point_border_color) +
    ggplot2::scale_shape_manual(name = NULL, values = point_shapes) +
    ggplot2::scale_size() +
    scale_axis(
      axis = "y",
      scale = y_scale,
      title = y_axis_title,
      breaks = waiver(),
      labels = waiver(),
      expand_lower = expand_y
    ) +
    scale_axis(
      axis = "x",
      scale = x_scale,
      title = x_axis_title,
      breaks = waiver(),
      labels = waiver(),
      expand_lower = expand_x
    ) +
    #scale_continuous(axis = "y", scale = y_scale, limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn) +
    #scale_continuous(axis = "x", scale = x_scale, limits = x_limits, breaks = x_axis_breaks, labels = x_axis_labels, title = x_axis_title, expand_lower = expand_x, n_breaks = n_breaks, censor_fn = censor_fn) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::ggtitle(plot_title) +
    theme_custom(...)
  p
}


#' t-SNE plot colored by continuous or categorical variable
#'
#' @inheritParams plot_contour
#' @param df Data frame
#' @param x,y Variables used for x and y axis respectively. Default is `"tsne_1"` for x and `"tsne_2"` for y. Enter as quoted or unquoted variable names
#' @param point_color_var If not specified, plot is colored by density. Default is `NULL`
#' @param point_color_type Options: `"continuous"`, `"factor"`
#' @param facet_var Variable used to create plot facets. Enter as quoted or unquoted variable name
#' @param show_contour If `TRUE`, contour lines are displayed. Default is `FALSE`
#' @param show_centroid_label If `TRUE`, labels of the centroid of each factor are shown on plot. Only relevant when `point_color_type = "factor"`
#' @param hex If `TRUE`, data are binned by expression level and mean for each bin is plotted
#' @param n_bins Number of bins to split data into. Only relevant when `hex = TRUE`. Default is `30`
#' @param colors Character vector of hexadecimal codes (or color names)
#' @param min_col_threshold Minimum threshold for color scale. Values below this limit will be colored as the chosen minimum threshold. Default is `0.01`
#' @param max_col_threshold Maximum threshold for color scale. Values above this limit will be colored as the chosen maximum threshold. Default is `0.995`
#' @param show_legend If `TRUE`, legend will be displayed
#' @param plot_title Plot title. Default is `NULL`
#' @param point_size Size of points. Default is `0.8`
#' @param alpha,point_alpha Point opacity. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param axis_arrows If `TRUE`, axes are displayed as arrows
#' @param ratio,aspect_ratio Aspect ratio
#' @param theme_fn Function used to generate theme. Enter with or without parenthesis. Default is `facs_arrows`
#' @param n_breaks Number of desired breaks. Default is `5`
#' @param x_title,y_title Titles for x and y axis, respectively. Only relevant when `theme_fn = facs_arrows`
#' @param rasterize If `TRUE`, points are generated using `scattermore::geom_scattermore`. Default is `FALSE`
#' @param ... Arguments passed to `facet_wrap`
#' @export
plot_tsne <- function(
    df,
    x = "tsne_1",
    y = "tsne_2",
    color_var = NULL,
    point_color_var = color_var,
    point_color_type = "continuous",
    facet_var = NULL,
    show_contour = FALSE,
    contour_lines = 0.1,
    contour_line_thickness = 0.25,
    show_centroid_label = FALSE,
    hex = FALSE,
    n_bins = 30,
    colors = "turbo",
    min_col_threshold = 0.01,
    max_col_threshold = 0.995,
    show_legend = FALSE,
    plot_title = NULL,
    point_size = 0.8,
    alpha = 0.95,
    point_alpha = alpha,
    n_breaks = 5,
    ratio = 1,
    aspect_ratio = ratio,
    theme_fn = facs_arrows,
    x_title = waiver(),
    y_title = waiver(),
    x_axis_title = x_title,
    y_axis_title = y_title,
    rasterize = FALSE,
    axis_arrows = TRUE,
    ...) {
  plot_fn <- "plot_tsne"

  # Variables
  x <- get_input(x)
  y <- get_input(y)
  if (is.null(point_color_var)) {
    point_color_continuous <- point_color_categorical <- FALSE
  } else {
    point_color_continuous <- point_color_type == "continuous"
    point_color_categorical <- !point_color_continuous && point_color_type == "factor"
  }
  if (point_color_continuous && !is.numeric(df[[point_color_var]])) {
    point_color_continuous <- FALSE
    point_color_categorical <- TRUE
  } else if (point_color_categorical && n_unique(df[[point_color_var]]) > 200L) {
    point_color_continuous <- TRUE
    point_color_categorical <- FALSE
  }

  # x axis
  x_limits <- range(df[[x]])
  y_limits <- range(df[[y]])
  if (!is.null(point_color_var)) {
    if (point_color_continuous) {
      color_limits <- Quantile(df[[point_color_var]], probs = c(min_col_threshold, max_col_threshold))
    } else if (point_color_categorical) {
      color_limits <- as.character(sort(unique.default(df[[point_color_var]])))
    }
  }
  # Plot
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data[[x]], y = .data[[y]]))
  if (show_contour) {
    z <- contour_lines/2
    contour_lines <- seq.int(from = z, to = 1 - z, by = contour_lines)
    p <- p + ggdensity::geom_hdr(probs = contour_lines, alpha = 1, fill = "#FFFFFF", color = "#333333", linewidth = contour_line_thickness, show.legend = FALSE)
  }
  if (!is.null(point_color_var)) {
    if (point_color_continuous) {
      if (hex) {
        p <- p +
          ggplot2::stat_summary_hex(ggplot2::aes(z = .data[[point_color_var]]), fun = "mean", bins = n_bins, show.legend = show_legend) +
          ggplot2::scale_fill_gradientn(colors = facs_palette(colors)(n_bins), limits = color_limits)
      } else {
        if (rasterize) {
          pkg_required("scattermore")
          p <- p + scattermore::geom_scattermore(pointsize = point_size*15.2, mapping = aes(color = .data[[point_color_var]]), alpha = point_alpha, show.legend = show_legend, pixels = c(1000, 1000))
        } else {
          p <- p + ggplot2::geom_point(size = point_size, mapping = ggplot2::aes(color = .data[[point_color_var]]), alpha = point_alpha, show.legend = show_legend)
        }
        p <- p + ggplot2::scale_color_gradientn(colors = facs_palette(colors)(n_bins), limits = color_limits)
      }
    } else if (point_color_categorical) {
      if (length(colors) == 1L && is.null(tryNULL(col2hex(colors)))) {
        colors <- rep_len(c("#333333", "#56B4E9", "#CC79A7", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#BC3C29", "#B3B3B3", "#7D5FA7"), length.out = length(unique(df[[point_color_var]])))
      }
      if (rasterize) {
        pkg_required("scattermore")
        p <- p + scattermore::geom_scattermore(mapping = ggplot2::aes(color = factor(.data[[point_color_var]], levels = create_levels(.data[[point_color_var]]))), pointsize = point_size*15.2, alpha = point_alpha, show.legend = show_legend, pixels = c(1000, 1000))
      } else {
        p <- p + ggplot2::geom_point(mapping = ggplot2::aes(color = factor(.data[[point_color_var]], levels = create_levels(.data[[point_color_var]]))), size = point_size, alpha = point_alpha, show.legend = show_legend)
      }
      p <- p + ggplot2::scale_color_manual(values = colors)
    }
  } else {
    color <- tryCatch(col2hex(colors[1L]), error = function(e) facs_palette(colors)(1L))
    if (rasterize) {
      pkg_required("scattermore")
      p <- p + scattermore::geom_scattermore(pointsize = point_size*15.2, alpha = point_alpha, color = color, show.legend = show_legend, pixels = c(1000, 1000))
    } else {
      p <- p + ggplot2::geom_point(size = point_size, alpha = point_alpha, color = color, show.legend = show_legend)
    }
  }

  p <- p +
    ggplot2::ggtitle(plot_title) +
    theme_plain(aspect_ratio = aspect_ratio)

  # Axes
  if (inherits(x_axis_title, "waiver")) {
    x_axis_title <- gsub("umap", "UMAP", x, ignore.case = TRUE)
    x_axis_title <- gsub("tsne", "tSNE", x_axis_title, ignore.case = TRUE)
    x_axis_title <- gsub("_([1-9]+)$", "-\\1", x_axis_title)
  }
  if (inherits(y_axis_title, "waiver")) {
    y_axis_title <- gsub("umap", "UMAP", y, ignore.case = TRUE)
    y_axis_title <- gsub("tsne", "tSNE", y_axis_title, ignore.case = TRUE)
    y_axis_title <- gsub("_([1-9]+)$", "-\\1", y_axis_title)
  }
  if (axis_arrows) {
    p <- p + facs_arrows(x_title = x_axis_title, y_title = y_axis_title)
  } else {
    p <- p + ggplot2::labs(x = x_axis_title, y = y_axis_title)
  }

  # Facet
  if (!is.null(facet_var)) {
    facet_var <- as.name(get_input(facet_var))
    p <- p + ggplot2::facet_wrap(dplyr::vars(!!facet_var), ...)
  }
  p
}

#' Plots the cells along with their trajectories.
#'
#' Functionality from Cole Trapnell's excellent package monocle
#' @inheritParams plot_point
#' @inheritParams plot_tsne
#' @param point_border_thickness Point border thickness. Default is `0.5*point_size`
#' @param rasterize If `TRUE`, points displayed as rasters. If `FALSE` (default), points displayed using vector graphics
#' @param raster_dpi Points per inch. Only relevant when `rasterize = TRUE`
#' @returns ggplot
#' @export
plot_cells <- function(
    df,
    x = "tsne_1",
    y = "tsne_2",
    color_var = NULL,
    point_color_var = color_var,
    colors = "gray",
    point_colors = colors,
    point_border_colors = "black",
    point_size = 0.65,
    point_border_thickness = I(point_size/2),
    alpha = 1,
    rasterize = FALSE,
    raster_dpi = 300,
    facet_var = NULL,
    plot_title = NULL,
    x_axis_title = NULL,
    y_axis_title = NULL,
    axis_arrows = TRUE,
    show_legend = FALSE,
    ...) {
  point_geom <- if (rasterize) {
    pkg_required("ggrastr")
    function(...) ggrastr::geom_point_rast(..., dev = "ragg", raster.dpi = raster_dpi)
    #force(raster_dpi)
    #function(..., size) scattermore::geom_scattermore(..., pointsize = size, pixels = c(raster_dpi, raster_dpi))
  } else {
    ggplot2::geom_point
  }
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    point_geom(color = point_border_colors, size = 1.5*point_size, stroke = I(point_border_thickness), na.rm = TRUE, alpha = I(alpha))
  if (is.null(point_color_var)) {
    p <- p + point_geom(color = point_colors, size = I(point_size), stroke = I(point_border_thickness), na.rm = TRUE, alpha = I(alpha))
  } else {
    p <- p + point_geom(ggplot2::aes(color = .data[[point_color_var]]), size = I(point_size), stroke = I(point_border_thickness), na.rm = TRUE, alpha = alpha, show.legend = show_legend) +
      ggplot2::scale_color_manual(name = NULL, values = rep_len(point_colors, length.out = length(unique(.subset2(df, point_color_var)))))
  }
  blank <- ggplot2::element_blank()
  p <- p +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL, override.aes = list(size = 4))) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.25, color = "black"),
      strip.background = ggplot2::element_rect(colour = "white", fill = "white"),
      panel.background = blank,
      panel.border = blank,
      panel.grid = blank,
      legend.key = blank,
      ...
    )

  # Axes
  if (inherits(x_axis_title, "waiver")) {
    x_axis_title <- gsub("umap", "UMAP", x, ignore.case = TRUE)
    x_axis_title <- gsub("tsne", "tSNE", x_axis_title, ignore.case = TRUE)
    x_axis_title <- gsub("_([1-9]+)$", "-\\1", x_axis_title)
  }
  if (inherits(y_axis_title, "waiver")) {
    y_axis_title <- gsub("umap", "UMAP", y, ignore.case = TRUE)
    y_axis_title <- gsub("tsne", "tSNE", y_axis_title, ignore.case = TRUE)
    y_axis_title <- gsub("_([1-9]+)$", "-\\1", y_axis_title)
  }
  if (axis_arrows) {
    p <- p + facs_arrows(x_title = x_axis_title, y_title = y_axis_title)
  } else {
    p <- p + ggplot2::labs(x = x_axis_title, y = y_axis_title)
  }

  # Facet
  if (!is.null(facet_var)) {
    facet_var <- as.name(get_input(facet_var))
    p <- p + ggplot2::facet_wrap(dplyr::vars(!!facet_var), ...)
  }
  p
}

# Add axis ----------------------------------------------------------------

#' Add axis scale for flow cytometry data
#'
#' @param object New scale to add
#' @param plot ggplot object
#' @param object_name Name of object
#' @returns Not called directly by user. Used by `scale_x_facs` and `scale_y_facs`. Must be exported to work correctly
#' @export
ggplot_add.axis_facs <- function(object, plot, object_name) {
  x_or_y <- object$axis
  axis_fn <- match_fun(sprintf("scale_%s_continuous", x_or_y))
  object$position <- object$position %||% if (x_or_y == "y") "left" else "bottom"
  object$name <- object$name %W% get_plot_axis_title(plot, x_or_y)
  channel <- get_plot_axis_vars(plot, x_or_y) %||% plot$plot_env[[x_or_y]]
  if (is.function(object$name)) {
    object$name <- object$name(channel)
  }
  # plot$plot_env$df
  range_channel <- range(plot$data[[channel]], na.rm = TRUE)
  if (is_scatter(channel)) {
    # Scatter channel
    object$trans <- object$trans %W% "identity"
    ## Consider removing 100K/200K if x_or_y == "x"
    object$breaks <- object$breaks %W% c(0, 50000, 100000, 150000, 200000, 250000)
    object$labels <- object$labels %W% .facs_axis_labels_scatter
    if (object$clean) {
      object$expand <- object$expand %W% c(0.05, 0, 0, 0)
      object$guide <- guide_clean_axis()
    } else {
      object$guide <- waiver()
    }
    if (is.null(object$limits)) {
      #axis_limits <- ggplot2::summarise_layout(ggplot2::ggplot_build(plot))
      #cols <- paste0(x_or_y, c("min", "max"))
      #object$limits <- range(object$breaks, axis_limits[[cols[1L]]], axis_limits[[cols[2L]]], na.rm = TRUE)
      object$limits <- range(range_channel, object$breaks, na.rm = TRUE)
      if (object$include_0) {
        object$limits[1L] <- 0
      }
    } else {
      if (object$include_0) {
        object$limits[1L] <- 0
      }
    }
  } else if (grepl("time", channel, ignore.case = TRUE)) {
    # Time channel
    object$trans <- "identity"
    if (object$clean) {
      object$expand <- object$expand %W% c(0, 0, 0, 0)
      object$guide <- guide_clean_axis()
    } else {
      object$guide <- waiver()
    }
  } else {
    # Fluorochrome channels
    object$n_breaks <- object$n_breaks %||% 4
    object$trans <- object$trans %W% trans_biexp(pos_decades = object$pos_decades %||% 4.5, neg_decades = object$neg_decades %||% 0, width_basis = object$width_basis %||% -25, max_value = object$max_value %||% 262144, n_breaks = object$n_breaks)
    object$guide <- object$guide %W% ggplot2::guide_axis_logticks(long = 2.25*rel(0.5), mid = 1.5*rel(0.5), short = 0.75*rel(0.5))
    #object$guide <- object$guide %W% guide_facs_ticks()
    limits_not_entered <- is.null(object$limits)
    if (limits_not_entered) {
      #axis_limits <- ggplot2::summarise_layout(ggplot2::ggplot_build(plot))
      #cols <- paste0(x_or_y, c("min", "max"))
      #object$limits <- range(object$breaks, axis_limits[[cols[1L]]], axis_limits[[cols[2L]]], na.rm = TRUE)
      object$limits <- range(range_channel, na.rm = TRUE)
      #object$limits[1L] <- max(object$limits[1L], -10000)
    }
    object$breaks <- object$breaks %W% breaks_facs(n_breaks = object$n_breaks)(object$limits)
    if (limits_not_entered && is.numeric(object$breaks)) {
      object$limits[2L] <- max(object$limits, object$breaks, na.rm = TRUE)
    }
  }
  object$labels <- object$labels %W% .facs_axis_labels_log()
  suppress({
    plot + axis_fn(name = object$name, limits = object$limits, breaks = object$breaks, labels = object$labels, trans = object$trans, oob = object$censor_fn, position = object$position, expand = object$expand, guide = object$guide)
  })
}

#' y axis scale function for FACS data
#'
#' @param channel Channel. Enter as string
#' @param marker Marker for `channel`. Only relevant if `title` not specified. Enter as string
#' @param trans Transformation object for axis
#' @param title Axis title
#' @param name Alias for `title`
#' @param limits Minimum and maximum values in data. Must enter in order (i.e., c(lower, upper))
#' @param n_breaks Desired number of axis breaks
#' @param breaks Numeric vector or function specifying location of ticks along axis
#' @param labels Vector or function specifying axis tick labels
#' @param position Location of axis. Options: `"left"` (default for y axis), `"right"`, `"bottom"` (default for x axis), `"top"`
#' @param censor_fn Function used to transform data outside axis limits. Options: `rescale_none` (default), `squish`, `scales::censor`
#' @param clean If `TRUE` (default) and `channel` is a scatter or time variable, axis drawn with final tick
#' @param expand Expansion applied to axis. Enter using `ggplot::expand` or length 2 or 4 numeric vector
#' @param guide Axis guide function
#' @param include_0 If `TRUE` (default), 0 is included in breans
#' @param auto_breaks If `TRUE` (default), breaks automatically determined
#' @param ... Arguments passed to scale function
#' @returns Axis scale. Enter as `ggplot + scale_y_facs()`
#' @export
scale_y_facs <- function(
    title = waiver(),
    name = title,
    trans = waiver(),
    limits = NULL,
    breaks = waiver(),
    labels = waiver(),
    n_breaks = NULL,
    position = "left",
    censor_fn = rescale_none,
    expand = waiver(),
    clean = TRUE,
    guide = waiver(),
    include_0 = TRUE,
    ...) {
  structure(list(axis = "y", name = name, limits = limits, breaks = breaks, labels = labels, n_breaks = n_breaks, position = position, censor_fn = censor_fn, trans = trans, clean = clean, guide = guide, expand = expand, include_0 = include_0, ...), class = "axis_facs")
}

#' x axis scale function for FACS data
#'
#' @rdname scale_y_facs
#' @export
scale_x_facs <- function(
    title = waiver(),
    name = title,
    trans = waiver(),
    limits = NULL,
    breaks = waiver(),
    labels = waiver(),
    n_breaks = NULL,
    position = "bottom",
    censor_fn = rescale_none,
    expand = waiver(),
    clean = TRUE,
    guide = waiver(),
    include_0 = TRUE,
    ...) {
  structure(list(axis = "x", name = name, limits = limits, breaks = breaks, labels = labels, n_breaks = n_breaks, position = position, censor_fn = censor_fn, trans = trans, clean = clean, guide = guide, expand = expand, include_0 = include_0, ...), class = "axis_facs")
}

#' FACS axis arrows
#'
#' @param x_arrow_length,y_arrow_length Length of x and y axis arrows in npc units. Default is `0.4`
#' @param x_title,y_title Titles for x and y axes. Enter as string
#' @param axis_title_size Font size for axis titles. Default is `16`
#' @param arrowhead_length Length of arrowhead. Enter as units object. Default is `0.25cm`
#' @param arrowhead_filled If `TRUE` (default), filled triangle is used for arrow head. If `FALSE`, arrowhead is is not filled
#' @param ... Arguments passed to `ggplot2::theme`
#' @returns List that can be added to ggplot object. Enter as `ggplot + facs_arrows()`
#' @export
facs_arrows <- function(
    x_arrow_length = 0.4,
    y_arrow_length = x_arrow_length,
    x_title = "",
    y_title = "",
    axis_title_size = 16,
    arrowhead_length = grid::unit(0.025, "npc"),
    arrowhead_filled = FALSE,
    ...) {
  blank <- ggplot2::element_blank()
  zero_margin <- ggplot2::margin(0, 0, 0, 0)
  axis_space <- axis_title_size + 2
  theme <- ggplot2::theme(
    panel.border = blank,
    panel.grid = blank,
    axis.title.x = blank,
    axis.title.y = blank,
    axis.line.x = blank,
    axis.line.y = blank,
    axis.ticks.x = blank,
    axis.ticks.y = blank,
    axis.text.x = blank,
    axis.text.y = blank,
    legend.background = blank,
    legend.box.margin = zero_margin,
    legend.margin = zero_margin,
    plot.margin = ggplot2::margin(0, 0, axis_space, axis_space, unit = "points"),
    ...
  )
  npc0 <- grid::unit(0, "npc")
  npc00 <- grid::unit(c(0, 0), "npc")
  arrow <- grid::arrow(length = arrowhead_length, type = if (arrowhead_filled) "closed" else "open")
  title <- grid::gpar(fontsize = axis_title_size)
  lwd <- grid::gpar(lwd = 2)
  arrows <- grid::grobTree(
    grid::gList(
      grid::linesGrob(
        x = grid::unit(c(0, x_arrow_length), "npc"),
        y = npc00,
        arrow = arrow,
        gp = lwd
      ),
      grid::textGrob(
        label = x_title,
        x = npc0,
        y = npc0,
        vjust = 1.75,
        hjust = 0,
        gp = title
      ),
      grid::linesGrob(
        x = npc00,
        y = grid::unit(c(0, y_arrow_length), "npc"),
        arrow = arrow,
        gp = lwd
      ),
      grid::textGrob(
        label = y_title,
        x = npc0,
        y = npc0,
        vjust = -2/3,
        hjust = 0,
        rot = 90,
        gp = title
      )
    )
  )
  list(
    list(ggplot2::annotation_custom(arrows)),
    list(theme),
    list(ggplot2::coord_cartesian(clip = "off"))
  )
}

#' Display arrowhead at ends of x and y axis lines
#'
#' Contrast with `facs_arrows` which draws short segments for x and y axis
#' @param axis_title_font_size Font size for axis title. Default is `16`
#' @param axis_title_font_color,x_axis_title_font_color,y_axis_title_font_color Font color for axis title
#' @param arrow If `TRUE` (default), arrowheads included at axis ends
#' @param arrowhead_length Length of arrowhead. Enter as units object. Default is `0.25cm`
#' @param arrowhead_filled If `TRUE` (default), filled triangle is used for arrow head. If `FALSE`, arrowhead is is not filled
#' @param arrowhead_ends Style of triangle edges for arrows. Options: `"mitre"` (default), `"bevel"`, `"round"`. Not currently used
#' @param line_type Axis line type. Enter as length 1 integer (0-6) or character. Default is `"solid"`
#' @param line_thickness Axis line thickness. Default is `0.7`
#' @param line_color,x_line_color,y_line_color Axis line color
#' @returns theme object. Enter as `plot + facs_axis_arrows()`. Ticks and labels are hidden
#' @export
facs_axis_arrows <- function(
    axis_title_font_size = 16,
    axis_title_font_color = NULL,
    x_axis_title_font_color = axis_title_font_color,
    y_axis_title_font_color = axis_title_font_color,
    arrow = TRUE,
    arrowhead_length = grid::unit(0.25, units = "cm"),
    arrowhead_filled = TRUE,
    arrowhead_ends = c("mitre", "bevel", "round"),
    line_type = "solid",
    line_thickness = 0.7,
    line_color = NULL,
    x_line_color = line_color,
    y_line_color = line_color) {
  blank <- element_blank()
  #arrowhead_ends <- match.arg(arrowhead_ends, choices = c("mitre", "bevel", "round"))
  if (arrow) {
    arrow <- grid::arrow(length = arrowhead_length, type = if (arrowhead_filled) "closed" else "open")
  }
  #ggplot2::element_line
  x_axis_line <- y_axis_line <- structure(
    list(
      colour = line_color,
      linewidth = line_thickness,
      linetype = line_type,
      lineend = "square",
      arrow = arrow,
      inherit.blank = FALSE
    ),
    class = c("element_line", "element")
  )
  if (!is.null(x_line_color)) {
    x_axis_line$colour <- x_line_color
  }
  if (!is.null(y_line_color)) {
    y_axis_line$colour <- y_line_color
  }
  ggplot2::theme(
    # x axis
    axis.line.x = x_axis_line,
    axis.ticks.x = blank,
    axis.text.x = blank,
    axis.title.x = ggplot2::element_text(
      size = axis_title_font_size,
      color = x_axis_title_font_color,
      hjust = 0,
      vjust = 0.5,
      margin = ggplot2::margin(t = axis_title_font_size/3, unit = "pt")
    ),
    # y axis
    axis.line.y = y_axis_line,
    axis.ticks.y = blank,
    axis.text.y = blank,
    axis.title.y = ggplot2::element_text(
      size = axis_title_font_size,
      color = y_axis_title_font_color,
      hjust = 0,
      vjust = 0.5,
      margin = ggplot2::margin(r = axis_title_font_size/3, unit = "pt")
    )
  )
}

#' Display arrowhead at ends of x and y axis lines for facetted plot
#'
#' @param x ggplot object
#' @inheritParams facs_axis_arrows
#' @returns theme object. Enter as ` facs_axis_arrows_simple(plot)`. Ticks and labels are hidden
#' @export
facs_axis_arrows_simple <- function(
    x,
    axis_title_font_size = 16,
    axis_title_font_color = NULL,
    x_axis_title_font_color = axis_title_font_color,
    y_axis_title_font_color = axis_title_font_color,
    arrowhead_length = grid::unit(0.25, units = "cm"),
    arrowhead_filled = TRUE,
    line_thickness = 0.7,
    line_color = NULL,
    x_line_color = line_color,
    y_line_color = line_color) {
  x <- x + facs_axis_arrows(
    axis_title_font_size = 16,
    axis_title_font_color = axis_title_font_color,
    x_axis_title_font_color = x_axis_title_font_color,
    y_axis_title_font_color = y_axis_title_font_color,
    arrowhead_length = grid::unit(0.25, units = "cm"),
    arrowhead_filled = TRUE,
    line_thickness = 0.7,
    line_color = line_color,
    x_line_color = x_line_color,
    y_line_color = y_line_color
  )
  nrow <- x$facet$params$nrow
  if (is.null(nrow)) return(x)
  ncol <- x$facet$params$ncol
  x_axis <- paste(paste0("axis-b-", seq_len(ncol)[-1L]), collapse = "|")
  y_axis <- paste(paste0("axis-l-", seq_len(nrow - 1L)), collapse = "|")
  .remove_grobs(x, pattern = paste(x_axis, y_axis, sep = "|"))
}
