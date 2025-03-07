# All functions start with "get_plot"

# Data --------------------------------------------------------------------

#' Extract plot data
#'
#' @param x ggplot object
#' @param source Source of data returned. Options: `"plot"` (`x$data`, default. Data entered to `ggplot` function), `"build"` (`ggplot_build(x)$data`. Data used internally by ggplot2 to construct each layer), `user` (user input data to each layer)
#' @returns Data frame or list of data frames
#' @export
get_plot_data <- function(x, source = c("plot", "build", "user")) {
  source <- match.arg(source, choices = c("plot", "build", "user"))
  switch(source,
         plot = x$data,
         build = {
           # ggplot2::get_layer_data (same as ggplot2::layer_data)
           out <- ggplot2::ggplot_build(x)$data
           names(out) <- get_plot_geom_names(x)
           out
         },
         user = {
           out <- lapply(x$layers, function(y) {
             plot_data <- if (is_waiver(y$data)) x$data else y$data
             if (!is.null(x$facet$params$facets)) {
               facet <- x$facet$params$facets[[1]]
               facet_var <- if (is.symbol(facet)) {
                 as.character(facet)
               } else if (inherits(facet, "quosure")) {
                 rlang::quo_name(facet)
               } else {
                 as.character(facet)
               }
               groups <- plot_data[[facet_var]]
               if (!is.null(groups)) {
                 group_levels <- attr(groups, "levels")
                 plot_data <- plot_data[order(match(groups, group_levels)), ]
               }
             }
             plot_data
           })
           names(out) <- get_plot_geom_names(x)
           out
         })
}

# Axis --------------------------------------------------------------------

#' Extract transformation/inverse functions applied to x and y axes
#'
#' @param x ggplot object
#' @returns List of length 2 ("x", "y"). "x" and "y" each consist of a list of length 2 ("trans", "inverse")
#' @export
get_plot_axis_trans <- function(x) {
  plot_layout <- ggplot2::ggplot_build(x)$layout
  x <- plot_layout$panel_scales_x[[1L]]$trans
  y <- plot_layout$panel_scales_y[[1L]]$trans
  list(
    x = list(
      trans = x$transform %||% identity,
      inverse = x$inverse %||% identity
    ),
    y = list(
      trans = y$transform %||% identity,
      inverse = y$inverse %||% identity
    )
  )
}

#' Extract axis scale
#'
#' @param x ggplot object
#' @param axis Axis to extract. Options: `"x"` or `"y"`
#' @returns Same as scale_(x|y)_(discrete|continuous) output
#' @export
get_plot_axis_scale <- function(x, axis) {
  .get_plot_scale(x, axis)
  # z <- ggplot2::ggplot_build(x)$layout$get_scales(1)$x
}

#' Draw axes only
#'
#' @param x ggplot object
#' @param axis Options: `"both"` (default), `"x"`, `"y"`
#' @export
draw_plot_axis <- function(x, axis = "both") {
  x$layers <- NULL
  x
}

# Color scales ------------------------------------------------------------

#' Extract fill scale
#'
#' @param x ggplot object
#' @returns Same class as scale_fill output
#' @export
get_plot_fill_scale <- function(x) .get_plot_scale(x, "fill")

#' Extract color scale
#'
#' @param x ggplot object
#' @returns Same class as scale_color output
#' @export
get_plot_color_scale <- function(x) .get_plot_scale(x, "color")

# geoms -------------------------------------------------------------------

#' Extract data used in a specified geom
#'
#' @param x ggplot object
#' @param geom Name of geom (output from `get_plot_geom_names`). First letter only must be capitalized with lowercase for other letters
#' @returns Data frame or list of data frames
#' @export
get_plot_geom_data <- function(x, geom) {
  df <- ggplot2::ggplot_build(x)$data
  names(df) <- get_plot_geom_names(x)
  df <- df[geom]
  if (length(df) == 1L) df[[1L]] else df
}

# Legend ------------------------------------------------------------------

#' Get plot legend
#'
#' @param x ggplot object
#' @returns Plot legend as ggplot object
#' @export
get_plot_legend <- function(x) {
  plot_grob <- ggplot2::ggplotGrob(x)
  # Must use which() in line below
  idx <- which(vapply(plot_grob$grobs, function(z) z$name == "guide-box", logical(1), USE.NAMES = FALSE))
  if (length(idx) == 0L) return(NULL)
  plot_grob <- plot_grob$grobs[[idx]]
  ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    theme_clean() +
    draw_grob(as_grob(plot_grob), x = 0, y = 0, width = 1, height = 1, scale = 1, hjust = 0, vjust = 0, halign = 0.5, valign = 0.5)
}

# aes ---------------------------------------------------------------------

#' Extract `aes` arguments for each plot layer
#'
#' @param x ggplot object
#' @returns List of aes/variables for each plot layer. Output length equal to number of plots layers
#' @export
get_plot_aes <- function(x) {
  layers <- x$layers
  plot_mapping <- x$mapping
  lapply(layers, function(y) {
    layer_mapping <- if (y$inherit.aes) plot_mapping else aes()
    if (length(y$mapping) > 0L) {
      layer_mapping[names(y$mapping)] <- y$mapping
    }
    lapply(layer_mapping, function(z) {
      if (is.symbol(z)) {
        as.character(z)
      } else if (inherits(z, "quosure")) {
        pkg_required("rlang")
        rlang::quo_name(z)
      } else {
        as.character(z)
      }
    })
  })
}

#' Get R code for plot aesthetics
#'
#' @param x ggplot object
#' @returns Named list of aes input for each plot layer. List from 1 layer can be passed to `aes()`
#' @export
get_plot_aes_code <- function(x) {
  plot_build <- ggplot2::ggplot_build(x)
  plot_summary <- ggplot2::summarise_layers(plot_build)$mapping
  lapply(seq_along(plot_build$data), function(i) {
    lapply(plot_summary[[i]], rlang::quo_get_expr)
  })
}

# Facets ------------------------------------------------------------------

#' Extract facet variables
#'
#' @param x ggplot object
#' @export
get_plot_facet_vars <- function(x) {
  if (inherits(x$facet, "FacetNull")) return(NULL)
  names(x$facet$params$facets)
  ggplot2::ggplot_build(x)$layout$facet$vars()
}

#' Extract facet labels
#'
#' @param x ggplot object
#' @export
get_plot_facet_labels <- function(x) {
  x_grob <- ggplot2::ggplotGrob(x)
  x_grob <- x_grob$grobs[grepl("strip-t", x_grob$layout$name, fixed = TRUE)]
  out <- lapply(x_grob, function(z) z[["grobs"]][[1L]][["children"]][[2L]][["children"]][[1L]][["label"]])
  if (all(vapply(out, is.character, logical(1), USE.NAMES = FALSE))) {
    out <- unlist(out, use.names = FALSE)
  }
  out
}

# Other -------------------------------------------------------------------

#' Extract theme from ggplot object
#'
#' @param x ggplot object
#' @returns theme object
#' @export
get_plot_theme <- function(x) {
  plot_theme <- x$theme
  class(plot_theme) <- c("theme", "gg")
  plot_theme
}

#' Extract scale from ggplot object
#'
#' @param x ggplot object
#' @param scale Scale to extract. Enter as string
#' @returns Scale object
#' @noRd
.get_plot_scale <- function(x, scale) {
  plot_scales <- x$scales$scales
  scale_names <- unlist(lapply(plot_scales, function(z) z$aesthetics[[1L]]), use.names = FALSE)
  plot_scales[[match(scale, scale_names, nomatch = 0L)]]
}

#' Get significance annotation data frame
#'
#' @param x ggplot object
#' @returns List of data frames from `ggplot_build` used to construct significance annotation
#' @noRd
get_plot_sigificance_anno_data <- function(x) {
  df <- ggplot2::ggplot_build(x)$data
  idx_bar <- which(vapply(df, function(x) all(c("x", "xend", "y", "yend") %in% names(x)), logical(1)))
  idx_star <- which(vapply(df, function(x) {
    c("label", "fontface") %in% names(x) && any(grepl("*", x$label, fixed = TRUE))
  }, logical(1)))
  idx_text <- which(vapply(df, function(x) {
    c("label", "fontface") %in% names(x) && all(!grepl("*", x$label, fixed = TRUE))
  }, logical(1)))
  get_df <- function(y, y_name) {
    y_length <- length(y)
    if (y_length == 1L) {
      df[[y]]
    } else if (y_length == 0L) {
      empty_df()
    } else {
      sprintf("The following data frames in 'ggplot_build(x)$data' might contain the data used to build the %s annotation: %s", y_name, paste0(y, collapse = ", "))
    }
  }
  list(
    bar = get_df(idx_bar, "bar"),
    star = get_df(idx_star, "star"),
    text = get_df(idx_text, "text"))
}
