# All functions start with "get_plot"

#' Extract `aes` arguments for each plot layer
#'
#' @param x ggplot object
#' @returns List of aes/variables for each plot layer. Output length equal to number of plots layers
#' @noRd
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
