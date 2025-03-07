#' Apply alpha filter to color
#'
#' Functionality from `grDevices::adjustcolor`
#' @param color Character vector of color names or hexadecimal codes
#' @param alpha Opacity. Enter as numeric 0-1 (1 for completely opaque, 0 for completely transparent). Default is `0.8`
#' @export
alpha <- function(color, alpha = 1) {
  # Alternative: color <- t(grDevices::col2rgb(color, alpha = FALSE));grDevices::rgb(color[, c(1L, 2L, 3L), drop = FALSE], alpha = 255*alpha, maxColorValue = 255)
  color <- grDevices::col2rgb(color, alpha = TRUE)/255
  alpha[is.na(alpha)] <- 1
  color[] <- pmax(0, pmin(1, diag(c(1, 1, 1, alpha)) %*% color + matrix(0, nrow = 4L, ncol = ncol(color))))
  grDevices::rgb(color[1L, ], color[2L, ], color[3L, ], color[4L, ])
}

# Continuous palette ------------------------------------------------------

#' General color ramp function
#'
#' @param x Colors to generate color ramp. Enter as a character vector of hexadecimal codes (or color names) or 1 of the following length 1 character vectors: `"light"` (default), `"medium"`, `"dark"`, `"flowjo"`, `"turbo"`, `"viridis"`, `"cividis"`, `"mako"`, `"plasma"`, `"inferno"`, `"rocket"`
#' @param saturation Saturation factor. Only relevant when `x = "flowjo"`. Enter as length 1 numeric 0-1. Default is `1`
#' @param ... Arguments passed to `grDevices::colorRampPalette` or `scales::viridis_pal`
#' @returns Function with argument `n` that expects a length 1 numeric indicating the desired number of colors to generate
#' @export
clr_ramp <- function(x = "light", saturation = 1, ...) {
  if (is.character(x) && length(x) == 1L) {
    if (x %in% c("turbo", "viridis", "cividis", "mako", "plasma", "inferno", "rocket")) {
      return(scales::viridis_pal(option = x, ...))
    }
    x <- switch(x,
                #light = c("blue4", "blue", "cyan", "green", "yellow", "orange", "red"),
                #light = Rev(rainbow(100)[seq_len(70)]),
                light = c("blue", "cyan", rep("green", 5L), "yellow", "orange", "orangered", "red"),
                medium = c("blue3", "blue", "turquoise", "green", "yellow", "orange", "red", "darkred"),
                #medium = c("dodgerblue4", "steelblue2", "olivedrab3", "darkgoldenrod1", "brown"),
                dark = c("#5E4FA2", "#555AA7", "#4C66AD", "#4371B2", "#3A7DB8", "#3389BC", "#3D95B7", "#48A0B2", "#52ACAD", "#5DB8A8", "#68C3A4", "#76C8A4", "#84CEA4", "#93D3A4", "#A1D9A4", "#AEDEA3", "#BAE3A0", "#C6E89E", "#D2ED9B", "#DEF299", "#E8F59B", "#EDF7A3", "#F2F9AB", "#F7FBB3", "#FCFDBB", "#FEFBB9", "#FEF5AF", "#FEEFA4", "#FEE899", "#FEE28F", "#FDD985", "#FDCF7D", "#FDC574", "#FDBB6C", "#FDB163", "#FBA45C", "#F99756", "#F88A50", "#F67C4A", "#F46F44", "#EE6544", "#E85B47", "#E25249", "#DB484C", "#D53E4E", "#CA324C", "#BF2649", "#B41947", "#A90D44", "#9E0142"),
                flowjo = {
                  grDevices::hsv(
                    h = c(seq.int(from = 2/3, to = 0.5, length.out = 10), seq.int(from = 0.5, to = 0, length.out = 10)),
                    #s = rep(saturation, len = 20),
                    s = saturation,
                    v = 1
                  )
                },
                x
    )
  }
  grDevices::colorRampPalette(x, ...)
}

#' Generate continuous color scale
#'
#' @rdname clr_ramp
#' @param ... Colors. If 3 colors are entered, output is a continuous scale using 2nd color as midpoint. Can also be one of the following palettes: `"light"`, `"medium"`, `"dark"`, `"flowjo"`, `"turbo"`, `"viridis"`, `"cividis"`, `"mako"`, `"plasma"`, `"inferno"`, `"rocket"`
#' @param n Number of colors. Enter as length 1 numeric. Default is `20`
#' @param reverse If `TRUE`, output will be reversed
#' @returns Character vector of hexadecimal codes. Length determined by `n`
#' @export
clr_continuous <- function(..., n = 20, reverse = FALSE) {
  out <- clr_ramp(col2hex(.color_input(...)))(n)
  if (reverse) {
    out <- Rev(out)
  }
  out
}

#' Greyscale
#'
#' Rewritten version of `grDevices::grey.colors`
#' @rdname clr_ramp
#' @param start,end Hues used for first and last colors, respectively. Enter as length 1 numeric, 0-1. Defaults are `start = 0.3` and `end = 0.8`
#' @param reverse Whether to reverse default color order. If `FALSE` (default), colors are ordered from darkest to lightest. If `TRUE`, colors are ordered from lightest to darkest
#' @export
clr_continuous_grey <- function(n = 20, start = 0.3, end = 0.8, reverse = FALSE) {
  out <- grDevices::grey(seq.int(from = start^2.2, to = end^2.2, length.out = n)^(1/2.2), alpha = NULL)
  if (reverse) {
    out <- Rev(out)
  }
  out
}

# Color calculations ------------------------------------------------------

#' Calculate luminance
#'
#' @param color Character vector of color names or hexadecimal codes
#' @returns Numeric vector with same length as input
#' @noRd
clr_luminance <- function(color) {
  col_rgb <- t(grDevices::col2rgb(color))/255
  col_rgb[] <- ifelse(col_rgb <= 0.03928, col_rgb/12.92, ((col_rgb + 0.055)/1.055)^2.4)
  as.numeric(col_rgb %*% c(0.2126, 0.7152, 0.0722))
}

#' Calculate contrast ratio between colors
#'
#' Functionality from colorspace package
#' @param col1,col2 Colors 1 and 2. `col1` can be vector of colors. Default for `col2` is `"white"`
#' @param as_df If `TRUE`, output will be data frame with columns "col1", "col2", "ratio", "contrast"
#' @returns Ratio of luminance for 2 colors. > 3 is decent, > 4.5 is preferred. If `as_df = TRUE`, output will be data frame with columns "col1", "col2", "ratio", "contrast"
#' @noRd
clr_contrast_ratio <- function(col1, col2 = "white", as_df = FALSE) {
  r <- (clr_luminance(col1) + 0.05)/(clr_luminance(col2) + 0.05)
  r[r < 1] <- 1/r[r < 1]
  if (as_df) {
    df <- vec_to_df(col1 = col1, col2 = col2, ratio = r)
    df$contrast <- ifelse(r > 4.5, "High", ifelse(r > 3, "Medium", "Low"))
    df[-df$ratio, , drop = FALSE]
  } else {
    r
  }
}

#' Determine whether black or white text is better for a given background color
#'
#' @rdname clr_luminance
#' @param ... List, character vector, or comma separated list of colors as quoted color names or hexadecimal codes
#' @returns Character vector with same length as input. Values are "black" or "white"
#' @noRd
clr_text <- function(...) {
  colors <- .color_input(...)
  #ifelse(clr_luminance(colors) > 0.179, "black", "white")
  ifelse(clr_contrast_ratio(colors, "white") > clr_contrast_ratio(colors, "black"), "white", "black")
}

# Color conversion --------------------------------------------------------

#' Convert color name to hexadecimal code
#'
#' @param x Character vector of color names or hexadecimal codes
#' @returns Character vector of hexadecimal codes with same length as input
#' @noRd
col2hex <- function(x) {
  rgb <- grDevices::col2rgb(x)/255
  grDevices::rgb(red = rgb[1L, ], green = rgb[2L, ], blue = rgb[3L, ])
}

#' Convert dots input to character vector of colors
#'
#' @param ... List, character vector, or comma separated list of colors as quoted color names or hexadecimal codes
#' @noRd
.color_input <- function(...) if (is.list(colors <- c(...))) unlist(colors, recursive = FALSE) else colors
