#' Scale for continuous axis
#'
#' @param axis Options: `"y"` (default) or `"x"`
#' @param scale Options: `"regular"` (default), `"scientific"`, `"log10"`, `"log2"`, `"log"` (same as `"log10"`), `"log10_1"`
#' @param title Axis title
#' @param limits Minimum and maximum values in data. Must enter in order, i.e. c(lower, upper)
#' @param breaks Numeric vector or function specifying location of ticks along axis
#' @param labels Vector or function specifying axis tick labels
#' @param limits Numeric vector of axis limits positions or function to generate limits when passed raw data
#' @param breaks Numeric vector of axis break positions or function to generate breaks when passed axis limits
#' @param expand_lower,expand_upper Expansion to add to ends of axis. Default for `expand_lower` is `0.05` if `expand_method` is `"mult` and `0.6` if `expand_method` is `add`. Default for `expand_upper` is `0`
#' @param expand Alias for `c(expand_lower,expand_upper)`
#' @param expand_method Method for expanding limits of axes. Options: `"mult"` (default. Proportion of entire axis range to add to axis ends), `"add"` (add `expand_upper` value to upper end and subtract `expand_lower` from lower end of axis)
#' @param cap Input passed to `ggplot2::guide_axis`
#' @param position Location of axis. Options: `"left"` (default for y axis), `"right"`, "bottom" (default for x axis), "top"
#' @param censor_fn Function used to transform data outside axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param ... Arguments passed to scale function
#' @returns ggproto object
#' @export
scale_axis <- function(
    axis = "y",
    scale = "regular",
    #title = NULL,
    title = waiver(),
    limits = NULL,
    breaks = NULL,
    labels = NULL,
    expand_lower = NULL,
    expand_upper = 0,
    expand = c(expand_lower, expand_upper),
    expand_method = c("mult", "add"),
    cap = "upper",
    position = NULL,
    censor_fn = rescale_none,
    transform = NULL,
    ...) {
  scale <- match.arg(scale, choices = c("regular", "scientific", "log10", "log10_1", "log", "log2"))
  is_log <- startsWith(scale, "log")
  position <- position %||% if (axis == "y") "left" else "bottom"
  axis_fn <- sprintf("scale_%s_continuous", axis)

  # Log vs. linear scale
  if (is_log) {
    base <- if (scale %in% c("log", "log10", "log10_1")) 10 else 2
    breaks <- breaks %||% if (scale == "log10_1") breaks_log(log_fn = log10_1) else breaks_log(base = base)
    if (missing(labels)) {
      labels <- labels %||% scales::label_log(base = base)
    }
    transform <- transform %||% scales::transform_log(base = base)
  } else {
    breaks <- breaks %||% breaks_linear()
    if (missing(labels)) {
      labels <- if (scale == "regular") axis_label_numeric else axis_label_x10
    }
    transform <- transform %||% scales::transform_identity()
  }

  # Limits
  if (is.null(limits)) {
    if (is.function(breaks)) {
      limits <- function(x) range(x, breaks(x), na.rm = TRUE)
    } else if (is.numeric(breaks)) {
      limits <- function(x) range(x, breaks, na.rm = TRUE)
    }
  }

  # Expand
  expand_method <- match.arg(expand_method, choices = c("mult", "add"))
  mult_expand <- expand_method == "mult"
  if (missing(expand) && is.null(expand_lower)) {
    expand_lower <- if (mult_expand) 0.05 else 0.6
  }
  expand <- if (max(expand) > 0) {
    expand <- rep(expand, length.out = 2)
    if (mult_expand) {
      c(expand[1L], 0, expand[2L], 0)
    } else {
      c(0, expand[1L], 0, expand[2L])
    }
  } else {
    c(0, 0, 0, 0)
  }

  # All arguments
  args <- list(
    name = title,
    limits = limits,
    breaks = breaks,
    labels = labels,
    transform = transform,
    oob = censor_fn,
    position = position,
    expand = expand,
    ...
  )
  if (!any(names(args) == "guide")) {
    if (missing(cap)) {
      cap_upper <- max(expand[c(3L, 4L)]) == 0
      cap_lower <- max(expand[c(1L, 2L)]) == 0
      cap <- if (cap_upper) {
        cap <- if (cap_lower) "both" else "upper"
      } else if (cap_lower) {
        "lower"
      } else {
        "none"
      }
    }
    args$guide <- ggplot2::guide_axis(cap = cap)
  }

  # Output
  do.call(axis_fn, args)
}

#' Scale for continuous x axis
#'
#' @rdname scale_axis
#' @export
scale_y <- scale_axis

#' Scale for continuous x axis
#'
#' @rdname scale_axis
#' @export
scale_x <- function(
    scale = "regular",
    title = NULL,
    limits = NULL,
    breaks = NULL,
    labels = NULL,
    expand_lower = 0.05,
    expand_upper = expand_lower,
    expand = c(expand_lower, expand_upper),
    expand_method = c("mult", "add"),
    cap = "none",
    position = "bottom",
    censor_fn = rescale_none,
    transform = NULL,
    ...) {
  scale_axis(
    axis = "x",
    scale = scale,
    title = title,
    limits = limits,
    breaks = breaks,
    labels = labels,
    transform = transform,
    censor_fn = censor_fn,
    position = position,
    expand_lower = expand_lower,
    expand_upper = expand_upper,
    expand = expand,
    expand_method = expand_method,
    cap = cap,
    ...
  )
}

#' Add continuous axis scale to ggplot
#'
#' @param object Output from `scale_continuous`
#' @param plot ggplot object
#' @param object_name Name of object
#' @returns ggplot object. Needed for `scale_continuous`. Not called directly by user. Must be exported
#' @export
ggplot_add.axis_clean <- function(object, plot, object_name) {
  x_or_y <- object$axis
  axis_fn <- match.fun(sprintf("scale_%s_continuous", x_or_y))

  # Axis limits as length 2 numeric vector
  limits <- object$limits %||% get_plot_data_limits(plot, axis = x_or_y) %||% get_plot_axis_limits(plot, axis = x_or_y)

  # Inputs for axis transformation, breaks, labels, lower expansion
  switch(object$scale,
         regular = {
           trans <- scales::transform_identity()
           labels <- object$labels %||% axis_label_numeric
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "regular", .breaks_fn = object$breaks_fn, .n = object$n_breaks)
           expand_lower <- object$expand_lower %||% if (x_or_y == "x" || Min(breaks) == 0) 0.1 else 0
         },
         scientific = {
           trans <- scales::transform_identity()
           labels <- object$labels %||% axis_label_x10
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "regular", .breaks_fn = object$breaks_fn, .n = object$n_breaks)
           expand_lower <- object$expand_lower %||% if (x_or_y == "x" || Min(breaks) == 0) 0.1 else 0
         },
         log2 = {
           trans <- scales::transform_log(2)
           #labels <- object$labels %||% scales::trans_format("log2", scales::label_math(2^.x))
           labels <- object$labels %||% scales::label_log(2)
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "log2", .n = object$n_breaks, .breaks_fn = object$breaks_fn)
           expand_lower <- object$expand_lower %||% if (x_or_y == "x" || Min(breaks) == 1) 0.1 else 0
         },
         log = ,
         log10 = {
           trans <- scales::transform_log(10)
           #labels <- object$labels %||% scales::trans_format("log10", scales::label_math(10^.x))
           labels <- object$labels %||% scales::label_log(10)
           breaks <- object$breaks %||% .create_axis_breaks(.limits = limits, .scale = "log10", .n = object$n_breaks, .breaks_fn = object$breaks_fn)
           expand_lower <- object$expand_lower %||% if (x_or_y == "x" || Min(breaks) == 1) 0.1 else 0
         },
         Stop("In 'scale_continuous', 'scale' must be one of the following: 'regular', 'scientific', 'log2', 'log10', 'log'")
  )

  position <- object$position %||% if (x_or_y == "y") "left" else "bottom"
  title <- object$title %W% get_plot_axis_title(plot, x_or_y)
  # old version used guide = guide_clean_axis() in line below
  plot <- plot +
    axis_fn(
      name = title,
      limits = range(limits, breaks, na.rm = TRUE),
      breaks = breaks,
      labels = labels,
      trans = trans,
      oob = object$censor_fn,
      position = position,
      expand = c(expand_lower, 0, object$expand_upper, 0),
      guide = ggplot2::guide_axis(cap = object$cap)
    )
  plot
}

#' Continuous axis scale constructor
#'
#' @param axis Options include `"y"` (default) or `"x"`
#' @param scale Options include `"regular"` (default), `"scientific"`, `"log10"`, `"log2"`, `"log"` (same as `"log10"`)
#' @param title Axis title
#' @param limits Minimum and maximum values in data. Must enter in order, i.e. c(lower, upper)
#' @param n_breaks Desired number of axis breaks. Default is `4`
#' @param breaks Numeric vector or function specifying location of ticks along axis
#' @param labels Vector or function specifying axis tick labels
#' @param expand_lower,expand_upper Expansion multiplier around lower and upper limits of axis
#' @param position Location of axis. Options: `"left"` (default for y axis), `"right"`, "bottom" (default for x axis), "top"
#' @param censor_fn Function used to transform data outside axis limits. Default is `rescale_none.` Alternative: `scales::censor`
#' @param breaks_fn Function used to create breaks. Enter as call using (). Default is `pretty`. Alternative is `breaks_extended`
#' @param cap Input passed to `ggplot2::guide_axis`
#' @param ... Arguments passed to scale function
#' @returns List passed to `ggplot_add.axis_clean`. Enter as `plot + scale_continuous()`
#' @export
scale_continuous <- function(
    axis = "y",
    scale = "regular",
    title = "",
    limits = NULL,
    breaks = NULL,
    labels = NULL,
    expand_lower = NULL,
    expand_upper = 0,
    n_breaks = 4,
    position = NULL,
    censor_fn = rescale_none,
    breaks_fn = pretty,
    cap = "upper",
    ...) {
  structure(list(axis = axis, scale = scale, title = title, limits = limits, breaks = breaks, labels = labels, expand_lower = expand_lower, expand_upper = expand_upper, n_breaks = n_breaks, position = position, censor_fn = censor_fn, breaks_fn = breaks_fn, cap = cap, ...), class = "axis_clean")
}

#' Add axis scale that removes " x 10^x" from axis labels
#'
#' @param object New scale to add
#' @param plot ggplot object
#' @param object_name Name of object
#' @returns ggplot object. Needed for `scale_x10`. Not called directly by user. Must be exported
#' @export
ggplot_add.axis_x10 <- function(object, plot, object_name) {
  x_or_y <- object$axis
  axis_fn <- match.fun(sprintf("scale_%s_continuous", x_or_y))
  limits <- object$limits %W% get_plot_data_limits(plot, axis = x_or_y)
  breaks <- object$breaks
  if (is.null(breaks) || is_waiver(breaks)) {
    breaks <- get_plot_axis_breaks(plot, x_or_y)
  }
  max_break <- max(breaks, na.rm = TRUE)
  if (max_break <= 0) {
    Warning("Can't use 'scale_x10' when maximum break is <= 0")
    labels <- object$labels
    if (is_waiver(labels)) {
      labels <- get_plot_axis_labels(plot, x_or_y)
      if (is_waiver(labels)) {
        labels <- axis_label_numeric
      }
    }
    title <- object$title %W% get_plot_axis_title(plot, x_or_y)
  } else {
    z <- log10(max_break)
    if (ceiling(z) >= object$max_digits) {
      z <- object$exp %||% floor(z)
      labels <- axis_label_numeric(breaks/10^z)
      title <- object$title
      if (!is.null(title)) {
        if (is_waiver(title)) {
          plot_title <- get_plot_axis_title(plot, x_or_y)
          title <- if (is_waiver(plot_title) || is.null(plot_title)) "" else plot_title
        }
        title_suffix <- if (object$add_space) {
          if (object$show_parentheses) {
            sprintf("'(\u00D7'~'10'^'%s'*')'", z)
          } else {
            sprintf("'\u00D7'~'10'^'%s'", z)
          }
        } else {
          if (object$show_parentheses) {
            sprintf("'(\u00D710'^'%s'*')'", z)
          } else {
            sprintf("'\u00D710'^'%s'", z)
          }
        }
        title <- str2expression(paste0(if (is.character(title)) shQuote(title) else paste0(title), "~", title_suffix))
      }
    } else {
      labels <- object$labels
      labels <- labels %W% get_plot_axis_labels(plot, x_or_y)
      labels <- labels %W% axis_label_numeric
      title <- object$title %W% get_plot_axis_title(plot, x_or_y)
    }
  }
  object$position <- object$position %||% if (x_or_y == "x") "bottom" else "left"
  expand_lower <- object$expand_lower %||% if (x_or_y == "x" || Min(breaks) == 0) 0.1 else 0
  plot <- plot +
    axis_fn(
      name = title,
      limits = range(limits, breaks, na.rm = TRUE),
      breaks = breaks,
      labels = labels,
      trans = scales::identity_trans(),
      oob = object$censor_fn,
      position = object$position,
      expand = c(expand_lower, 0, object$expand_upper, 0),
      guide = ggplot2::guide_axis(cap = object$cap)
    )
  plot
}

#' Programmatically create simplified x10^n axis scale to ggplot
#'
#' @inheritParams scale_continuous
#' @param max_digits Maximum number of digits allowed for maximum break. Default is `3` (number indicated on axis labels must have 3 or fewer digits thus multiplier will be applied for values 1000 or higher)
#' @param exp Exponent (10^x) for axis multiplier. Enter as length 1 numeric
#' @param show_parentheses If `TRUE` (default), x10^n multiplier is wrapped in `"()"`
#' @param add_space If `TRUE` (default), space is added between multiplication symbol and 10^n
#' @param cap Input passed to `ggplot2::guide_axis`
#' @returns axis_x10 object. Enter as` plot + scale_x10()`
#' @export
scale_x10 <- function(
    axis = "y",
    title = waiver(),
    limits = waiver(),
    breaks = waiver(),
    labels = waiver(),
    expand_lower = 0,
    expand_upper = 0,
    max_digits = 3,
    n_breaks = 4,
    position = NULL,
    censor_fn = rescale_none,
    breaks_fn = pretty,
    exp = NULL,
    show_parentheses = TRUE,
    add_space = TRUE,
    cap = "upper",
    ...) {
  structure(list(axis = axis, scale = scale, title = title, limits = limits, breaks = breaks, labels = labels, expand_lower = expand_lower, expand_upper = expand_upper, max_digits = max_digits, n_breaks = n_breaks, position = position, censor_fn = censor_fn, breaks_fn = breaks_fn, exp = exp, show_parentheses = show_parentheses, add_space = add_space, cap = cap, ...), class = "axis_x10")
}

#' Clean axis
#'
#' Needed to position significance annotation outside of plot limits
#' @inheritParams scale_continuous
#' @param breaks Axis breaks
#' @param expand_lower,expand_upper Expansion to add to ends of axis
#' @param plot_limits Limits of data for plotting all objects in plotting space
#' @param axis_limits Limits used to generate axis breaks
#' @returns scale object
#' @export
scale_axis_clean <- function(
  axis = "y",
  scale = "regular",
  plot_limits,
  axis_limits,
  title = NULL,
  breaks = NULL,
  breaks_fn = pretty,
  n_breaks = 4,
  labels = NULL,
  expand_lower = 0,
  expand_upper = 0,
  position = NULL,
  censor_fn = rescale_none,
  ...) {
  scale <- match.arg(scale, choices = c("regular", "scientific", "log10", "log", "log2"))
  position <- position %||% if (axis == "y") "left" else "bottom"
  axis_fn <- match.fun(sprintf("scale_%s_continuous", axis))

  # Breaks
  breaks <- breaks %||% .create_axis_breaks(.limits = axis_limits, .scale = scale, .breaks_fn = breaks_fn, .n = n_breaks)

  # Labels
  #labels <- labels %||% switch(scale, regular = axis_label_numeric, scientific = axis_label_x10, log2 = scales::trans_format("log2", scales::label_math(2^.x)), log10 = , log = scales::trans_format("log10", scales::label_math(10^.x)))
  labels <- labels %||% switch(scale, regular = axis_label_numeric, scientific = axis_label_x10, log2 = scales::label_log(2), log10 = , log = scales::label_log(10))

  # Trans
  #trans <- switch(scale, log2 = scales::log2_trans(), log = , log10 = scales::log10_trans(), scales::identity_trans())
  trans <- switch(scale,
    log2 = scales::transform_log(2),
    log = ,
    log10 = scales::transform_log(10),
    scales::transform_identity()
  )

  # Limits
  plot_limits <- range(plot_limits, breaks, na.rm = TRUE)
  if (startsWith(scale, "log") && plot_limits[1L] <= 0) {
    plot_limits[1L] <- -Inf
  }

  # Output
  axis_fn(
    name = title,
    limits = plot_limits,
    breaks = breaks,
    labels = labels,
    trans = trans,
    oob = censor_fn,
    position = position,
    expand = c(expand_lower, 0, expand_upper, 0),
    ...
  )
}

# Breaks ------------------------------------------------------------------

#' Create axis breaks for continuous variable
#'
#' @param n Desired number of axis breaks. Enter as length 1 numeric. Default is `4`
#' @param breaks_fn Function to generate breaks. Default is `pretty.` Alternative is `breaks_extended`
#' @param min_breaks Minimum number of possible axis breaks. Enter as length 1 numeric. Default is `4`
#' @param max_breaks Maximum number of breaks allowed. Enter as length 1 numeric. Default is `6`
#' @param include_negative If `FALSE` (default), negative numbers are excluded from breaks
#' @returns Function. Enter as `scale_y_continuous(breaks = breaks_linear())`
#' @export
breaks_linear <- function(
    n = 4,
    breaks_fn = pretty,
    min_breaks = 4,
    max_breaks = 6,
    include_negative = FALSE) {
  list(min_breaks, max_breaks, include_negative)
  n_default <- n
  .breaks <- if (any(c("n", "...") %in% names(formals(breaks_fn)))) {
    breaks_fn
  } else {
    function(x, ...) breaks_fn(x)
  }
  function(x, n = n_default) {
    limits <- suppressWarnings(range(x[is.finite(x)]))
    #if (length(limits) == 0L) return(numeric())
    out <- .breaks(limits, n = n)
    if (!include_negative) {
      out <- out[out >= 0]
    }
    n_breaks <- length(out)
    if (n_breaks > max_breaks && is_odd(n_breaks)) {
      out <- out[seq.int(from = 1, to = n_breaks, by = 2)]
    }
    if (n_breaks == 3 && min_breaks > 3) {
      out <- seq.int(from = out[1L], to = out[3L], by = (out[3L] - out[1L])/4)
    }
    out
  }
}

#' Calculate position of log breaks
#'
#' @param base Log base. Enter as length 1 numeric. Default is `10`
#' @param n Desired number of tick marks. Enter as length 1 numeric. Default is `5`
#' @param breaks_fn Function to generate breaks. Must accept a numeric vector as input and return a numeric vector as output. Default is `breaks_extended`
#' @param log_fn Logarithm function. Must have argument named `base`. Default is `log`
#' @returns Function that can be used to generate numeric vector of break positions on log scale
#' @export
breaks_log <- function(base = 10, n = 5, breaks_fn = breaks_extended, log_fn = log) {
  Log <- function(x) log_fn(x, base = base)
  n_default <- n
  .breaks <- if (any(c("n", "...") %in% names(formals(breaks_fn)))) {
    breaks_fn
  } else {
    function(x, ...) breaks_fn(x)
  }
  function(x, n = n_default) {
    limits <- suppressWarnings(range(x, na.rm = TRUE))
    #if (length(limits) == 0L) return(numeric())
    log_limits <- Log(limits)
    upper <- base^ceiling(log_limits[2L])
    lower <- base^floor(log_limits[1L])
    out <- base^.breaks(Log(c(upper, lower)), n = n)
    out[Log(out) %% 1 == 0]
  }
}

# Labels ------------------------------------------------------------------

#' Labeling function for continuous axis
#'
#' @param x Numeric vector
#' @param max_leading_zeroes Maximum number of leading zeroes when max < 1. Default is `3`
#' @param max_trailing_zeroes Maximum number of leading zeroes when max > 1. Default is `4`
#' @param unicode If `TRUE` (default), multiplication sign is displayed as unicode. If `FALSE`, unicode not used
#' @returns Character vector. Enter as `scale_y_continuous(labels = axis_label_numeric)`
#' @export
axis_label_numeric <- function(x, max_leading_zeroes = 3, max_trailing_zeroes = 4, unicode = TRUE) {
  x_max <- abs(Max(x))
  if (x_max < 10^(-1 - max_leading_zeroes) || x_max >= 10^max_trailing_zeroes) {
    axis_label_x10(x, unicode)
  } else {
    format(x, scientific = FALSE)
  }
}

#' Create axis labels with "base^x" format (i.e., without "multipler x" prior to base^x)
#'
#' Functionality from `scales::label_log`
#' @param base Logarithm base. Enter as length 1 numeric. Default is `10`
#' @returns Function that returns an expression. Enter as `scale_(x|y)_continuous(labels = axis_label_log(10))`
#' @export
axis_label_log <- function(base = 10) {
  force(base)
  function(x) {
    if (length(x) == 0L) return(expression())
    #label <- format.default(log(x, base = base), digits = 1, trim = TRUE)
    label <- paste0(base, "^", round(log(abs(x), base = base)))
    idx_na <- is.na(x)
    idx_neg <- !idx_na & x < 0
    if (is.na(z <- any(idx_neg)) || z) {
      label[idx_neg] <- paste0("-", label[idx_neg])
    }
    label[x == 0] <- "0"
    #n <- length(label)
    #out <- vector("expression", n)
    #for (i in seq_len(n)) {
    #  z <- parse(text = label[[i]])
    #  out[[i]] <- if (length(z) == 0L) NA else z[[1L]]
    #}
    label <- parse(text = label)
    label[idx_na] <- NA
    label
  }
}

#' Create axis labels with "multiplier x 10^exp" format
#'
#' @param x Numeric vector containing position of axis breaks
#' @param unicode If `TRUE` (default), multiplication sign is displayed as unicode. If `FALSE`, unicode not used
#' @returns Expression. Enter as `scale_y_continuous(labels = axis_label_x10)`. Output includes multiplier prior to x sign
#' @export
axis_label_x10 <- function(x, unicode = TRUE) {
  if (unicode) {
    has_decimal <- any(grepl(".", format(x, scientific = TRUE), fixed = TRUE))
    axis_labels <- function(j) {
      if (is.na(j)) {
        return("")
      } else if (j == 0) {
        return(0)
      }
      j <- unlist(strsplit(format(j, scientific = TRUE), "e", fixed = TRUE), use.names = FALSE)
      z <- j[1L]
      if (has_decimal && !grepl(".", z, fixed = TRUE)) {
        z <- paste0(z, ".0")
      }
      bquote(.(paste(z, "\u00d7", "10"))^.(as.integer(j[2L])))
    }
    # do.call("expression", lapply(x, function(z) tryCatch(axis_labels(z), error = function(e) NA)))
    as.expression(lapply(x, axis_labels))
  } else {
    z <- format(x, scientific = TRUE)
    z <- gsub("0e\\+00", "0", z)
    z <- gsub("^(.*)e", "'\\1'e", z)
    z <- gsub("e\\+", "e", z)
    parse(text = gsub("e", "%*%10^", z))
  }
}

# Guide for clean axis ----------------------------------------------------

#' Guide for clean axis
#'
#' @inheritParams ggplot2::guide_axis
#' @returns guide object. Enter as `guides(axis = guide_clean_axis())` or `scale_x|y_continuous(guide = guide_clean_axis())`
#' @export
guide_clean_axis <- function(title = waiver(), theme = NULL, check.overlap = FALSE,  angle = waiver(), n.dodge = 1, minor.ticks = FALSE, cap = "upper", order = 0, position = waiver()) {
  #title = waiver(), angle = NULL, order = 0, position = waiver()
  #structure(
  #  list(
  #    title = title,
  #    angle = angle,
  #    order = order,
  #    position = position,
  #    available_aes = c("x", "y"),
  #    name = "axis"
  #  ),
  #  class = c("guide", "clean", "axis")
  #)
  guide_axis(title = title, theme = theme, check.overlap = check.overlap, angle = angle, n.dodge = n.dodge, minor.ticks = minor.ticks, cap = cap, order = order, position = position)
}

#' Method for for `guide_gengrob` applied to "clean" object
#'
#' @param guide Guide
#' @param theme Theme
#' @returns Not called directly. Used in `guide_clean_axis` function. Must be exported
#' @export
guide_gengrob.clean <- function(guide, theme) {
  guide_names <- names(guide$key)
  aesthetic <- guide_names[!grepl("^\\.", guide_names)][1L]
  draw_axis_clean(
    break_positions = guide$key[[aesthetic]],
    break_labels = guide$key$.label,
    axis_position = guide$position,
    theme = theme,
    angle = guide$angle
  )
}

#' Grob to draw clean axis
#'
#' Functionality from Charlotte Dawson's excellent package ggprism
#' @param break_positions position of ticks
#' @param break_labels labels at ticks
#' @param axis_position Axis position. Options: `"left"` (default for y axis), `"bottom"` (default for x axis), `"right"`, `"top"`
#' @param theme Complete theme
#' @param angle Angle for axis tick labels
#' @returns Called by `guide_gengrob.clean`
#' @export
draw_axis_clean <- function(break_positions, break_labels, axis_position, theme, angle = NULL) {
  axis_position <- match.arg(axis_position, choices = c("top", "bottom", "right", "left"))
  z <- sprintf("%s.%s", c("axis.line", "axis.ticks", "axis.ticks.length", "axis.text"), if (axis_position %in% c("top", "bottom")) "x" else "y")
  line_element <- ggplot2::calc_element(z[1L], theme)
  tick_element <- ggplot2::calc_element(z[2L], theme)
  tick_length <- ggplot2::calc_element(z[3L], theme)
  label_element <- ggplot2::calc_element(z[4L], theme)

  # Angle for tick labels
  if (!is.null(angle) && inherits(label_element, "element_text")) {
    label_element$angle <- angle
    switch(
      axis_position,
      bottom = {
        label_element$hjust <- if (angle > 0) 1 else if (angle < 0) 0 else 0.5
        label_element$vjust <- if (abs(angle) == 90) 0.5 else 1
      },
      left = {
        label_element$hjust <- if (abs(angle) == 90) 0.5 else 1
        label_element$vjust <- if (angle > 0) 0 else if (angle < 0) 1 else 0.5
      },
      top = {
        label_element$hjust <- if (angle > 0) 0 else if (angle < 0) 1 else 0.5
        label_element$vjust <- if (abs(angle) == 90) 0.5 else 0
      },
      right = {
        label_element$hjust <- if (abs(angle) == 90) 0.5 else 0
        label_element$vjust <- if (angle > 0) 1 else if (angle < 0) 0 else 0.5
      }
    )
  }

  # Parameters for x or y axis
  is_vertical <- axis_position %in% c("left", "right")
  if (is_vertical) {
    position_dim <- "y"
    non_position_dim <- "x"
    label_margin_name <- "margin_x"
    position_size <- "height"
    non_position_size <- "width"
    gtable_element <- gtable::gtable_row
    measure_gtable <- gtable::gtable_width
    measure_labels_non_pos <- grid::grobWidth
  } else {
    position_dim <- "x"
    non_position_dim <- "y"
    label_margin_name <- "margin_y"
    position_size <- "width"
    non_position_size <- "height"
    gtable_element <- gtable::gtable_col
    measure_gtable <- gtable::gtable_height
    measure_labels_non_pos <- grid::grobHeight
  }

  # Parameters for primary or secondary axis
  is_second <- axis_position %in% c("right", "top")
  if (is_second) {
    tick_direction <- 1
    non_position_panel <- grid::unit(0, "npc")
    tick_coordinate_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- grid::unit(1, "npc")
    tick_coordinate_order <- c(1, 2)
  }

  # gtable ordering
  labels_first_gtable <- axis_position %in% c("left", "top")

  # Common parameters
  n_breaks <- length(break_positions)
  opposite_positions <- c(top = "bottom", bottom = "top", right = "left", left = "right")
  axis_position_opposite <- opposite_positions[axis_position]
  names(axis_position_opposite) <- NULL

  # Line
  line_grob <- rlang::exec(
    ggplot2::element_grob, line_element,
    # to extend axis line to first break (instead of 0), replace 0 in line below with min(break_positions)
    !!position_dim := grid::unit(c(0, max(break_positions)), "npc"),
    !!non_position_dim := grid::unit.c(non_position_panel, non_position_panel)
  )
  if (n_breaks == 0L) {
    return(grid::gTree(children = grid::gList(line_grob), width = grid::grobWidth(line_grob), height = grid::grobHeight(line_grob), xmin = NULL, ymin = NULL, vp = NULL, cl = "absoluteGrob"))
  }

  # Labels
  if (is.list(break_labels)) {
    break_labels <- if (any(vapply(break_labels, is.language, logical(1), USE.NAMES = FALSE))) {
      do.call(expression, break_labels)
    } else {
      unlist(break_labels)
    }
  }
  args <- list(element = label_element, label = break_labels, check.overlap = FALSE)
  args[[label_margin_name]] <- TRUE
  args[[position_dim]] <- grid::unit(break_positions, "native")
  label_grobs <- do.call(ggplot2::element_grob, args)

  # Ticks
  ticks_grob <- rlang::exec(ggplot2::element_grob, tick_element, !!position_dim := rep(grid::unit(break_positions, "native"), each = 2), !!non_position_dim := rep(grid::unit.c(non_position_panel + (tick_direction*tick_length), non_position_panel)[tick_coordinate_order], times = n_breaks), id.lengths = rep(2, times = n_breaks))

  # gtable
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- grid::unit.c(measure_labels_non_pos(label_grobs))
  grobs <- c(list(ticks_grob), list(label_grobs))
  grob_dims <- grid::unit.c(tick_length, label_dims)
  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }
  gt <- rlang::exec(gtable_element, name = "axis", grobs = grobs, !!non_position_sizes := grob_dims, !!position_size := grid::unit(1, "npc"))

  # Create viewport
  justvp <- rlang::exec(grid::viewport, !!non_position_dim := non_position_panel, !!non_position_size := measure_gtable(gt), just = axis_position_opposite)
  grid::gTree(children = grid::gList(line_grob, gt), width = gtable::gtable_width(gt), height = gtable::gtable_height(gt), xmin = NULL, ymin = NULL, vp = justvp, cl = "absoluteGrob")
}

# Helper functions --------------------------------------------------------

#' Calculate break positions
#'
#' Functionality from `extended` function in labelled package
#' @param x Limits or raw data values
#' @param n Desired number of axis breaks. Default is `5`
#' @param expanded If `TRUE` (default), first breaks is less than `min(x)` and last breaks is greater than `max(x)`
#' @param nice Numeric vector of "nice" numbers
#' @returns Numeric vector of break positions. `breaks_extended(x, n)` is equivalent to `scales::breaks_extended(n = n, only.loose = TRUE)(x)`
#' @export
breaks_extended <- function(x, n = 5, expanded = TRUE, nice = c(1, 5, 2, 2.5, 4, 3)) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(numeric())
  eps <- .Machine$double.eps*100
  rng <- range(x)
  l <- rng[1L]
  u <- rng[2L]
  rng <- u - l
  rng_sq <- rng*rng*0.01
  best <- list(score = -2)
  j <- 1
  h <- length(nice)
  hm1 <- h - 1
  idx <- seq_len(h)
  idx_m1 <- idx - 1
  m <- n - 1
  while (j < Inf) {
    for (i in idx) {
      v <- nice[i]
      q1 <- idx_m1[i]
      sm <- 2 - q1/hm1 - j
      if ((0.25*sm + 0.75) < best$score) {
        j <- Inf
        break
      }
      k <- 2
      while (k < Inf) {
        dm <- if (k >= n) {
          2 - (k - 1)/m
        } else {
          1
        }
        if ((0.25*sm + 0.5*dm + 0.25) < best$score) {
          break
        }
        z <- ceiling(log10(rng/(k + 1)/j/v))
        while (z < Inf) {
          step <- j*v*10^z
          span <- step*(k - 1)
          cm <- if (span > rng) {
            half <- (span - rng)/2
            1 - half*half/rng_sq
          } else {
            1
          }
          if ((0.25*sm + 0.2*cm + 0.5*dm + 0.05) < best$score) {
            break
          }
          min_start <- floor(u/step)*j - (k - 1)*j
          max_start <- ceiling(l/step)*j
          if (min_start > max_start) {
            z <- z + 1
            next
          }
          for (start in seq.int(from = min_start, to = max_start)) {
            lmin <- start*step/j
            lmax <- lmin + step*(k - 1)
            #s <- ifelse((lmin %% step < eps || step - (lmin %% step) < eps) && lmin <= 0 && lmax >= 0, 1, 0)
            s <- as.numeric((lmin %% step < eps || step - (lmin %% step) < eps) && lmin <= 0 && lmax >= 0)
            s <- 1 - q1/hm1 - j + s
            w <- 1 - 0.5*((u - lmax)^2 + (l - lmin)^2)/rng_sq
            r <- (k - 1)/(lmax - lmin)
            rt <- m/(max(lmax, u) - min(l, lmin))
            g <- 2 - max(r/rt, rt/r)
            score <- 0.25*s + 0.2*w + 0.5*g + 0.05
            if (score > best$score && (!expanded || (lmin <= l && lmax >= u))) {
              best <- list(lmin = lmin, lmax = lmax, lstep = step, score = score)
            }
          }
          z <- z + 1
        }
        k <- k + 1
      }
    }
    j <- j + 1
  }
  seq.int(from = best$lmin, to = best$lmax, by = best$lstep)
}

#' Set axis limits
#'
#' @param .min,.max Minimum and maximum values respectively
#' @param .breaks Numeric vector of axis breaks
#' @param .values Numeric vector containing raw data along axis of interest
#' @noRd
.set_axis_limits <-  function(.min, .max, .breaks, .values) {
  min_value <- if (!is.null(.min)) {
    .min
  } else if (!is.null(.breaks)) {
    Min(.breaks)
  } else {
    Min(.values)
  }
  max_value <- if (!is.null(.max)) {
    .max
  } else if (!is.null(.breaks)) {
    Max(.breaks)
  } else {
    Max(.values)
  }
  c(min_value, max_value)
}

#' Set axis breaks for continuous axis
#'
#' @param .limits Length 2 numeric vector with lower limit first and upper limit second. Input is usually limits of raw data
#' @param .scale Options: `"regular"` (default), `"scientific"`, `"log2"`, `"log10"`, `"log"`
#' @param .breaks_fn Breaks function. Input must take numeric length 2 numeric vector (limits) as input and an argument named "n" or "...". Only relevant when `.scale` is "regular" or "scientific." Extended breaks are used if `NULL.` Default is `pretty`
#' @param .n Number of axis breaks. Default is `4`
#' @noRd
.create_axis_breaks <- function(.limits, .scale = "regular", .breaks_fn = pretty, .n = 4) {
  .breaks_fn <- .breaks_fn %||% breaks_extended
  if (.scale %in% c("regular", "scientific")) {
    breaks <- .breaks_fn(.limits, n = .n)
    len_breaks <- length(breaks)
    if (len_breaks > 6L && is_odd(len_breaks)) {
      breaks <- breaks[seq.int(from = 1, to = len_breaks, by = 2)]
    }
    if (len_breaks == 3L) {
      breaks <- seq.int(from = breaks[1L], to = breaks[3L], by = (breaks[3L] - breaks[1L])/4)
    }
  } else if (grepl("log", .scale, fixed = TRUE)) {
    base <- if (.scale == "log2") 2 else 10
    log_limits <- log(.limits, base = base)
    upper <- base^(ceiling(log_limits[2L]))
    lower <- base^(floor(log_limits[1L]))
    .limits <- c(upper, lower)
    breaks <- base^.breaks_fn(log(.limits, base = base), n = .n)
    breaks <- breaks[log(breaks, base = base) %% 1 == 0]
  }
  breaks
}

#' log transformer
#'
#' @param base Log base. Enter as length 1 numeric. Default is `10`
#' @returns Enter as input to `transform` argument of `scale_(x|y)_continuous` function
#' @export
log_transform <- function(base = 10) {
  force(base)
  scales::new_transform(
    name = paste("log", format(base), sep = "_"),
    transform = function(x) log(x, base),
    inverse = function(x) base^x,
    d_transform = function(x) 1/x/log(base),
    d_inverse = function(x) base^x*log(base),
    breaks = breaks_log(base = base),
    domain = c(1e-100, Inf)
  )
}

#' lognp transformer
#'
#' @rdname log_transform
#' @param n Constant added to raw values prior to log transformation. Enter as length 1 numeric. Default is `1`
#' @returns Enter as input to `transform` argument of `scale_(x|y)_continuous` function
#' @export
lognp_transform <- function(base = 10, n = 1) {
  force(base)
  constant <- n
  scales::new_transform(
    name = paste("log", format(base), format(n), sep = "_"),
    transform = function(x) log(x + constant, base),
    inverse = function(x) base^(x + constant),
    d_transform = function(x) 1/(x + constant)/log(base),
    d_inverse = function(x) base^(x + constant)*log(base),
    breaks = breaks_log(base = base),
    domain = c(0, Inf)
  )
}

#' lognp transformer
#'
#' @rdname log_transform
#' @param n Constant added to raw values prior to log transformation. Enter as length 1 numeric. Default is `1`
#' @returns Enter as input to `transform` argument of `scale_(x|y)_continuous` function
#' @export
log_nonzero_transform <- function(base = 10) {
  force(base)
  scales::new_transform(
    name = paste("log", format(base), "nonzero", sep = "_"),
    transform = function(x) {
      z <- log(x, base)
      z[x == 0] <- 0
      z
    },
    inverse = function(x) {
      z <- base^x
      z[x == 0] <- 0
      z
    },
    breaks = breaks_log(base = base),
    domain = c(0, Inf)
  )
}
