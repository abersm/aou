# Scale functions ---------------------------------------------------------

#' New fill scale
#'
#' Functionality from ggnewscale package
#' @returns Called directly by user. Requires `ggplot_add.new_color`
#' @noRd
scale_fill_new <- function() {
  #structure(ggplot2::standardise_aes_names("fill"), class = "new_color")
  structure("fill", class = "new_color")
}

#' Add new color scale
#'
#' @param object New scale to add
#' @param plot ggplot object
#' @param object_name Name of object
#' @returns Not called directly by user. Must be exported. Needed for `scale_fill_new` and `scale_color_new`
#' @export
ggplot_add.new_color <- function(object, plot, object_name) {
  if (is.null(plot$scales$get_scales(object))) {
    plot$scales <- ggplot2::ggplot_build(plot)$plot$scales
  }
  z <- gsub("(_new)*", "", names(plot$mapping))
  old_aes <- names(plot$mapping)[z %in% object]
  new_aes <- paste0(old_aes, "_new")
  names(plot$mapping)[names(plot$mapping) == old_aes] <- new_aes
  plot$layers <- lapply(plot$layers, function(x) {
    original_aes <- object
    new_layer <- ggplot2::ggproto(NULL, x)
    names_mapping <- names(new_layer$mapping)
    z <- gsub("(_new)*", "", names(new_layer$mapping))
    old_aes <- names_mapping[z %in% object]
    if (length(old_aes) == 0) {
      names_stat <- names(new_layer$stat$default_aes)
      z <- gsub("(_new)*", "", names_stat)
      old_aes <- names_stat[z %in% object]
      if (length(old_aes) == 0) {
        names_geom <- names(new_layer$geom$default_aes)
        z <- gsub("(_new)*", "", names_geom)
        old_aes <- names_geom[z %in% object]
      }
    }
    new_aes <- paste0(old_aes, "_new")
    old_geom <- new_layer$geom
    old_handle_na <- old_geom$handle_na
    change_name <- function(list, old, new) {
      if (is.null(list)) {
        NULL
      } else if (is.character(list)) {
        list[list %in% old] <- new
        list
      } else {
        name <- names(list)
        name[name %in% old] <- new
        names(list) <- name
        list
      }
    }
    new_geom <- ggplot2::ggproto(paste0("New", class(old_geom)[1]), old_geom, handle_na = function(self, data, params) {
      names(data)[names(data) %in% new_aes] <- original_aes
      old_handle_na(data, params)
    })
    new_geom$default_aes <- change_name(new_geom$default_aes, old_aes, new_aes)
    new_geom$non_missing_aes <- change_name(new_geom$non_missing_aes, old_aes, new_aes)
    new_geom$required_aes <- change_name(new_geom$required_aes, old_aes, new_aes)
    new_geom$optional_aes <- change_name(new_geom$optional_aes, old_aes, new_aes)
    draw_key <- new_geom$draw_key
    new_draw_key <- function(data, params, size) {
      names(data)[names(data) == new_aes] <- original_aes
      draw_key(data, params, size)
    }
    new_geom$draw_key <- new_draw_key
    new_layer$geom <- new_geom
    old_stat <- new_layer$stat
    parent <- if (!is.null(old_stat$is_new)) old_stat$super() else ggplot2::ggproto(NULL, old_stat)
    new_stat <- ggplot2::ggproto(paste0("New", class(old_stat)[1L]), parent, setup_data = function(self, data, scales, ...) {
      names(data)[names(data) %in% new_aes] <- original_aes
      data <- ggplot2::ggproto_parent(self$super(), self)$setup_data(data, scales, ...)
      names(data)[names(data) %in% original_aes] <- new_aes
      data
    }, handle_na = function(self, data, params) {
      names(data)[names(data) %in% new_aes] <- original_aes
      ggplot2::ggproto_parent(self$super(), self)$handle_na(data, params)
    }, is_new = TRUE)
    new_stat$default_aes <- change_name(new_stat$default_aes, old_aes, new_aes)
    new_stat$non_missing_aes <- change_name(new_stat$non_missing_aes, old_aes, new_aes)
    new_stat$required_aes <- change_name(new_stat$required_aes, old_aes, new_aes)
    new_stat$optional_aes <- change_name(new_stat$optional_aes, old_aes, new_aes)
    new_layer$stat <- new_stat
    new_layer$mapping <- change_name(new_layer$mapping, old_aes, new_aes)
    new_layer$aes_params <- change_name(new_layer$aes_params, old_aes, new_aes)
    new_layer
  })
  plot$scales$scales <- lapply(plot$scales$scales, function(x) {
    z <- x$aesthetics
    old_aes <- gsub("(_new)*", "", z)
    old_aes <- z[old_aes %in% object]
    if (length(old_aes) != 0) {
      z <- paste0(old_aes, "_new")
      x$aesthetics[x$aesthetics %in% old_aes] <- z
      no_guide <- x$guide == "none" || !(is.logical(x$guide) && length(x$guide) == 1L && !is.na(x$guide) && !x$guide)
      if (!no_guide) {
        if (is.character(x$guide)) {
          x$guide <- get(paste0("guide_", x$guide), mode = "function")()
        }
        x$guide$available_aes[x$guide$available_aes %in% old_aes] <- z
      }
    }
    x
  })
  z <- gsub("(_new)*", "", names(plot$labels))
  old_aes <-  names(plot$labels)[z %in% object]
  names(plot$labels)[names(plot$labels) %in% old_aes] <- paste0(old_aes, "_new")
  plot
}

# Data formatting ---------------------------------------------------------

#' Create x and y variables for plotting
#'
#' @param .df Data frame
#' @param .formula y ~ x. Prioritized over `.y` and `.x` if .formula and `.y` and `.x` are provided
#' @param .y,.x Variables forming y and x axis respectively. Enter as quoted or unquoted variable names. Only relevant if formula is missing
#' @param .vars_remove_na Variables other than `.x` and `.y` from which missing values should be removed. Enter as character vector
#' @returns Data frame with original columns and new columns "x" and "y". Rows with missing values for "x", "y", and `.vars_remove_na` are removed
#' @noRd
.create_plot_df <- function(.df, .formula = NULL, .y = NULL, .x = NULL, .vars_remove_na = NULL) {
  vars <- formula2vars(formula = .formula, x = .x, y = .y)
  .df$y <- .subset2(.df, vars$y)
  .df$x <- .subset2(.df, vars$x)
  .vars_remove_na <- Intersect(c("x", "y", .vars_remove_na), names(.df))
  .df[stats::complete.cases(.df[.vars_remove_na]), , drop = FALSE]
}

#' Use a data frame and variable names to split a continuous variable by a grouping variable into lists of numeric vectors
#'
#' @param df Data frame
#' @param formula Entered as continuous variable ~ grouping variable
#' @param x Categorical grouping variable. Enter as quoted variable name. Unused levels will be dropped
#' @param y Continuous variable. Enter as quoted variable name
#' @param excl_group_na If `TRUE` (default), missing values for grouping variable are not considered a distinct group
#' @returns List containing "x" (raw values for grouping variable as a factor), "y" (raw values for continuous variable as a numeric), "y_grouped" (list of numeric values for each level of grouping variable. Length of "y_grouped" is equal to the number of groups in grouping variable. Names refer to group levels)
#' @noRd
.df_to_split_vals <- function(df, formula = NULL, x = NULL, y = NULL, excl_group_na = TRUE) {
  vars <- formula2vars(formula = formula, x = x, y = y, parent_fn = .fn_called())
  x <- vars$x
  y <- vars$y
  if (excl_group_na) {
    df <- remove_na(df, cols = c(x, y))
    x <- as.factor(.subset2(df, x))
  } else {
    df <- remove_na(df, cols = y)
    x <- .subset2(df, x)
    x <- factor(x, exclude = if (inherits(x, "factor") && !anyNA(attr(x, "levels"))) NA else NULL)
  }
  y <- .subset2(df, y)
  list(x = x, y = y, y_grouped = split.default(y, f = x))
}

#' Create a categorical vector to be entered as column in data frame used for plotting
#'
#' @param df Data frame
#' @param var Variable to create or convert to factor. Enter as quoted variable name
#' @param if_null Character vector to return if `var` does not refer to a column in `df`
#' @param as_fct If `TRUE` (default), output is coerced to a factor vector
#' @param levels Levels to use in newly generated factor variable. Only relevant when `as_fct = TRUE`
#' @param reverse If `TRUE` order of levels is reversed. `FALSE` by default. Only relevant when `as_fct = TRUE`
#' @param droplevels If `TRUE` (default) and input is a factor, unused levels are dropped
#' @returns Character or factor vector with length equal to `nrow(df)`
#' @noRd
.new_cat_var <- function(df, var = NULL, if_null = "a", as_fct = TRUE, levels = NULL, reverse = FALSE, droplevels = TRUE) {
  var <- var %||% "1"
  vals <- .subset2(df, var) %||% if_null
  if (as_fct) {
    if (is_date(vals)) {
      vals <- as.character(vals)
    }
    if (is.null(levels)) {
      levels <- create_levels(vals, reverse = reverse, droplevels = droplevels)
    } else if (reverse) {
      levels <- Rev(levels)
    }
    factor(vals, levels = levels)
  } else {
    vals
  }
}

#' Convert plot to ggplot object
#'
#' @param x Code to generate base plot
#' @returns ggplot object
#' @noRd
as_ggplot <- function(x) {
  if (is.null(x)) {
    x <- substitute(x)
    x <- bquote(~.(x))
    #x <- as_formula(x, env = parent.frame())
  }
  ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    theme_clean() +
    draw_grob(as_grob(x), x = 0, y = 0, width = 1, height = 1, scale = 1, hjust = 0, vjust = 0, halign = 0.5, valign = 0.5)
}

#' Convert base plot to ggplot object
#'
#' @param expr Plotting expression
#' @param envir Environment in which `expr` should be evaluated. Default is parent frame
#' @returns ggplot
#' @noRd
base_to_ggplot <- function(expr, envir = parent.frame()) {
  expr <- substitute(expr)
  pkg_required("gridGraphics")
  grDevices::pdf(if (.Platform$OS.type == "windows") "NUL" else "/dev/null")
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  eval(expr, envir = envir, enclos = baseenv())
  out <- grDevices::recordPlot()
  out <- gridGraphics::echoGrob(out, device = function(width, height) {
    grDevices::pdf(NULL, width = width, height = height)
    grDevices::dev.control("enable")
  })
  ggplot2::ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    theme_clean() +
    draw_grob(out, x = 0, y = 0, width = 1, height = 1, scale = 1, hjust = 0, vjust = 0, halign = 0.5, valign = 0.5)
}

#' Record most recent base plot
#'
#' @returns recordedplot object
#' @noRd
capture_base_plot <- function() {
  x <- grDevices::dev.cur()
  if (x == 1 || names(x)[1L] == "null device") return(invisible())
  invisible(grDevices::recordPlot())
}

#' Check a vector for zero range
#'
#' Functionality from `scales::zero_range`
#' @param x Range of numeric vector. Enter as length 1 or 2 numeric vector
#' @param tol Tolerance for 0. Enter as length 1 numeric vector
#' @returns Logical vector of length 1
#' @noRd
.has_zero_range <- function(x, tol = 1000*.Machine$double.eps) {
  if (length(x) == 1L) return(TRUE)
  if (anyNA(x)) return(NA)
  #if (anyNA(x)) return(TRUE)
  x_min <- x[1L]
  x_max <- x[2L]
  if (x_min == x_max) return(TRUE)
  if (all(is.infinite(x))) return(FALSE)
  m <- min(abs(x))
  if (m == 0) return(FALSE)
  abs((x_min - x_max)/m) < tol
}

#' Determine geom for plotting points
#'
#' Choose between plotting points using vector graphics (geom_point) or raster graphics using scattermore or ggraster
#'
#' @param ... Arguments passed to `raster_geom`
#' @param size Point size in pts units
#' @param rasterize If `TRUE`, points displayed as raster. If `FALSE` (default), points displayed using vector graphics
#' @param raster_geom geom for plotting points as raster. Default is `scattermore::geom_scattermore`. Can also use `ggrastr::geom_point_rast`. Only relevant when `rasterize = TRUE`
#' @param dpi Points per inch. Only relevant when `rasterize = TRUE`. Default is `300`
#' @returns ggproto
#' @noRd
.raster_point <- function(
    ...,
    size = NULL,
    rasterize = FALSE,
    raster_geom = scattermore::geom_scattermore,
    dpi = NULL) {
  if (rasterize) {
    if ("pointsize" %in% names(formals(raster_geom))) {
      # scattermore
      raster_geom(..., pointsize = size, pixels = rep(dpi %||% 512, length.out = 2L))
    } else {
      # ggrastr
      raster_geom(..., size = size, raster.dpi = dpi %||% 300, dev = "ragg_png")
    }
  } else {
    ggplot2::geom_point(..., size = size)
  }
}
