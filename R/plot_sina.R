#' Sina plot
#'
#' Functionality from Thomas Lin Pedersen's excellent package ggforce
#' @inheritParams plot_point
#' @inheritParams plot_violin
#' @param method Method used to make width equal across groups. Options: `"density"` (default), `"counts"`
#' @param n_bins Number of bins. Default is `50`
#' @param band_width Bandwidth. Options: `"nrd0"` (default), `"nrd"`, `"ucv"`, `"bcv"`, `"sj"`, `"sj-ste"`, `"sj-dpi"`, or integer
#' @param seed Number to input into random number generator for reproducibility
#' @returns ggplot object
#' @examples
#' # plot_sina(covid, age ~ gender, y_axis_title = "Age")
#' # plot_sina(covid, age ~ severity, x_angle = 45, y_axis_title = "Age")
#'
#' @export
plot_sina <- function(
    df,
    formula,
    grouping_var = NULL,
    grouping_var_order = NULL,
    rev_grouping_var_order = FALSE,
    x_order = NULL,
    fn_x_order = mean,
    rev_x_order = FALSE,
    x = NULL, y = NULL,
    colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#F0E442", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#FFD500", "#00C5CD", "#F7F7F7"),
    point_color_var = NULL,
    point_shape_var = NULL,
    point_shapes = c("circle", "circle"),
    point_border_color = "black",
    alpha = 0.9,
    point_alpha = alpha,
    width = 0.2,
    dodge = 0.7,
    point_border_thickness = 0.75,
    show_legend = FALSE,
    legend_title = "",
    y_scale = "regular",
    y_title = waiver(),
    y_axis_title = y_title,
    x_title = waiver(),
    x_axis_title = x_title,
    plot_title = NULL,
    n_breaks = 3,
    breaks_fn = pretty,
    x_breaks = NULL,
    x_axis_breaks = x_breaks,
    x_labels = NULL,
    x_axis_labels = x_labels,
    y_breaks = NULL,
    y_axis_breaks = y_breaks,
    y_labels = NULL,
    y_axis_labels = y_labels,
    y_max = NULL,
    y_min = NULL,
    expand_y = 0.1,
    expand_x = waiver(),
    censor_fn = rescale_none,
    point_size = 2,
    point_colors = colors,
    band_width = "nrd0",
    n_bins = 50,
    seed = 1234,
    scaling = c("width", "count", "area"),
    method = c("density", "counts"),
    show_sig = TRUE,
    sig_method = "p_by_normality",
    stars = TRUE,
    show_ns = TRUE,
    ns_symbol = "ns",
    sig_bar_nudge = if (y_scale == "log") 0.9 else 0.06,
    sig_star_nudge = if (y_scale == "log") -0.45 else sig_bar_nudge - 0.015,
    sig_text_nudge = if (y_scale == "log") 0.5 else sig_bar_nudge + 0.03,
    step_increase = if (y_scale == "log") 0 else sig_bar_nudge + 0.05,
    sig_bar_thickness = 0.6,
    sig_bar_color = "black",
    sig_font_color = "black",
    sig_star_size = 20,
    sig_text_size = 12,
    p_case = "upper",
    p_spaces = TRUE,
    ...) {
  # Plotting function
  plot_fn <- "plot_sina"

  # Data setup
  #df <- dplyr::select(df, grouping_var = {{grouping_var}}, point_color_var = {{point_color_var}}, point_shape_var = {{point_shape_var}}, dplyr::everything())
  grouping_var <- get_input(grouping_var)
  point_color_var <- get_input(point_color_var)
  point_shape_var <- get_input(point_shape_var)
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "plot_sina")
  x <- vars$x
  y <- vars$y
  x_vals <- .subset2(df, x)
  y_vals <- .subset2(df, y)
  #df_names <- names(df)
  #vars_remove_na <- Intersect(c("x", "y", "grouping_var", "point_color_var", "point_shape_var"), df_names)
  df <- remove_na(df, c(x, y))

  x_levels <- if (is.null(x_order)) {
    create_levels(x_vals)
  } else if (length(x_order) == 1L) {
    if (x_order == "ascending") {
      attr(fct_reorder(x_vals, y_vals, fn_x_order, .increasing = TRUE), "levels")
    } else if (x_order == "descending") {
      attr(fct_reorder(x_vals, y_vals, fn_x_order, .increasing = FALSE), "levels")
    }
  } else {
    x_order
  }
  if (rev_x_order) {
    x_levels <- Rev(x_levels)
  }
  df[[x]] <- factor(x_vals, levels = x_levels)

  # grouping_var
  df$grouping_var <- .new_cat_var(df, var = grouping_var, if_null = "a", levels = grouping_var_order, reverse = rev_grouping_var_order)

  # point_color_var
  df$point_color_var <- .new_cat_var(df, var = point_color_var, if_null = df$grouping_var)
  point_colors <- rep(point_colors, length.out = n_unique(df$point_color_var, na.rm = FALSE))

  # point_shape_var
  df$point_shape_var <- .new_cat_var(df, var = point_shape_var, if_null = "a")
  point_shapes <- rep(point_shapes, length.out = n_unique(df$point_shape_var, na.rm = FALSE))
  if (is.character(point_shapes)) {
    look_up_point_shape <- c(circle = 21, square = 22, triangle = 24, diamond = 23)
    point_shapes <- look_up_point_shape[point_shapes]
    names(point_shapes) <- NULL
  }
  scaling <- match.arg(scaling, choices = c("width", "count", "area"))
  method <- match.arg(method, choices = c("density", "counts"))
  unique_shapes <- unique(point_shapes)
  if (length(unique_shapes) == 1L) {
    point_args <- list(mapping = ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = point_color_var), shape = unique_shapes, color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend, inherit.aes = FALSE)
    scale_shape <- NULL
  } else {
    point_args <- list(mapping = ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = point_color_var, shape = point_shape_var), color = point_border_color, size = point_size, alpha = point_alpha, stroke = point_border_thickness, show.legend = show_legend)
    scale_shape <- scale_shape_manual(name = NULL, values = point_shapes, inherit.aes = FALSE)
  }

  # y axis
  y_axis_title <- y_axis_title %W% gsub("_", " ", str_capitalize(y), fixed = TRUE)
  y_limits <- .set_axis_limits(.min = y_min, .max = y_max, .breaks = y_axis_breaks, .values = y_vals)
  # Change y values if log scale is used
  if (startsWith(y_scale, "log")) {
    minimum_y_value <- min(y_vals)
    if (minimum_y_value < 1) {
      if (minimum_y_value < 0) {
        Warning("Some y values are < 0 (min = ", round_up(minimum_y_value, 2), ")")
        message("Will transform y values by adding ", round_up(minimum_y_value, 2), " to each value")
        df[[y]] <- y_vals + minimum_y_value
      } else if (minimum_y_value == 0) {
        z <- y_vals[y_vals > 0 & y_vals < 1]
        if (length(z) != 0L) {
          y_limits[1L] <- min(z)
        } else {
          df[[y]] <- y_vals + 1
          y_limits[1L] <- min(.subset2(df, y))
        }
      }
    }
  }

  # Points
  p <- do.call(ggplot2::geom_sina, c(point_args, scaling = scaling, method = method, width = width, position = ggplot2::position_dodge(width = dodge), seed = seed, n_bins = n_bins, band_width = band_width))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]], group = grouping_var)) +
  #p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    p +
    ggplot2::scale_fill_manual(name = legend_title, values = point_colors) +
    scale_shape

  # x axis
  x_axis_title <- x_axis_title %W% gsub("_", " ", str_capitalize(x), fixed = TRUE)
  x_breaks <- x_axis_breaks %||% x_levels
  x_labels <- if (!missing(x_axis_labels) && is.null(x_axis_labels)) {
    NULL
  } else {
    x_axis_labels %||% x_levels
  }
  p <- p + ggplot2::scale_x_discrete(name = x_axis_title, breaks = x_breaks, labels = x_labels)

  # Plot title/theme
  p <- p + ggplot2::coord_cartesian(clip = "off", default = TRUE) + ggplot2::ggtitle(plot_title) + theme_custom(...)

  # Significance annotation
  if (show_sig) {
    p <- if (n_unique(df$grouping_var, na.rm = FALSE) > 1L) {
      p + geom_sig(nested = TRUE)
    } else {
      p + geom_sig()
    }
    y_plot_limits <- get_plot_data_limits(p, axis = "y")
    p <- p + scale_axis_clean(axis = "y", scale = y_scale, plot_limits = y_plot_limits, axis_limits = y_limits, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn, guide = guide_axis(cap = "upper"))
  } else {
    p <- p + scale_continuous(axis = "y", scale = y_scale, limits = y_limits, breaks = y_axis_breaks, labels = y_axis_labels, title = y_axis_title, expand_lower = expand_y, n_breaks = n_breaks, censor_fn = censor_fn)
  }
  p
}

#' ggproto for sina plot
#'
#' @export
StatSina <- ggplot2::ggproto(
  "StatSina",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  setup_data = function(data, params) {
    # data: x, y, group, fill, PANEL. Values have not been transformed by scale_*_ functions
    data
  },
  setup_params = function(data, params) {
    # data: same data frame passed to "setup_data"
    params$width <- params$width %||% (ggplot2::resolution(data$x %||% 0)*0.9)
    if (is.null(params$bin_width) && is.null(params$n_bins)) {
      params$n_bins <- 50
    }
    params
  },
  compute_panel = function(
    self,
    data,
    scales,
    scaling = TRUE,
    method = "density",
    band_width = "nrd0",
    kernel = "gaussian",
    bin_width = NULL,
    n_bins = NULL,
    width = 1,
    adj = 1,
    bin_limit = 1,
    seed = NA) {
    # data: x, y, group, fill, PANEL. Values have not been transformed by scale_*_ functions
    # Calculate number of bins
    pad <- 1e-8
    x_range <- scales$y$dimension() + pad
    if (!is.null(bin_width)) {
      # If x variable contains dates, convert x_range and bin_width to numerics
      boundary <- bin_width/2
      shift <- floor((x_range[1L] - boundary)/bin_width)
      origin <- boundary + shift*bin_width
      max_x <- x_range[2L] + (1 - pad)*bin_width
      breaks <- seq.int(from = origin, to = max_x, by = bin_width)
      fuzz <- Median(Diff(breaks))*pad
      breaks <- sort(breaks)
      # If closed == "right"
      fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1L))
      # If closed == "left": fuzzes <- c(rep.int(-fuzz, length(breaks) - 1L), fuzz)
      n_bins <- structure(list(breaks = breaks, fuzzy = breaks + fuzzes, right_closed = TRUE), class = "ggplot2_bins")
    } else {
      # n_bins <- as.integer(n_bins)
      z <- Diff(x_range)
      if (n_bins == 1) {
        boundary <- x_range[1L]
      } else {
        z <- z/(n_bins - 1)
        boundary <- z/2
      }

      # If input consists of dates, need to coerce x_range, z, boundary to numeric
      shift <- floor((x_range[1L] - boundary)/z)
      origin <- boundary + shift*z
      max_x <- x_range[2L] + (1 - pad)*z
      breaks <- seq.int(from = origin, to = max_x, by = z)
      fuzz <- Median(Diff(breaks))*pad
      breaks <- sort(breaks)
      # If closed == "right"
      fuzzes <- c(-fuzz, rep.int(fuzz, length(breaks) - 1L))
      # If closed == "left": fuzzes <- c(rep.int(-fuzz, length(breaks) - 1), fuzz)
      n_bins <- structure(list(breaks = breaks, fuzzy = breaks + fuzzes, right_closed = TRUE), class = "ggplot2_bins")
    }
    data <- ggplot2::ggproto_parent(Stat, self)$compute_panel(data, scales, scaling = scaling, method = method, band_width = band_width, kernel = kernel, n_bins = n_bins$breaks, width = width, adj = adj, bin_limit = bin_limit)

    # Determine method of scaling
    data$sinawidth <- switch(
      scaling,
      # area: original densities scaled to max width of 1 for plotting purposes only
      area = data$density/max(data$density),
      # count: original densities scaled to max width of 1, then scaled according to number of observations
      count = data$density/max(data$density)*data$n/max(data$n),
      # width: constant width (each density scaled to maximum of 1)
      width = data$scaled)
    if (!is.na(seed)) {
      if (exists(".Random.seed", where = globalenv())) {
        random_seed <- .Random.seed
        on.exit(assign(".Random.seed", random_seed, pos = globalenv()), add = TRUE)
      }
      set.seed(seed)
    }
    data$xmin <- data$x - width/2
    data$xmax <- data$x + width/2
    data$x_diff <- stats::runif(Nrow(data), min = -1, max = 1)*width*data$sinawidth/2
    data$width <- width
    if (is_integerish(data$y)) {
      set.seed(seed)
      data$y <- jitter(data$y)
    }
    data
  },
  compute_group = function(
    data,
    scales,
    scaling = TRUE,
    method = "density",
    band_width = "nrd0",
    kernel = "gaussian",
    width = 1,
    adj = 1,
    bin_limit = 1,
    n_bins = NULL) {
    n_rows <- Nrow(data)
    if (n_rows == 0L) return(NULL)
    if (n_rows < 3L) {
      data$density <- 0
      data$scaled <- 1
    } else if (method == "density") {
      range <- range(data$y, na.rm = TRUE)
      band_width <- if (is.character(band_width)) {
        switch(band_width,
               nrd0 = {
                 hi <- SD(data$y)
                 if (!(lo <- min(hi, Diff(Quantile(data$y, probs = c(0.25, 0.75))/1.34)))) {
                   (lo <- hi) || (lo <- abs(data$y[1L])) || (lo <- 1)
                 }
                 0.9*lo*length(data$y)^(-0.2)
               },
               nrd = {
                 r <- Quantile(data$y, c(0.25, 0.75))
                 1.06*min(SD(data$y), (r[2L] - r[1L])/1.34)*length(data$y)^(-0.2)
               },
               ucv = stats::bw.ucv(data$y),
               bcv = stats::bw.bcv(data$y),
               sj = ,
               `sj-ste` = stats::bw.SJ(data$y, method = "ste"),
               `sj-dpi` = stats::bw.SJ(data$y, method = "dpi")
        )
      } else {
        band_width
      }
      w <- data$w
      if (is.null(w)) {
        w <- rep(1/n_rows, n_rows)
      }
      dens <- if (n_rows < 2) {
        vec_to_df(x = NA_real_, density = NA_real_, scaled = NA_real_, ndensity = NA_real_, count = NA_real_, n = NA_integer_)
      } else {
        d <- stats::density(data$y, weights = w, bw = band_width, adjust = adj, kernel = kernel, n = 512, from = range[1L], to = range[2L])
        y_vals <- d$y
        y_scaled <- y_vals/max(y_vals, na.rm = TRUE)
        vec_to_df(
          x = d$x,
          density = y_vals,
          scaled =  y_scaled,
          ndensity = y_scaled,
          count = y_vals*n_rows,
          n = n_rows
        )
      }
      fn_density <- stats::approxfun(dens$x, dens$density, rule = 2)
      data$density <- fn_density(data$y)
      data$scaled <- data$density/max(dens$density)
      data
    } else {
      bin_index <- cut(data$y, n_bins, include.lowest = TRUE, labels = FALSE)
      data$density <- tapply(bin_index, bin_index, length)[as.character(bin_index)]
      data$density[data$density <= bin_limit] <- 0
      data$scaled <- data$density/max(data$density)
    }

    # Calculate width if x has multiple values
    if (n_unique(data$x) > 1L) {
      width <- Diff(range(data$x))*width
    }
    data$width <- width
    data$n <- Nrow(data)
    data$x <- mean(range(data$x))
    data
  },
  finish_layer = function(data, params) {
    z <- (data$xmax - data$xmin)/data$width
    data$x <- data$x + data$x_diff*z
    data
  },
  extra_params = "na.rm"
)

#' stat function for sina plot
#'
#' @param scaling Scaling for shape of sina plot. Options: `"area"` (default), `"count"`, `"width"`
#' @param method Method to determine spread of points along x axis
#' @param width Width of each group along x axis
#' @param adj Adjusts bandwidth by a specified multiplier
#' @param bin_width Width of each bin
#' @param n_bins Number of bins
#' @param seed Value used to set seed for reproducibility
#' @param na.rm If `TRUE` (default), missing values are removed
#' @inheritParams ggplot2::stat_identity
#' @export
stat_sina <- function(
    mapping = NULL,
    data = NULL,
    geom = "sina",
    position = "dodge",
    scaling = "area",
    method = "density",
    width = NULL,
    adj = 1,
    bin_width = NULL,
    n_bins = NULL,
    seed = 1234,
    ...,
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSina,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      scaling = scaling,
      method = method,
      band_width = "nrd0",
      kernel = "gaussian",
      width = width,
      adj = adj,
      bin_limit = 1,
      bin_width = bin_width,
      n_bins = n_bins,
      seed = seed,
      na.rm = na.rm,
      ...
    )
  )
}

#' geom function for sina plot
#'
#' @inheritParams ggplot2::geom_point
#' @export
geom_sina <- function(
    mapping = NULL,
    data = NULL,
    stat = "sina",
    position = "dodge",
    ...,
    na.rm = TRUE,
    show.legend = NA,
    inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
