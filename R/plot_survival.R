#' Kaplan-Meier plot
#'
#' @param df Data frame. May contain missing values
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `"death"`
#' @param predictor_var Variable(s) to use as covariates in model. Enter as comma separated list of quoted variables names wrapped in `c()`
#' @param show_censor If `FALSE` (default), censored events not shown on plot. If `TRUE`, vertical line appears on plot at censored events
#' @param show_error If `FALSE` (default), error estimates of survival probability not displayed on plot. If `TRUE`, error estimate for survival probability displayed
#' @param colors Colors for groups. Enter as quoted color names or quoted hexadecimal codes. Default is blue and red
#' @param line_thickness Line thickness for KM curves. Default is `1`
#' @param linetype Linetype for KM curves. Default is `1` (`"solid"`). Enter as numeric or quoted linetype
#' @param x_axis_title Title for x axis. Enter as quoted text. Default is `"Weeks since onset"`
#' @param y_axis_title Title for y axis. Enter as quoted text. Default is `"Survival probability"`
#' @param plot_title Title of plot. Enter as quoted text. Default is `NULL`
#' @param show_hr If `TRUE` (default), HR annotation displayed on plot. If `FALSE`, not displayed on plot
#' @param hr_prefix Prefix for HR annotation. Default is `"HR "`. Enter as quoted text
#' @param hr_anno_x Location of HR annotation along x axis. Default is `0`. Enter as numeric
#' @param hr_anno_y Location of HR annotation along y axis. Default is `0.1`. Enter as numeric
#' @param show_p If `TRUE` (default), p value displayed on plot. If `FALSE`, p value not displayed on plot
#' @param x_axis_limits Limits for x axis. Enter as numeric vector of length 2. Default from 0 to max time
#' @param y_axis_limits Limits for x axis. Enter as numeric vector of length 2. Default is `c(0, 1)`
#' @param break_time_by Interval for breaks along x axis. Enter as numeric. Default is `1`
#' @param x_axis_breaks Breaks for x axis. Enter as numeric vector
#' @param base_size Size of text passed to theme_custom. Default is `16`
#' @param expand_y Expansion applied to lower end of y axis. Enter as numeric. Default is `0`
#' @param hr_anno_size Size of text for HR annotation. Enter as numeric. Default is `base_size/4.5`
#' @param y_axis_breaks Breaks along y axis. Default `seq.int(0, 1, by = 0.25)`. Enter as numeric vector
#' @param legend Location of legend. Default shows no legend. Enter as quoted position
#' @param y_axis_format Options: `"frequency"` (default, proportion used to create y axis tick labels) or `"percent"` (% used to create y axis tick labels). Enter as quoted text
#' @param cdf If `FALSE` (default) KM curve plotted. If `TRUE`, cumulative distribution function (incidence) plotted
#' @param show_error_labels If `FALSE` (default), shaded error regions are not displayed around curves
#' @param error_colors Colors used for error region around survival curves. Enter as quoted color names or quoted hexadecimal codes
#' @param error_alpha Alpha for shaded error region. Default is `0.2`. Enter as numeric
#' @param show_curve_labels If `TRUE` (default), KM curve labels are displayed at rightmost aspect of plot
#' @param labels Labels for KM curves. Enter as character vector
#' @param show_legend If `FALSE` (default), legend is not shown
#' @param expand_time If `TRUE` (default), output expanded to include all times within `x_axis_limits`
#' @param ... Arguments passed to `theme_custom()`
#' @export
plot_km <- function(
    df,
    predictor_var,
    time_var = "time",
    outcome_var = "death",
    show_censor = FALSE,
    show_error = FALSE,
    colors = c("#0072B5", "#BC3C29", "#999999", "#333333", "#CC79A7", "#56B4E9", "#E69F00", "#20854E", "#FFD500", "#7D5FA7", "#97572B", "#005E7A", "#00A0B0", "#8D9D58", "#DA3978", "#61D04F", "#FBA27D", "#00C5CD"),
    line_thickness = 1,
    linetype = 1,
    x_axis_title = "Weeks since onset",
    y_axis_title = if (y_axis_format == "percent") "Survival (%)" else "Survival",
    show_p = TRUE,
    show_hr = TRUE,
    hr_prefix = "HR ",
    hr_anno_x = 0,
    hr_anno_y = 0.12,
    plot_title = NULL,
    x_axis_limits = NULL,
    y_axis_limits = c(0, 1),
    break_time_by = 1,
    x_axis_breaks = NULL,
    base_size = 16,
    expand_y = 0,
    hr_anno_size = base_size/4.5,
    y_axis_breaks = seq.int(from = 0, to = 1, by = 0.25),
    legend = "none",
    y_axis_format = "percent",
    cdf = FALSE,
    show_curve_labels = TRUE,
    labels = NULL,
    show_error_labels = FALSE,
    error_colors = colors,
    error_alpha = 0.2,
    show_legend = FALSE,
    expand_time = TRUE,
    ...) {
  plot_fn <- "plot_km"

  # y axis
  y_axis_labels <- if (startsWith(y_axis_format, "perc")) {
    round(y_axis_breaks*100, digits = 0)
  } else {
    waiver()
  }

  # x axis
  x_axis_limits <- x_axis_limits %||% Range(x_axis_breaks)
  if (!all(is.finite(x_axis_limits))) {
    x_axis_limits <- c(0, Max(df[[time_var]]))
  }
  x_axis_breaks <- x_axis_breaks %||% pretty(x_axis_limits)

  # Plot data
  df_surv <- tryNULL(create_km_data(df = df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, times = seq.int(from = min(x_axis_breaks), to = max(x_axis_breaks), by = break_time_by), expand_time = expand_time))
  if (is.null(df_surv)) return(NULL)

  # Cumulative distribution function
  if (cdf) {
    df_surv$surv <- df_surv$cumulative_hazard
    if (missing(y_axis_title)) {
      y_axis_title <- "Cumulative incidence"
    }
    if (hr_anno_x == 0) {
      hr_anno_x <- diff(x_axis_limits)*0.55
    }
  }

  # HR/p-value annotation
  if (show_hr) {
    p_lrt <- p_log_rank(df = df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
    p_lrt <- format_p_value(p_lrt, prefix = "", trim_ws = TRUE)
    p_lrt <- paste0("paste(italic(P), '", if (startsWith(p_lrt, "0")) paste0(" = ", p_lrt) else sub("<", " < ", p_lrt, fixed = TRUE), " by log-rank test')")
    hr_annotation <- surv_tidy(Coxph(df = df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var))
    hr_annotation <- paste0(hr_prefix, format_num_range(hr_annotation$hr, hr_annotation$hr_lower, hr_annotation$hr_upper), "\n")
    hr_annotation <- list(ggplot2::annotate("text", x = hr_anno_x, y = hr_anno_y - 0.7*expand_y, label = hr_annotation, size = hr_anno_size, hjust = 0, color = "#000000"), ggplot2::annotate("text", x = hr_anno_x, y = hr_anno_y/1.5 - 0.7*expand_y, label = p_lrt, size = hr_anno_size, hjust = 0, color = "#000000", parse = TRUE))
  } else {
    hr_annotation <- NULL
  }

  # Plot title
  p <- ggplot(df_surv, aes(x = time, y = surv, group = group))

  # Colors
  colors <- rep(colors, length.out = n_unique(df_surv$group, na.rm = FALSE))

  # Error
  if (show_error) {
    error_colors <- rep(error_colors, length.out = n_unique(df_surv$group, na.rm = FALSE))
    df_error <- df_surv[order(df_surv$time), , drop = FALSE]
    df_error <- split(df_error, df_error$group)
    df_error <- do.call(rbind, lapply(df_error, function(z) {
      n <- Nrow(z)
      if (n == 1L) {
        ys <- xs <- 1
      } else {
        idx <- seq_len(n)
        ys <- rep(idx, each = 2)[-2*n]
        xs <- c(1, rep(idx[-1L], each = 2))
      }
      z <- data.frame(
        time = z$time[xs],
        surv_lower = z$surv_lower[ys],
        surv_upper = z$surv_upper[ys],
        z[xs, Setdiff(names(z), c("time", "surv_lower", "surv_upper"))]
      )
      z
    }))
    if (length(error_colors) == 1L) {
      p <- p + geom_ribbon(data = df_error, mapping = aes(ymin = surv_lower, ymax = surv_upper), alpha = error_alpha, fill = error_colors)
    } else {
      p <- p +
        geom_ribbon(data = df_error, mapping = aes(ymin = surv_lower, ymax = surv_upper, fill = group), alpha = error_alpha, show.legend = FALSE) +
        scale_fill_manual(NULL, values = error_colors)
    }
  }

  # KM curve
  p <- p +
    geom_step(aes(group = group, color = group), show.legend = show_legend, linewidth = line_thickness, linetype = linetype) +
    scale_color_manual(values = colors) +
    hr_annotation +
    scale_y_continuous(name = y_axis_title, limits = y_axis_limits, breaks = y_axis_breaks, labels = y_axis_labels, expand = c(expand_y, 0, 0, 0)) +
    scale_x_continuous(name = x_axis_title, limits = range(x_axis_limits, x_axis_breaks), breaks = x_axis_breaks) +
    ggtitle(plot_title) +
    coord_cartesian(clip = "off") +
    theme_custom(base_size = base_size, ...)
  if (show_curve_labels) {
    p <- suppressWarnings(add_km_labels(p, labels = labels, font_size = base_size))
    p$data <- df_surv
  }
  suppressWarnings(p)
}

#' Label for Kaplan-Meier plots
#'
#' @param x ggplot2 object containing Kaplan-Meier plot
#' @param labels Character vector of labels for each curve
#' @param font_size Size of text used to generated labels for KM curves. Unit in pts. Default uses `base_size` from plot theme
#' @noRd
add_km_labels <- function(x, labels = NULL, font_size = NULL) {
  # x$data must be a data frame with columns "surv", "time", "group"
  df <- x$data
  levels(df$group) <- labels %||% attr(df$group, "levels")
  plot_build <- ggplot2::ggplot_build(x)
  plot_summary <- ggplot2::summarise_layout(plot_build)
  y_axis_limits <- c(plot_summary$ymin, plot_summary$ymax)
  y_min <- abs(Min(y_axis_limits))
  if (y_min > 0) {
    df$surv <- df$surv*(1 + y_min)
  }
  df <- dplyr::group_by(df, group)
  df <- dplyr::filter(df, time == Max(time))
  df <- dplyr::ungroup(df)
  p <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(limits = NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = y_axis_limits, expand = c(0, 0)) +
    theme_clean()
  p <- p +
    ggplot2::geom_text(data = df, mapping = ggplot2::aes(y = surv, label = paste0(group)), x = 0, hjust = 0, vjust = 0.5, size = font_size %||% x$theme$text$size, size.unit = "pt")
  z <- as_grob(p)
  p <- z$grobs[grepl("panel", z$layout$name, fixed = TRUE)]
  p <- p[[1L]]
  p <- as_gtable(p)
  gt <- as_grob(x)
  pp <- gt$layout[gt$layout$name == "panel", ]
  g <- gtable::gtable_add_cols(gt, grid::unit(0.2, "null"), pp$r)
  g <- gtable::gtable_add_grob(g, p, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "yaxis-grob-r")
  ggplot() +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    ggplot2::scale_x_continuous(name = NULL) +
    ggplot2::scale_y_continuous(name = NULL) +
    theme_clean() +
    draw_grob(as_grob(g), x = 0, y = 0, width = 1, height = 1, scale = 1, hjust = 0, vjust = 0, halign = 0.5, valign = 0.5)
}

#' Add error region to line plot or Kaplan-Meier curve
#'
#' @param x ggplot object
#' @param mapping Enter using `aes()`. `ymin` and `ymax` must be entered. Default is `ymin = ymin` and `ymax = ymax`
#' @param colors Color for error region. Default is `"grey70"`. If > length is 1, `x$data` must contain a column names `group`
#' @param alpha Transparency setting. Enter as numeric 0-1. Default is `0.4`
#' @param ... Arguments passed to `geom_ribbon`
#' @returns ggplot object
#' @export
add_line_error <- function(x, mapping = ggplot2::aes(ymin = ymin, ymax = ymax), colors = "#B3B3B3", alpha = 0.4, ...) {
  if (length(colors) == 1L || ((n1 <- length(x$data$group)) < 2L && (n2 <- length(mapping$group)))) {
    x + ggplot2::geom_ribbon(mapping = mapping, alpha = alpha, fill = colors, ...)
  } else {
    colors <- rep(colors, length.out = max(n1, n2))
    x +
      ggplot2::geom_ribbon(mapping = mapping, alpha = alpha, ...) +
      scale_fill_new() +
      ggplot2::scale_fill_manual(NULL, values = colors)
  }
}

#' Generate Kaplan-Meier estimate data for each group
#'
#' Functionality from print and summary methods for `survfit` object
#' @param df Data frame. May contain missing values
#' @param predictor_var Variable(s) to use as covariates in model. Enter as comma separated list of quoted variables names wrapped in `c()`
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as `1` (event) or `0` (no event/censored). Enter as quoted variable name. Default is `"death"`
#' @param times Time points to be included in output data. Default uses 0 to max time by 1
#' @param expand_time If `TRUE` (default), output expanded to include all times specified by `times` argument
#' @returns Data frame used for plotting with columns for group, time, n_risk, n_event, n_censor, surv, surv_lower, surv_upper, surv_se, cumulative_hazard, cumulative_hazard_sd
#' @noRd
create_km_data <- function(
    df,
    predictor_var,
    time_var = "time",
    outcome_var = "death",
    times = seq.int(from = 0, to = ceiling(max(df[[time_var]])), by = 1L),
    expand_time = TRUE) {
  fit <- Survfit(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
  newstrat <- fit$strata
  newstrat_length <- length(newstrat)
  insert <- 1 + cumsum(c(0, newstrat[-newstrat_length]))
  names(insert) <- NULL
  same <- fit$time[insert] == 0
  insert <- insert[!same]
  insert_length <- length(insert)
  insert_idx <- seq_len(insert_length)
  i2 <- insert + insert_idx - 1

  newstrat_idx <- seq_len(newstrat_length)
  newstrat[!same] <- newstrat[!same] + 1
  addto <- function(.x, .z) {
    x_length <- length(.x)
    x_insert_idx <- x_length + insert_length
    x_idx <- seq.int(from = 1, to = x_insert_idx)[-i2]
    x_new <- rep(.x[1L], x_insert_idx)
    x_new[x_idx] <- .x
    x_new[i2] <- .z
    x_new
  }
  fit_names <- names(fit)
  fit_new <- vector("list", length(fit_names))
  names(fit_new) <- fit_names
  for (i in fit_names) {
    if (i %in% c("time", "n.event", "n.censor", "cumhaz", "std.err", "std.chaz")) {
      fit_new[[i]] <- addto(.subset2(fit, i), 0L)
    } else if (i == "n.risk") {
      fit_new[[i]] <- addto(.subset2(fit, i), fit$n.risk[insert])
    } else if (i == "strata") {
      fit_new[[i]] <- newstrat
    } else if (i %in% c("surv", "lower", "upper")) {
      fit_new[[i]] <- addto(.subset2(fit, i), 1L)
    } else {
      fit_new[[i]] <- .subset2(fit, i)
    }
  }
  class(fit_new) <- "survfit"
  findrow <- function(.fit) {
    ptimes <- if (expand_time) times else times[times <= max(.fit$time)]
    # TODO: add checkSorted = FALSE in findInterval once available in new version of R
    index1 <- findInterval(ptimes, .fit$time)
    z <- pmax(1, index1)
    # TODO: add checkSorted = FALSE in findInterval once available in new version of R
    index2 <- 1 + findInterval(ptimes, .fit$time, left.open = TRUE)
    .fit$time <- ptimes
    for (i in c("surv", "upper", "lower", "std.err", "cumhaz", "std.chaz")) {
      .fit[[i]] <- .subset(.subset2(.fit, i), z)
    }
    .fit$n.risk <- c(.fit$n.risk, 0)[index2]
    z <- index1 + 1
    for (i in c("n.event", "n.censor")) {
      .fit[[i]] <- diff(c(0, c(0, cumsum(.subset2(.fit, i)))[z]))
    }
    .fit
  }
  ltemp <- vector("list", newstrat_length)
  for (i in newstrat_idx) {
    ltemp[[i]] <- findrow(fit_new[i])
  }
  for (i in c("time", "surv", "upper", "lower", "std.err", "cumhaz", "n.risk", "n.event", "n.censor", "std.chaz")) {
    fit_new[[i]] <- unlist(lapply(ltemp, function(x) x[[i]]), use.names = FALSE)
  }
  fit_new$strata[] <- vapply(ltemp, function(x) length(x$time), integer(1))
  fit_new$std.err <- fit_new$std.err*fit_new$surv
  fit_new$strata <- factor(rep(newstrat_idx, fit_new$strata), newstrat_idx, labels = sub(paste0(predictor_var, "="), "", names(fit_new$strata)))
  fit_new <- unclass(fit_new)
  vec_to_df(
    group = fit_new$strata,
    time = fit_new$time,
    n_risk = fit_new$n.risk,
    n_event = fit_new$n.event,
    n_censor = fit_new$n.censor,
    surv = fit_new$surv,
    surv_lower = fit_new$lower,
    surv_upper = fit_new$upper,
    surv_se = fit_new$std.err,
    cumulative_hazard = fit_new$cumhaz,
    cumulative_hazard_sd = fit_new$std.chaz
  )
}
