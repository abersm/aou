# Stat --------------------------------------------------------------------

#' ggproto for Stat to create significance annotation
#'
#' @export
StatSig <- ggplot2::ggproto(
  "StatSig",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(hjust = 0.5, vjust = 0.5),
  setup_params = function(data, params) {
    # data: data frame with columns x (mapped discrete), y, PANEL (factor with integer levels). May have group (integer)
    # params: list containing inputs to stat_compare_means
    # Determine type of grouping. 2 options:
    ## 1. "Ungrouped" comparisons. 1 group per x axis tick. Comparisons are BETWEEN each x value
    ## 2. "Nested/grouped" comparisons. > 1 group per x axis tick (i.e., grouped). Comparisons are WITHIN each x value
    # Need to handle case in which faceting variable and grouping variable are identical (i.e., 1 group per facet)
    # Also need to determine default comparison function

    # Need to determine n_groups here to maintain consistent dodging across facets
    params$n_groups <- n_groups <- length(unique(.subset2(data, "group")))
    df_by_panel <- split_df(data, "PANEL")
    can_nest <- n_groups > 1L && !all(vapply(df_by_panel, function(x) all(x$x == x$group), logical(1), USE.NAMES = FALSE))
    if (is.null(params$nested)) {
      params$nested <- can_nest
    } else if (params$nested && !can_nest) {
      return(NULL)
    }

    # If comparison function not specified, need to determine max number of groups will be compared
    if (is.null(params$comparison_fn)) {
      if (params$nested) {
        n_groups <- max(vapply(df_by_panel, function(x) length(unique(x$x)), integer(1), USE.NAMES = FALSE))
      } else {
        n_groups <- max(vapply(split_df(data, c("PANEL", "x")), function(x) length(unique(x$group)), integer(1), USE.NAMES = FALSE))
      }
      if (n_groups > 2L) {
        params$comparison_fn <- p_sig_dunn
        if (is.null(params$p_adj_method)) {
          params$p_adj_method <- "BH"
        }
      } else {
        params$comparison_fn <- p_sig_2_groups
      }
    }
    params
  },
  setup_data = function(data, params) {
    remove_na(data, "y")
  },
  compute_panel = function(
    data,
    scales,
    paired = FALSE,
    nested = NULL,
    comparison_fn = NULL,
    y_max_fn = Max,
    sig_anno_fn = sig_stars,
    p_adj_method = NULL,
    p_sig_threshold = 0.05,
    n_groups = NULL,
    show_ns = FALSE) {
    # data: data frame with columns x (mapped discrete), y, group (integer), PANEL (factor with integer levels)
    # scales: list with components x and y, each a Scale object
    args <- list(
      .df = data,
      .continuous_var = "y",
      .grouping_var = "x",
      .comparison_fn = comparison_fn,
      .p_adj_method = p_adj_method,
      .p_sig_threshold = p_sig_threshold,
      .show_ns = show_ns,
      .anno_fn = sig_anno_fn,
      .y_max_fn = y_max_fn,
      .paired = paired
    )
    if (nested) {
      args$.grouping_var <- "group"
      out <- lapply(split_df(data, "x"), function(x) {
        args$.df <- x
        out <- do.call(create_sig_anno_df, args)
        if (is.null(out)) return(NULL)
        #if (is.null(out) || Nrow(out) == 0L) return(NULL)
        out$x <- .subset(.subset2(x, "x"), 1L)
        out
      })
      out <- remove_null(out)
      if (length(out) == 0L) return(NULL)
      out <- do.call(rbind.data.frame, out)
      out$n_groups <- n_groups
    } else {
      out <- do.call(create_sig_anno_df, args)
    }
    out$PANEL <- data$PANEL[1L]
    out$xmin <- as.double(out$xmin)
    #class(out$xmin) <- c("mapped_discrete", "numeric")
    out$xmax <- as.double(out$xmax)
    #class(out$xmin) <- c("mapped_discrete", "numeric")
    out
  }
)

#' geom function to generate significance bars
#'
#' Enter as `plot + stat_sig()`
#' @inheritParams ggplot2::stat_summary
#' @export
stat_sig <- function(
    mapping = NULL,
    data = NULL,
    paired = FALSE,
    nested = NULL,
    comparison_fn = NULL,
    y_max_fn = Max,
    p_adj_method = "BH",
    show_ns = FALSE,
    sig_anno_fn = sig_stars,
    p_sig_threshold = 0.05,
    geom = "sig",
    position = "identity",
    inherit.aes = TRUE,
    ...) {
  ggplot2::layer(
    stat = StatSig,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(
      paired = paired,
      nested = nested,
      comparison_fn = comparison_fn,
      y_max_fn = y_max_fn,
      p_adj_method = p_adj_method,
      p_sig_threshold = p_sig_threshold,
      sig_anno_fn = sig_anno_fn,
      show_ns = show_ns,
      ...
    )
  )
}

# Geom --------------------------------------------------------------------

#' ggproto for Geom to create significance annotation
#'
#' @export
GeomSig <- ggplot2::ggproto(
  "GeomSig",
  ggplot2::Geom,
  required_aes = c("xmin", "xmax", "y", "label"),
  non_missing_aes = c("linetype", "linewidth"),
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.6,
    linetype = 1,
    star_size = 20,
    text_size = 12,
    vjust = 0.5,
    hjust = 0.5,
    bar_nudge = 0.06,
    step_increase = 0.11,
    star_nudge = 0.02,
    text_nudge = 0.05,
    dodge = 0.7,
    parse = FALSE),
  setup_params = function(data, params) {
    # data: data frame with columns xmin (numeric), xmax (numeric), y, label, p, PANEL (factor with integer levels for each facet). If nested, x (numeric)
    # params: list of inputs to geom function
    params$nested <- !is.null(.subset2(data, "x"))
    params
  },
  setup_data = function(data, params) {
    # data: data frame with columns xmin (numeric), xmax (numeric), y, label, p, PANEL (factor with integer levels for each facet). If nested, x (numeric)
    # params: list of inputs to geom function
    if (params$nested) {
      data$xmin <- .dodge_x_position(x = data$xmin, n_groups = data$n_groups, dodge = params$dodge) + data$x
      data$xmax <- .dodge_x_position(x = data$xmax, n_groups = data$n_groups, dodge = params$dodge) + data$x
    }
    data
  },
  draw_key = function(...) grid::nullGrob(),
  draw_panel = function(data, panel_params, coord, nested = FALSE, na.rm = TRUE) {
    # data: data frame with columns PANEL (factor with integer levels), x, y, xmin, xmax, label, p, colour, linewidth, linetype, bar_nudge, star_nudge, text_nudge, step_increase, star_size, text_size, vjust, hjust, parse
    #if (is.null(data) || nrow(data) == 0L || ncol(data) == 0L || is_waiver(data)|| (nrow(data) == 1L && anyNA(c(data$x, data$xend, data$y, data$label)))) {
    if (is.null(data) || nrow(data) == 0L || ncol(data) == 0L || inherits(data, "waiver")) {
      return(ggplot2::zeroGrob())
    }
    # Convert from raw data units to numeric 0-1
    coord <- coord$transform(data, panel_params)
    coord$y <- coord$y + coord$bar_nudge
    if (nested) {
      #coord$xmin <- .dodge_x_position(x = coord$xmin, n_groups = coord$n_groups, dodge = coord$dodge) + coord$x
      #coord$xmax <- .dodge_x_position(x = coord$xmax, n_groups = coord$n_groups, dodge = coord$dodge) + coord$x
      coord <- lapply(split_df(coord, "x"), function(x) {
        vertically_dodge_overlap(x, y = "y", xmin = "xmin", xmax = "xmax", step_increase = "step_increase")
      })
      #coord <- do.call(rbind.data.frame, coord)
      coord <- dplyr::bind_rows(coord)
    } else {
      coord <- vertically_dodge_overlap(coord, y = "y", xmin = "xmin", xmax = "xmax", step_increase = "step_increase")
    }

    idx_sig <- grepl("*", coord$label, fixed = TRUE)
    y_label <- coord$y + ifelse(idx_sig, coord$star_nudge, coord$text_nudge)
    label_size <- ifelse(idx_sig, coord$star_size, coord$text_size)
    labels <- if (coord$parse[1L]) .parse_safe(coord$label) else coord$label
    color <- coord$colour %||% coord$color
    grid::gList(
      grid::textGrob(
        label = labels,
        x =  (coord$xmin + coord$xmax)/2,
        y = y_label,
        default.units = "native",
        gp = grid::gpar(col = color, fontsize = label_size)),
      grid::segmentsGrob(
        x0 = coord$xmin,
        x1 = coord$xmax,
        y0 = coord$y,
        y1 = coord$y,
        default.units = "native",
        gp = grid::gpar(
          col = color,
          fill = color,
          lwd = coord$linewidth*.pt,
          lty = coord$linetype,
          lineend = "square",
          linejoin = "mitre"
        ),
        arrow = NULL
      )
    )
  }
)

#' geom function to generate significance bars
#'
#' Enter as `plot + geom_sig() + facet_wrap(vars(x_numeric))`. x_numeric must be present in plot$data and `data`
#' @inheritParams ggplot2::geom_segment
#' @param linewidth Note that units are equivalent to 0.75 mm
#' @export
geom_sig <- function(
    #data = NULL,
    mapping = NULL,
    data = NULL,
    stat = "sig",
    position = "identity",
    paired = FALSE,
    nested = NULL,
    comparison_fn = NULL,
    y_max_fn = Max,
    sig_anno_fn = sig_stars,
    p_adj_method = "BH",
    show_ns = FALSE,
    star_size = 20,
    text_size = 12,
    bar_nudge = 0.06,
    step_increase = bar_nudge + 0.06,
    star_nudge = 0.02,
    text_nudge = 0.05,
    dodge = 0.7,
    vjust = 0.5,
    hjust = 0.5,
    linewidth = 0.6,
    color = "black",
    ...,
    show.legend = NA,
    inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSig,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      paired = paired,
      nested = nested,
      comparison_fn = comparison_fn,
      y_max_fn = y_max_fn,
      sig_anno_fn = sig_anno_fn,
      p_adj_method = p_adj_method,
      star_size = star_size,
      text_size = text_size,
      bar_nudge = bar_nudge,
      step_increase = step_increase,
      star_nudge = star_nudge,
      text_nudge = text_nudge,
      dodge = dodge,
      vjust = vjust,
      hjust = hjust,
      show_ns = show_ns,
      ...
    )
  )
}

# Helpers -----------------------------------------------------------------

#' Create data frame with significance annotations
#'
#' @param .df Data frame used internally by ggplot2
#' @param .continuous_var Column name of continuous variable in `.df`
#' @param .grouping_var Column name of variable in `.df` containing group membership for comparisons of `.continuous_var`
#' @param .comparison_fn Function used to compare groups. Must take a numeric vector as first argument, vector of group membership for corresponding values as 2nd input and an argument named `.paired`
#' @param .paired If `FALSE` (default), unpaired testing is performed
#' @param .p_adj_method Method to adjust P values. Options: `NULL` (default), `"none"` (same as `NULL`), `"BH"`, `"holm"`, `"hochberg"`, `"hommell"`, `"bonferroni"`, `"BY"`, `"fdr"`, `"none"`
#' @param .y_max_fn Function to be applied to `.df[[.continuous_var]]` to determine y axis position of significance annotation
#' @param .anno_fn Function to generate text for significance annotation. Must accept numeric vector of P values as input
#' @param .show_ns If `FALSE` (default), no annotation for `P >= .p_sig_threshold`. If `TRUE`, non-significant comparisons displayed
#' @param .p_sig_threshold Threshold P value below which comparisons will be considered statistically significant
#' @returns Data frame. Used for `geom_sig`
#' @param ... Not used
#' @noRd
create_sig_anno_df <- function(
    .df,
    .continuous_var,
    .grouping_var,
    .comparison_fn = NULL,
    .paired = FALSE,
    .p_adj_method = NULL,
    .p_sig_threshold = 0.05,
    .show_ns = FALSE,
    .anno_fn = sig_stars,
    .y_max_fn = Max) {
  y <- .subset2(.df, .continuous_var)
  groups <- .subset2(.df, .grouping_var)
  groups <- as.character(groups)
  if (length(unique(groups)) < 2L) return(NULL)
  groups <- as_fct(groups)
  df_pvalues <- .comparison_fn(y, groups, .fn_y_max = .y_max_fn, .paired = .paired)
  if (Nrow(df_pvalues) == 0L) return(NULL)
  if (!is.null(.p_adj_method)) {
    df_pvalues$p <- p_adjust(df_pvalues$p, method = .p_adj_method)
  }
  if (!.show_ns) {
    df_pvalues <- df_pvalues[df_pvalues$p < .p_sig_threshold, , drop = FALSE]
  }
  if (Nrow(df_pvalues) == 0L) return(NULL)
  df_pvalues$label <- .anno_fn(df_pvalues$p)

  # Need to adjust y position to account for comparisons that horizontally spans groups other than those being compared (want to position significance annotation above any raw data, so need to take into account local ymax for those groups). Must do here because calculation requires all data (not just statistically significant annotations. Thus, can't do in geom)
  group_max <- vapply(split(y, groups), function(x) tryCatch(.y_max_fn(x), error = function(e) NA_real_), numeric(1), USE.NAMES = TRUE)
  group_levels <- levels(groups)
  xmin_idx <- match(df_pvalues$xmin, group_levels)
  xmax_idx <- match(df_pvalues$xmax, group_levels)
  df_pvalues$y <- vapply(seq_along(xmin_idx), function(i) {
    max(group_max[seq(xmin_idx[i], xmax_idx[i])], na.rm = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
  df_pvalues
}

#' Calculate P values for `geom_sig` - 2 groups
#'
#' @param .y Numeric vector of continuous values
#' @param .groups Vector of group membership for each valuer in `.y`
#' @param .comparison_fn Function used to compare 2 groups. Options: `.p_ttest`, `.p_mann_whitney.` Must take 2 numeric vectors as input and return length 1 numeric (P value). Options: `.p_by_normality` (default), `.p_mann_whitney`, `.p_ttest`
#' @param .paired If `FALSE` (default), unpaired test is performed
#' @param .y_max_fn Function used to determine maximum y value for each comparison
#' @param ... Not used
#' @returns Data frame containing columns xmin, xmax, y, p (unadjusted). Used for `geom_sig`
#' @noRd
p_sig_2_groups <- function(
    .y,
    .groups,
    .y_max_fn = Max,
    .comparison_fn = .p_by_normality,
    .paired = FALSE,
    ...) {
  group_vals <- split(.y, .groups)
  #group_max <- vapply(group_vals, function(x) tryCatch(.y_max_fn(x), error = function(e) NA_real_), numeric(1), USE.NAMES = TRUE)
  out <- lapply(combos_list(names(group_vals), na.rm = FALSE), function(x) {
    z <- group_vals[x]
    list(
      xmin = x[1L],
      xmax = x[2L],
      #y = max(group_max[x], na.rm = TRUE),
      p = .comparison_fn(.subset2(z, 1L), .subset2(z, 2L), paired = .paired)
    )
  })
  do.call(rbind.data.frame, out)
}

#' Calculate P values for `geom_sig` - 3 or more groups
#'
#' @inheritParams p_sig_2_groups
#' @returns Data frame containing columns xmin, xmax, y, p (unadjusted). Used for `geom_sig`
#' @noRd
p_sig_dunn <- function(
    .y,
    .groups,
    .y_max_fn = Max,
    ...) {
  group_vals <- split(.y, .groups)
  ranks <- rank(.y)
  ranks_sorted <- sort.int(ranks)
  idx <- 1
  tie_sum <- 0
  n <- length(.y)
  while (idx <= n) {
    n_ties <- length(ranks_sorted[ranks_sorted == ranks_sorted[idx]])
    idx <- idx + n_ties
    if (n_ties > 1L) {
      tie_sum <- tie_sum + n_ties*n_ties*n_ties - n_ties
    }
  }
  inv_group_n <- n*(n + 1)/12 - tie_sum/(12*(n - 1))
  inv_group_n <- inv_group_n/lengths(group_vals)
  #group_max <- vapply(group_vals, function(x) tryCatch(.y_max_fn(x), error = function(e) NA_real_), numeric(1), USE.NAMES = TRUE)
  group_rank <- vapply(split(ranks, .groups), Mean, numeric(1), USE.NAMES = TRUE)
  out <- lapply(combos_list(names(group_vals), na.rm = FALSE), function(x) {
    xmin <- x[1L]
    xmax <- x[2L]
    z <- abs(group_rank[xmin] - group_rank[xmax])/sqrt(inv_group_n[xmin] + inv_group_n[xmax])
    list(
      xmin = xmin,
      xmax = xmax,
      #y = max(group_max[x], na.rm = TRUE),
      p = tryCatch(2*stats::pnorm(z, lower.tail = FALSE), error = function(e) NA_real_)
    )
  })
  do.call(rbind.data.frame, out)
}

#' Avoid overlapping significance annotations
#'
#' @param df Data frame
#' @param xmin,xmax Column names for x min and x max of significance bar, respectively
#' @param y,step_increase Column name for y value of significance bar and step increase in y values, respectively
#' @param min_y_delta Minimum tolerable distance along y axis between adjacent significance annotations (in npc units) below which vertical dodge will be applied. Enter as length 1 numeric 0-1. Default is `0.01`
#' @returns Data frame. Used for `geom_sig`
#' @noRd
vertically_dodge_overlap <- function(
    df,
    y = "y",
    xmin = "xmin",
    xmax = "xmax",
    step_increase = "step_increase",
    min_y_delta = 0.01) {
  # Vertically dodge significance bars that have same y value (same as .avoid_overlap)
  n_comparisons <- Nrow(df)
  if (n_comparisons < 2L || n_comparisons == length(unique(df[[y]]))) {
    return(df)
  }
  xmin_vals <- .subset2(df, xmin)
  xmax_vals <- .subset2(df, xmax)
  df <- df[order(df[[y]], xmax_vals - xmin_vals, -xmin_vals, -xmax_vals), , drop = FALSE]
  df <- lapply(split_df(df, y), function(x) {
    x$n <- seq_len(Nrow(x)) - 1L
    x
  })
  df <- do.call(rbind.data.frame, df)
  #df[[y]] <- df[[y]] + plot_delta*df[[step_increase]]*df$n
  df[[y]] <- df[[y]] + df[[step_increase]]*df$n
  df
}
