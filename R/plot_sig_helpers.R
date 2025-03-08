#' Plot significance for ungrouped data
#'
#' @param df Data frame used internally by ggplot2
#' @param y Name of continuous variable in `df`. Default is `"y"`. Enter as quoted variable name
#' @param x Name of x axis variable in `df`. Default is "x_numeric". Enter as quoted variable name
#' @param comparison_fn Function used to compare groups. Must take data frame (1st argument), formula (2nd argument), grouping variable (3rd argument), ns_symbol (4th argument). Must return a data frame containing columns `"p"`, `"label"`, grouping variable indicated by x, `"max_1"`, `"max_2"`, `"sd_1"`, `"sd_2"`, `"se_1"`, `"se_2"`, `"mean_1"`, `"mean_2"`
#' @param test_2_groups Function used to compare groups. If `NULL` (default), Dunn's test vs. 2 group testing is selected based on the number of groups. Other options: `.sig_compare_dunn`, `.sig_compare_2_groups`
#' @param normality_test Normality test. Default is `p_shapiro`
#' @param variance_test Test used to compare variance between groups. Default is `.p_F_test`
#' @param welch If `NULL` (default), variance_test used to determine whether t-test or Welch's correction is applied to t-test. If `TRUE`, Welch's correction applied to t-test
#' @param paired If `FALSE` (default), unpaired testing is performed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param p_adj_method Method to adjust P values. Default is `"BH"`
#' @param y_max_type Type of data plotted. Used to determine the location of significance annotation along y axis. If points are plotted, use `"raw"`. If bars with error bars used, select "error". Options: `"raw"` (default), `"error"`,` "mean"`. Enter as quoted type.
#' @param summary_fn Function used to generated summary estimate for y. Default is `Mean`
#' @param error_fn Function used to determine error estimate for y. Default is `SE`
#' @param stars If `TRUE` (default), significance stars are used instead of actual P values
#' @param show_ns If `FALSE` (default), no annotation for P >= 0.05. If `TRUE`, non-significant comparisons displayed
#' @param ns_symbol Symbol to use when P >= 0.05. Only used if `show_nn = TRUE.` Default is `"ns"`
#' @param p_case Case of P used in significance annotations when sig_annotation is set to "values". Options: `"upper"` (default) or `"lower"`. Enter as quoted text
#' @param p_spaces If `TRUE` (default), spaces placed between p, equality symbol, and p-value in significance annotation. If `FALSE`, no spaces placed between p, equality symbol, and p-value in significance annotation. Only relevant when sig_annotation is set to "values"
#' @param y_scale Scaling for y axis. Options: `"regular"` (default), `"scientific"`, `"log10"`, `"log2"`
#' @param breaks_fn Function used to generate y axis breaks. Passed to `.create_axis_breaks` when y_scale is `"regular"` or `"scientific"`. Default is `pretty`
#' @param n_breaks Number of breaks for y axis. Passed to `.create_axis_breaks` when y_scale is `"regular"` or `"scientific"`. Default is `4`
#' @param bar_nudge Nudge factor along y axis for significance bars. Distance between point or bar and significance bar is bar_nudge*(max y value on plot). Default is `0.1`
#' @param star_nudge Nudge factor along y axis for significance stars. Distance between point or bar and significance stars is star_nudge*(max y value on plot). Default is `0.07`
#' @param text_nudge Nudge factor along y axis for significance annotation text. Distance between point or bar and significance annotation text is text_nudge*(max y value on plot). Default is `0.13`
#' @param text_size Font size for P values in pts. Only relevant when stars is `FALSE = "values"`
#' @param star_size Size of significance stars in pts. Only relevant when stars is `TRUE`
#' @param bar_thickness Thickness of significance bars. Default is `0.6`
#' @param bar_color Color of significance bars. Enter as quoted color name or quoted hexadecimal code. Default is `"black"`
#' @param font_color Color of significance annotation text and stars. Enter as quoted color name or quoted hexadecimal code. Default is `"black"`
#' @param p_adj_method Method for p value adjustment. Options: `"BH"` (default), `"holm"`, `"hochberg"`, `"hommell"`, `"bonferroni"`, `"BY"`, `"fdr"`, `"none"`. Enter as quoted method
#' @param step_increase Amount of vertical space to add to overlapping significance annotations. Enter as proportion of y axis to add between adjacent annotations. Default is `0.11`
#' @param ... Not used
#' @noRd
.plot_sig_anno <- function(
    df,
    y = "y",
    x = "x_numeric",
    comparison_fn = NULL,
    test_2_groups = .p_by_normality,
    normality_test = p_shapiro,
    variance_test = .p_F_test,
    welch = NULL,
    paired = FALSE,
    hypothesis_type = "two.sided",
    p_adj_method = "BH",
    y_max_type = "raw",
    summary_fn = mean,
    error_fn = SE,
    stars = TRUE,
    show_ns = FALSE,
    ns_symbol = "ns",
    p_case = "upper",
    p_spaces = TRUE,
    y_scale = "regular",
    bar_nudge = 0.1,
    star_nudge = 0.07,
    text_nudge = 0.13,
    star_size = 22,
    text_size = 12,
    bar_thickness = 0.6,
    bar_color = "black",
    font_color = "black",
    step_increase = 0.11,
    breaks_fn = pretty,
    n_breaks = 4,
    ...) {
  p_adj_method <- if (nchar(p_adj_method) > 2) tolower(p_adj_method) else toupper(p_adj_method)
  if (is.null(comparison_fn)) {
    comparison_fn <- if (n_unique(df[[x]]) > 2L) .sig_compare_dunn else .sig_compare_2_groups
  }
  y_max_type <- match.arg(y_max_type, choices = c("raw", "error", "summary"))
  fn_y_max <- switch(y_max_type,
                     raw = max,
                     error = function(x) summary_fn(x) + error_fn(x),
                     summary = match.fun(summary_fn))

  # Ensure x is numeric
  x_vals <- .subset2(df, x)
  #if (!is.null(x_vals) && !is.numeric(x_vals)) {
  if (!is.numeric(x_vals)) {
    if (!inherits(x_vals, "factor")) {
      df[[x]] <- factor(x_vals, levels = create_levels(x_vals))
    }
    df[[x]] <- match(df[[x]], attr(df[[x]], "levels"))
  }

  # P values
  df_pvalues <- comparison_fn(.df = df, .continuous_var = y, .group = x, .test_fn = test_2_groups, .normality_test = normality_test, .variance_test = variance_test, .welch = welch, .paired = paired, .hypothesis_type = hypothesis_type, .p_adj_method = p_adj_method, .fn_y_max = fn_y_max)

  # Significance labels and x coordinates for annotations
  if (Nrow(df_pvalues) == 0L) return(NULL)
  df_pvalues$label <- sig_stars(df_pvalues$p, symbols = c("****", "***", "**", "*",  ns_symbol))
  df_pvalues$xmin <- as.numeric(df_pvalues$Group1)
  df_pvalues$xend <- as.numeric(df_pvalues$Group2)
  df_pvalues$x_label <- (df_pvalues$xend + df_pvalues$xmin)/2

  # Minimum y value for each comparison (pair of x values)
  y_values <- split.default(.subset2(df, y), .subset2(df, x))
  x_values <- as.numeric(names(y_values))
  y_values <- vapply(y_values, fn_y_max, numeric(1), USE.NAMES = FALSE)

  # For every row (comparison), determine which x values are between xmin and xend
  df_pvalues$y <- apply(df_pvalues[c("xmin", "xend")], 1, function(j) {
    max(y_values[x_values >= min(j) & x_values <= max(j)])
  })

  # Hide comparisons with P >= 0.05 if show_ns = FALSE
  if (!show_ns) {
    df_pvalues <- df_pvalues[df_pvalues$p < 0.05, ]
  }
  if (Nrow(df_pvalues) == 0L) return(NULL)

  # Significance annotation if text is used
  if (!stars) {
    df_pvalues$label <- format_p_value(df_pvalues$p, trim_ws = !p_spaces, prefix = if (p_case == "upper") "P" else "p")
  }

  # Change column names for plotting
  names(df_pvalues)[names(df_pvalues) == "x"] <- "x_old"
  names(df_pvalues)[names(df_pvalues) == "xmin"] <- "x"

  # y coordinates for significance bar and annotation
  if (startsWith(y_scale, "log")) {
    df_pvalues$y_sig_bar <- df_pvalues$y*(1 + bar_nudge)
    df_pvalues$y_sig_label <- df_pvalues$y*(1 + ifelse(grepl("*", df_pvalues$label, fixed = TRUE), star_nudge + 1, text_nudge + 1))
    df_pvalues <- .avoid_overlap(.df = df_pvalues, .step = step_increase, .scale = "log")
  } else {
    min_y_plot <- min(df[[y]], na.rm = TRUE)
    max_y_plot <- max(df_pvalues$y, na.rm = TRUE)
    delta_y <- diff(range(.create_axis_breaks(.limits = range(min_y_plot, max_y_plot), .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)))
    df_pvalues$y_sig_bar <- df_pvalues$y + delta_y*bar_nudge
    df_pvalues$y_sig_label <- df_pvalues$y + ifelse(grepl("*", df_pvalues$label, fixed = TRUE), delta_y*star_nudge, delta_y*text_nudge)
    df_pvalues <- .avoid_overlap(.df = df_pvalues, .step = step_increase, .scale = "regular", .delta = delta_y)
  }

  # Significance bars
  sig_bars_plot <- ggplot2::geom_segment(data = df_pvalues, ggplot2::aes(x = x, xend = xend, y = y_sig_bar, yend = y_sig_bar), linewidth = bar_thickness, color = bar_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)

  # Significance stars
  if (stars) {
    df_stars <- df_pvalues[grepl("*", df_pvalues$label, fixed = TRUE), ]
    if (Nrow(df_stars) != 0) {
      sig_stars_plot <- ggplot2::geom_text(data = df_stars, ggplot2::aes(x = x_label, y = y_sig_label, label = label), size = star_size, size.unit = "pt", vjust = 0, hjust = 0.5, color = font_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)
    } else {
      sig_stars_plot <- NULL
    }
  } else {
    sig_stars_plot <- NULL
  }

  # Significance text
  df_text <- df_pvalues[!grepl("*", df_pvalues$label, fixed = TRUE), ]
  if (Nrow(df_text) != 0L) {
    sig_text_plot <- ggplot2::geom_text(data = df_text, ggplot2::aes(x = x_label, y = y_sig_label, label = label), size = text_size, size.unit = "pt", vjust = 0, hjust = 0.5, color = font_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)
  } else {
    sig_text_plot <- NULL
  }
  list(sig_bars_plot, sig_stars_plot, sig_text_plot)
}

#' Plot significance for grouped data
#'
#' @inheritParams .plot_sig_anno
#' @param grouping_var Name of grouping variable in df. Default is `"grouping_var"`. Enter as quoted variable name
#' @param dodge Dodge width entry for position_dodge. Default is `0.7`
#' @param drop_unused_levels If `FALSE` (default), unused levels of grouping_var are considered in plot construction. If `TRUE`, unused levels of grouping_var are removed
#' @noRd
.plot_sig_anno_grouped <- function(
    df,
    y = "y",
    x = "x_numeric",
    grouping_var = "grouping_var",
    comparison_fn = NULL,
    test_2_groups = .p_by_normality,
    normality_test = p_shapiro,
    variance_test = .p_F_test,
    welch = NULL, paired = FALSE,
    hypothesis_type = "two.sided",
    p_adj_method = "BH",
    y_max_type = "raw",
    summary_fn = mean,
    error_fn = SE,
    stars = TRUE,
    show_ns = FALSE,
    ns_symbol = "ns",
    p_case = "upper",
    p_spaces = TRUE,
    y_scale = "regular",
    dodge = 0.7,
    drop_unused_levels = FALSE,
    bar_nudge = 0.1,
    star_nudge = 0.07,
    text_nudge = 0.13,
    star_size = 20,
    text_size = 12,
    bar_thickness = 0.6,
    bar_color = "black",
    font_color = "black",
    breaks_fn = pretty,
    n_breaks = 4,
    step_increase = 0.11,
    ...) {
  p_adj_method <- if (nchar(p_adj_method) > 2) tolower(p_adj_method) else toupper(p_adj_method)
  y_max_type <- match.arg(y_max_type, choices = c("raw", "error", "summary"))
  if (!inherits(df[[grouping_var]], "factor")) {
    df[[grouping_var]] <- factor(df[[grouping_var]], levels = unique.default(df[[grouping_var]]))
  }
  df$group <- match(df[[grouping_var]], attr(df[[grouping_var]], "levels"))
  if (!is.numeric(df[[x]])) {
    if (!inherits(df[[x]], "factor")) {
      df[[x]] <- factor(df[[x]], levels = unique(df[[x]]))
    }
    df[[x]] <- match(df[[x]], attr(df[[x]], "levels"))
  }
  if (drop_unused_levels) {
    df <- .droplevels_df(df, na.rm = TRUE)
  }
  n_groups <- n_unique(df$group, na.rm = drop_unused_levels)
  df_pvalues <- dplyr::group_by(df, .data[[x]]) |>
    tidyr::nest() |>
    dplyr::mutate(p = lapply(data, function(.x) .create_df_sig_anno_ungrouped(df = .x, y = y, x = "group", comparison_fn = comparison_fn, test_2_groups = test_2_groups, normality_test = normality_test, variance_test = variance_test, welch = welch, paired = paired, hypothesis_type = hypothesis_type, p_adj_method = p_adj_method, y_max_type = y_max_type, summary_fn = summary_fn, error_fn = error_fn, stars = stars, show_ns = show_ns, ns_symbol = ns_symbol, p_case = p_case, p_spaces = p_spaces))) |>
    dplyr::select(-data) |>
    tidyr::unnest(p) |>
    dplyr::ungroup()

  df_pvalues$x <- .dodge_x_position(x = df_pvalues$x, n_groups = n_groups, dodge = dodge) + df_pvalues[[x]]
  df_pvalues$xend <- .dodge_x_position(x = df_pvalues$xend, n_groups = n_groups, dodge = dodge) + df_pvalues[[x]]
  df_pvalues$x_label <- (df_pvalues$x + df_pvalues$xend)/2

  #if (Nrow(df_pvalues) == 0) return(vec_to_df(x = NA_real_, xend = NA_real_, y = NA_real_, label = NA_character_))
  if (Nrow(df_pvalues) == 0) return(NULL)

  # y coordinates for significance bar and annotation
  if (startsWith(y_scale, "log")) {
    df_pvalues$y_sig_bar <- df_pvalues$y*(1 + bar_nudge)
    df_pvalues$y_sig_label <- df_pvalues$y*(1 + ifelse(grepl("*", df_pvalues$label, fixed = TRUE), star_nudge, text_nudge))
    df_pvalues <- .avoid_overlap(.df = df_pvalues, .step = step_increase, .scale = "log")
  } else {
    min_y_plot <- min(df[[y]], na.rm = TRUE)
    max_y_plot <- max(df_pvalues$y, na.rm = TRUE)
    delta_y <- diff(range(.create_axis_breaks(.limits = range(min_y_plot, max_y_plot), .scale = "regular", .breaks_fn = breaks_fn, .n = n_breaks)))
    df_pvalues$y_sig_bar <- df_pvalues$y + delta_y*bar_nudge
    df_pvalues$y_sig_label <- df_pvalues$y + ifelse(grepl("*", df_pvalues$label, fixed = TRUE), delta_y*star_nudge, delta_y*text_nudge)
    df_pvalues <- .avoid_overlap(.df = df_pvalues, .step = step_increase, .scale = "regular", .delta = delta_y)
  }

  if (!show_ns && length(df_pvalues$p) != 0L) {
    df_pvalues <- df_pvalues[df_pvalues$p < 0.05, ]
  }

  # Significance bars
  sig_bars_plot <- ggplot2::geom_segment(data = df_pvalues, ggplot2::aes(x = x, xend = xend, y = y_sig_bar, yend = y_sig_bar), linewidth = bar_thickness, color = bar_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)

  # geom_text for stars
  if (stars) {
    df_stars <- df_pvalues[grepl("*", df_pvalues$label, fixed = TRUE), ]
    if (Nrow(df_stars) > 0L) {
      sig_stars_plot <- ggplot2::geom_text(data = df_stars, ggplot2::aes(x = x_label, y = y_sig_label, label = label), size = star_size, size.unit = "pt", vjust = 0, hjust = 0.5, color = font_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)
    } else {
      sig_stars_plot <- NULL
    }
  } else {
    sig_stars_plot <- NULL
  }

  # geom_text for text
  df_text <- df_pvalues[!grepl("*", df_pvalues$label, fixed = TRUE), ]
  if (Nrow(df_text) > 0L) {
    sig_text_plot <- ggplot2::geom_text(data = df_text, ggplot2::aes(x = x_label, y = y_sig_label, label = label), size = text_size, size.unit = "pt", vjust = 0, hjust = 0.5, color = font_color, na.rm = TRUE, show.legend = FALSE, inherit.aes = FALSE)
  } else {
    sig_text_plot <- NULL
  }
  list(sig_bars_plot, sig_stars_plot, sig_text_plot)
}

# Helpers -----------------------------------------------------------------

#' t-test or Mann-Whitney U test to compare 2 groups
#'
#' @param .df Data frame
#' @param .continuous_var Continuous variable. Enter as quoted variable name. Default is `"y"`
#' @param .group Grouping variable. Enter as quoted variable name. Default is `"x"`
#' @param .test_fn Function used to compare 2 groups. Options: `.p_ttest`, `.p_mann_whitney.` Must take 2 numeric vectors as input and return length 1 numeric (P value). Options: `.p_by_normality` (default), `.p_mann_whitney`, `.p_ttest`
#' @param .normality_test Normality test. Default is `p_shapiro`
#' @param .variance_test Test used to compare variance between groups. Default is `.p_F_test`
#' @param .welch If `NULL` (default), variance_test used to determine whether t-test or Welch's correction is applied to t-test. If `TRUE`, Welch's correction applied to t-test
#' @param .paired If `FALSE` (default), unpaired test is performed
#' @param .hypothesis_type Type of hypothesis test to perform. Options: `"two.sided" `(default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param .fn_y_max Function used to determine maximum y value for each comparison
#' @param ... Not used
#' @returns Data frame containing the following columns: Group1, Group2, p, y
#' @noRd
.sig_compare_2_groups <- function(
    .df,
    .continuous_var = "y",
    .group = "x_numeric",
    .test_fn = .p_by_normality,
    .normality_test = p_shapiro,
    .variance_test = .p_F_test,
    .welch = NULL,
    .paired = FALSE,
    .hypothesis_type = "two.sided",
    .fn_y_max = function(x) max(x, na.rm = TRUE),
    ...) {
  g <- as.factor(.subset2(.df, .group))
  #y <- .subset2(.df, .continuous_var)
  #g <- factor(.df[[.group]])
  group_size <- summary(g)
  unique_groups <- names(group_size)
  if (length(unique_groups) < 2L) return(NULL)
  y_rel_max <- tapply(.subset2(.df, .continuous_var), g, .fn_y_max)
  purrr::pmap_dfr(.combos_1_vec_as_df(unique_groups, n = 2), ~{
    if (min(group_size[c(.x, .y)]) < 3) return(NULL)
    list(
      Group1 = .x,
      Group2 = .y,
      p = .test_fn(.df[.df[[.group]] == .x, .continuous_var][[1L]], .df[.df[[.group]] == .y, .continuous_var][[1L]], normality_test = .normality_test, welch = .welch, variance_test = .variance_test, paired = .paired, hypothesis_type = .hypothesis_type),
      y = max(y_rel_max[c(.x, .y)])
    )
  })
}

#' Dunn's test to compare > 2 groups
#'
#' @inheritParams .sig_compare_2_groups
#' @param .p_adj_method Method to adjust P values. Default is `"BH"`
#' @param ... Not used
#' @returns Data frame containing the following columns: Group1, Group2, p (adjusted P value), y
#' @noRd
.sig_compare_dunn <- function(
    .df,
    .continuous_var = "y",
    .group = "x_numeric",
    .p_adj_method = "BH",
    .fn_y_max = function(x) max(x, na.rm = TRUE),
    ...) {
  y <- .subset2(.df, .continuous_var)
  n <- length(y)
  g <- factor(.subset2(.df, .group))
  y_split <- split.default(y, g)
  group_size <- lengths(y_split, use.names = TRUE)
  unique_groups <- names(group_size)
  y_rank <- rank(y)
  mean_ranks <- vapply(split.default(y_rank, g), mean.default, numeric(1), na.rm = TRUE, USE.NAMES = TRUE)
  y_rel_max <- vapply(y_split, .fn_y_max, numeric(1), USE.NAMES = TRUE)
  y_rank_sorted <- sort.int(y_rank)
  pos <- 1
  tiesum <- 0
  while (pos <= n) {
    val <- y_rank_sorted[pos]
    nt <- length(y_rank_sorted[y_rank_sorted == val])
    pos <- pos + nt
    if (nt > 1L) {
      tiesum <- tiesum + nt*nt*nt - nt
    }
  }
  z <- n*(n + 1)/12 - tiesum/(12*(n - 1))
  compare_levels <- function(i, j) {
    delta <- abs(mean_ranks[i] - mean_ranks[j])
    z <- (z/group_size[i] + z/group_size[j])
    2*stats::pnorm(abs(delta/sqrt(z)), lower.tail = FALSE)
  }
  df_pval <- purrr::pmap_dfr(.combos_1_vec_as_df(unique_groups, n = 2), ~{
    list(Group1 = .x, Group2 = .y, p = compare_levels(.x, .y), y = max(y_rel_max[c(.x, .y)]))
  })
  df_pval$p <- p_adjust(df_pval$p, method = .p_adj_method)
  df_pval
}

#' Create data frame necessary for plotting significance annotation of ungrouped data
#'
#' @param df Data frame used internally by ggplot2
#' @param y Name of continuous variable in `df.` Default is `"y"`. Enter as quoted variable name
#' @param x Name of x axis variable in `df.` Default is `"x_numeric"`. Enter as quoted variable name
#' @param comparison_fn Function used to compare groups. Must take data frame (1st argument), formula (2nd argument), grouping variable (3rd argument), ns_symbol (4th argument). Must return a data frame containing columns `"p"`, `"label"`, grouping variable indicated by x, `"max_1"`, `"max_2"`, `"sd_1"`, `"sd_2"`, `"se_1"`, `"se_2"`, `"mean_1"`, `"mean_2"`
#' @param test_2_groups Function used to compare groups. If `NULL` (default), Dunn's test vs. 2 group testing is selected based on the number of groups. Other options: `.sig_compare_dunn`, `.sig_compare_2_groups`
#' @param normality_test Normality test. Default is `p_shapiro`
#' @param variance_test Test used to compare variance between groups. Default is `.p_F_test`
#' @param welch If `NULL` (default), variance_test used to determine whether t-test or Welch's correction is applied to t-test. If `TRUE`, Welch's correction applied to t-test
#' @param paired If `FALSE` (default), unpaired testing is performed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param p_adj_method Method to adjust P values. Default is `"BH"`
#' @param y_max_type Type of data plotted. Used to determine the location of significance annotation along y axis. If points are plotted, use `"raw"`. If bars with error bars used, select `"error"`. Options: `"raw"` (default), `"error"`,` "summary"`. Enter as quoted type.
#' @param summary_fn Function used to generated summary estimate for `y`. Default is `Mean`
#' @param error_fn Function used to determine error estimate for `y`. Default is `SE`
#' @param stars If `TRUE` (default), significance stars are used instead of actual P values
#' @param show_ns If `FALSE` (default), no annotation for P >= 0.05. If `TRUE`, non-significant comparisons displayed
#' @param ns_symbol Symbol to use when P >= 0.05. Only used if `show_ns = TRUE.` default is `"ns"`
#' @param p_case Case of P used in significance annotations when sig_annotation is set to `"values"`. Options: `"upper"` (default) or `"lower"`. Enter as quoted text
#' @param p_spaces If `TRUE` (default), spaces placed between p, equality symbol, and p-value in significance annotation. If `FALSE`, no spaces placed between p, equality symbol, and p-value in significance annotation. Only relevant when `sig_annotation` is set to `"values"`
#' @param ... Not used
#' @noRd
.create_df_sig_anno_ungrouped <- function(
    df,
    y = "y",
    x = "x_numeric",
    comparison_fn = NULL,
    test_2_groups = .p_by_normality,
    normality_test = p_shapiro,
    variance_test = .p_F_test,
    welch = NULL,
    paired = FALSE,
    hypothesis_type = "two.sided",
    p_adj_method = "BH",
    y_max_type = "raw",
    summary_fn = mean,
    error_fn = SE,
    stars = TRUE,
    show_ns = FALSE,
    ns_symbol = "ns",
    p_case = "upper",
    p_spaces = TRUE,
    ...) {
  p_adj_method <- if (nchar(p_adj_method) > 2) tolower(p_adj_method) else toupper(p_adj_method)
  if (is.null(comparison_fn)) {
    comparison_fn <- if (n_unique(df[[x]]) > 2) .sig_compare_dunn else .sig_compare_2_groups
  }
  y_max_type <- match.arg(y_max_type, choices = c("raw", "error", "summary"))
  fn_y_max <- switch(y_max_type, raw = max, error = function(x) summary_fn(x) + error_fn(x), summary = summary_fn)

  # Ensure x is numeric
  if (!is.numeric(df[[x]])) {
    if (!inherits(df[[x]], "factor")) {
      df[[x]] <- factor(df[[x]], levels = unique.default(df[[x]]))
    }
    df[[x]] <- match(df[[x]], attr(df[[x]], "levels"))
  }

  # P values
  df_pvalues <- comparison_fn(.df = df, .continuous_var = y, .group = x, .test_fn = test_2_groups, .normality_test = normality_test, .variance_test = variance_test, .welch = welch, .paired = paired, .hypothesis_type = hypothesis_type, .p_adj_method = p_adj_method, .fn_y_max = fn_y_max)

  # Significance labels and x coordinates for annotations
  #if (Nrow(df_pvalues) == 0L) return(vec_to_df(x = NA_real_, xend = NA_real_, y = NA_real_, label = NA_character_))
  if (Nrow(df_pvalues) == 0L) return(NULL)

  df_pvalues$label <- sig_stars(df_pvalues$p, symbols = c("****", "***", "**", "*",  ns_symbol))
  df_pvalues$xmin <- as.numeric(df_pvalues$Group1)
  df_pvalues$xend <- as.numeric(df_pvalues$Group2)
  #x_label = (xend + xmin)/2)

  # Minimum possible y coordinate for significance bars
  df_by_x <- dplyr::group_by(df, .data[[x]])
  df_by_x <- dplyr::summarise(df_by_x, y = fn_y_max(.data[[y]]))
  df_by_x <- dplyr::ungroup(df_by_x)
  x_values <- df_by_x[[x]]

  # For every row (comparison), determine which x values are between xmin and xend
  df_pvalues$y <- apply(df_pvalues[, c("xmin", "xend")], 1, function(j) {
    z <- x_values[x_values >= min(j) & x_values <= max(j)]
    max(df_by_x$y[df_by_x[[x]] %in% z])
  })

  # Hide comparisons with P >= 0.05 if show_ns = FALSE
  if (!show_ns && length(df_pvalues$p) != 0L) {
    df_pvalues <- df_pvalues[df_pvalues$p < 0.05, ]
  }
  #if (Nrow(df_pvalues) == 0) return(vec_to_df(x = NA_real_, xend = NA_real_, y = NA_real_, label = NA_character_))
  if (Nrow(df_pvalues) == 0) return(NULL)

  # Significance annotation if text is used
  if (!stars) {
    df_pvalues$label <- format_p_value(df_pvalues$p, trim_ws = !p_spaces, prefix = if (p_case == "upper") "P" else "p")
  }

  # Change column names for plotting
  names(df_pvalues)[names(df_pvalues) == "x"] <- "x_old"
  names(df_pvalues)[names(df_pvalues) == "xmin"] <- "x"
  df_pvalues
}

#' Plot significance for grouped data
#'
#' @inheritParams .create_df_sig_anno_ungrouped
#' @param grouping_var Name of grouping variable in `df`. Default is `"grouping_var"`. Enter as quoted variable name
#' @param dodge Dodge width entry for position_dodge. Default is `0.7`
#' @param drop_unused_levels If `FALSE` (default), unused levels of `grouping_var` are considered in plot construction. If `TRUE`, unused levels of `grouping_var` are removed
#' @noRd
.create_df_sig_anno_grouped <- function(
    df,
    y = "y",
    x = "x_numeric",
    grouping_var = "grouping_var",
    comparison_fn = NULL,
    test_2_groups = .p_by_normality,
    normality_test = p_shapiro,
    variance_test = .p_F_test,
    welch = NULL,
    paired = FALSE,
    hypothesis_type = "two.sided",
    p_adj_method = "BH",
    y_max_type = "raw",
    summary_fn = mean,
    error_fn = SE,
    stars = TRUE,
    show_ns = FALSE,
    ns_symbol = "ns",
    p_case = "upper",
    p_spaces = TRUE,
    dodge = 0.7,
    drop_unused_levels = FALSE,
    ...) {
  p_adj_method <- if (nchar(p_adj_method) > 2) tolower(p_adj_method) else toupper(p_adj_method)
  y_max_type <- match.arg(y_max_type, choices = c("raw", "error", "summary"))
  if (!inherits(df[[grouping_var]], "factor")) {
    df[[grouping_var]] <- factor(df[[grouping_var]], levels = unique.default(df[[grouping_var]]))
  }
  df$group <- match(df[[grouping_var]], attr(df[[grouping_var]], "levels"))
  df$x_var <- df[[x]]
  if (!is.numeric(df$x_var)) {
    if (!inherits(df$x_var, "factor")) {
      df$x_var <- factor(df$x_var, levels = unique.default(df$x_var))
    }
    df$x_var <- match(df$x_var, attr(df$x_var, "levels"))
  }
  if (drop_unused_levels) {
    df <- .droplevels_df(df, na.rm = TRUE)
  }
  n_groups <- n_unique(df$group, na.rm = drop_unused_levels)
  df_pvalues <- dplyr::group_by(df, .data$x_var) |>
    tidyr::nest() |>
    dplyr::mutate(p = lapply(data, function(.x) .create_df_sig_anno_ungrouped(df = .x, y = y, x = "group", comparison_fn = comparison_fn, test_2_groups = test_2_groups, normality_test = normality_test, variance_test = variance_test, welch = welch, paired = paired, hypothesis_type = hypothesis_type, p_adj_method = p_adj_method, y_max_type = y_max_type, summary_fn = summary_fn, error_fn = error_fn, stars = stars, show_ns = show_ns, ns_symbol = ns_symbol, p_case = p_case, p_spaces = p_spaces))) |>
    dplyr::select(-data) |>
    tidyr::unnest(p) |>
    dplyr::ungroup()
  df_pvalues <- df_pvalues[stats::complete.cases(df_pvalues), ]
  df_pvalues$x <- .dodge_x_position(x = df_pvalues$x, n_groups = n_groups, dodge = dodge) + df_pvalues$x_var
  df_pvalues$xend <- .dodge_x_position(x = df_pvalues$xend, n_groups = n_groups, dodge = dodge) + df_pvalues$x_var
  df_names <- names(df_pvalues)
  if (x %!in% df_names) {
    names(df_pvalues)[df_names == "x_var"] <- x
  }
  df_pvalues
}

#' Choose function to generate significance plotting data for ungrouped vs. grouped data
#'
#' @param .df Data frame
#' @param .grouping_var Grouping variable. Enter as quoted column name
#' @param ... Arguments passed to `.create_df_sig_anno_ungrouped` or `.create_df_sig_anno_grouped`
#' @noRd
.create_df_sig <- function(.df, .grouping_var = "grouping_var", ...) {
  z <- .df[[.grouping_var]]
  z <- unique.default(z[!is.na(z)])
  if (length(z) > 1L) .create_df_sig_anno_grouped(.df, ...) else .create_df_sig_anno_ungrouped(.df, ...)
}

#' Determine x coordinates for grouped significance annotation
#'
#' @param x Group rank (i.e. integer representing group number)
#' @param n_groups Number of unique values of grouping variable
#' @param dodge Dodge width entry for position_dodge. Default is `0.7`
#' @noRd
.dodge_x_position <- function(x, n_groups, dodge = 0.7) dodge*((x - 0.5)/n_groups - 0.5)

#' Apply step increase to plotting data
#'
#' @param .df Data frame containing columns `x`, `xend`, `y`, `step_increase`
#' @returns Data frame used by `GeomSigBar`
#' @noRd
.step_increase <- function(.df) {
  if (Nrow(.df) == n_unique(.df$y)) return(.df)
  .df$bar_width <- .df$xend - .df$x
  .df <- .df[order(.df$y, .df$bar_width, -.df$x, -.df$xend), ]
  .df <- dplyr::group_by(.df, y)
  .df <- dplyr::mutate(.df, n = seq_len(dplyr::n()) - 1)
  .df <- dplyr::ungroup(.df)
  .df$y <- .df$y + .df$step_increase*.df$n
  .df
}

#' Avoid overlapping significance annotations
#'
#' @param .df Data frame. Must have columns `x`, `xend`, `y_sig_bar`, `y_sig_label`
#' @param .step Step increase
#' @param .scale Options: `"regular"`, `"scientific"`, `"log"`
#' @param .delta Difference between min and max y value for entire plot. Not relevant when `.scale = "log"`
#' @returns Data frame. Used by `.plot_sig_anno` and `.plot_sig_anno_grouped`
#' @noRd
.avoid_overlap <- function(.df, .step, .scale, .delta = NULL) {
  if (Nrow(.df) == n_unique(.df$y_sig_bar)) return(.df)
  .df$bar_width <- .df$xend - .df$x
  .df <- .df[order(.df$y_sig_bar, .df$bar_width, -.df$x, -.df$xend), ]
  .df <- dplyr::group_by(.df, y_sig_bar)
  .df <- dplyr::mutate(.df, n = seq_len(dplyr::n()) - 1)
  .df <- dplyr::ungroup(.df)
  if (.scale == "log") {
    .step <- 1 + .step
    .step <- .step*.df$n
    .df$y_sig_bar <- ifelse(.df$n == 0, .df$y_sig_bar, expm1(log1p(.df$y_sig_bar) + .step))
    .df$y_sig_label <- ifelse(.df$n == 0, .df$y_sig_label, expm1(log1p(.df$y_sig_label) + .step))
  } else {
    .step <- .delta*.step*.df$n
    .df$y_sig_bar <- .df$y_sig_bar + .step
    .df$y_sig_label <- .df$y_sig_label + .step
  }
  .df
}
