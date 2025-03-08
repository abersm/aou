#' ROC curve
#'
#' @param df Data frame
#' @param outcome_var Binary variable. Enter as quoted or unquoted variable name
#' @param predictor_var Continuous variable. Enter as quoted or unquoted variable name
#' @param type Type of comparison. Options: `">="` (default), `">"`, `"<="`, `"<"`
#' @param line_color Color of ROC curve. Default is red
#' @param line_thickness Thickness of ROC curve. Default is `1`
#' @param point_color Color of points. Default is `"black"`
#' @param point_size Size of points. Default is `1.5`
#' @param show_points If `FALSE` (default), individual points are not displayed
#' @param show_error If `FALSE` (default), error in ROC curve estimation is not displayed
#' @param show_auc If `TRUE` (default), AUC is displayed
#' @param show_n If `TRUE` (default), number of observations is displayed
#' @param auc_position Position of AUC annotation. Options: `"left"` (default), `"right"`
#' @param auc_prefix Text used to prefix AUC annotation. Default is `"AUC "`
#' @param n_prefix Text used to prefix n annotation. Default is `"n = "`
#' @param annotation_x,annotation_y Position of annotation along x and y axis respectively
#' @param error_color Colors for error region around ROC curve. Default is `"#B3B3B3"`
#' @param error_alpha Alpha for colored error region. Default is `0.4`
#' @param x_axis_title Title for x axis. Default is `"FPR"`
#' @param x_title Alias for `x_axis_title`
#' @param y_axis_title Title for x axis. Default is `"TPR"`
#' @param y_title Alias for `x_axis_title`
#' @param expand Expansion added around x and y axis. Default is `0.1`
#' @param expand_x,expand_y Expansion added around x and y axis respectively. Default is `0.1`
#' @param breaks Axis breaks. Enter as numeric vector. Default is `c(0, 0.25, 0.5, 0.75, 1)`
#' @param percent If `FALSE` (default), axes reflect proportions. If `TRUE`, axes reflect percentages
#' @param ci Confidence interval. Default is `0.95`
#' @param ... Arguments passed to `theme_custom()`
#' @returns ggplot object
#' @export
plot_roc <- function(
  df,
  outcome_var,
  predictor_var,
  type = ">=",
  line_color = "#0072B5",
  line_thickness = 1,
  point_color = "black",
  point_size = 1.5,
  show_points = FALSE,
  show_error = FALSE,
  show_auc = TRUE,
  show_n = TRUE,
  auc_position = "left",
  auc_prefix = "AUC ",
  n_prefix = "n = ",
  annotation_x = 0.45,
  annotation_y = 0.15,
  error_color = "#B3B3B3",
  error_alpha = 0.4,
  x_title = "FPR",
  x_axis_title = x_title,
  y_title = "TPR",
  y_axis_title = y_title,
  expand = 0.1,
  expand_x = expand,
  expand_y = expand,
  breaks = seq.int(from = 0, to = 1, by = 0.25),
  percent = FALSE,
  ci = 0.95,
  ...) {
  plot_fn <- "plot_auc"
  predictor_var <- get_input(predictor_var)
  outcome_var <- get_input(outcome_var)
  df <- df[c(outcome_var, predictor_var)]
  df <- df[stats::complete.cases(df), ]
  if (startsWith(type, "<")) {
    df[[outcome_var]] <- as_numeric_factor(df[[outcome_var]], reverse = TRUE)
  }
  df_plot <- .create_roc_data(outcome = df[[outcome_var]], predictor = df[[predictor_var]])
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y))
  if (show_points) {
    p <- p + ggplot2::geom_point(size = point_size, color = point_color, show.legend = FALSE)
  }
  axis_labels <- if (percent) function(x) paste0(as.integer(x), "%") else waiver()
  auc_annotation <- if (show_auc) {
    z <- auc_ci.data.frame(df, continuous_var = predictor_var, outcome_var = outcome_var, ci = ci)
    paste0(auc_prefix, format_num_range(z$auc, z$auc_lower, z$auc_upper, digits = 2))
  } else {
    NULL
  }
  n_annotation <- if (show_n) {
    n <- nrow(df)
    n <- if (n >= 1000L) add_comma(n) else n
    paste0(n_prefix, n)
  } else {
    NULL
  }
  z <- if (show_auc && show_n) "\n" else ""
  annotation_text <- paste(auc_annotation, n_annotation, sep = z)
  annotation <- if (show_auc || show_n) {
    z <- sum(show_auc, show_n)
    ggplot2::annotate(x = annotation_x, y = annotation_y - expand_y, hjust = 0, label = annotation_text, geom = "text", parse = FALSE)
  } else {
    NULL
  }
  p +
    # geom_smooth(formula = y ~ x, method = "loess", color = line_color, size = line_thickness, fill = error_color, alpha = error_alpha, se = show_error, show.legend = FALSE) +
    # if (shaded) {
    #  geom_polygon(fill = fill_color) +
    # }
    ggplot2::geom_path(color = line_color, size = line_thickness, show.legend = FALSE) +
    annotation +
    ggplot2::scale_y_continuous(name = y_axis_title, limits = c(0, 1), breaks = breaks, labels = axis_labels, expand = c(expand_y, 0, 0, 0)) +
    ggplot2::scale_x_continuous(name = x_axis_title, limits = c(0, 1), breaks = breaks, labels = axis_labels, expand = c(expand_x, 0, 0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    theme_custom(...)
}

#' Generate data frame to generate ROC curve
#'
#' @param outcome Binary categorical variable. Must have no missing values. Enter as vector
#' @param predictor Numeric predictor_var variable. Must have no missing values. Enter as numeric vector
#' @returns Data frame with columns "cp" (cutoff), "x" (false positive rate), "y" (true positive rate)
#' @noRd
.create_roc_data <- function(outcome, predictor) {
  groups <- create_levels(outcome)
  outcome <- factor(outcome, levels = groups, ordered = TRUE)
  predictor_ordered <- order(predictor, decreasing = TRUE)
  predictor_sorted <- predictor[predictor_ordered]
  tp <- cumsum(outcome[predictor_ordered] == groups[2L])
  fp <- cumsum(outcome[predictor_ordered] == groups[1L])
  dupes <- rev(duplicated(rev(predictor_sorted)))
  fpr <- c(0, fp[!dupes])/sum(outcome == groups[1L])
  tpr <- c(0, tp[!dupes])/sum(outcome == groups[2L])
  idx <- is.finite(fpr) & is.finite(tpr)
  vec_to_df(cp = c(1, predictor_sorted[!dupes]), x = fpr[idx], y = tpr[idx])
}
