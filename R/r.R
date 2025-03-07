#' Generic function for rapid display of objects
#'
#' @param x Object
#' @param ... Arguments passed to method-specific function
#' @export
r <- function(x, ...) UseMethod("r", x)

#' r - default
#'
#' @rdname r
#' @export
r.default <- function(x, ...) NULL

#' r - factor vector
#'
#' @rdname r
#' @param show_plot If `TRUE` (default), histogram is plotted
#' @param plot_fn Function used for plotting. Default is `plot_freq`
#' @param ... Arguments passed to `plot_fn`
#' @export
r.factor <- function(x, show_plot = TRUE, plot_fn = plot_freq, ...) {
  x_summary <- if (anyNA(x)) {
    idx_na <- is.na(x)
    x_nna <- x[!idx_na]
    x_unique <- unique.default(x_nna)
    out <- tabulate(match(x_nna, x_unique))
    names(out) <- x_unique
    c(sort.int(out, method = "quick", decreasing = TRUE), "NA" = sum(idx_na))
  } else {
    x_unique <- unique.default(x)
    out <- tabulate(match(x, x_unique))
    names(out) <- x_unique
    sort.int(out, method = "quick", decreasing = TRUE)
  }
  if (show_plot) {
    print(plot_fn(x, ...))
  }
  x_summary
}

#' r - character vector
#'
#' @rdname r
#' @export
r.character <- function(x, ...) r.factor(factor(x, levels = create_levels(x)), ...)

#' r - integer vector
#'
#' @rdname r
#' @export
r.integer <- r.character

#' r - logical vector
#'
#' @rdname r
#' @export
r.logical <- r.character

#' r - date vector
#'
#' @rdname r
#' @param x Data vector
#' @param month_format Options: `"number"` (default), `"name"`, `"abbreviation"`
#' @param date_sep Separator between month, day, and year
#' @param leading_zero If `FALSE` (default), leading 0s are removed from month and day if < 10
#' @param full_year If `FALSE` (default), last 2 digits are used. If `TRUE`, year is displayed using 4 digits
#' @param n_as_factor If number of unique values of `x` is < `n_as_factor`, `x` will be treated as a factor. Default is `10`
#' @param show_plot If `TRUE` (default), histogram is plotted
#' @param ... Arguments passed to `plot_incidence` or `plot_fn`
#' @export
r.Date <- function(x, month_format = c("number", "name", "abbreviation"), leading_zero = FALSE, full_year = FALSE, date_sep = "-", n_as_factor = 10, show_plot = TRUE, ...) {
  n <- if (anyNA(x)) {
    idx_na <- is.na(x)
    x_nna <- x[!idx_na]
    x_unique <- unique.default(x_nna)
    out <- tabulate(match(x_nna, x_unique))
    names(out) <- x_unique
    c(sort.int(out, method = "quick", decreasing = TRUE), "NA" = sum(idx_na))
  } else {
    x_unique <- unique.default(x)
    out <- tabulate(match(x, x_unique))
    names(out) <- x_unique
    sort.int(out, method = "quick", decreasing = TRUE)
  }


  n_dates <- length(n)
  month_format <- match.arg(month_format, choices = c("number", "name", "abbreviation"))
  month <- switch(month_format, number = "%m", name = "%B", "%b")
  year_format <- if (full_year) "%Y" else "%y"
  date_format <- paste(month, if (!leading_zero && n_dates < n_as_factor) "%e" else "%d", year_format, sep = "-")
  formatted_date <- format(as.Date(names(n)), date_format)
  if (n_dates < n_as_factor) {
    formatted_date <- as.character(formatted_date)
    formatted_date <- if (leading_zero) formatted_date else gsub("^0", "", formatted_date)
    return(r.factor(factor(rep(formatted_date, n), levels = formatted_date), show_plot = show_plot, ...))
  }
  idx <- is.na(x)
  n_na <- sum(idx)
  if (show_plot) {
    print(plot_incidence(x[!idx], month_format = month_format, full_year = full_year, leading_zero = leading_zero, date_sep = date_sep, ...))
  }
  if (n_na > 0) {
    message(n_na, " missing dates")
  }
  out <- vec_to_df(date = formatted_date, n = n)
  out[order(out$date), ]
}

#' r - numeric vector
#'
#' @rdname r
#' @param n_bins Number of bins for x axis. Default is `20`
#' @param color Color used for histogram bars. Default is `"#328EC3"`
#' @param show_plot If `TRUE` (default), histogram is plotted
#' @param x_axis_limits,x_axis_breaks,x_axis_title Limits, breaks, and title for x axis respectively
#' @param x_scale Scale for x axis. Default is `"regular"`
#' @param expand_x Expansion multiplier for x axis. Default is `0.1` if `n_bins > 10` and 0.15 if `n_bins <= 10`
#' @param n_as_factor If number of unique values of `x` is < `n_as_factor`, `x` will be treated as a factor. Default is `10`
#' @param n_breaks Number of desired breaks for x axis ticks. Default is `6`
#' @param y_axis_title Title for y axis. Default is `"n"`
#' @export
r.numeric <- function(x, n_bins = 20, color = "#328EC3", show_plot = TRUE, x_axis_limits = NULL, x_axis_breaks = NULL, x_axis_title = NULL, x_scale = "regular", expand_x = if (n_bins > 10) 0.1 else 0.15, n_as_factor = 10, n_breaks = 6, y_axis_title = "n", ...) {
  x_mean <- Mean(x)
  x_median <- Median(x)
  rng <- Range(x)
  n_total <- length(x)
  n_na <- N_na(x)
  n_nna <- n_total - n_na
  out <- paste(
    paste0("n (total) = ", n_total),
    paste0("n (non-missing) = ", n_nna),
    paste0("n (missing) = ", n_na),
    paste0("Mean ", "\u00B1", " SD = ", gsub("e", paste0("\u00D7", "10^"), format_number(x_mean, digits = 1)), " ", "\u00B1", " ", gsub("e", paste0("\u00D7", "10^"), format_number(SD(x), digits = 1))),
    paste0("Median [IQR] = ", gsub("e", paste0("\u00D7", "10^"), format_num_range(x_median, Q1(x), Q3(x), sep = " - ", digits = 1, bracket_type = "["))),
    paste0("Min = ", gsub("e", paste0("\u00D7", "10^"), format_number(rng[1], digits = 1))),
    paste0("Max = ", gsub("e", paste0("\u00D7", "10^"), format_number(rng[2], digits = 1))),
    paste0("P Shapiro test = ", format_p_value(p_shapiro(x))),
    sep = "\n")
  if (!is.null(x_axis_title)) {
    cat(x_axis_title, "\n")
  }
  cat(out)
  if (show_plot) {
    x_values <- x[!is.na(x)]
    x_axis_breaks <- x_axis_breaks %||% .create_axis_breaks(.limits = x_axis_limits %||% range(x_values), .scale = x_scale, .n = n_breaks)
    x_axis_limits <- x_axis_limits %||% range(x_axis_breaks)
    n <- length(unique.default(x_values))
    if (n < n_as_factor) {
      return(r.factor(factor(x, levels = create_levels(x))))
    }
    right_skew <- x_mean > x_median
    p <- ggplot(vec_to_df(x = x_values), aes(x = x)) + geom_histogram(fill = color, alpha = 0.8, color = "black", bins = n_bins) + scale_x_continuous(x_axis_title, limits = x_axis_limits, breaks = x_axis_breaks, expand = expansion(mult = c(expand_x, expand_x)), oob = rescale_none) + theme_custom(...) + theme(axis.title.y = element_text(angle = 0, margin = margin(r = 15)))
    y <- ggplot_build(p)$data[[1]]$count
    y_axis_breaks <- pretty(c(0, y))
    y_axis_breaks <- y_axis_breaks[y_axis_breaks %% 1 == 0]
    y_axis_limits <- range(y_axis_breaks)
    p <- p + scale_y_continuous(y_axis_title, limits = y_axis_limits, breaks = y_axis_breaks, expand = c(0, 0, 0, 0))
    if (right_skew) {
      x_annotation <- max(x_axis_breaks)
      hjust <- 1
    } else {
      x_annotation <- min(x_axis_breaks)
      hjust <- 0
    }
    y_annotation <- max(y_axis_breaks)
    p <- p + annotate("text", label = paste0("N = ", n_nna), x = x_annotation, y = y_annotation, hjust = hjust, vjust = 1)
    suppressWarnings(suppressMessages(print(p)))
  }
}

#' r - list
#'
#' @rdname r
#' @export
r.list <- function(x, ...) {
  x_names <- names(x)
  for (i in seq_along(x)) {
    r(.subset2(x, i), x_axis_title = .subset(x_names, i), ...)
    cat("\n")
  }
}

#' r - data frame
#'
#' @rdname r
#' @export
r.data.frame <- function(x, ...) {
  df_name <- get_input(x)
  dims <- dim(x)
  n_rows <- dims[1L]
  n_cols <- dims[2L]
  cols_cont <- vars_numeric(x)
  cols_cat <- vars_cat(x)
  cols_binary <- vars_binary(x)
  cols_chr <- vars_chr(x)
  cols_fct <- vars_fct(x)
  cols_date <- vars_date(x)
  id_vars <- vars_id(x)
  out <- paste(
    sprintf("Data frame: %s", df_name),
    sprintf("Rows = %s", n_rows),
    paste0("Columns = ", n_cols, "\n"),
    if (length(id_vars) > 0) paste0("ID variables = ", paste0(id_vars, collapse = ", "), "\n") else "No ID vars",
    sprintf("Numeric variables = %s", length(cols_cont)),
    paste0(paste0(cols_cont, collapse = ", "), "\n\n"),
    sprintf("Categorical variables = %s", length(cols_cat)),
    paste0(paste0(cols_cat, collapse = ", "), "\n\n"),
    sprintf("Binary variables = %s", length(cols_binary)),
    paste0(paste0(cols_binary, collapse = ", "), "\n\n"),
    sprintf("Character variables = %s", length(cols_chr)),
    paste0(paste0(cols_chr, collapse = ", "), "\n\n"),
    sprintf("Factor variables = %s", length(cols_fct)),
    paste0(paste0(cols_fct, collapse = ", "), "\n\n"),
    sprintf("Date variables = %s", length(cols_date)),
    paste0(paste0(cols_date, collapse = ", "), "\n"),
    sep = "\n")
  cat(out)
  col_name <- names(x)
  col_number <- seq_along(x)
  is_column <- function(z) as.integer(col_name %in% z)
  col_class <- vapply(x, class, character(1), USE.NAMES = FALSE)
  n_vals <- vapply(x, n_unique, integer(1), USE.NAMES = FALSE)
  vec_to_df(
    var = col_name,
    col_number = col_number,
    class = col_class,
    n_unique = n_vals,
    categorical = is_column(vars_cat(x)),
    binary = is_column(vars_binary(x)),
    binary_01 = is_column(vars_binary_01(x)),
    can_be_numeric = is_column(vars_can_be_numeric(x)),
    id_var = is_column(vars_id(x)),
    constant = is_column(vars_constant(x)),
    no_na = is_column(vars_no_na(x)),
    all_na = is_column(vars_all_na(x)))
}

#' r - grouped_df
#'
#' @rdname r
#' @export
r.grouped_df <- function(x, ...) dplyr::summarize(x, n = n(), .groups = "drop_last")

#' r - lm
#'
#' @rdname r
#' @export
r.lm <- function(x, ...) lm_tidy(x)

#' r - glm
#'
#' @rdname r
#' @export
r.glm <- function(x, ...) glm_tidy(x)

#' r - coxph
#'
#' @rdname r
#' @export
r.coxph <- function(x, ...) surv_tidy(x)

#' r - survdiff
#'
#' @rdname r
#' @export
r.survdiff <- function(x, ...) survdiff_summary(x)

#' r - table
#'
#' @rdname r
#' @export
r.table <- function(x, ...) cbind_list(odds_ratio(x))

#' r - matrix
#'
#' @rdname r
#' @export
r.matrix <- r.table
