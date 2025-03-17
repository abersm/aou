#' Compare continuous variable between 2 groups
#'
#' @param df Data frame in long format
#' @param formula enter as continuous variable ~ grouping variable
#' @param ... Subgrouping variables (statistical test will be performed for each unique combination of subgrouping variables). Enter as comma separated list of quoted or unquoted variable names. If quoted, can wrap in `c()`. Can also enter character vector containing variable names
#' @param x,y Variables for grouping variable (`x`) and continuous variable (`y`). Enter as quoted or unquoted variable names
#' @param ns_symbol Symbol to use when P > 0.05. Default is `"ns"`
#' @param normality_test Normality test used to determine whether t-test or Mann Whitney U test will be used. Enter as unquoted function name. Function must take a numeric vector as input and returns p value. Default is `p_shapiro`
#' @param variance_test Variance test used to determine whether Welch's correction should be used for t-test (if P value for variance test is < 0.05). Enter as unquoted function name. Function must take a numeric vector as input and returns p value. Default is `p_F_test`
#' @param summary_fns Named list of summary functions that will be applied to each group. Enter as `list(col_name = fn_name)`
#' @param paired If `FALSE` (default), unpaired test performed. If `TRUE`, paired test performed
#' @param vars_remove Variables to remove from output. Enter as comma separated list of quoted variable names wrapped in `c()`
#' @returns Data frame
#' @export
compare_means <- function(
    df,
    formula = NULL,
    ...,
    x = NULL,
    y = NULL,
    ns_symbol = "ns",
    normality_test = p_shapiro,
    variance_test = p_F_test,
    summary_fns = list(
      median = Median,
      mean = Mean,
      q1 = Q1,
      q3 = Q3,
      sd = SD,
      se = SE,
      ci = CI,
      min = min,
      max = max
    ),
    vars_remove = c("p_normality_1", "p_normality_2", "p_var_test"),
    paired = FALSE) {
  # Continuous and grouping variables
  vars <- formula2vars(formula, x = get_input(x), y = get_input(y), parent_fn = "compare_means")
  grouping_var <- vars$x
  continuous_var <- vars$y

  # Subgrouping variables
  if (n_dots(...) > 0) {
    is_input_vector <- tryCatch(is.atomic(c(...)), error = function(e) FALSE)
    subgroups <- Intersect(if (is_input_vector) c(...) else dots_as_quoted(...), names(df))
    if (length(subgroups) == 0L) {
      subgroups <- NULL
    }
  } else {
    subgroups <- NULL
  }

  # Remove missing data
  df <- df[c(continuous_var, grouping_var, subgroups)]
  df <- df[stats::complete.cases(df), , drop = FALSE]

  # Remove unused levels of factors
  if (is.factor(df[[grouping_var]])) {
    df[[grouping_var]] <- as.character(df[[grouping_var]])
  }
  df <- .droplevels_df(df)

  # Ensure continuous variable is numeric
  if (!is.numeric(df[[continuous_var]])) {
    if (all(can_be_numeric(df[[continuous_var]]))) {
      Warning(sprintf("In 'compare_means', continuous variable ('%s') is a %s", continuous_var, .quote_collapse(class(df[[continuous_var]]))), "\nWill convert to numeric variable")
      df[[continuous_var]] <- as.numeric(df[[continuous_var]])
    } else {
      Stop(sprintf("In 'compare_means', continuous variable ('%s') is a %s", continuous_var, .quote_collapse(class(df[[continuous_var]]))), "\nThis variable is not coercible to a numeric variable")
    }
  }

  # Variance test
  variance_test <- if (identical(p_levene, variance_test)) {
    .p_levene
  } else if (identical(p_F_test, variance_test)) {
    .p_F_test
  } else if (identical(p_bartlett, variance_test)) {
    .p_bartlett
  } else {
    variance_test
  }

  # Summary statistics
  summary_fns <- summary_fns %||% list()
  summary_fns$median <- summary_fns$median %||% Median
  summary_fns$p_normality <- normality_test
  df_summary <- dplyr::group_by(df, !!!rlang::syms(c(grouping_var, subgroups)))
  df_summary <- dplyr::summarize(df_summary, dplyr::across(.cols = dplyr::all_of(continuous_var), .fns = summary_fns, .names = "{.fn}"), .groups = "drop")

  # P value, n
  suppressMessages(suppressWarnings({
    df_pval <- dplyr::group_by(df, !!!rlang::syms(subgroups)) |>
      tidyr::nest() |>
      dplyr::mutate(pvals = lapply(data, function(z) .compare_means_pairs(.df = z, .continuous_var = continuous_var, .group = grouping_var, .variance_test = variance_test, .paired = paired))) |>
      dplyr::select(-data) |>
      tidyr::unnest(pvals) |>
      dplyr::ungroup()
  }))

  # If no p values can be calculated, stop downstream processing
  if (Nrow(df_pval) == 0L) return(NULL)

  # Join df_summary and df_pval
  names(df_summary)[names(df_summary) == grouping_var] <- "Group1"
  df_pval <- dplyr::left_join(df_pval, df_summary, by = c(subgroups, "Group1"))
  df_pval <- dplyr::left_join(df_pval, df_summary, by = c(subgroups, "Group2" = "Group1"), suffix = c("_1", "_2"))
  df_pval <- dplyr::mutate(
    df_pval,
    greater = dplyr::case_when(
      median_1 > median_2 ~ paste0(Group1, " > ", Group2),
      median_2 > median_1 ~ paste0(Group2, " > ", Group1),
      median_1 == median_2 ~ paste0(Group1, " = ", Group2)
    ),
    t_method =  dplyr::case_when(
      p_var_test < 0.05 ~ "t-test (with Welch's correction)",
      p_var_test >= 0.05 ~ "t-test (without Welch's correction)"
    ),
    p_t_test_by_var_test = dplyr::case_when(
      p_var_test < 0.05 ~ p_t_test_welch,
      p_var_test >= 0.05 ~ p_t_test_no_welch,
      .default = NA_real_
    ),
    p = dplyr::case_when(
      p_normality_1 < 0.05 | p_normality_2 < 0.05 ~ p_mann_whitney,
      p_normality_1 >= 0.05 & p_normality_2 >= 0.05 ~ p_t_test_by_var_test,
      .default = NA_real_
    ),
    label = sig_stars(p, symbols = c("****", "***", "**", "*",  ns_symbol)),
    method = dplyr::case_when(
      p_normality_1 < 0.05 | p_normality_2 < 0.05 ~ "Mann-Whitney test",
      p_normality_1 >= 0.05 & p_normality_2 >= 0.05 ~ t_method
    ),
    fc_1_vs_2_mean = mean_1/mean_2,
    fc_1_vs_2_median = median_1/median_2,
    fc_2_vs_1_mean = mean_2/mean_1,
    fc_2_vs_1_median = median_2/median_1,
    fc_mean = ifelse(fc_1_vs_2_mean > 1, fc_1_vs_2_mean, fc_2_vs_1_mean),
    fc_median = ifelse(fc_1_vs_2_median > 1, fc_1_vs_2_median, fc_2_vs_1_median))

  # Remove and reorder columns
  df_pval <- dplyr::select(df_pval, dplyr::all_of(subgroups), Group1, Group2, greater, p, label, method, n_1, n_2, dplyr::everything(), -dplyr::any_of(vars_remove))

  # Arrange rows by p value
  df_pval[order(df_pval$p), , drop = FALSE]
}

# Helper ------------------------------------------------------------------

#' Calculate summary statistics for each pair of a binary grouping variable values
#'
#' @param .df Data frame
#' @param .continuous_var Continuous variable. Enter as length 1 character vector
#' @param .group Grouping variable. Enter as length 1 character vector
#' @param .variance_test Variance test. Must take data frame and formula as input and return a length 1 numeric vector. Default is `.p_levene`
#' @param .paired If `FALSE` (default), unpaired test performed. If `TRUE`, paired test performed
#' @returns List of lists containing "Group1", "Group2", "p_var_test", "p_mann_whitney", "p_t_test_welch", "p_t_test_no_welch", "n_1", "n_2"
#' @noRd
.compare_means_pairs <- function(.df, .continuous_var, .group, .variance_test = .p_levene, .paired = FALSE) {
  groups <- .subset2(.df, .group)
  unique_groups <- unique(groups)
  if (length(unique_groups) < 2L) return(NULL)
  .df <- .df[c(.continuous_var, .group)]
  y_vals <- .subset2(.df, .continuous_var)
  purrr::pmap_dfr(.combos_1_vec_as_df(unique_groups, n = 2), ~{
    y1 <- y_vals[groups == .x]
    y2 <- y_vals[groups == .y]
    n1 <- length(y1)
    n2 <- length(y2)
    if (n1 < 2L || n2 < 2L) {
      list(Group1 = .x, Group2 = .y, p_var_test = NA_real_, p_mann_whitney = NA_real_, p_t_test_welch = NA_real_, p_t_test_no_welch = NA_real_, n_1 = n1, n_2 = n2)
    } else {
      list(
        Group1 = .x,
        Group2 = .y,
        p_var_test = .variance_test(y1, y2),
        p_mann_whitney = .p_mann_whitney(x = y1, y = y2, paired = .paired),
        p_t_test_welch = .p_ttest(x = y1, y = y2, welch = TRUE, paired = .paired),
        p_t_test_no_welch = .p_ttest(x = y1, y = y2, welch = FALSE, paired = .paired),
        n_1 = n1,
        n_2 = n2
      )
    }
  })
}
