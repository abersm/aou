# t-test ------------------------------------------------------------------

#' P value for t-test
#'
#' @param df Data frame
#' @param formula Entered in format of continuous variable ~ grouping variable
#' @param y Continuous variable. Enter as quoted variable name
#' @param x Categorical grouping variable. Enter as quoted variable name
#' @param id Identification variable to link paired observations. Enter as quoted variable name. Only relevant when paired is `TRUE`
#' @param welch If `NULL` (default), use of Welch's correction is determined by variance test (If P < 0.05, variance is not equal between groups and Welch's correction is applied). If `TRUE`, Welch's correction is applied, otherwise student's t-test is performed
#' @param variance_test Function used to perform variance test. Must take numeric vector and returns P value. Default is `p_F_test`
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param hypothesis_type Type of hypothesis testing to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as length 1 character vector
#' @param otherwise Output if t-test fails. Default is `NA_real_`
#' @param ... Not used
#' @returns P value as length 1 numeric vector
#' @export
p_ttest <- function(
    df,
    formula = NULL,
    y = NULL,
    x = NULL,
    id = NULL,
    welch = NULL,
    variance_test = .p_F_test,
    paired = FALSE,
    hypothesis_type = "two.sided",
    otherwise = NA_real_,
    ...) {
  values <- if (paired) {
    vars <- formula2vars(formula, x = x, y = y, parent_fn = "p_ttest")
    y <- vars$y
    x <- vars$x
    df <- .droplevels_df(remove_na(df[c(y, x, id)], c(y, x, id)))
    .paired_values(df = df, y = y, x = x, id = id)
  } else {
    df <- remove_na(df, c(y, x))
    .df_to_split_vals(df = df, formula = formula, x = x, y = y, excl_group_na = TRUE)$y_grouped
  }
  if (length(values) != 2L) return(otherwise)
  tryCatch(suppressWarnings(.p_ttest(x = .subset2(values, 1), y = .subset2(values, 2), welch = welch, paired = paired, variance_test = variance_test, hypothesis_type = hypothesis_type, otherwise = otherwise)), error = function(e) otherwise)
}

#' P values for t-test
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of x and y must be the same and their lengths must be equal. Must have missing values removed
#' @param welch If `NULL` (default), variance_test is used to determine whether to apply Welch's correction. If TRUE, Welch's correction is applied (assumes variance is not equal between groups), otherwise student's t-test performed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param variance_test Function used to perform variance test. Must take 2 numeric vectors and return P value. Default is .p_F_test
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param otherwise Output if t-test fails. Default is `NA_real_`
#' @param ... Not used
#' @returns P value as length 1 numeric vector
#' @noRd
.p_ttest <- function(
    x,
    y,
    welch = NULL,
    paired = FALSE,
    variance_test = .p_F_test,
    hypothesis_type = "two.sided",
    otherwise = NA_real_,
    ...) {
  if (paired) {
    x <- x - y
  }
  x_len <- length(x)
  if (x_len < 2 || !is.numeric(x) || !is.vector(x)) return(otherwise)
  x_mean <- mean.default(x)
  x_var <- Var(x)
  if (paired) {
    deg_free <- x_len - 1
    stderr <- sqrt(x_var/x_len)
    if (stderr < 10*.Machine$double.eps*abs(x_mean)) return(otherwise)
    tstat <- x_mean/stderr
  } else {
    y_len <- length(y)
    if (y_len < 2L || !is.numeric(y) || !is.vector(y)) return(otherwise)
    y_mean <- mean.default(y)
    y_var <- Var(y)
    welch <- welch %||% (variance_test(x = x, y = y, hypothesis_type = hypothesis_type, otherwise = 0) < 0.05)
    if (welch) {
      vx <- sqrt(x_var/x_len)
      vx <- vx*vx
      vy <- sqrt(y_var/y_len)
      vy <- vy*vy
      stderr <- sqrt(vx + vy)
      stderr <- stderr*stderr
      deg_free <- stderr*stderr/(vx*vx/(x_len - 1) + vy*vy/(y_len - 1))
    } else {
      deg_free <- x_len + y_len - 2
      v <- (x_len - 1)*x_var
      v <- v + (y_len - 1)*y_var
      v <- v/deg_free
      stderr <- sqrt(v*(1/x_len + 1/y_len))
    }
    if (stderr < 10*.Machine$double.eps*max(abs(x_mean), abs(y_mean))) return(otherwise)
    tstat <- (x_mean - y_mean)/stderr
  }
  switch(hypothesis_type,
         two.sided = 2*stats::pt(-abs(tstat), deg_free),
         less = stats::pt(tstat, deg_free),
         greater = stats::pt(tstat, deg_free, lower.tail = FALSE))
}

# Mann-Whitney test -------------------------------------------------------

#' Mann-Whitney U test (Wilcoxon rank sum test)
#'
#' @rdname p_ttest
#' @export
p_mann_whitney <- function(df, formula = NULL, y = NULL,  x = NULL, id = NULL, paired = FALSE, otherwise = NA_real_, hypothesis_type = "two.sided", ...) {
  values <- if (paired) {
    vars <- formula2vars(formula, x = x, y = y, parent_fn = "p_mann_whitney")
    y <- vars$y
    x <- vars$x
    vars <- c(y, x, id)
    df <- .droplevels_df(remove_na(df[vars], vars))
    .paired_values(df = df, y = y, x = x, id = id)
  } else {
    df <- remove_na(df, c(y, x))
    .df_to_split_vals(df = df, formula = formula, x = x, y = y, excl_group_na = TRUE)$y_grouped
  }
  if (length(values) != 2L) return(otherwise)
  tryCatch(.p_mann_whitney(x = .subset2(values, 1L), y = .subset2(values, 2L), paired = paired, hypothesis_type = hypothesis_type), error = function(e) otherwise)
}

#' Alias for p_mann_whitney
#'
#' @rdname p_ttest
#' @export
p_wilcox <- p_mann_whitney

#' P values for Mann-Whitney test
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of x and y must be the same and their lengths must be equal. Must have missing values removed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param otherwise Output if Mann-Whitney U test fails. Default is `NA_real_`
#' @param hypothesis_type Type of hypothesis test to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as quoted hypothesis test type
#' @param correct_p If `TRUE` (default), continuity correction is applied to P value
#' @param ... Not used
#' @returns P value as length 1 numeric vector
#' @noRd
.p_mann_whitney <- function(x, y, paired = FALSE, otherwise = NA_real_, hypothesis_type = c("two.sided", "less", "greater"), correct_p = TRUE, ...) {
  hypothesis_type <- match.arg(hypothesis_type, choices = c("two.sided", "less", "greater"))
  correction <- 0
  if (paired) {
    x <- x - y
    zeros <- any(x == 0)
    if (zeros) {
      x <- x[x != 0]
    }
    n <- length(x)
    exact <- n < 50
    r <- rank(abs(x))
    statistic <- sum(r[x > 0])
    ties <- length(r) != n_unique(r, na.rm = FALSE)
    if (exact && !ties && !zeros) {
      switch(hypothesis_type,
        two.sided = {
          p <- if (statistic > n*(n + 1L)/4) {
            stats::psignrank(statistic - 1, n, lower.tail = FALSE)
          } else {
            stats::psignrank(statistic, n)
          }
          min(2*p, 1)
        },
        greater = stats::psignrank(statistic - 1, n, lower.tail = FALSE),
        less = stats::psignrank(statistic, n)
      )
    } else {
      n_ties <- table(r)
      z <- statistic - n*(n + 1L)/4
      sig <- sqrt(n*(n + 1L)*(2*n + 1L)/24 - sum(n_ties*n_ties*n_ties - n_ties)/48)
      if (correct_p) {
        correction <- switch(hypothesis_type, two.sided = sign(z)*0.5, greater = 0.5, less = -0.5)
      }
      z <- (z - correction)/sig
      switch(hypothesis_type,
        less = stats::pnorm(z),
        greater = stats::pnorm(z, lower.tail = FALSE),
        two.sided = 2*min(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
      )
    }
  } else {
    r <- rank(c(x, y))
    x_length <- length(x)
    y_length <- length(y)
    exact <- x_length < 50L && y_length < 50L
    statistic <- sum(r[seq_len(x_length)]) - x_length*(x_length + 1L)/2
    ties <- length(r) != n_unique(r, na.rm = FALSE)
    if (exact && !ties) {
      switch(hypothesis_type,
        two.sided = {
          p <- if (statistic > x_length*y_length/2) {
            stats::pwilcox(statistic - 1, x_length, y_length, lower.tail = FALSE)
          } else {
            stats::pwilcox(statistic, x_length, y_length)
          }
          min(2*p, 1)
        },
        greater = stats::pwilcox(statistic - 1, x_length, y_length, lower.tail = FALSE),
        less = stats::pwilcox(statistic, x_length, y_length)
      )
    } else {
      n_ties <- table(r)
      j <- x_length*y_length/2
      z <- statistic - j
      xy <- x_length + y_length
      sig <- sqrt(j/6*((xy + 1L) - sum(n_ties*n_ties*n_ties - n_ties)/(xy*(xy - 1L))))
      if (correct_p) {
        correction <- switch(hypothesis_type, two.sided = sign(z)*0.5, greater = 0.5, less = -0.5)
      }
      z <- (z - correction)/sig
      switch(hypothesis_type,
        less = stats::pnorm(z),
        greater = stats::pnorm(z, lower.tail = FALSE),
        two.sided = 2*min(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
      )
    }
  }
}

# P by normality ----------------------------------------------------------

#' P value for comparison using test as determined by application of normality and variance tests
#'
#' @rdname p_ttest
#' @param normality_test Function used to determine normality. Must take numeric vector and return P value as length 1 numeric vector. Default is `p_shapiro`
#' @returns Length 1 numeric vector containing P value
#' @export
p_by_normality <- function(
    df,
    formula = NULL,
    y = NULL,
    x = NULL,
    id = NULL,
    paired = FALSE,
    welch = NULL,
    normality_test = p_shapiro,
    variance_test = p_F_test,
    hypothesis_type = "two.sided",
    otherwise = NA_real_,
    ...) {
  if (is.null(formula)) {
    formula <- create_formula(y, x)
  }
  normality <- is_normal(df = df, formula = formula, normality_test = normality_test)
  if (!is.na(normality) && normality) {
    p_ttest(df = df, formula = formula, id = id, welch = welch, paired = paired, otherwise = otherwise, ...)
    } else {
      p_mann_whitney(df = df, formula = formula, id = id, paired = paired, variance_test = variance_test, hypothesis_type = hypothesis_type, otherwise = otherwise, ...)
  }
}

#' P values as determined by normality test
#'
#' @param x,y Values in group 1 and 2 respectively. If paired, order of x and y must be the same and their lengths must be equal. Must have missing values removed
#' @param x_normality,y_normality P values of normality tests for x and y respectively
#' @param normality_test Function used to determine normality. Must take numeric vector and returns P value. Default is `p_shapiro`
#' @param variance_test Function used to perform variance test. Must take 2 numeric vectors and return P value. Default is `.p_F_test`
#' @param welch If `TRUE` (default), Welch's correction is applied (assumes variance is not equal between groups), otherwise student's t-test is performed
#' @param paired If `FALSE` (default), unpaired test is performed
#' @param hypothesis_type Type of hypothesis test to perform. Options: `two.sided` (default), `less`, `greater.` Enter as quoted hypothesis test type
#' @param otherwise Output if test fails. Default is `NA_real_`
#' @param ... Not used
#' @returns Length 1 numeric vector containing P value
#' @noRd
.p_by_normality <- function(
    x,
    y,
    x_normality = NULL,
    y_normality = NULL,
    normality_test = p_shapiro,
    variance_test = .p_F_test,
    welch = NULL,
    paired = FALSE,
    hypothesis_type = "two.sided",
    otherwise = NA_real_,
    ...) {
  if (length(x) < 2L || length(y) < 2L) {
    otherwise
  } else if (normality_test(x) < 0.05 || normality_test(y) < 0.05) {
    .p_mann_whitney(x = x, y = y, paired = paired, hypothesis_type = hypothesis_type, otherwise = otherwise)
  } else {
    .p_ttest(x = x, y = y, welch = welch, paired = paired, variance_test = variance_test, hypothesis_type = hypothesis_type, otherwise = otherwise)
  }
}

# ANOVA -------------------------------------------------------------------

#' One-way ANOVA with or without Welch's correction
#'
#' Functionality from `stats::oneway.test`
#' @param df Data frame
#' @param formula Enter as y ~ x (continuous variable ~ grouping variable)
#' @param x,y Columns containing continuous values for each group. Enter as length 1 character vectors
#' @param na.rm If `TRUE` (default), missing values for grouping variable are not considered as a distinct group
#' @param welch If `NULL` (default), variance testing is used to determine whether Welch's correction should be applied. If `TRUE`, Welch's correction is performed. If `FALSE`, regular ANOVA performed
#' @param variance_test Test used to determine whether variance differs by group. Default is `p_levene`
#' @param otherwise Output if ANOVA fails. Default is `NA_real_`
#' @param ... Not used
#' @returns Length 1 numeric vector containing P value
#' @export
p_anova <- function(
    df,
    formula = NULL,
    y = NULL,
    x = NULL,
    na.rm = TRUE,
    welch = NULL,
    variance_test = p_levene,
    otherwise = NA_real_,
    ...) {
  if (is.null(formula)) {
    formula <- create_formula(y, x)
  } else {
    y <- all.vars(formula)
    x <- y[2L]
    y <- y[1L]
  }
  df <- remove_na(df, c(if (na.rm) x, y))
  x_values <- .subset2(df, x)
  g <- df[[x]] <- if (is.factor(x_values)) .droplevels(x_values, na.rm = na.rm) else factor(x_values)
  variance_test <- match.fun(variance_test)
  welch <- welch %||% (variance_test(df = df, formula = formula, otherwise = 0) < 0.05)
  y <- .subset2(df, y)
  n_groups <- length(attr(g, "levels"))
  if (n_groups < 2L) return(otherwise)
  y_split <- split.default(y, f = g)
  y_length <- lengths(y_split, use.names = FALSE)
  if (any(y_length < 2L)) return(otherwise)
  y_mean <- vapply(y_split, mean.default, numeric(1), USE.NAMES = FALSE)
  y_variance <- vapply(y_split, Var, numeric(1), USE.NAMES = FALSE)
  if (welch) {
    z <- y_length/y_variance
    z_sum <- sum(z)
    j <- sum((1 - z/z_sum)^2/(y_length - 1L))/(n_groups*n_groups - 1L)
    m <- sum(z*y_mean)/z_sum
    statistic <- sum(z*(y_mean - m)^2)/((n_groups - 1L)*(1 + 2*(n_groups - 2L)*j))
    tryCatch(stats::pf(statistic, n_groups - 1L, 1/(3*j), lower.tail = FALSE), error = function(e) otherwise)
  } else {
    n <- sum(y_length)
    statistic <- (sum(y_length*(y_mean - mean.default(y))^2)/(n_groups - 1L))/(sum((y_length - 1L)*y_variance)/(n - n_groups))
    tryCatch(stats::pf(statistic, n_groups - 1L, n - n_groups, lower.tail = FALSE), error = function(e) otherwise)
  }
}

# Kruskal-Wallis ----------------------------------------------------------

#' Kruskal-Wallis rank sum test
#'
#' @rdname p_anova
#' @export
p_kruskal <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE, otherwise = NA_real_, ...) {
  values <- .df_to_split_vals(df, formula = formula, x = x, y = y, excl_group_na = na.rm)
  y <- values$y
  n <- length(y)
  if (n < 2L) return(otherwise)
  y_split <- values$y_grouped
  n_groups <- length(y_split)
  if (n_groups < 2L) return(otherwise)
  n_per_group <- lengths(y_split, use.names = FALSE)
  y_ranks <- rank(y)
  z <- table(y)
  statistic <- vapply(split.default(y_ranks, f = values$x), sum, numeric(1), USE.NAMES = FALSE)
  statistic <- sum(statistic*statistic/n_per_group)
  statistic <- (12*statistic/(n*(n + 1L)) - 3*(n + 1L))/(1 - sum(z*z*z - z)/(n*n*n - n))
  tryCatch(stats::pchisq(statistic, n_groups - 1L, lower.tail = FALSE), error = function(e) otherwise)
}

# Dunn test ---------------------------------------------------------------

#' Dunn test
#'
#' @inheritParams compare_means
#' @param p_adj_method Method for p value adjustment. Options: `"BH"` (default), `"Holm"`, `"hochberg"`, `"hommell"`, `"bonferroni"`, `"BY"`, `"fdr"`, `"none"`
#' @returns Data frame with similar output to `compare_means`
#' @export
p_dunn <- function(
    df,
    formula,
    ...,
    x = NULL, y = NULL,
    p_adj_method = "BH",
    ns_symbol = "ns",
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
    )) {
  vars <- formula2vars(formula, x = get_input(x), y = get_input(y), parent_fn = "p_dunn")
  grouping_var <- vars$x
  continuous_var <- vars$y

  # Create character vector containing names of subgrouping variables
  if (n_dots(...) != 0L) {
    is_input_vector <- tryCatch(is.atomic(c(...)), error = function(e) FALSE)
    subgroups <- Intersect(if (is_input_vector) c(...) else dots_as_quoted(...), names(df))
    if (length(subgroups) == 0L) {
      subgroups <- NULL
    }
  } else {
    subgroups <- NULL
  }

  # Remove missing values
  df <- remove_na(df[c(continuous_var, grouping_var, subgroups)])
  df <- .droplevels_df(df, na.rm = TRUE)

  # Nested statistical analysis
  suppress({
    pval_table <- dplyr::group_by(df, !!!lapply(subgroups, as.name)) |>
      tidyr::nest() |>
      dplyr::mutate(p_val_table = lapply(data, function(.x) .p_dunn_helper(.df = .x, .continuous_var = continuous_var, .group = grouping_var, .p_adj_method = p_adj_method, .summary_fns = summary_fns))) |>
      dplyr::select(-data) |>
      tidyr::unnest(p_val_table) |>
      dplyr::ungroup()
  })

  # Add column for comparisons and significance labels
  pval_table <- dplyr::mutate(pval_table,
                              grouping_var = grouping_var,
                              p_adj_label = sig_stars(p, symbols = c("****", "***", "**", "*",  ns_symbol)),
                              greater = dplyr::case_when(
                                mean_1 > mean_2 ~ paste0(Group1, " > ", Group2),
                                mean_2 > mean_1 ~ paste0(Group2, " > ", Group1),
                                mean_1 == mean_2 ~ paste0(Group1, " = ", Group2)))

  pval_table <- dplyr::select(pval_table, dplyr::any_of(subgroups), Group1, Group2, greater, p_adj, p_adj_label, p, n_1, n_2, median_1, median_2, mean_1, mean_2, q1_1, q1_2, q3_1, q3_2, sd_1, sd_2, se_1, se_2, min_1, min_2, max_1, max_2, ci_1, ci_2, dplyr::everything())
  pval_table[order(pval_table$p_adj), , drop = FALSE]
}

#' Helper function to perform Dunn's test
#'
#' @param .df Data frame
#' @param .continuous_var Column in `.df` containing continuous variable to compare between levels of grouping variable. Enter as length 1 character vector
#' @param .group Column in `.df` containing grouping variable levels. Enter as length 1 character vector
#' @param .p_adj_method Method for P value adjustment. Enter as length 1 character vector
#' @param .summary_fns Named list of summary functions. Enter as list(col_name = fn_name)
#' @returns Data frame with columns "Group1", "Group2", "p", "n_1", "n_2", and columns for each function specified in `.summary_fns`. Used by `p_dunn`
#' @noRd
.p_dunn_helper <- function(
    .df,
    .continuous_var,
    .group,
    .p_adj_method = "BH",
    .summary_fns = list(
      mean = Mean,
      median = Median,
      sd = SD,
      se = SE,
      q1 = Q1,
      q3 = Q3,
      ci = CI,
      min = Min,
      max = Max
    )) {
  y <- .subset2(.df, .continuous_var)
  g <- factor(.subset2(.df, .group))
  y_rank <- rank(y)
  y_rank_split <- split.default(y_rank, g)
  if (length(y_rank_split) < 2L) return(NULL)
  group_sizes <- lengths(y_rank_split, use.names = TRUE)
  unique_groups <- names(group_sizes)
  mean_ranks <- vapply(y_rank_split, mean.default, numeric(1), USE.NAMES = TRUE, na.rm = TRUE)
  n <- length(y)
  y_rank_sorted <- sort.int(y_rank)
  idx <- 1
  tiesum <- 0
  while (idx <= n) {
    val <- y_rank_sorted[idx]
    nt <- length(y_rank_sorted[y_rank_sorted == val])
    idx <- idx + nt
    if (nt > 1L) {
      tiesum <- tiesum + nt*nt*nt - nt
    }
  }
  A <- n*(n + 1L)/12 - tiesum/(12*(n - 1L))
  df_pval <- purrr::pmap_dfr(.combos_1_vec_as_df(unique_groups, n = 2), function(.x, .y) {
    n1 <- group_sizes[.x]
    n2 <- group_sizes[.y]
    z <- abs(mean_ranks[.x] - mean_ranks[.y])/sqrt(A*(1/n1 + 1/n2))
    list(Group1 = .x, Group2 = .y, p = 2*pnorm(abs(z), lower.tail = FALSE), n_1 = n1, n_2 = n2)
  })
  df_pval$p_adj <- p_adjust(df_pval$p, method = .p_adj_method)

  df_summary <- dplyr::summarize(dplyr::group_by(.df, as.character(.data[[.group]])), dplyr::across(.cols = dplyr::all_of(.continuous_var), .fns = .summary_fns, .names = "{.fn}"))
  names(df_summary)[1L] <- "Group1"
  df_pval <- dplyr::left_join(df_pval, df_summary, by = "Group1")
  dplyr::left_join(df_pval, df_summary, by = c("Group2" = "Group1"), suffix = c("_1", "_2"))
}

# Tukey test --------------------------------------------------------------

#' aov
#'
#' @inheritParams Lm
#' @export
Aov <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE) {
  structure(Lm(df, formula = formula, y = y, x = x, na.rm = na.rm), class = c("aov", "lm"), projections = NULL)
}

#' Tukey's HSD (honestly significant difference)
#'
#' @rdname p_anova
#' @returns Data frame with comparisons similar to output from `compare_means` and `p_dunn`
#' @export
p_tukey <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE, variance_test = p_F_test, ...) {
  vars <- get_vars_formula(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "p_tukey")
  formula <- vars$formula
  grouping_var <- vars$x
  continuous_var <- vars$y
  df <- remove_na(df, c(if (na.rm) grouping_var, continuous_var))
  y <- .subset2(df, continuous_var)
  x <- .subset2(df, grouping_var)
  g <- as.character(x)
  if (length(unique(g)) < 2L) return(NULL)
  g <- gsub("-", "___@@@___", g, fixed = TRUE)

  df_summary <- dplyr::group_by(df, as.character(.data[[grouping_var]]))
  df_summary <- dplyr::summarize(df_summary, dplyr::across(.cols = continuous_var, .fns = list(n = N, mean = Mean, median = Median, sd = SD, se = SE, q1 = Q1, q3 = Q3, ci = CI, min = Min, max = Max), .names ="{.fn}"))
  pval_kruskal <- p_kruskal(df, formula = formula)
  pval_var <- variance_test(df, formula = formula)
  pval_anova_welch <- p_anova(df, formula = formula, welch = TRUE)
  anova_model <- tryNULL(Aov(df = df, formula = formula))
  pval_anova <- if (is.null(anova_model)) NA_real_ else summary(anova_model)[[1L]]$`Pr(>F)`[1L]
  anova_model$call <- quote(aov(formula = formula, data = df))
  #df_tukey <- TukeyHSD(anova_model)[[1L]]
  df_tukey <- .tukey_summary(anova_model, continuous_var = continuous_var, grouping_var = grouping_var)
  df_tukey$p_adj_label <- sig_stars(df_tukey$p_adj)
  df_tukey <- df_tukey[c("grouping_var", "Group1", "Group2", "p_adj", "p_adj_label", "diff", "ci_lower", "ci_upper")]
  df_tukey$p_kruskal <- pval_kruskal
  df_tukey$p_anova <- pval_anova
  df_tukey$p_anova_welch <- pval_anova_welch
  df_tukey$p_var_test <- pval_var
  df_tukey$variance_test <- if (identical(variance_test, p_levene)) {
    "Levene"
  } else if (identical(variance_test, p_bartlett)) {
    "Bartlett"
  } else {
    "F-test"
  }
  # Add columns for summary statistics
  names(df_summary)[1L] <- "Group1"
  df_tukey <- dplyr::left_join(df_tukey, df_summary, by = "Group1")
  df_tukey <- dplyr::left_join(df_tukey, df_summary, by = c("Group2" = "Group1"), suffix = c("_1", "_2"))
  df_tukey$Group1 <- gsub("___@@@___", "-", df_tukey$Group1, fixed = TRUE)
  df_tukey$Group2 <- gsub("___@@@___", "-", df_tukey$Group2, fixed = TRUE)
  df_tukey
}

# Helpers -----------------------------------------------------------------

#' Create paired values
#'
#' @param df Data frame
#' @param y Continuous variable.Enter as length 1 character vector
#' @param x Categorical variable (should have 2 levels). Enter as length 1 character vector
#' @param id Identification variable. Enter as length 1 character vector
#' @returns List of length 2 containing paired vectors y1 and y2. Used by `p_ttest`, `p_mann_whitney`
#' @noRd
.paired_values <- function(df, y = NULL, x = NULL, id = NULL) {
  df <- df[c(y, x, id)]
  null_id <- is.null(id)
  null_x <- is.null(x)
  if (!null_id && !null_x) {
    names(df) <- c("y", "x", "id")
  } else if (null_id && !null_x) {
    names(df) <- c("y", "x")
    df <- df[order(df$x), ]
    x_unique <- unique(df$x)
    x_unique <- x_unique[!is.na(x_unique)]
    if (length(x_unique) != 2L) return(NULL)
    y1 <- df$y[df$x == x_unique[1L]]
    y2 <- df$y[df$x == x_unique[2L]]
    n_1 <- length(y1)
    n_2 <- length(y2)
    if (n_1 != n_2) return(NULL)
    df$id <- rep(seq_len(n_1), 2)
    message("Paired t-test requested but no id variable entered. Will assume order of individuals (rows) in group 1 matches that of group 2")
  } else if (null_x && !null_id) {
    names(df) <- c("y", "id")
    df <- df[order(df$id), , drop = FALSE]
    n <- length(df$y)
    if (!is_even(n)) return(NULL)
    df$x <- rep(seq_len(2), n)
    message("Paired t-test requested but 'x' (grouping variable) not provided. Unclear which values should are pre vs. post. Will assume order of values for each individual is pre before post")
  } else {
    return(NULL)
  }
  df <- remove_na(tidyr::pivot_wider(id_cols = "id", names_from = "x", values_from = "y"))
  list(y1 = .subset2(df, 2L), y2 = .subset2(df, 3L))
}

#' Extract summary table from Tukey's HSD
#'
#' Rewritten version of `stats::TukeyHSD`
#' @param x aov object
#' @param continuous_var,grouping_var Names of continuous and grouping variables, respectively. Enter each as length 1 character vector
#' @param ci Confidence level. Enter as length 1 numeric numeric 0-1. Default is `0.95`
#' @returns Data frame with columns "Group1", "Group2", "diff", "ci_lower", "ci_upper", "p_adj". Used by `p_tukey`
#' @noRd
.tukey_summary <- function(x, continuous_var, grouping_var, ci = 0.95) {
  df <- stats::model.frame(x)
  continuous_var <- split.default(.subset2(df, continuous_var), .subset2(df, grouping_var))
  n_groups <- length(continuous_var)
  group_names <- names(continuous_var)
  group_means <- vapply(continuous_var, mean.default, numeric(1), USE.NAMES = FALSE)
  inv_sum <- 1/lengths(continuous_var, use.names = FALSE)
  inv_sum <- outer(inv_sum, inv_sum, "+")
  center <- outer(group_means, group_means, "-")
  idx <- lower.tri(center)
  center <- center[idx]
  d <- x$df.residual
  r <- x$residuals
  z <- sqrt(inv_sum*sum(r*r)/d/2)[idx]
  width <- stats::qtukey(ci, n_groups, d)*z
  p <- stats::ptukey(abs(center/z), n_groups, d, lower.tail = FALSE)
  comps <- outer(group_names, group_names, paste, sep = "___@@@___")[idx]
  comps <- do.call(rbind, strsplit(comps, split = "___@@@___", fixed = TRUE))
  dimnames(comps) <- list(NULL, c("Group1", "Group2"))
  comps <- cbind(grouping_var = grouping_var, comps)
  comps <- matrix_to_df(comps)
  cbind(comps, array(c(center, center - width, center + width, p), c(length(width), 4L), list(NULL, c("diff", "ci_lower", "ci_upper", "p_adj"))))
}
