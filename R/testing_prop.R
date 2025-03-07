#' P value from either chi-squared or Fisher's exact test depending on group size
#'
#' @param x Matrix or table containing counts. If `x` is a vector of counts, enter as tn, fn, fp, tp. Values entered into matrix by column. Middle 2 numbers must share true/false designation (i.e. either tp and tn or fp and fn)
#' @param test Function for testing. Options: NULL (function determined by group size), `p_chi`, `p_fisher`
#' @param n_min_chisq Minimum group size that will be allowed for chi-squared test. Default is `5`. Only relevant when test is `NULL`
#' @param otherwise Value to return if P value can't be determined
#' @param ... Arguments passed to `p_chi()` or `p_fisher()`
#' @returns Length 1 numeric vector containing P value
#' @export
p_prop <- function(x, test = NULL, n_min_chisq = 5, otherwise = NA_real_, ...) {
  test_fn <- test %||% if (min(x) >= n_min_chisq) p_chi else p_fisher
  test_fn(x, otherwise = otherwise, ...)
}

#' P value for Pearson's Chi-squared test
#'
#' Functionality from `stats::chisq.test`
#' @rdname p_prop
#' @param yates If `TRUE` and `x` is 2 x 2, Yates' continuity correction is applied
#' @export
p_chi <- function(x, yates = FALSE, otherwise = NA_real_, ...) {
  if (anyNA(x)) return(otherwise)
  x <- if (is.matrix(x)) x else matrix(x, nrow = 2)
  dims <- dim(x)
  nrows <- dims[1L]
  ncols <- dims[2L]
  deg_free <- (nrows - 1)*(ncols - 1)
  sum_rows <- rowSums(x)
  sum_cols <- colSums(x)
  E <- tcrossprod(sum_rows, sum_cols)/sum(x)
  z <- abs(x - E)
  yates <- if (yates && nrows == 2 && ncols == 2) min(0.5, z) else 0
  X2 <- sum((z - yates)^2/E)
  tryCatch(stats::pchisq(X2, deg_free, lower.tail = FALSE), error = function(e) otherwise)
}

#' P value for Fisher's exact test
#'
#' @rdname p_prop
#' @export
p_fisher <- function(x, otherwise = NA_real_, ...) {
  x <- if (is.matrix(x)) x else matrix(x, nrow = 2)
  tryCatch(suppressWarnings(stats::fisher.test(x, ...)$p.value), error = function(e) otherwise)
}

# Proportion test ---------------------------------------------------------

#' P value for proportion test
#'
#' Functionality from `stats::prop.test`
#' @param x Vector containing number of positive individuals, 1D table with 2 entries, or table (or matrix) with 2 columns containing number positive in 1 column and number negative in other column
#' @param n If `x` is a vector, `n` must be a vector of equal length and contain the corresponding total number of individuals
#' @param ci Confidence level. Default is `0.95`
#' @param hypothesis_type Type of hypothesis test. Options: `"two.sided"` (default), `"less"`, `"greater"`
#' @param yates If `FALSE` (default), Yates' correction not applied
#' @param otherwise Value to return if unable to run test. Default is `NA_real_`
#' @returns Length 1 numeric vector containing P value
#' @export
p_prop_test <- function(x, n, ci = 0.95, hypothesis_type = "two.sided", yates = FALSE, otherwise = NA_real_) {
  tryCatch(.prop_test(x = x, n = n, ci = ci, hypothesis_type = hypothesis_type, yates = yates, otherwise = otherwise)$p.value, error = function(e) otherwise)
}

#' Proportion test
#'
#' Functionality from `stats::prop.test`
#' @param x Vector containing number of positive individuals, 1D table with 2 entries, or table (or matrix) with 2 columns containing number positive in 1 column and number negative in other column
#' @param n If x is a vector, n must be a vector of equal length and contain the corresponding total number of individuals
#' @param ci Confidence level. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param hypothesis_type Type of hypothesis test. Options: `"two.sided"` (default), `"less"`, `"greater"`
#' @param yates If FALSE (default), Yates' correction not applied
#' @param otherwise Value to return if unable to run test. Default is `NA_real_`
#' @returns List containing "chi_squared", "p.value", "estimate", "ci"
#' @noRd
.prop_test <- function(
    x,
    n,
    ci = 0.95,
    hypothesis_type = c("two.sided", "less", "greater"),
    yates = FALSE,
    otherwise = NA_real_) {
  dims <- dim(x)
  if (is.table(x) && length(dims) == 1L) {
    if (dims != 2L) return(otherwise)
    l <- 1
    n <- sum(x)
    x <- x[1L]
  } else if (is.matrix(x)) {
    if (dims[2L] != 2L) return(otherwise)
    l <- dims[1L]
    n <- rowSums(x)
    x <- x[, 1L]
  } else {
    l <- length(x)
    if (l != length(n)) return(otherwise)
  }
  idx_nna <- stats::complete.cases(x, n)
  x <- x[idx_nna]
  n <- n[idx_nna]
  k <- length(x)
  p <- if (k == 1L) 0.5 else NULL
  hypothesis_type <- match.arg(hypothesis_type, choices = c("two.sided", "less", "greater"))
  if (k > 2 || k == 2 && !is.null(p)) {
    hypothesis_type <- "two.sided"
  }
  estimate <- x/n
  conf_int <- NULL
  yates <- if (yates && k <= 2) 0.5 else 0
  if (k == 1) {
    z <- stats::qnorm(if (hypothesis_type == "two.sided") 0.5 + ci/2 else ci)
    yates <- min(yates, abs(x - n*p))
    j <- 0.5*z*z/n
    m <- estimate + yates/n
    u <- if (m >= 1) 1 else (m + j + z*sqrt(m*(1 - m)/n + j/(2*n)))/(1 + 2*j)
    m <- estimate - yates/n
    h <- if (m <= 0) 0 else (m + j - z*sqrt(m*(1 - m)/n + j/(2*n)))/(1 + 2*j)
    conf_int <- switch(hypothesis_type,
      two.sided = c(max(h, 0), min(u, 1)),
      greater = c(max(h, 0), 1),
      less = c(0, min(u, 1))
    )
  } else if (k == 2 && is.null(p)) {
    estimate_delta <- estimate[1L] - estimate[2L]
    yates <- min(yates, abs(estimate_delta)/sum(1/n))
    width <- stats::qnorm(if (hypothesis_type == "two.sided") 0.5 + ci/2 else ci)
    width <- width*sqrt(sum(estimate*(1 - estimate)/n)) + yates*sum(1/n)
    conf_int <- switch(hypothesis_type,
      two.sided = c(max(estimate_delta - width, -1), min(estimate_delta + width, 1)),
      greater = c(max(estimate_delta - width, -1), 1),
      less = c(-1, min(estimate_delta + width, 1))
    )
  }
  if (is.null(p)) {
    p <- sum(x)/sum(n)
    param <- k - 1
  } else {
    param <- k
  }
  x <- cbind(x, n - x)
  E <- cbind(n*p, n*(1 - p))
  statistic <- sum((abs(x - E) - yates)^2/E)
  if (hypothesis_type == "two.sided") {
    p_value <- stats::pchisq(statistic, param, lower.tail = FALSE)
  } else {
    z <- if (k == 1) sign(estimate - p)*sqrt(statistic) else sign(estimate_delta)*sqrt(statistic)
    p_value <- stats::pnorm(z, lower.tail = hypothesis_type == "less")
  }
  list(
    chi_squared = statistic,
    p.value = p_value,
    estimate = estimate,
    ci = conf_int
  )
}

# McNemar's test ----------------------------------------------------------

#' McNemar's Chi-squared test
#'
#' Functionality from `stats::mcnemar.test`
#' @param x,y Integer vector for each group
#' @param corrected If `TRUE` (default), continuity correction applied
#' @param otherwise Value returned if P value cannot be calculated. Default is `NA_real_`
#' @returns Length 1 numeric vector containing P value
#' @export
p_mcnemar <- function(x, y, corrected = TRUE, otherwise = NA_real_) {
  if (length(x) != length(y)) return(otherwise)
  idx_nna <- stats::complete.cases(x, y)
  x <- as.factor(x[idx_nna])
  y <- as.factor(y[idx_nna])
  r <- length(attr(x, "levels"))
  if (r < 2) return(otherwise)
  if (length(attr(y, "levels")) != r) return(otherwise)
  x <- table(x, y)
  b <- r*(r - 1)/2
  xt <- t.default(x)
  xxt <- x - xt
  y <- if (corrected && r == 2 && any(xxt != 0)) abs(xxt) - 1 else xxt
  x <- x + xt
  xt <- upper.tri(x)
  z <- sum(y[xt]^2/x[xt])
  tryCatch(stats::pchisq(z, b, lower.tail = FALSE), error = function(e) otherwise)
}

# Trend in proportions ----------------------------------------------------

#' Chi-squared test for trend in proportions
#'
#' Functionality from `stats::prop.trend.test`
#' @param n_positive,n_total Integer vectors containing number of positive observations and total number of observations at each time point respectively
#' @returns Length 1 numeric vector containing P value
#' @export
p_prop_test_trend <- function(n_positive, n_total) {
  score <- seq_along(n_positive)
  p <- sum(n_positive)/sum(n_total)
  w <- n_total/p/(1 - p)
  a <- stats::anova(stats::lm(freq ~ score, data = list(freq = n_positive/n_total, score = score), weights = w))
  stats::pchisq(as.numeric(a["score", "Sum Sq"]), 1, lower.tail = FALSE)
}

#' Cochrane Armitage test for trend
#'
#' Functionality from Andri Signorell's excellent package DescTools
#' @param x Table containing 2 columns (1 for each group). Rows contain values at each time point
#' @param hypothesis_type Type of hypothesis test. Options: `"two.sided"` (default), `"increasing"`, `"decreasing"`
#' @returns Length 1 numeric vector containing P value
#' @export
p_cochrane_armitage <- function(x, hypothesis_type = c("two.sided", "increasing", "decreasing")) {
  hypothesis_type <- match.arg(hypothesis_type, choices = c("two.sided", "increasing", "decreasing"))
  col1 <- x[, 1L]
  dims <- dim(x)
  idx <- seq_len(dims[1L])
  n_counts <- .rowSums(x, n_rows, dims[2L])
  n <- sum(n_counts)
  z <- idx - sum(n_counts*idx)/n
  out <- sum(col1)/n
  out <- sum(col1*z)/sqrt(out*(1 - out)*sum(n_counts*z*z))
  switch(hypothesis_type,
    two.sided = 2*stats::pnorm(abs(out), lower.tail = FALSE),
    increasing = stats::pnorm(out),
    decreasing = stats::pnorm(out, lower.tail = FALSE)
  )
}
