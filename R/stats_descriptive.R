#' Number of non-missing values
#'
#' @param x Vector
#' @returns Length 1 numeric vector
#' @export
N <- function(x) sum(!is.na(x))

#' Number of missing values
#'
#' @rdname N
#' @export
N_na <- function(x) sum(is.na(x))

#' Percent of values that are true or 1
#'
#' @rdname N
#' @export
Perc <- function(x) Sum(x)/N(x)

#' Percent of values that are missing
#'
#' @rdname N
#' @export
Perc_na <- function(x) mean.default(is.na(x))

#' Sum
#'
#' @rdname N
#' @export
Sum <- function(x) sum(x, na.rm = TRUE)

#' Median
#'
#' Functionality from `stats::median`
#' @rdname N
#' @export
Median <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) return(NaN)
  z <- (n + 1L) %/% 2L
  if (n %% 2L == 1L) {
    sort.int(x, partial = z)[z]
  } else {
    z <- z + c(0L, 1L)
    mean.default(sort.int(x, partial = z)[z])
  }
}

#' Mean
#'
#' @rdname N
#' @export
Mean <- function(x) mean.default(x, na.rm = TRUE)

#' Min
#'
#' @rdname N
#' @export
Min <- function(x) {
  if (length(x) == 0L) return(NaN)
  min(x, na.rm = TRUE)
}

#' Max
#'
#' @rdname N
#' @export
Max <- function(x) {
  if (length(x) == 0L) return(NaN)
  max(x, na.rm = TRUE)
}

#' Variance
#'
#' @rdname N
#' @export
Var <- function(x) var(x, na.rm = TRUE, use = "na.or.complete")

#' Standard deviation
#'
#' @rdname N
#' @export
SD <- function(x) sqrt(Var(x))

#' Standard error
#'
#' @rdname N
#' @export
SE <- function(x) sqrt(Var(x)/N(x))

#' Coefficient of variation
#'
#' @rdname N
#' @export
CV <- function(x) SD(x)/Mean(x)*100

#' Range
#'
#' @param x Numeric or logical vector
#' @param finite If `TRUE` (default), `Inf` (and `-Inf`) are removed from `x` prior to determining range
#' @returns Length 2 numeric vector
#' @export
Range <- function(x, finite = TRUE) {
  x <- if (finite) x[is.finite(x)] else x[!is.na(x)]
  if (length(x) == 0L) return(c(NaN, NaN))
  c(min(x), max(x))
}

#' Difference between values in a vector
#'
#' Functionality from `base::diff`
#' @param x Numeric vector (not dates, time series, or matrices)
#' @param lag Difference in position between numbers to be compared. Enter as integer. Default is `1L`
#' @returns Numeric vector with length equal to `length(x) - lag`
#' @export
Diff <- function(x, lag = 1L) {
  n <- length(x)
  #if (lag >= n) return(x[0L])
  x[-seq_len(lag)] - x[-n:-(n - lag + 1L)]
}

#' Quantile
#'
#' Functionality from `stats::quantile`
#' @param x Numeric vector
#' @param probs Quantiles. Enter as numeric 0-1. Default uses min, Q1, median, Q3, max
#' @param prism If `TRUE`, calculation performed using same algorithm used by Prism. Default is `FALSE`
#' @returns Numeric vector with length equal to the length of `probs`
#' @export
Quantile <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1), prism = FALSE) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0L) return(rep_len(NA_real_, length.out = length(probs)))
  if (prism) {
    fuzz <- 4*.Machine$double.eps
    nppm <- probs*(n + 1)
    j <- floor(nppm + fuzz)
    h <- nppm - j
    sml <- abs(h) < fuzz
    if (any(sml)) {
      h[sml] <- 0
    }
    x <- sort.int(x, partial = unique.default(c(1, j[j > 0L & j <= n], (j + 1)[j > 0L & j < n], n)))
    x <- c(x[1L], x[1L], x, x[n], x[n])
    z <- x[j + 2L]
    z[h == 1] <- x[j + 3L][h == 1]
    other <- (h > 0) & (h < 1) & (x[j + 2L] != x[j + 3L])
    if (any(other)) {
      z[other] <- ((1 - h)*x[j + 2L] + h*x[j + 3L])[other]
    }
    z
  } else {
    index <- 1 + (n - 1)*probs
    lo <- floor(index)
    hi <- ceiling(index)
    x <- sort.int(x, partial = unique.default(c(lo, hi)))
    z <- x[lo]
    i <- which(index > lo & x[hi] != z)
    h <- (index - lo)[i]
    z[i] <- (1 - h)*z[i] + h*x[hi[i]]
    z
  }
}

#' 25th percentile
#'
#' @rdname Quantile
#' @export
Q1 <- function(x) Quantile(x, probs = 0.25)

#' 75th percentile
#'
#' @rdname Quantile
#' @export
Q3 <- function(x) Quantile(x, probs = 0.75)

#' IQR
#'
#' @rdname Quantile
#' @returns Length 2 numeric vector containing 25th and 75th percentile values
#' @export
iqr <- function(x) Quantile(x, probs = c(0.25, 0.75))

#' Confidence interval difference from mean
#'
#' CI = z*SE
#' @param x Numeric vector
#' @param ci Type of confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @returns Length 1 numeric vector containing the absolute difference between mean and the upper (or lower) bound of CI
#' @export
CI <- function(x, ci = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  #if (n == 0L) return(NA_real_)
  if (n < 2L) return(NA_real_)
  stats::qt(0.5 + ci/2, n - 1)*SD(x)/sqrt(n)
}

#' Confidence interval for proportion
#'
#' Functionality from Frank Harrell's excellent package HMisc
#' @param n Numerator of proportion. Enter as length 1 integer
#' @param total Denominator of proportion. Enter as length 1 integer
#' @param ci Type of confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param method Method for calculating confidence interval. Options include: `"wilson"` (default, used by Prism), `"wald"`, `"wald_corrected"`, `"agresti_coull"`, `"exact"`, `"asymptotic"`, `"all"`
#' @returns Length 3 numeric vector containing proportion, lower bound of CI, and upper bound of CI (in that order). If `method = "all"`, output is a matrix with 1 row for each method
#' @export
CI_prop <- function(n, total, ci = 0.95, method = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic", "all")) {
  method <- match.arg(method, choices = c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "exact", "asymptotic", "all"))
  l <- 0.5 + ci/2
  p <- n/total
  switch(method,
    wilson = {
      z_crit <- stats::qnorm(l)
      z2 <- z_crit*z_crit
      ci_wilson <- (p + z2/2/total + c(-1, 1)*z_crit*sqrt((p*(1 - p) + z2/4/total)/total))/(1 + z2/total)
      if (n == 1) {
        ci_wilson[1L] <- -log(ci)/total
      }
      if (n == total - 1) {
        ci_wilson[2L] <- 1 + log(ci)/total
      }
      c(p, ci_wilson)
    },
    exact = {
      nu2 <- 2*n
      nu1 <- 2*total - nu2 + 2
      ll_exact <- if (n > 0) n/(n + stats::qf(l, nu1, nu2)*(total - n + 1)) else 0
      z <- if (n < total) stats::qf(l, nu2 + 2, nu1 - 2) else 1
      ul_exact <- (n + 1)*z/(total - n + (n + 1)*z)
      c(p, ll_exact, ul_exact)
    },
    wald = {
      z_crit <- stats::qnorm(l)
      z <- z_crit*sqrt(p*(1 - p)/total)
      c(p, max(0, p - z), min(1, p + z))
    },
    wald_corrected = {
      z_crit <- stats::qnorm(l)
      z <- z_crit*sqrt(p*(1 - p)/total) + 0.5/n
      c(p, max(0, p - z), min(1, p + z))
    },
    asymptotic = {
      z_crit <- stats::qnorm(l)
      c(p, p + z_crit*sqrt(p*(1 - p)/total)*c(-1, 1))
    },
    agresti_coull = {
      z_crit <- stats::qnorm(l)
      z2 <- z_crit*z_crit
      n_ac <- n + z2/2
      total_ac <- total + z2
      p_ac <- n_ac/total_ac
      z <- z_crit*sqrt(p_ac*p_ac*(1 - p_ac)/total_ac)
      c(p_ac, max(0, p_ac - z), min(1, p_ac + z))
    },
    {
      y <- lapply(c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "asymptotic"), function(x) CI_prop(n = n, total = total, ci = ci, method = x))
      y <- matrix_to_df(Reduce(rbind, y))
      names(y) <- c("prop", "ci_lower", "ci_upper")
      y$method <- c("wilson", "exact", "wald", "wald_corrected", "agresti_coull", "asymptotic")
      y[c("method", "prop", "ci_lower", "ci_upper")]
    }
  )
}

# Geometric summary statistics --------------------------------------------

#' Geometric mean
#'
#' @param x Numeric vector
#' @param log_fn Function used to log transform data. Default is `log1p`. Prism uses `log10`
#' @param exp_fn Function used to back transform log data. Default is `expm1`. Prism uses `function(x) 10^x`
#' @returns Length 1 numeric vector
#' @export
geometric_mean <- function(x, log_fn = log1p, exp_fn = expm1) {
  x <- log_fn(x[is.finite(x)])
  x <- mean.default(x)
  exp_fn(x)
}

#' Geometric standard deviation
#'
#' @rdname geometric_mean
#' @export
geometric_sd <- function(x, log_fn = log1p, exp_fn = expm1) {
  x <- log_fn(x[is.finite(x)])
  x <- SD(x)
  exp_fn(x)
}

#' Geometric standard error
#'
#' @rdname geometric_mean
#' @export
geometric_se <- function(x, log_fn = log1p, exp_fn = expm1) {
  x <- log_fn(x[is.finite(x)])
  x <- sqrt(Var(x)/length(x))
  exp_fn(x)
}

#' Geometric confidence interval
#'
#' @rdname geometric_mean
#' @param ci Confidence level. Default is `0.95`
#' @export
geometric_ci <- function(x, log_fn = log1p, exp_fn = expm1, ci = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  x <- log_fn(x)
  exp_fn(stats::qt(0.5 + ci/2, n - 1)*sqrt(Var(x)/n))
}
