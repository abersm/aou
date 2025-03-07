# Shapiro-Wilk test -------------------------------------------------------

#' Shapiro-Wilk test
#'
#' `stats::shapiro.test` can't be simplified
#' @param x Numeric vector. Missing values allowed. Need at least 3 values to get a reliable result for `p_shapiro` (8 for `p_dagostino`)
#' @param otherwise Output if normality test fails. Default is `0`
#' @returns Length 1 numeric vector containing P value
#' @export
p_shapiro <- function(x, otherwise = 0) {
  tryCatch(suppressWarnings(stats::shapiro.test(x)$p.value), error = function(e) otherwise)
}

# D'Agostino-Pearson omnibus test -----------------------------------------

#' D'Agostino-Pearson omnibus test
#'
#' Functionality from fBasics package
#' @rdname p_shapiro
#' @export
p_dagostino <- function(x, otherwise = 0) {
  x <- x[!is.na(x)]
  x_len <- length(x)
  if (x_len < 8L) return(otherwise)
  z <- x - mean.default(x)
  z2 <- z*z
  s <- sqrt(mean.default(z2))
  s <- s*s*s
  a4 <- mean.default(z2*z2)/(s*s)
  n1 <- x_len + 1L
  U4 <- (a4 - 3 + 6/n1)/sqrt(24*(x_len - 2L)*(x_len - 3L)*x_len/((n1*n1)*(n1 + 2)*(n1 + 4L)))
  b  <- (3*(x_len*x_len + 27*x_len - 70L)*n1*(n1 + 2L))/((x_len - 2L)*(n1 + 4L)*(n1 + 6L)*(n1 + 8L)) - 1
  W2 <- sqrt(2*b) - 1
  a <- ((mean.default(z2*z)/s)/sqrt(6*(x_len - 2L)/(n1*(n1 + 2))))/sqrt(2/(W2 - 1))
  Z3 <- log(a + sqrt(a*a + 1))/sqrt(log(sqrt(W2)))
  B <- (6*(x_len*x_len - 5*x_len + 2L)/((n1 + 6L)*(n1 + 8L)))*sqrt((6*(n1 + 2L)*(n1 + 4L))/(x_len*(x_len - 2L)*(x_len - 3L)))
  A <- 6 + (8/B)*((2/B) + sqrt(1 + 4/(B*B)))
  pos <- ((1 - 2/A)/(1 + U4*sqrt(2/(A - 4))))^(1/3)
  Z4 <- 2/(9*A)
  Z4 <- (1 - Z4 - pos)/sqrt(Z4)
  stats::pchisq(Z3*Z3 + Z4*Z4, 2, lower.tail = FALSE)
}

# Anderson-Darling test ---------------------------------------------------

#' Anderson-Darling test
#'
#' Functionality from nortest package
#' @rdname p_shapiro
#' @export
p_anderson <- function(x, otherwise = 0) {
  x <- sort.int(x[!is.na(x)], method = "quick")
  x_len <- length(x)
  if (x_len < 8) return(otherwise)
  z <- z_score(x)
  logp1 <- stats::pnorm(z, log.p = TRUE)
  logp2 <- stats::pnorm(-z, log.p = TRUE)
  A <- -x_len - mean.default((2*seq_len(x_len) - 1L)*(logp1 + Rev(logp2)))
  AA <- (1 + 0.75/x_len + 2.25/(x_len*x_len))*A
  if (AA < 0.2) {
    1 - exp(-13.436 + 101.14*AA - 223.73*AA*AA)
  } else if (AA < 0.34) {
    1 - exp(-8.318 + 42.796*AA - 59.938*AA*AA)
  } else if (AA < 0.6) {
    exp(0.9177 - 4.279*AA - 1.38*AA*AA)
  } else if (AA < 10) {
    exp(1.2937 - 5.709*AA + 0.0186*AA*AA)
  } else {
    3.7e-24
  }
}

# Kolmogorov-Smirnov test -------------------------------------------------

#' Kolmogorov-Smirnov test
#'
#' `stats::ks.test` can't be simplified
#' Input can include missing values
#' @rdname p_shapiro
#' @export
p_ks <- function(x, otherwise = 0) tryCatch(stats::ks.test(x)$p.value, error = function(e) otherwise)

# Check for normality -----------------------------------------------------

#' Determine whether values in 2 groups are both normally distributed
#'
#' @inheritParams compare_means
#' @returns Length 1 logical vector. `TRUE` if normally distributed (all P >= 0.05), `FALSE` if not normally distributed (any P < 0.05)
#' @export
is_normal <- function(df, formula = NULL, y = NULL, x = NULL, normality_test = p_shapiro, ...) {
  if (!is.null(formula)) {
    y <- all.vars(formula)
    x <- y[2L]
    y <- y[1L]
  }
  df <- remove_na(df, c(x, y))
  p_vals <- tapply(.subset2(df, y), factor(.subset2(df, x)), normality_test, ...)
  min(p_vals) >= 0.05
}
