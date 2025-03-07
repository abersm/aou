# Adjusted P values -------------------------------------------------------

#' Adjusted P value
#'
#' Modified version of `stats::p.adjust`
#' @param x Numeric vector of unadjusted P values
#' @param method Method for P value correction. Options: `"holm"` (default), `"BH"`, `"BY"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"fdr"` (same as `"BH")`, `"none"` (no adjustment). Enter as length 1 character vector. Case doesn't matter
#' @returns Numeric vector of adjusted P values with the same length as input
#' @export
p_adjust <- function(x, method = "holm") {
  method <- tolower(method)
  if (method == "none") return(x)
  idx_nna <- !is.na(x)
  p <- x[idx_nna]
  n <- length(p)
  if (n < 3L) {
    if (n < 2L) return(x)
    if (method == "hommel") {
      method <- "hochberg"
    }
  }
  out <- rep_len(NA_real_, length(x))
  out[idx_nna] <- switch(
    method,
    bonferroni = pmin(1, n*p),
    holm = {
      z <- order(p)
      pmin(1, cummax((n + 1L - seq_len(n))*p[z]))[order(z)]
    },
    hommel = {
      z <- order(p)
      p <- p[z]
      q <- m <- rep.int(min(n*p/seq_len(n)), n)
      for (i in seq.int(from = n - 1L, to = 2L)) {
        idx1 <- seq_len(n - i + 1L)
        idx2 <- seq.int(from = n - i + 2L, to = n)
        q[idx1] <- pmin(i*p[idx1], min(i*p[idx2]/seq.int(from = 2L, to = i)))
        q[idx2] <- q[n - i + 1L]
        m <- pmax(m, q)
      }
      pmax(m, p)[order(z)]
    },
    hochberg = {
      z <- order(p, decreasing = TRUE)
      pmin(1, cummin((n + 1L - seq.int(from = n, to = 1L))*p[z]))[order(z)]
    },
    fdr = ,
    bh = {
      idx <- seq.int(from = n, to = 1L)
      z <- order(p, decreasing = TRUE)
      pmin(1, cummin(n/idx*p[z]))[order(z)]
    },
    by = {
      idx <- seq.int(from = n, to = 1L)
      z <- order(p, decreasing = TRUE)
      q <- sum(1/(seq_len(n)))
      pmin(1, cummin(q*n/idx*p[z]))[order(z)]
    },
    Stop(sprintf("In 'p_adjust', 'method' must be one of the following: %s", .quote_collapse(c("BH", "fdr", "BY", "hochberg", "hommel", "bonferroni", "none"))))
  )
  out
}

#' q value
#'
#' Functionality from `qvalue::qvalue`
#' @param x Numeric vector of P values
#' @returns Numeric vector with the same length as input
#' @export
q_value <- function(x) {
  qvals <- x
  idx_nna <- !is.na(x)
  x <- x[idx_nna]
  n <- length(x)
  lambda <- seq.int(from = 0.05, to = 0.95, by = 0.05)
  idx <- seq.int(from = 19L, to = 1L)
  # TODO: add checkSorted = FALSE in findInterval once available in new version of R
  pi0 <- cumsum(tabulate(findInterval(x, vec = lambda))[idx])/(n*(1 - lambda[idx]))
  pi0 <- min(stats::predict(stats::smooth.spline(lambda, pi0[idx], df = 3), x = lambda)$y[19L], 1)
  if (pi0 <= 0) {
    pi0 <- 1
  }
  idx <- seq.int(from = n, to = 1L)
  x_order <- order(x, decreasing = TRUE)
  qvals[idx_nna] <- pi0*pmin(1, cummin(x[x_order]*n/idx))[order(x_order)]
  qvals
}

# Formatting --------------------------------------------------------------

#' Format P values
#'
#' @param p Numeric vector containing P values
#' @param prefix Prefix for P value. Default is `"P"`
#' @param show_leading_0 If `TRUE` (default), 0 before the decimal is included in output. If `FALSE`, 0 before decimal is removed in output (for example, .01)
#' @param trim_ws If `FALSE` (default), white space is allowed in output. If `TRUE`, white space is removed from output
#' @param min_digits Number of digits included after decimal when P > 0.06. Enter as length 1 numeric. Default is `2`
#' @returns Character vector with same length as input
#' @export
format_p_value <- function(p, prefix = "P", trim_ws = FALSE, show_leading_0 = TRUE, min_digits = 2) {
  # TODO: add checkSorted = FALSE in findInterval once available in new version of R
  label <- as.character(findInterval(as.numeric(p), vec = c(0, 0.001, 0.01, 0.045, 0.06, 1), all.inside = TRUE, left.open = TRUE, rightmost.closed = TRUE))
  label[label == "1"] <- paste(prefix, "< 0.001")
  label[label == "2"] <- paste(prefix, "=", sprintf("%.3f", p[label == "2"]))
  label[label == "3"] <- paste(prefix, "=", sprintf("%.2f", p[label == "3"]))
  label[label == "4"] <- paste(prefix, "=", sprintf("%.3f", p[label == "4"]))
  label[label == "5"] <- paste(prefix, "=", sprintf(paste0("%.", min_digits, "f"), pmin(p[label == "5"], 0.99)))
  if (is.null(prefix) || !nzchar(prefix)) {
    label[grepl("= ", label, fixed = TRUE)] <- gsub("= ", "", label, fixed = TRUE)
  }
  if (trim_ws) {
    label <- gsub(" ", "", label, fixed = TRUE)
  }
  if (!show_leading_0) {
    label <- gsub("0.", ".", label, fixed = TRUE)
  }
  label
}

#' Convert P values into significance stars
#'
#' @param x Numeric vector of P values
#' @param cutpoints Cutpoints for significance stars. Enter as ordered numeric vector that contains 0 as the first value and 1 as the last value. Length should be equal to `length(symbols) + 1L`
#' @param symbols Symbols representing significance levels from lowest to highest P value. Enter as character vector of length `length(cutpoints) - 1L`
#' @returns Character vector with same length as input
#' @export
sig_stars <- function(x, cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*",  "ns")) {
  # TODO: add checkSorted = FALSE in findInterval once available in new version of R
  idx <- findInterval(as.numeric(x), vec = cutpoints, all.inside = TRUE, left.open = TRUE, rightmost.closed = TRUE)
  symbols[idx]
}
