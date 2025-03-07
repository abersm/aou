#' Crossing for 2 or more vectors
#'
#' Similar to `base::expand.grid` but faster
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @param .colnames Column names (or prefix for generating column names) in output data frame. Default is `"V"`
#' @returns Data frame with 1 column for each vector entered into `...` and each row containing unique combinations of 1 element per vector
#' @noRd
crossings <- function(..., .colnames = "V") {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  n_lists <- length(lists)
  out <- vector(mode = "list", length = n_lists)
  idx <- seq_len(n_lists)
  if (n_lists != length(.colnames)) {
    .colnames <- if (missing(.colnames) && all_named(lists)) {
      names(lists)
    } else {
      paste0(.colnames, idx)
    }
  }
  names(out) <- .colnames
  h <- 1L
  z <- lengths(lists, use.names = FALSE)
  n_total <- prod(z)
  if (n_total == 0L) {
    for (i in idx) {
      out[[i]] <- .subset(.subset2(lists, i), FALSE)
    }
  } else {
    for (i in idx) {
      l <- .subset2(lists, i)
      #n_l <- length(l)
      n_l <- .subset(z, i)
      n_total <- n_total/n_l
      l <- l[rep.int(rep.int(seq_len(n_l), rep.int(h, n_l)), n_total)]
      out[[i]] <- l
      h <- h*n_l
    }
  }
  structure(out, class = "data.frame", row.names = seq_len(prod(z)))
}

#' List of combinations
#'
#' Functionality from `utils::combn`
#' @param x Vector. `length(x)` must be > `n`
#' @param n Number of elements per group. Enter as length 1 numeric. Must be > 0. Default is `2` (pairs of elements in `x`)
#' @param na.rm If `TRUE` (default), missing values are removed from x prior to generating combinations
#' @returns List of vectors (each of length `n`) containing unique `n` element combos of values in `x`
#' @noRd
combos_list <- function(x, n = 2, na.rm = TRUE)  {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x_len <- length(x)
  if (x_len < n) {
    Warning(sprintf("In 'combos_list', length(x) must be greater than 'n'.\nCurrently, length(x) = %s and n = %s", x_len, n))
    return(NULL)
  }
  e <- 0
  h <- n
  a <- seq_len(n)
  r <- x[a]
  out <- vector(mode = "list", length = choose(n = x_len, k = n))
  out[[1L]] <- r
  i <- 2L
  z <- x_len - n + 1L
  while (a[1L] != z) {
    if (e < x_len - h) {
      h <- 1L
      e <- a[n]
      j <- 1L
    } else {
      e <- a[n - h]
      h <- h + 1L
      j <- seq_len(h)
    }
    a[n - h + j] <- e + j
    r <- x[a]
    out[[i]] <- r
    i <- i + 1L
  }
  out
}

#' Generate all combinations of any number of input vectors
#'
#' @param ... Enter 1 or more vectors
#' @param n Number of elements from `x` per combination. Default is `2`. Only relevant when input to `...` is a single vector
#' @param col_names Character vector containing names of columns to be generated
#' @param na.rm If `TRUE` (default), missing values are removed from vectors prior to generating combinations. Only relevant if 1 vector is supplied
#' @returns Data frame of combinations. If 1 vector, output is a data frame with 1 row for each unique combination of elements taken n at a time. Pairs of elements not repeated (i.e. if a row of output is a data frame contains a in V1 and B in V2 there will be no row with b in V1 and a in V2) entered. If > 1 vector entered, output consists of a data frame containing 1 column for each input vector and 1 row for each possible combination of values across vectors
#' @noRd
combos <- function(..., n = 2, col_names = paste0("V", seq_len(n)), na.rm = TRUE) {
  if (n_dots(...) > 1 || is.list(c(...))) {
    crossings(..., .colnames = col_names)
  } else {
    .combos_1_vec_as_df(..., n = n, col_names = col_names, na.rm = na.rm)
  }
}

# Combination helpers -----------------------------------------------------

#' Combination of elements in a vector taken n at time
#'
#' @param x Vector
#' @param n Number of elements from x per combination. Default is `2`
#' @param col_names Character vector containing names of columns to be generated
#' @param na.rm If `TRUE` (default), missing values are removed from x prior to generating combinations
#' @returns Data frame with 1 row for each unique combination of elements taken n at a time. Pairs of elements not repeated (i.e. if a row of output data frame contains a in V1 and B in V2 there will be no row with b in V1 and a in V2)
#' @noRd
.combos_1_vec_as_df <- function(x, n = 2, na.rm = TRUE, col_names = paste0("V", seq_len(n))) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  x_len <- length(x)
  if (x_len < n) {
    message(sprintf("Input 'x' to combos_list() must have length > 'n'. Currently, length of 'x' is %s and 'n = %s'", x_len, n))
    return(NULL)
  }
  e <- 0
  h <- n
  a <- col_idx <- seq_len(n)
  r <- x[a]
  n_rows <- choose(n = x_len, k = n)
  m <- matrix(r, nrow = length(r), ncol = n_rows)
  i <- 2L
  z <- x_len - n + 1L
  while (a[1L] != z) {
    if (e < x_len - h) {
      h <- 1L
      e <- a[n]
      j <- 1L
    } else {
      e <- a[n - h]
      h <- h + 1L
      j <- seq_len(h)
    }
    a[n - h + j] <- e + j
    r <- x[a]
    m[, i] <- r
    i <- i + 1L
  }
  # dim(m) <- c(n, n_rows)
  m <- t.default(m)
  df <- vector(mode = "list", length = n)
  for (i in col_idx) {
    df[[i]] <- m[, i]
  }
  class(df) <- "data.frame"
  attr(df, "row.names") <- c(NA_integer_, -n_rows)
  names(df) <- col_names
  df
}
