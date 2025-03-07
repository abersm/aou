#' Convert units
#'
#' @param x Numeric vector of quantities to convert. Units of `x` specified in `from` argument
#' @param from Units for `x`. Options: `"in"`, `"cm"`, `"mm"`, `"pts"` (or `"points"`), `"lines"`, `"linewidth"` (ggplot-specific units. 1 unit is equivalent to 0.75 mm), `"npc"`, `"C"`, `"F"`, `"kg"`, `"lbs"`. Enter as length 1 character vector
#' @param to Desired output units. Same options as `from`. Enter as length 1 character vector
#' @param pt_per_in Points per inch. Default is `72.27` (used by ggplot2). PowerPoint uses 72.009. Enter as length 1 numeric vector
#' @returns Numeric vector expressed in units determined by `to` argument
#' @export
convert <- function(x, from, to, pt_per_in = 72.27) {
  size_units <- c("inches", "cm", "mm", "pts", "points", "lines", "linewidth", "npc")
  units <- c(size_units, "C", "F", "kg", "lbs")
  from <- match.arg(from, choices = units)
  to <- match.arg(to, choices = units)
  if (from %in% size_units) {
    if (to %!in% size_units) {
      Stop(sprintf("In 'convert', when 'from = %s', 'to' must be one of the following: %s. Currently, 'to = %s'", .quote_collapse(size_units), from, to))
    }
    .convert_size(x, from = from, to = to, pt_per_in = pt_per_in)
  } else if (from %in% (temp_units <- c("C", "F"))) {
    if (to %!in% temp_units) {
      Stop(sprintf("In 'convert', when 'from = %s', 'to' must be either 'C' or 'F'. Currently, 'to = %s'", from, to))
    }
    if (from == "C" && to == "F") {
      x*(9/5) + 32
    } else if (from == "F" && to == "C") {
      (x-32)*(5/9)
    } else {
      x
    }
  } else {
    if (to %!in% c("kg", "lbs")) {
      Stop(sprintf("In 'convert', when 'from = %s', 'to' must be either 'kg' or 'lbs'. Currently, 'to = %s'", from, to))
    }
    if (from == "kg" && to == "lbs") {
      x/0.45359237
    } else if (from == "lbs" && to == "kg") {
      x*0.45359237
    } else {
      x
    }
  }
}

#' Convert graphical size units
#'
#' @param x Numeric vector of starting sizes. Units of `x` specified in `from` argument
#' @param from Units for `x`. Options: `"inches"`, `"cm"`, `"mm"`, `"pts"`, `"points"`, `"lines"`, `"linewidth"`, `"npc"`. Enter as length 1 character vector
#' @param to Desired output units. Same options as `from`. Enter as length 1 character vector
#' @param pt_per_in Points per inch. Default is `72.27` (used by ggplot2). PowerPoint uses 72.009. Enter as length 1 numeric vector
#' @returns Numeric vector expressed in units determined by `to` argument
#' @noRd
.convert_size <- function(x, from, to, pt_per_in = 72.27) {
  z <- c(1, 2.54, 25.4, pt_per_in, pt_per_in, 5, 25.4/0.75, 0.1130298)
  names(z) <- c("inches", "cm", "mm", "pts", "points", "lines", "linewidth", "npc")
  z <- x/z[from]*z[to]
  names(z) <- NULL
  z
}

#' Convert distance units
#'
#' @param x Numeric vector of starting distances. Units of `x` specified in `from` argument
#' @param from Units for `x`. Options: `"m"`, `"in"`, `"ft"`, `"yd"`, `"mile"`. Enter as length 1 character vector
#' @param to Desired output units. Same options as `from`. Enter as length 1 character vector
#' @returns Numeric vector expressed in units determined by `to` argument
#' @noRd
.convert_distance <- function(x, from, to) {
  z <- c(1, 0.254, 3.048, 0.9144, 1609.344)
  names(z) <- c("m", "in", "ft", "yd", "mile")
  z <- x/z[from]*z[to]
  names(z) <- NULL
  z
}

#' Convert volume units
#'
#' @param x Numeric vector of starting volumes. Units of `x` specified in `from` argument
#' @param from Units for `x`. Options: `"l"` (or `"liters"`), `"gallons"`, `"quarts"`, `"pints"`, `"cups"`, `"floz"` (fluid ounces), `"tablespoons"`, `"teaspoons"`. Enter as length 1 character vector. All units are case insensitive
#' @param to Desired output units. Same options as `from`. Enter as length 1 character vector
#' @returns Numeric vector expressed in units determined by `to` argument
#' @noRd
.convert_volume <- function(x, from, to) {
  from <- tolower(from)
  to <- tolower(to)
  units <- c("l", "liters", "gallons", "quarts", "pints", "cups", "floz", "tablespoons", "teaspoons")
  from <- match.arg(from, choices = units)
  to <- match.arg(to, choices = units)
  # Convert to L
  z <- c(1000, 1000, 4546.09, 1136.523, 568.261, 240, 28.413, 15, 5)/1000
  names(z) <- units
  z <- x/z[from]*z[to]
  names(z) <- NULL
  z
}

#' Squish a numeric vector into a specified range
#'
#' Rewritten version of `scales::squish`
#' @param x Numeric vector
#' @param min,max Desired minimum and maximum values. Values in `x` below `min` will be set to `min`. Values in `x` above `max` will be set to `max`. If `NULL` (default for both), values outside the limit will not be changed
#' @returns Numeric vector with same length as input
#' @export
squish <- function(x, min = NULL, max = NULL) {
  if (!is.null(min)) {
    x[x < min] <- min
  }
  if (!is.null(max)) {
    x[x > max] <- max
  }
  x
}

#' log10 that leaves 0 unchanged
#'
#' @param x Numeric vector
#' @returns Numeric vector with same length as input. Output is `NaN` when `x < 0`, `0` when `x == 0`, `NA_real_` when `is.na(x)`, and `log10(x)` for all other values of `x`
#' @export
log10_1 <- function(x) {
  idx <- x != 0
  x[idx] <- log10(x[idx])
  x
}

#' Log after specified shift
#'
#' Similar to `base::log1p` but can control amount added to initial raw values (in contrast to log1p which uses 1)
#' @rdname log10_1
#' @param n Constant added to `x` prior to log transformation. Default uses 1 (if `x` doesn't contain negative values) or shifts values so that the most negative value is 0, then adds 1 (if `x` contains negative values). Enter as length 1 numeric
#' @param base Logarithm base. Default is `exp(1)`. Enter as length 1 numeric
#' @returns Numeric vector with same length as input
#' @export
lognp <- function(x, n = abs(Min(c(Min(x), 0))) + 1, base = exp(1)) log(x + n, base = base)

#' Percentile
#'
#' Functionality from `stats::ecdf`
#' @param x Numeric vector
#' @param inclusive If `TRUE`, percentile calculated as percentage at OR below a given score (values can range from 0-1). If `FALSE` (default), percentile calculated as percentage below a given score (values can't be 0 or 1)
#' @returns Numeric vector with same length as input. Values range from 0-1. Missing values in `x` will also be missing in output. Percentile calculated after removing all missing values from `x`. Percentile only calculated when `x` contains more than 2 unique values. Otherwise, output values will be `NA_real_`
#' @export
percentile <- function(x, inclusive = FALSE) {
  idx_na <- is.na(x)
  x_nna <- x[!idx_na]
  n <- length(x_nna)
  if (n < 3L || length(x_unique <- unique(x_sorted <- sort(x))) < 3L) {
    x[] <- NA
    return(x)
  }
  f <- stats::approxfun(x_unique, cumsum(tabulate(match(x_sorted, x_unique)))/n, method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  #class(f) <- c("ecdf", "stepfun", class(f))
  #assign("nobs", n, envir = environment(f))
  #attr(f, "call") <- sys.call()
  out <- f(x)
  if (inclusive) return(out)
  if (Max(out) == 1) {
    idx <- out == 1
    z <- 1 - sum(idx, na.rm = TRUE)/n
    if (z > Max(out[!idx])) {
      out[idx] <- z
    }
  }
  if (Min(out) == 0) {
    idx <- out == 0
    z <- sum(idx, na.rm = TRUE)/n
    if (z < Min(out[!idx])) {
      out[idx] <- z
    }
  }
  out
}

#' Z-score (standardization)
#'
#' @rdname percentile
#' @export
z_score <- function(x) (x - mean.default(x, na.rm = TRUE))/SD(x)

#' Scale values to specified range
#'
#' Functionality from `scales::rescale`
#' @param x Numeric vector
#' @param to Desired range of output. Enter as length 2 numeric vector. Must be in ascending order. Default is `c(0, 1)`
#' @param from Starting range. Enter as length 2 numeric vector. Must be in ascending order. Default uses range of `x`
#' @returns Numeric vector with same length as input
#' @export
rescale <- function(x, to = c(0, 1), from = Range(x)) {
  if (.has_zero_range(from) || .has_zero_range(to)) {
    return(ifelse(is.na(x), NA_real_, mean.default(to)))
  }
  delta_to <- to[2L] - to[1L]
  delta_from <- from[2L] - from[1L]
  (x - from[1L])/delta_from*delta_to + to[1L]
}

#' Normalize a numeric vector
#'
#' @rdname rescale
#' @export
normalize <- function(x, to = c(0, 1)) rescale(x, to = to)

#' Normalize row-wise
#'
#' @rdname transform_rowwise
#' @param df Data frame
#' @param ... Columns to include in normalization. Enter using tidyselect syntax
#' @param to Desired range for output. Enter as length 2 numeric vector in ascending order. Default is `c(0, 1)`
#' @returns Data frame with same dimensions as input
#' @export
normalize_rowwise <- function(df, ..., to = c(0, 1)) transform_rowwise(.df = df, .fn = normalize, to = to, ...)

#' Normalize column-wise
#'
#' @rdname transform_rowwise
#' @export
normalize_colwise <- function(df, ..., to = c(0, 1)) {
  vars <- vars_numeric(names(Select(df, ...)))
  for (i in vars) {
    df[[i]] <- normalize(.subset2(df, i), to = to)
  }
  df
}
