#' Determine whether any values in a vector are equal to zero
#'
#' @param x Numeric vector
#' @param na.rm If `TRUE` (default), missing values removed. If `FALSE` and `x` contains `NA`, output will be `NA`
#' @returns Length 1 logical vector
#' @export
any_zero <- function(x, na.rm = TRUE) any(x == 0, na.rm = na.rm)

#' Determine whether a vector is a binary variable
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing values are removed
#' @returns Length 1 logical vector
#' @export
is_binary <- function(x, na.rm = TRUE) n_unique(x, na.rm = na.rm) == 2L

#' Determine whether a vector consists exclusively of 0 and 1
#'
#' @rdname is_binary
#' @export
is_binary_01 <- function(x, na.rm = TRUE) {
  allowed_values <- if (na.rm) c(0, 1, NA_real_) else c(0, 1)
  if (!is_binary(x, na.rm)) return(FALSE)
  if (is.factor(x) && all(attr(x, "levels") %in% allowed_values)) return(TRUE)
  x <- suppressWarnings(as.numeric(x))
  is_binary(x, na.rm) && all(x %in% allowed_values)
}

#' Determine whether numeric values are nearly equivalent to integers
#'
#' @param x Numeric vector
#' @returns Length 1 logical vector. `TRUE` if all values in `x` are integers. `FALSE` if any values in `x` are not integers
#' @export
is_integerish <- function(x) is.numeric(x) && all(floor(x) == x, na.rm = TRUE)

#' Determine whether a numeric vector is continuous
#'
#' @rdname is_integerish
#' @returns Length 1 logical vector. `TRUE` if all values in `x` are non-integer numbers
#' @export
is_continuous <- function(x) is.numeric(x) && !all(floor(x) == x, na.rm = TRUE)

#' Check if vector contains only missing values
#'
#' @param x Vector
#' @returns Length 1 logical vector
#' @export
allNA <- function(x) all(is.na(x))

#' Determine whether values are between min and max values
#'
#' @param x Numeric vector
#' @param min,max Lower and upper bound of values respectively
#' @param include_min,include_max If `TRUE`, include values equal to min or max, respectively. Both `TRUE` by default
#' @returns Logical vector with same length as input. If `x` is `NA`, output will be `NA`
#' @export
is_between <- function(x, min, max, include_min = TRUE, include_max = TRUE) {
  #Faster than findInterval(x, c(min, max), rightmost.closed = TRUE) == 1L
  above_min <- if (include_min) `>=` else `>`
  below_max <- if (include_max) `<=` else `<`
  above_min(x, min) & below_max(x, max)
}

# Counts ------------------------------------------------------------------

#' Determine number of unique values for vector
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing levels are removed
#' @param drop If `TRUE` and `x` is a factor, unused levels are dropped before determining number of levels. Default is `FALSE`
#' @returns Length 1 integer vector
#' @export
n_unique <- function(x, na.rm = TRUE, drop = FALSE) {
  x_levels <- if (is.factor(x)) {
    if (drop) {
      unique(as.character(x))
    } else {
      attr(x, "levels")
    }
  } else {
    unique(x)
  }
  if (na.rm) {
    x_levels <- x_levels[!is.na(x_levels)]
  }
  length(x_levels)
}

#' Counts for each unique value
#'
#' @param x Vector or data frame
#' @param ... Columns in `x`. Enter using tidyselect syntax. Ignored if `x` is a vector
#' @returns If `x` is a vector, out is a named integer vector with values containing number of observations per level and name representing the corresponding level of `x`, listed in descending order of group size with NAs listed last. If `x` is a data frame, output is a data frame containing columns "variable", "group" (unique value for variable), "Freq" (number of rows in `x` in which variable = group). Similar to output from `dplyr::count`
#' @export
n_per_group <- function(x, ...) {
  if (is.data.frame(x)) {
    x <- Select(x, ...)
    purrr::map2_df(x, names(x), function(.x, .y) {
      x_unique <- if (anyNA(.x)) {
        idx_na <- is.na(.x)
        .x <- .x[!idx_na]
        x_unique <- unique(.x)
        out <- tabulate(match(.x, x_unique))
        names(out) <- x_unique
        c(sort.int(out, method = "quick", decreasing = TRUE), "NA" = sum(idx_na))
      } else {
        x_unique <- unique(.x)
        out <- tabulate(match(.x, x_unique))
        names(out) <- x_unique
        sort.int(out, method = "quick", decreasing = TRUE)
      }
      list(variable = .y, group = names(x_unique), Freq = as.integer(x_unique))
    })
  } else {
    if (anyNA(x)) {
      idx_na <- is.na(x)
      x <- x[!idx_na]
      x_unique <- unique(x)
      out <- tabulate(match(x, x_unique))
      names(out) <- x_unique
      c(sort.int(out, method = "quick", decreasing = TRUE), "NA" = sum(idx_na))
    } else {
      x_unique <- unique(x)
      out <- tabulate(match(x, x_unique))
      names(out) <- x_unique
      sort.int(out, method = "quick", decreasing = TRUE)
    }
  }
}

#' Alias for `n_per_group`
#'
#' @rdname n_per_group
#' @export
npg <- n_per_group

# Duplicates --------------------------------------------------------------

#' Determine whether vector contains any duplicate values
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing values are removed prior to determining whether duplicates are present
#' @returns Length 1 logical vector
#' @export
any_dupes <- function(x, na.rm = TRUE) anyDuplicated(x, incomparables = if (na.rm) NA else FALSE) > 0L

#' Determine whether all values in a vector are unique
#'
#' @rdname any_dupes
#' @export
all_unique <- function(x, na.rm = TRUE) anyDuplicated(x, incomparables = if (na.rm) NA else FALSE) == 0L

#' Alias for `all_unique`
#'
#' @rdname any_dupes
#' @export
is_id_var <- all_unique

#' Include the first occurrence of the subsequently duplicated value in duplicated output
#'
#' Similar to `duplicated` but is also `TRUE` for first occurence of duplicated value
#' @rdname dupe_vals
#' @returns Logical vector in which any/all duplicated values are `TRUE` and all unique values are `FALSE`
#' @export
duplicated_incl_first <- function(x) {
  duplicated.default(x, fromLast = FALSE) | duplicated.default(x, fromLast = TRUE)
}

#' Extract duplicated entries in a vector
#'
#' @param x Vector
#' @returns Values in `x` that are duplicated. Each duplicated value in `x` is represented once in output regardless of the number of times it appears in `x`
#' @export
dupe_vals <- function(x) unique(x[duplicated.default(x)])

# Trend -------------------------------------------------------------------

#' Determine if a vector is orderec (i.e., sorted)
#'
#' @param x Vector
#' @param na.rm If `TRUE` (default), missing values are ignored
#' @returns Logical of length 1. If `x` is in increasing or decreasing order, output is `TRUE`. Otherwise, output is `FALSE`
#' @export
is_sorted <- function(x, na.rm = TRUE) !is.unsorted(x, na.rm = na.rm) || !is.unsorted(Rev(x), na.rm = na.rm)

#' Alias for is_sorted
#'
#' @rdname is_sorted
#' @export
is_ordered <- is_sorted

#' Determine if a numeric vector contains any increases between adjacent values
#'
#' @param x Numeric vector
#' @param na.rm If `TRUE` (default), missing values are ignored
#' @returns Length 1 logical. `TRUE` if any occurrences of a value greater than the previous value. Otherwise output is `FALSE`
#' @noRd
.any_increasing <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  any(x_diff > 0L, na.rm = TRUE)
}

#' Determine if a numeric vector contains any decreases
#'
#' @param x Numeric vector
#' @param na.rm If `TRUE` (default), missing values are ignored
#' @returns Length 1 logical. `TRUE` if any occurrences of a value less than the previous value
#' @noRd
.any_decreasing <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  any(x_diff < 0L, na.rm = TRUE)
}

#' Determine if a numeric vector is strictly increasing (increases monotonically)
#'
#' @param x Numeric vector
#' @param na.rm If `TRUE` (default), missing values are ignored
#' @param incl_plateau If `TRUE` (default), output can still be `TRUE` even if `x` contains instances in which adjacent values. If `FALSE`, values must be strictly monotonically increasing
#' @returns Length 1 logical. `TRUE` if each successive value is greater than the previous value. If `incl_plateau` is `TRUE`, output can still be `TRUE` even if `x` contains instances in which adjacent values
#' @noRd
.all_increasing <- function(x, incl_plateau = TRUE, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  if (incl_plateau) all(x_diff >= 0L, na.rm = TRUE) else all(x_diff > 0L, na.rm = TRUE)
}

#' Determine if a numeric vector is strictly decreasing (decreases monotonically)
#'
#' @param x Numeric vector
#' @param na.rm If `TRUE` (default), missing values are ignored
#' @param incl_plateau If `TRUE` (default), output can still be `TRUE` even if `x` contains instances in which adjacent values. If `FALSE`, values must be strictly monotonically decreasing
#' @returns Logical of length 1. `TRUE` if each successive value is less than the previous value. If `incl_plateau` is `TRUE`, output can still be `TRUE` even if `x` contains instances in which adjacent values
#' @noRd
.all_decreasing <- function(x, incl_plateau = TRUE, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  x_diff <- x[-1L] - x[-n]
  if (incl_plateau) all(x_diff <= 0L, na.rm = TRUE) else all(x_diff < 0L, na.rm = TRUE)
}

# Convert class -----------------------------------------------------------

#' Convert a vector to the most likely class intended
#'
#' @param x Vector
#' @returns Vector of most likely intended class
#' @export
type_convert <- function(x) utils::type.convert(x, as.is = TRUE)

#' Convert a vector to numeric
#'
#' Similar to `base::as.numeric` but suppresses warnings
#' @param x Vector
#' @returns Numeric vector with same length as input
#' @export
as_numeric <- function(x) suppressWarnings(as.numeric(x))

#' Convert a vector to integer
#'
#' Similar to `base::as.integer` but suppresses warnings
#' @rdname as_numeric
#' @export
as_integer <- function(x) suppressWarnings(as.integer(x))

# Other -------------------------------------------------------------------

#' Remove missing values from vector or rows with missing values for certain columns
#'
#' @param x Vector or data frame
#' @param cols Columns to search for missing values. Enter as character vector. Default uses all columns in `x`. Only relevant when `x` is a data frame
#' @returns Same class as input
#' @export
na_rm <- function(x, cols = names(x)) {
  if (is.data.frame(x)) {
    remove_na(x, cols = cols)
  } else {
    x[!is.na(x)]
  }
}

#' Reverse a vector
#'
#' Identical to `base::rev.default`
#' @param x Vector
#' @returns Vector with same length and class as input
#' @export
Rev <- function(x) {
  n <- length(x)
  if (n == 0L) x else x[seq.int(from = n, to = 1L)]
}

#' Convert numeric vector of ages to age groups
#'
#' @param x Numeric vector of ages in years
#' @param width Width of each age interval in years. Enter as length 1 numeric. Default is `10` (creates age deciles)
#' @param include_lower If `TRUE` (default), all values in a given age interval must be greater than or equal to the lower limit that defines that age interval. If `FALSE`, all values in a given age interval  must be greater than the lower limit that defines that age interval
#' @param include_upper If `TRUE`, all values in a given age interval  must be less than or equal to the upper limit that defines that age interval. If `FALSE` (default), all values in a given age interval  must be less than the upper limit that defines that age interval
#' @param min Lower limit of first age age interval. Enter as length 1 numeric. Default is `0`
#' @param max Upper limit of last age age interval. Enter as length 1 numeric. Default uses nearest multiple of 10 that is greater than or equal to the maximum value in `x`
#' @returns Factor vector with same length as input
#' @export
age_group <- function(
    x,
    width = 10,
    include_lower = TRUE,
    include_upper = FALSE,
    min = 0,
    max = range(x, finite = TRUE)[2L]) {
  breaks <- seq(min, ceiling(max/width)*width, by = width)
  # TODO: add checkSorted = FALSE in findInterval once available in new version of R
  x <- findInterval(x, breaks, rightmost.closed = include_upper, left.open = !include_lower, all.inside = FALSE)
  n <- length(breaks)
  breaks <- paste0(if (include_upper) "(" else "[", breaks[-n], "-", breaks[-1L], if (include_upper) "]" else ")")
  if (include_lower) {
    if (include_upper) {
      substr(breaks[1L], 1L, 1L) <- "["
    } else {
      n <- n - 1L
      substring(breaks[n], nchar(breaks[n])) <- "]"
    }
  }
  attr(x, "levels") <- breaks
  class(x) <- "factor"
  x
}

#' Convert year to decade
#'
#' @rdname age_group
#' @param x Integer vector containing years
#' @param as_fct If `TRUE` (default), output converted to a factor. If `FALSE`, output will be an integer
#' @returns Factor (if `as_fct = TRUE`) or integer (if `as_fct = FALSE`) vector with same length as input indicating decade
#' @export
decade <- function(x, as_fct = TRUE) {
  out <- (x %/% 10)*10
  if (as_fct) {
    out <- paste0(out, "s")
    out[is.na(x)] <- NA
    factor(out)
  }
  out
}

#' Split a vector by a grouping variable
#'
#' Alternative to `base::split`
#' @param x Vector
#' @param group Group membership for each value in `x`. Must have same length as `x`. `group` will be converted to a factor
#' @param levels Levels for `group`. Enter as vector with length equal to the numer of unique values in `group`. Order of `levels` will match order of output
#' @param excl_group_na Handling of `NA` values in `group` (this does NOT refer to `NA` values in `x`). If `TRUE` (default), output will not include a distinct group for values in which `group` is `NA` (thus, values in which `group` is `NA` will be removed from output. If `FALSE`, output will include a group in which `group` is `NA`
#' @returns List with length equal to the number of factor levels in `group`. Each element of output contains elements of `x` for a single group. Names of output refer to factor levels of `group`. Output is ordered by levels of `group`
#' @noRd
.split_vals <- function(x, group, levels = NULL, excl_group_na = TRUE) {
  group <- if (is.null(levels) || is.numeric(group)) {
    as_fct(group, levels = levels)
  } else {
    factor(group, levels = levels, exclude = if (excl_group_na) NA else NULL)
  }
  split.default(x, f = group)
}
