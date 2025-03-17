#' Interleave vectors or lists
#'
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @param coerce_fn Function applied to each input vector prior to interleaving. Default is `as.character`
#' @returns Vector or list
#' @export
interleave <- function(..., coerce_fn = as.character) {
  z <- lapply(list(...), match.fun(coerce_fn))
  n_args <- length(z)
  if (n_args < 2L) return(c(...))
  z <- lapply(z, rep, length.out = max(lengths(z, use.names = FALSE), na.rm = TRUE))
  elements <- unlist(z)
  elements[as.integer(matrix(seq_along(elements), nrow = n_args, byrow = TRUE))]
}

#' Elements in x but not y
#'
#' Similar to `base::setdiff` but doesn't convert `x` and `y` to vectors
#' @param x,y Vectors
#' @noRd
Setdiff <- function(x, y) unique(x[match(x, y, nomatch = 0L) == 0L])

#' Elements in x and y
#'
#' Similar to `base::intersect` but doesn't convert `x` and `y` to vectors
#' @noRd
Intersect <- function(x, y) unique(y[match(x, y, nomatch = 0L)])

#' Determine whether all elements of x are also present in y and vice versa (independent of order)
#'
#' Similar to `base::setequal` but doesn't convert `x` and `y` to vectors
#' @noRd
Setequal <- function(x, y) !(anyNA(match(x, y)) || anyNA(match(y, x)))

#' Setequal on > 2 vectors
#'
#' Equivalent to sequentially running `Setequal` on > 2 vectors
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @returns Length 1 logical vector
#' @export
Setequal_all <- function(...) {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  Setequal(Reduce(Intersect, lists), lists[[1L]])
}

#' Setdiff on > 2 vectors
#'
#' Equivalent to sequentially running `Setdiff` on > 2 vectors
#' @rdname Setequal_all
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @export
Setdiff_all <- function(...) {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  Reduce(Setdiff, lists)
}

#' Extract values that are unique to a single entry
#'
#' Equivalent to combining the results from running `Setdiff` and running `Setdiff` with input order reversed
#' @rdname Setequal_all
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @export
Setdiff_any <- function(...) {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  unique(c(Reduce(Setdiff, lists), Reduce(Setdiff, Rev(lists))))
}

#' Elements common to all input vectors
#'
#' Equivalent to sequentially running `Intersect` on > 2 vectors
#' @rdname Setequal_all
#' @param ... Vectors or list of vectors
#' @export
Intersect_all <- function(...) {
  lists <- if (is.list(lists <- c(...))) lists else list(...)
  Reduce(Intersect, lists)
}

#' Recursively determine whether all inputs are equal
#'
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @param tolerance Threshold difference between values above which output will be `FALSE`. Default is `1.5e-8`
#' @returns Length 1 logical. `TRUE` if predicate met, `FALSE` if not
#' @export
all_equal <- function(..., tolerance = 1.5e-8) .lapply_predicate(..., .f = function(x, y) all.equal(x, y, tolerance = tolerance))

#' Recursively determine whether all inputs are identical
#'
#' @rdname all_equal
#' @export
all_identical <- function(...) .lapply_predicate(..., .f = identical)

#' Helper function to generate predicate functions that accept > 2 inputs
#'
#' @param ... Either a comma separated list of vectors not wrapped in `c()` or a list containing vectors
#' @param .f Predicate function. Must have 1 argument for each of 2 objects to be tested in a given loop
#' @returns Length 1 logical. `TRUE` if predicate met, `FALSE` if not
#' @noRd
.lapply_predicate <- function(..., .f) {
  #lists <- if (is.list(lists <- c(...))) lists else list(...)
  lists <- list(...)
  first_element <- lists[[1L]]
  idx <- seq_along(lists)
  for (i in idx[-1L]) {
    out <- .f(.subset2(lists, i), first_element)
    if (!is.logical(out)) {
      Warning("Predicate function does not return TRUE or FALSE")
      return(NA_integer_)
    }
    if (!out) return(FALSE)
  }
  TRUE
}
