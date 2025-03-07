# Create factor -----------------------------------------------------------

#' Create reasonable levels for factor regardless of input variable class
#'
#' @param x Vector
#' @param reverse If `TRUE` order of levels is reversed. `FALSE` by default
#' @param droplevels If `TRUE` (default) and `x` is a factor, unused levels are dropped
#' @returns Character vector containing factor levels. Enter as `factor(x, levels = create_levels(x))`
#' @export
create_levels <- function(x, reverse = FALSE, droplevels = TRUE) {
  if (is.factor(x)) {
    levels <- attr(x, "levels")
    if (droplevels) {
      x <- factor(x, exclude = if (anyNA(levels)) NULL else NA)
      levels <- attr(x, "levels")
    }
  } else if (is.logical(x)) {
    levels <- c(TRUE, FALSE)
  } else {
    levels <- sort.int(unique(x), method = "quick")
  }
  if (reverse) {
    Rev(levels)
  } else {
    levels
  }
}

# Type conversion ---------------------------------------------------------

#' Convert integer to factor
#'
#' @param x Integer vector
#' @param levels Factor levels. Must match unique values of `x`. Default uses unique values of `x` in increasing order
#' @returns Factor with same length as input
#' @noRd
int_to_fct <- function(x, levels = NULL) {
  levels <- levels %||% sort.int(unique(x))
  x <- match(x, levels)
  levels(x) <- as.character(levels)
  class(x) <- "factor"
  x
}

#' Convert categorical variable to factor encoded as integer
#'
#' Similar to `as.integer(factor(x))` but faster
#' @param x Factor, character, numeric, or logical vector
#' @param levels Order of levels in `x`
#' @param reverse If `TRUE` order of levels is reversed. `FALSE` by default
#' @returns Integer version of `x` (lowest level is 1, highest value is equal to the number of levels)
#' @noRd
as_numeric_factor <- function(x, levels = NULL, reverse = FALSE) {
  x_levels <- levels %||% create_levels(x)
  if (reverse) {
    x_levels <- Rev(x_levels)
  }
  match(x, x_levels, incomparables = NA_integer_)
}

#' Convert vector to a factor
#'
#' @param x Vector
#' @param levels Factor levels. Enter as character vector
#' @returns Factor vector
#' @export
as_fct <- function(x, levels = NULL) {
  if (is.numeric(x)) {
    int_to_fct(as.integer(x), levels = levels)
  } else if (is.factor(x) && is.null(levels)) {
    x
  } else {
    x <- as.character(x)
    if (is.null(levels)) {
      levels <- create_levels(x)
    } else {
      levels <- as.character(levels)
    }
    x <- match(x, levels)
    levels(x) <- levels
    class(x) <- "factor"
    x
  }
}

# Edit levels -------------------------------------------------------------

#' Reorder levels of a factor by a continuous variable
#'
#' Functionality from forcats package
#' @param .x Factor vector
#' @param .reorder_by Vector used to determine new order of `.x` levels. Levels of `.x` will be determined according to `.f(.reorder_by)` calculation
#' @param .f Function applied to reorder_by to determine order of `.x` levels. Default is `Mean`
#' @param ... Arguments passed to `.f`
#' @param .increasing If `TRUE` (default), levels of `.x` will be in ascending order of `.f(.reorder_by)` calculation. If `FALSE`, levels of `.x` will be in descending order of `.f(.reorder_by)` calculation
#' @returns Factor vector with updated levels according to `.f(.reorder_by)` calculation and increasing argument
#' @export
fct_reorder <- function(.x, .reorder_by, .f = Mean, ..., .increasing = TRUE) {
  if (!inherits(.x, "factor")) {
    .x <- factor(.x)
  }
  x_updated <- tapply(.reorder_by, .x, .f, ...)
  x_updated <- factor(.x, levels = attr(.x, "levels")[order(x_updated, decreasing = !.increasing)], exclude = NULL)
  attributes(x_updated) <- update_list(attributes(.x), attributes(x_updated))
  x_updated
}

#' Order levels of a factor by their order of appearance
#'
#' @rdname fct_reorder
#' @export
fct_by_input_order <- function(.x) {
  if (is.character(.x)) {
    .x <- factor(.x)
  }
  idx <- as.integer(.x)[!duplicated(.x)]
  idx <- idx[!is.na(idx)]
  new_f <- factor(.x, levels = attr(.x, "levels")[idx], exclude = NULL)
  attributes(new_f) <- update_list(attributes(.x), attributes(new_f))
  new_f
}

#' Order levels of a factor by their relative frequency (most to least common)
#'
#' @rdname fct_reorder
#' @export
fct_by_count <- function(.x, .increasing = TRUE) {
  if (is.character(.x)) {
    .x <- factor(.x)
  }
  idx <- order(table(.x), decreasing = !.increasing)
  new_f <- factor(.x, levels = attr(.x, "levels")[idx], exclude = NULL)
  attributes(new_f) <- update_list(attributes(.x), attributes(new_f))
  new_f
}

#' Order levels of a factor from least to most common
#'
#' @rdname fct_reorder
#' @export
fct_increasing <- function(.x) fct_by_count(.x, .increasing = TRUE)

# Info --------------------------------------------------------------------

#' Determine whether a variable is likely a categorical variable
#'
#' @param x Vector
#' @param max_n_unique If number of `n_unique(x) <= max_n_unique`, variable is considered a factor
#' @returns If `x` is likely a categorical variable, `TRUE.` Otherwise, `FALSE`
#' @noRd
is_categorical <- function(x, max_n_unique = 5L) {
  inherits(x, c("factor", "ordered", "logical", "character", "integer", "labelled")) || n_unique(x) <= max_n_unique
}

# Helpers -----------------------------------------------------------------

#' Update the "levels" attribute of a factor variable to remove unused levels
#'
#' Modified version of `base::droplevels.factor`
#' @param x Vector
#' @param na.rm Only relevant when `x` contains 1 or more `NA`. If `TRUE` (default), levels of output will not include `NA`. If `FALSE`, `NA` can be included in levels of output (if present in `x`)
#' @returns Vector with same class and length as input
#' @noRd
.droplevels <- function(x, na.rm = FALSE) {
  if (inherits(x, "factor")) {
    x <- factor(x, exclude = if (na.rm || !anyNA(attr(x, "levels"))) NA else NULL)
  }
  x
}

#' Update the "levels" attribute of each factor variable in a data frame to remove unused levels
#'
#' Modified version of `base::droplevels.data.frame`
#' @param x Data frame
#' @param na.rm Only relevant when a column in `x` contains 1 or more `NA`. If `TRUE` (default), levels in output will not include `NA`. If `FALSE`, `NA` can be included in levels in output (if present in a particular column in `x`)
#' @returns Data frame with same dimensions as input
#' @noRd
.droplevels_df <- function(x, na.rm = FALSE) {
  x[] <- lapply(x, .droplevels, na.rm = na.rm)
  x
}
