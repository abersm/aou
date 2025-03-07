# Add names ---------------------------------------------------------------

#' Set names of a vector
#'
#' @param x Vector
#' @param names Character vector of names (or coercible to a character vector)
#' @returns Named version of input
#' @export
set_names <- function(x, names = x) {
  #names(x) <- rep_len(as.character(names), length(x))
  names(x) <- names
  x
}

#' Create named list where elements of list are also the names of list
#'
#' named_list("a", "b") is equivalent to list(a = "a", b = "b")
#' @param ... Comma separated list of quoted variables
#' @returns Named list
#' @noRd
named_list <- function(...) {
  env <- parent.frame(n = 1)
  dots <- as.list(substitute(list(...)))[-1L]
  n_args <- length(dots)
  arg_names <- names(dots)
  z <- character(n_args)
  for (i in seq_len(n_args)) {
    m <- if (i <= length(arg_names) && nchar(arg_names[[i]]) > 0L) arg_names[[i]] else dots[[i]]
    z[[i]] <- as.character(m)
  }
  values <- vector(mode = "list", n_args)
  for (i in seq_len(n_args)) {
    k <- eval(dots[[i]], envir = env, enclos = env)
    if (!is.null(k)) {
      values[[i]] <- k
    }
  }
  names(values) <- z
  values
}

# Edit names --------------------------------------------------------------

#' Swap values with names or vice versa
#'
#' @param x Named vector
#' @returns Vector with names of `x` now forming values of output vector and values of `x` now forming names of output vector
#' @export
swap_names_vals <- function(x) {
  values <- names(x)
  if (is.null(values)) Stop("Input to 'swap_names_vals' must be a named vector")
  names(values) <- x
  values
}

# Information about names -------------------------------------------------

#' Determine whether each elements is named
#'
#' @param x List or vector
#' @returns Logical vector with same length as input
#' @export
is_named <- function(x) {
  x_names <- names(x)
  if (is.null(x_names)) return(rep(FALSE, length(x)))
  !is.na(x_names) & nzchar(x_names)
}

#' Determine whether any elements are named
#'
#' @param x List or vector
#' @returns Length 1 logical vector
#' @export
any_named <- function(x) {
  x_names <- names(x)
  if (is.null(x_names)) {
    FALSE
  } else if (any(nzchar(x_names))) {
    TRUE
  } else if (all(is.na(x_names))) {
    FALSE
  } else {
    TRUE
  }
}

#' Alias for `any_named`
#'
#' @rdname any_named
#' @export
has_names <- any_named

#' Determine whether all elements are named
#'
#' @param x List or vector
#' @returns Logical of length 1
#' @export
all_named <- function(x) all(is_named(x))

#' Determine whether names contain a specified pattern
#'
#' Equivalent to `grepl(pattern, names(x))`
#' @param x Data frame, matrix, list, or named vector
#' @param pattern Pattern in `names(x)` to search
#' @param invert If `TRUE`, search will be performed for names of `x` that do not contain `pattern`. Equivalent of `!name_contains(x)`
#' @param ignore_case If `FALSE` (default), case of both `names(x)` and `pattern` are ignored in search. If `TRUE`, case used in `names(x)` and `pattern` must match
#' @param ... Arguments passed to `str_contains`
#' @returns Logical vector with length equal to `length(x)` (or `ncol(x)` if `x` is a matrix)
#' @export
name_contains <- function(x, pattern, invert = FALSE, ignore_case = FALSE, ...) {
  x_names <- if (is.matrix(x)) {
    colnames(x)
  } else {
    names(x)
  }
  str_contains(x_names, pattern = pattern, invert = invert, ignore_case = ignore_case, ...)
}

#' Get subset of names which contain a specified pattern
#'
#' @inheritParams name_contains
#' @returns Character vector containing names that contain `pattern`. If no matches found, output has length 0
#' @export
names_which <- function(x, pattern, ignore_case = FALSE, ...) {
  x_names <- if (is.matrix(x)) {
    colnames(x)
  } else {
    names(x)
  }
  x_names <- x_names[grepl(pattern, x_names, ignore.case = ignore_case, ...)]
  if (length(x_names) == 0L) character(0) else x_names
}

# Helpers -----------------------------------------------------------------


#' Clean column names for data frame
#'
#' @param x Character vector of column names
#' @param prefix Prefix for names that begin with non-letter. Default is `"V"`
#' @param max_width Maximum number of characters in names. Default is `100`
#' @param unique If `TRUE` (default), names are made unique
#' @param lower If `TRUE`, output converted to lowercase. Default is `FALSE`
#' @returns Character vector containing names
#' @export
clean_names <- function(x, prefix = "V", max_width = 100L, unique = TRUE, lower = FALSE) {
  # Ensure names start with letter
  x <- sub("^[^A-Za-z\\.]+", paste0(prefix, "_"), x)

  # Replace ++ with _hi_
  x <- gsub("\\+\\++", "_hi_", x)

  # Replace + with _pos_
  x <- gsub("\\++", "_pos_", x)

  # Replace < with _less_than_
  x <- gsub("<+", "_more_than_", x)

  # Replace < with _more_than_
  x <- gsub(">+", "_more_than_", x)

  # Replace = with _equals_
  x <- gsub("=+", "_equals_", x)

  # Replace punctuation and spaces with _
  x <- gsub("[^A-Za-z0-9_\\s]", "_", x)

  # Replace multiple _ with single _
  x <- gsub("_+", "_", x)

  # Ensure names are present
  idx <- !nzchar(x)
  x[idx] <- paste0(prefix, seq_along(x[idx]))

  # Ensure width <= max_width
  x <- substr(x, 1, max_width)

  # Remove _ from end
  x <- gsub("_$", "", x)

  # Ensure names unique
  if (unique) {
    x <- make.unique(x, sep = "_")
  }
  if (lower) {
    x <- tolower(x)
  }
  x
}

#' Force an object to have names
#'
#' @param x Data frame, list, or vector
#' @param prefix Prefix for names. Enter as length 1 character vector. Default is `"v"`
#' @noRd
.force_names <- function(x, prefix = "V") {
  if (is.null(x_names <- names(x))) return(paste0(prefix, seq_along(x)))
  idx <- !nzchar(x_names)
  x_names[idx] <- paste0(prefix, seq_len(sum(idx)))
  x_names
}

#' Create unique name
#'
#' @param .new Proposed name. Enter as string
#' @param .old Existing names. Enter as character vector
#' @param .sep Separator to use for new names. New names are generated by appending `sep` followed by an integer to `.new`. The integer used will be the first (starting from 1) that doesn't result in a name that already exists in `.old`. Default is `"_"`
#' @noRd
.safe_name <- function(.new, .old, .sep = "_") {
  # .new must be after .old to ensure it doesn't take precedence over existing names
  z <- c(.old, .new)
  make.unique(z, sep = .sep)[length(z)]
}
