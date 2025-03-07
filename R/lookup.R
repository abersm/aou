# Lookup ------------------------------------------------------------------

#' Generic vlookup function
#'
#' @param x Data frame or vector to transform
#' @param lookup Mapping between old and new values. Enter 1 of the following:
#'   * Data frame. Must contain columns specified by `"old"` and `"new"` arguments
#'   * Named vector. Names are current ("old") values in `x`. Values are replacement ("new") values
#'   For example, to replace lower case values with upper case values, `lookup` use c(a = "A", b = "B", etc.)
#' @param old,new Column names in `lookup` containing values that match `x` (`old`) and replacement (`new`) values, respectively. Only relevant when `lookup` is a data frame. Enter as quoted or unquoted column names
#' @param silent If `FALSE` (default), messages regarding irregularities in matching are reported
#' @param ... Not used
#' @returns Same class as input
#' @export
vlookup <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, ...) UseMethod("vlookup", x)

#' vlookup - default method
#'
#' @rdname vlookup
#' @export
vlookup.default <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, ...) {
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  } else {
    lookup_names <- names(lookup)
  }

  # Each element of lookup_names must be unique
  if (any_dupes(lookup_names)) {
    # Remove translations in lookup not present in x
    lookup <- lookup[lookup_names %in% x]
    lookup_names <- names(lookup)
    # Check whether duplicates are identical
    idx <- duplicated.default(paste0(lookup, lookup_names))
    lookup <- lookup[!idx]
    lookup_names <- lookup_names[!idx]
    # Stop if duplicates in lookup_names (multiple potential translations for a single value x)
    if (any(dupe_names <- duplicated.default(lookup_names))) {
      z <- vapply(as.list(unique.default(lookup_names[dupe_names])), function(j) {
        m <- paste(sQuote(lookup[lookup_names == j], FALSE), collapse = ", ")
        sprintf("'%s' in lookup[[old]] maps to the following values in lookup[[new]]: %s", j, m)
      }, character(1), USE.NAMES = FALSE)
      Stop("In 'vlookup', 'lookup' contains multiple potential translations:", "\n", paste(Rev(z), collapse = "\n"))
    }
  }

  # Check for values in x not present in lookup
  if (!silent && length(extra_vals <- Setdiff(unique(x[!is.na(x)]), lookup_names)) > 0L) {
    message(paste0("The following values in x are not present in lookup[[old]] and will be replaced by NA: \n", paste(sQuote(extra_vals, FALSE), collapse = ", ")))
  }

  # New values
  new_values <- lookup[match(x, lookup_names)]
  names(new_values) <- NULL
  new_values
}

#' vlookup - factor
#'
#' @rdname vlookup
#' @export
vlookup.factor <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, ...) {
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  } else {
    lookup_names <- names(lookup)
  }

  x_levels <- attr(x, "levels")

  # Check for duplicate names of values in lookup
  if (any_dupes(lookup_names)) {
    # Remove irrelevant values from lookup
    lookup <- lookup[names(lookup) %in% x_levels]
    lookup_names <- names(lookup)
    # Check whether duplicates are identical
    idx <- duplicated.default(paste0(lookup, lookup_names))
    lookup <- lookup[!idx]
    lookup_names <- lookup_names[!idx]
    # Stop if duplicates in lookup_names (multiple potential translations for a single value x)
    if (any(dupe_names <- duplicated.default(lookup_names))) {
      z <- vapply(as.list(unique.default(lookup_names[dupe_names])), function(j) {
        m <- paste(sQuote(lookup[lookup_names == j], FALSE), collapse = ", ")
        sprintf("'%s' in lookup[[old]] maps to the following values in lookup[[new]]: %s", j, m)
      }, character(1), USE.NAMES = FALSE)
      Stop("In 'vlookup', 'lookup' contains multiple potential translations:", "\n", paste(Rev(z), collapse = "\n"))
    }
  }

  # Check for values in x not present in lookup
  if (!silent && length(extra_vals <- Setdiff(unique.default(x[!is.na(x)]), lookup_names)) > 0) {
    message(paste0("The following values in 'x' are not present in lookup[[old]] and will be replaced by NA: \n", paste(sQuote(extra_vals, FALSE), collapse = ", ")))
  }

  # Goal: map levels(x) to lookup values. Want order of lookup values to match order of levels(x)
  # For each level of x, find location of match in lookup names
  idx <- match(x_levels, lookup_names)

  # Using location of lookup names matches, extract corresponding lookup values
  new_levels <- lookup[idx]

  # When a factor is used as an index, the integer representation of levels(x) is used. The result is an integer vector with the same length as x (the input factor) whose values represent the position in levels(x). Because the order of new_values matches levels(x), new_levels[x] replaces old values with new values
  new_values <- new_levels[x]
  names(new_values) <- NULL
  factor(new_values, levels = new_levels)
}

#' vlookup - data frame
#'
#' @rdname vlookup
#' @param col Variable in `x` to match to names of lookup vector. Enter as quoted or unquoted column name
#' @param new_col_name Name of new variable to add to `z`. If `NULL` (default), values in `x$col` are replaced. If column name is specified, a new column will be added to `x` using this name. Enter as quoted or unquoted column name
#' @export
vlookup.data.frame <- function(x, lookup, old = NULL, new = NULL, silent = FALSE, col = NULL, new_col_name = NULL, ...) {
  col <- get_input(col)
  if (is.null(col)) Stop("In 'vlookup', must select column in 'x' to be modified using 'col' argument")
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  }
  new_col_name <- new_col_name %||% col
  x[[new_col_name]] <- vlookup(x[[col]], lookup = lookup, old = old, new = new, silent = silent)
  x
}

# Update values -----------------------------------------------------------

#' Generic update_values function
#'
#' Values in x are only replaced if included in names of lookup table
#' @param x Data frame or vector to transform
#' @param lookup Lookup table or named vector. If `lookup` is a vector, names of `lookup` correspond to values of `x` (i.e. old values) and values of `lookup` represent replacement values (i.e. new values). For example, to replace lower case values with upper case values, `lookup` is c(a = "A", b = "B", etc.)
#' @param old,new Column names of `lookup` containing values that match `x` and replacement values, respectively. Only relevant if `lookup` is a data frame. Enter as quoted or unquoted column names
#' @param ... Not used
#' @returns Updated version of data frame or vector
#' @export
update_values <- function(x, lookup, old = NULL, new = NULL, ...) UseMethod("update_values", x)

#' update_values - default method
#'
#' @rdname update_values
#' @export
update_values.default <- function(x, lookup, old = NULL, new = NULL, col = NULL, new_col_name = NULL, ...) {
  ## Consider placing ... after x to allow for update_values(x, a = "x", b = "y")
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  }

  # Remove irrelevant values from lookup
  #lookup <- lookup[names(lookup) %in% x]
  lookup <- lookup[names(lookup) %in% unique(x)]
  lookup_names <- names(lookup)

  # Check if any lookup_names are duplicated
  if (any_dupes(lookup_names)) {
    # Check whether duplicates are identical
    idx <- duplicated.default(paste0(lookup, lookup_names))
    lookup <- lookup[!idx]
    lookup_names <- lookup_names[!idx]
    # Stop if duplicates in lookup_names (multiple potential translations for a single value x)
    if (any(dupe_names <- duplicated.default(lookup_names))) {
      z <- vapply(as.list(unique.default(lookup_names[dupe_names])), function(j) {
        m <- paste(sQuote(lookup[lookup_names == j], FALSE), collapse = ", ")
        sprintf("'%s' in lookup[[old]] maps to the following values in lookup[[new]]: %s", j, m)
      }, character(1), USE.NAMES = FALSE)
      Stop("In 'update_values', 'lookup' contains multiple potential translations:", "\n", paste(Rev(z), collapse = "\n"))
    }
  }

  # New values
  idx <- match(x, lookup_names, nomatch = 0L)
  new_values <- lookup[idx]
  names(new_values) <- NULL
  x[idx > 0] <- new_values
  x
}

#' update_values - data frame
#'
#' @rdname update_values
#' @param col Var in `x` that matches names of `lookup` vector. Enter as quoted or unquoted column name
#' @param new_col_name Name of new variable to add to `x` If `NULL` (default), values in `x$col` are replaced. If column name is specified, a new column will be added to `x` using this name. Enter as quoted or unquoted column name
#' @export
update_values.data.frame <- function(x, lookup, old = NULL, new = NULL, col = NULL, new_col_name = NULL, ...) {
  if (is.data.frame(lookup)) {
    old <- get_input(old)
    new <- get_input(new)
    lookup_names <- lookup[[old]]
    lookup <- lookup[[new]]
    names(lookup) <- lookup_names
  }
  col <- get_input(col)
  if (is.null(col)) Stop("In 'update_values', must select column to be modified using 'col' argument")
  new_col_name <- new_col_name %||% col
  x[[new_col_name]] <- update_values.default(x[[col]], lookup = lookup, old = old, new = new)
  x
}

# Replace values ----------------------------------------------------------

#' Replace values in character or factor vector using lookup
#'
#' Functionality from ggplot2
#' @param x Character or factor vector
#' @param ... Enter as named character vector or comma separated list of name/value pairs (values as strings). Names match elements in `x` that will be replaced. Values are replacements
#' @returns Vector with same class as input
#' @export
replace_values <- function(x, ...) {
  replacements <- c(...)
  old <- names(replacements)
  if (is.null(old)) {
    Stop("In 'replace_values', input to '...' must be named")
  }
  if (is.character(x)) {
    idx <- match(old, x, nomatch = 0L)
    replacements <- replacements[idx > 0L]
    if (length(replacements) == 0L) return(x)
    x[idx] <- replacements
  } else if (is.factor(x)) {
    x_levels <- attr(x, "levels")
    idx <- match(old, x_levels, nomatch = 0L)
    replacements <- replacements[idx]
    if (length(replacements) == 0L) return(x)
    x_levels[idx] <- replacements
    attr(x, "levels") <- x_levels
  } else {
    .stop_input_class(x)
  }
  x
}

# Helpers -----------------------------------------------------------------

#' Process lookup, new, old arguments of a lookup functions
#'
#' @param x Values to be replaced. Only used when `lookup` is a function
#' @param lookup Entry into lookup argument of parent function. `lookup` can be either of the following:
#'  * A named character vector. `names(lookup)` must match values in `x`, while values of `lookup` represent the new (replacement) values. For example, `c(a = "A", b = "B", etc.)` will replace lower case values with upper case values
#'  * A data frame. Must specify column with values matching those in `x` using input `old` and column containing the replacement values using input `new`
#'  * `NULL`. Replacement will occur by matching values in `old` with the corresponding value in `new`
#' @param old Either a string referring to the column in `lookup` that contains values that match those in `x` or a character vector of values matching those in `x`. In the latter case replacement will occur by matching values in `old` with the corresponding value in `new` (i.e. 1st value in `new` replaces 1st value in `old`, 2nd value in `new` replaces 2nd value `old`, etc.). If `lookup` is a data frame, `old` and `new` must be length 1
#' @param new Either a string referring to the column in `lookup` that contains replacement values or a character vector of replacement values. In the latter case, length of `new` must be 1 (all values in `old` will be replaced by the same value of `new`) or the same length as `old`
#' @param dots Input to `...` of parent function
#' @param check_name_value_swap Only relevant when no matches are found between `lookup$old` and `x`. If `TRUE` (default), will check for matches between `lookup$new` and `x`. If `FALSE`, output determined by `allow_nomatch`
#' @param try_name_value_swap Only relevant when `check_name_value_swap = TRUE` no matches are found between `lookup$old` and `x` but matches between `lookup$new` and `x` are found. If `TRUE`, will proceed as though `lookup$old` and `lookup$new` were switched. If `FALSE` (default), an error will be issued
#' @param check_length Only relevant when lengths of `lookup$old` and `lookup$new` are not equal. If `TRUE`, error will be issued. If `FALSE`, error will be issued unless length of `lookup$new` is 1 (in which case, all values in `lookup$old` will be replaced by the value in `lookup$new`)
#' @param allow_nomatch Only relevant when no values in `lookup$old` match any values in `x`. If `TRUE` (default), output will be `NULL`. If `FALSE`, error will be issued
#' @param parent_fn Name of function from which `.process_lookup` is called
#' @param silent If `TRUE`, no warnings are issued. If `FALSE`, warnings may be issued
#' @returns List containing "idx" (indices of values in `x` that match values in `lookup$old`), "old" (values in `lookup$old` ordered to match corresponding value in "idx"), "new" (replacement values in `lookup$new` ordered to match corresponding value in "old"). If no values in `lookup$old` match any values in `x` and `allow_nomatch = TRUE`, output is `NULL`
#' @noRd
.process_lookup <- function(
    x,
    lookup = NULL,
    old = NULL,
    new = NULL,
    dots = NULL,
    check_name_value_swap = TRUE,
    try_name_value_swap = FALSE,
    check_length = FALSE,
    allow_nomatch = TRUE,
    parent_fn = .fn_called(sys.nframe() - 1),
    silent = TRUE) {
  # Make sure only a single type of input is used
  if (is.null(dots)) {
    # Dots not specified. Input options: "lookup" (data frame or named vector) or vectors for "old" and "new"
    if (is.null(lookup)) {
      # "lookup" not specified. Make sure "old" and "new" are specified
      if (is.null(old) || is.null(new)) {
        Stop(sprintf("In '%s', must specify lookup using '...',  'lookup', or 'old'/'new'", parent_fn))
      }
    } else {
      # "lookup" specified
      if (is.data.frame(lookup)) {
        # "lookup" is a data frame. Check that old and new columns exist
        old <- old %||% "old"
        new <- new %||% "new"
        idx <- match(c(old, new), names(lookup), nomatch = 0L) > 0L
        if (sum(idx) != 2L) {
          Stop(sprintf("In '%s', when 'lookup' is a data frame, there are 2 acceptable methods for specifying which columns in 'lookup' contain 'old' and 'new' values: 1. Enter column name (as a string) containing old and new values using the arguments 'old' and 'new', respectively. If this approach is taken, both 'old' and 'new' must be entered. 2. Do not enter any input into either the 'old' or 'new' arguments. In this case, 'lookup' must be a data frame that contains columns 'old' and 'new'", parent_fn))
        }
        old <- .subset2(lookup, old)
        new <- .subset2(lookup, new)
      } else {
        # "lookup" is a named vector. Make sure "old" and "new" are both empty
        if (!is.null(old) || !is.null(new)) {
          Stop(sprintf("In '%s', when 'lookup' is a vector, arguments 'old' and 'new' must be 'NULL'", parent_fn))
        }
        # Check that names are present
        old <- names(lookup)
        if (is.null(old)) {
          Stop(sprintf("In '%s', when 'lookup' is a vector, it must have names that match values in 'x' (i.e., old/current values). The values of 'lookup' refer to the new/replacements values", parent_fn))
        }
        new <- unname(lookup)
      }
    }
  } else {
    # dots entered. Check that other inputs are empty
    if (!all(is.null(lookup), is.null(old), is.null(new))) {
      Stop(sprintf("In '%s', when lookup is specified using '...', arguments 'lookup', 'old', and 'new' must be 'NULL'", parent_fn))
    }
    # Check that dots are named
    old <- names(dots)
    if (is.null(old)) {
      Stop(sprintf("In '%s', lookup specified using '...' must be a named vector in which names match values in 'x' (i.e., old/current values). The values of '...' refer to the new/replacements values", parent_fn))
    }
    new <- unname(dots)
  }

  # Check length of old and new
  n_old <- length(old)
  n_new <- length(new)
  if (n_old != n_new) {
    if (check_length) {
      Stop(sprintf("In '%s', 'old' and 'new' must have the same length.\nCurrently, length(old) = %s and length(new) = %s", parent_fn, n_old, n_new))
    } else if (n_new != 1L) {
      Stop(sprintf("In '%s', length of 'new' must be 1 (in which case, all old values will be replaced by the same new value) or length(old)", parent_fn))
    } else {
      new <- rep(new, length.out = n_old)
    }
  }

  # Determine indices of values in "x" that match values in "old"
  idx <- match(old, x, nomatch = 0L)
  idx_lookup <- idx > 0
  if (all(!idx_lookup)) {
    # No values in "old" match values in "x"
    if (check_name_value_swap && any(match(new, x, nomatch = 0L) != 0L)) {
      Stop(sprintf("In '%s', lookup names do not match any values in 'x'. However, lookup values match values in 'x'.\nWere the entries for 'old' and 'new' accidentally switched?\nIf so, try swapping names and values by entering 'swap_names_vals(lookup)' into 'lookup' argument of '%s'", parent_fn, parent_fn))
    }
    if (allow_nomatch) {
      if (!silent) {
        Warning(sprintf("In '%s', no values in 'lookup' match any values in 'x'", parent_fn))
      }
      return(NULL)
    } else {
      Stop(sprintf("In '%s', no values in 'lookup' match any values in 'x'", parent_fn))
    }
  }
  list(idx = idx[idx_lookup], old = old[idx_lookup], new = new[idx_lookup])
}
