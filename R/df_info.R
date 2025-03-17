#' Get names of variables based on predicate function applied to column values
#'
#' @param .df Data frame
#' @param .predicate_fn Predicate function that will be applied to column values to select variable names. Must evaluate to logical (not `NULL`) and should not be vectorized (i.e. for vector input, single value should be returned)
#' @param ... Arguments passed to `.predicate_fn`
#' @param .invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @returns Character vector containing variable names that result in `TRUE` when `.predicate_fn` is applied to columns in `df`
#' @export
vars_which <- function(.df, .predicate_fn, ..., .invert = FALSE) {
  vars <- vapply(.df, .predicate_fn, logical(1), ..., USE.NAMES = TRUE)
  if (.invert) {
    vars <- !vars
  }
  names(vars)[vars]
}

#' Get names of binary variables
#'
#' @rdname vars_which
#' @export
vars_binary <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is_binary, .invert = invert)

#' Get names of binary variables coded as 0 or 1
#'
#' @rdname vars_which
#' @export
vars_binary_01 <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is_binary_01, .invert = invert)

#' Get names of categorical variables
#'
#' @rdname vars_which
#' @param max_n_unique Maximum number of unique values, below which variable assumed to be categorical
#' @export
vars_cat <- function(df, max_n_unique = 10, invert = FALSE) {
  vars_which(df, .predicate_fn = is_categorical, max_n_unique = max_n_unique, .invert = invert)
}

#' Get names of factor variables
#'
#' @rdname vars_which
#' @export
vars_fct <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is.factor, .invert = invert)

#' Get names of character variables
#'
#' @rdname vars_which
#' @export
vars_chr <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is.character, .invert = invert)

#' Get names of numeric variables
#'
#' @rdname vars_which
#' @param incl_integer If `TRUE` (default), integer variables are included
#' @export
vars_numeric <- function(df, invert = FALSE, incl_integer = TRUE) {
  predicate <- if (incl_integer) is.numeric else is_continuous
  vars_which(df, .predicate_fn = predicate, .invert = invert)
}

#' Get names of continuous variables
#'
#' @rdname vars_which
#' @export
vars_continuous <- function(df, invert = FALSE) vars_which(df, is_continuous, .invert = invert)

#' Get names of integer variables
#'
#' @rdname vars_which
#' @export
vars_int <- function(df, invert = FALSE) {
  vars_which(df, .predicate_fn = function(x) is.integer(x) || is_integerish(x), .invert = invert)
}

#' Get names of variables which are coercible to a numeric
#'
#' @rdname vars_which
#' @export
vars_can_be_numeric <- function(df, invert = FALSE, incl_integer = TRUE) {
  if (incl_integer) {
    vars_which(df, .predicate_fn = function(x) all(can_be_numeric(x)), .invert = invert)
  } else {
    vars_which(df, .predicate_fn = function(x) {
      z <- all(can_be_numeric(x))
      if (!z) return(FALSE)
      x <- suppressWarnings(as.numeric(x))
      any(floor(x) != x, na.rm = TRUE)
    }, .invert = invert)
  }
}

#' Get names of logical variables
#'
#' @rdname vars_which
#' @export
vars_lgl <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is.logical, .invert = invert)

#' Get Names of date variables
#'
#' @rdname vars_which
#' @export
vars_date <- function(df, invert = FALSE) {
  vars_which(df, .predicate_fn = inherits, what = c("Date", "POSIXct"), .invert = invert)
}

#' Get names of id variables
#'
#' @rdname vars_which
#' @param na.rm If `TRUE`, missing values are not considered
#' @export
vars_id <- function(df, na.rm = FALSE, invert = FALSE) vars_which(df, .predicate_fn = all_unique, .invert = invert)

#' Get names of constant (zero-variance) variables
#'
#' @rdname vars_which
#' @export
vars_constant <- function(df, invert = FALSE) vars_which(df, .predicate_fn = is_constant, .invert = invert)

#' Get names of variables that contain one or more missing values
#'
#' @rdname vars_which
#' @export
vars_any_na <- function(df, invert = FALSE) vars_which(df, .predicate_fn = anyNA, .invert = invert)

#' Get names of variables that contain no missing values
#'
#' @rdname vars_which
#' @export
vars_no_na <- function(df, invert = FALSE) vars_which(df, .predicate_fn = anyNA, .invert = !invert)

#' Get names of variables that contain only missing values
#'
#' @rdname vars_which
#' @export
vars_all_na <- function(df, invert = FALSE) vars_which(df, .predicate_fn = allNA, .invert = invert)

#' Get names of variables that contain specified substring
#'
#' @param pattern Search pattern to detect in each column of `df`. Enter as length 1 character vector
#' @param ignore.case If `TRUE`, case-insensitive search is performed. Default is `FALSE`
#' @param fixed If `TRUE`, exact match search for `pattern` is performed. Default is `FALSE`
#' @param perl If `TRUE`, perl-compatible syntax is allowed for `pattern` input. Default is `FALSE`
#' @rdname vars_which
#' @export
vars_contain <- function(df, pattern, ignore.case = FALSE, fixed = FALSE, perl = FALSE, invert = FALSE) {
  fn <- function(x, ...) any(grepl(pattern, x, ...), na.rm = FALSE)
  vars_which(df, fn, .invert = invert, ignore.case = ignore.case, fixed = fixed, perl = perl)
}

#' Get names of variables based on predicate function applied to variable names
#'
#' @param .df Data frame
#' @param .predicate_fn Function that will be applied to `names(.df)`. Must evaluate to logical (not `NULL`) and should not be vectorized (i.e. for vector input, output should be length 1)
#' @param ... Arguments passed to `.predicate_fn`
#' @param .invert If `TRUE`, output includes variables for which the predicate is `FALSE`
#' @returns Character vector of variable names that result in `TRUE` when `.predicate_fn` is applied to `names(.df)`. Not used by any functions
#' @noRd
varnames_which <- function(.df, .predicate_fn, ..., .invert = FALSE) {
  df_names <- names(.df)
  idx <- .predicate_fn(df_names, ...)
  names(idx) <- df_names
  if (.invert) {
    idx <- !idx
  }
  df_names[idx]
}

# Duplicates --------------------------------------------------------------

#' Identify duplicate values in a vector or rows in a data frame
#'
#' @param x Data frame or vector
#' @param ... Columns to search for duplicates. Enter using tidyselect syntax. Default uses all columns. Only relevant when `x` is a data frame
#' @returns Data frame with columns "duplicated_value", "n_dupes", "positions"
#' @export
get_dupes <- function(x, ...) UseMethod("get_dupes")

#' get_dupes - default method
#'
#' @rdname get_dupes
#' @export
get_dupes.default <- function(x, ...) {
  dupe_values <- x[duplicated.default(x)]
  rbind_list(lapply(unique.default(dupe_values), function(y) {
    idx <- x == y
    c(duplicated_value = y, n_dupes = Sum(idx), positons = paste(which(idx), collapse = ", "))
  }))
}

#' get_dupes - data frame
#'
#' Searches for duplicate rows
#' @rdname get_dupes
#' @export
get_dupes.data.frame <- function(x, ...) {
  #vars <- if (n_dots(...) == 0) names(x) else names(tidyselect::eval_select(rlang::expr(c(...)), x))
  vars <- names(Select(x, ...))
  var_names <- rlang::syms(vars)
  dupes <- dplyr::group_by(x, !!!var_names)
  dupes <- dplyr::mutate(dupes, dupe_count = seq_len(dplyr::n()))
  dupes <- dplyr::ungroup(dupes)
  dupes <- dupes[dupes$dupe_count > 1L, , drop = FALSE]
  if (Nrow(dupes) == 0L) {
    message(sprintf("No duplicates found\nThe following variables were included in search:\n%s", paste(vars, collapse = ", ")))
    return(invisible())
  }
  dupes <- dupes[c(vars, "dupe_count", Setdiff(names(dupes), c(vars, "dupe_count")))]
  dupes[do.call(order, dupes[vars]), ]
}

#' get_dupes - grouped_df
#'
#' @rdname get_dupes
#' @export
get_dupes.grouped_df <- function(x, ...) {
  grouping_vars <- dplyr::group_vars(x)
  x <- get_dupes.data.frame(dplyr::ungroup(x), ...)
  if (length(x) == 0L) return(invisible())
  dplyr::group_by(x, !!!rlang::syms(grouping_vars))
}

#' Identify columns with identical values
#'
#' @param df Data frame
#' @param ignore_class If `TRUE`, all columns are converted to characters prior to analysis
#' @returns Character vector of column names whose values are duplicated in another column in `df`. If no columns are duplicated, output is `character(0)`
#' @export
dupe_cols <- function(df, ignore_class = FALSE) {
  if (ignore_class) {
    df <- lapply(df, as.character)
  }
  names(df)[duplicated.default(df)]
}

# Other -------------------------------------------------------------------

#' Faster version of `base::nrow`
#'
#' @param x Data frame (doesn't work on matrices)
#' @returns Length 1 integer vector
#' @export
Nrow <- function(x) .row_names_info(x, type = 2L)

#' Row indices
#'
#' @rdname Nrow
#' @export
seq_nrow <- function(x) seq_len(Nrow(x))

#' Unique values for each column in data frame
#'
#' @param df Data frame
#' @param ... Columns to evaluate in `df`. Enter using tidyverse syntax
#' @returns List of vectors containing unique values in each column
#' @noRd
unique_vals <- function(df, ...) lapply(Select(df, ...), function(x) sort(unique(x)))

#' Number of unique values for columns in data frame
#'
#' @param df Data frame
#' @param na.rm If `TRUE` (default), missing values are not considered a unique value
#' @returns Integer vector in which value are the number of unique elements in a column and names represent column names
#' @noRd
n_unique_per_var <- function(df, na.rm = TRUE) {
  vapply(df, n_unique, FUN.VALUE = integer(1), na.rm = na.rm, USE.NAMES = TRUE)
}

#' Determine whether vectors are 1:1 mapping of one another
#'
#' @param ... data frame with at least 2 columns, a list with length of at least 2, or a comma separated list of multiple vectors vectors to compare
#' @returns Logical vector of length 1
#' @noRd
is_one_to_one <- function(...) {
  out <- if (is.data.frame(...)) as.list(...) else if (is.list(...)) c(...) else list(...)
  if (length(out) < 2L) Stop("In 'is_one_to_one_mapping', input to '...' must be a data frame with at least 2 columns, a list with length of at least 2, or multiple vectors")
  if (length(unique(lengths(out, use.names = FALSE))) > 1L) Stop("In 'is_one_to_one_mapping', all inputs must have the same length")
  out <- lapply(out, function(x) match(x, unique(x), incomparables = 0L))
  length(unique(out)) == 1L
}

# Helpers -----------------------------------------------------------------

#' Number of missing values for each column in data frame
#'
#' @param df Data frame
#' @param ... Columns in `df` for which number of missing values will be determined. Enter using tidyselect syntax
#' @returns Data frame with columns for each variable entered as input to `...` and 1 row containing number of missing values for that variable
#' @export
count_na <- function(df, ...) {
  dplyr::summarize(dplyr::group_by(df, ...), dplyr::across(tidyselect::everything(), N_na), .groups = "drop")
}
