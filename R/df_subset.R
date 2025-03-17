# Subset columns ----------------------------------------------------------

#' Select columns using tidyselect syntax
#'
#' Similar to `dplyr::select` but includes all columns when `...` are empty
#' @param df Data frame
#' @param ... Columns to select. Enter using tidyselect syntax
#' @returns Data frame
#' @export
Select <- function(.df, ...) if (n_dots(...) == 0L) .df else dplyr::select(.df, ...)

#' Remove columns in which all values are the same
#'
#' @rdname Select
#' @export
remove_constant_cols <- function(df) {
  df[!vapply(df, function(x) all(is.na(x)) | is_constant(x), logical(1), USE.NAMES = TRUE)]
}

# Subset rows -------------------------------------------------------------

#' Remove duplicate rows
#'
#' Identical to `dplyr::distinct` but with `.keep_all = TRUE` by default
#' @param df Data frame
#' @param ... Columns used to detect duplicates. Enter using tidyselect syntax. Default uses all columns
#' @param keep_all If `TRUE` (default), all columns are included in output. If `FALSE`, output limited to columns specified in `...`
#' @returns Data frame with duplicate rows removed
#' @export
Distinct <- function(df, ..., keep_all = TRUE) dplyr::distinct(df, ..., .keep_all = keep_all)

#' Remove rows with missing values
#'
#' Similar to `tidyr::drop_na`
#' @param df Data frame
#' @param cols Columns to search for missing values. Enter as character vector. If `NULL` (default), all columns are used
#' @returns Data frame
#' @export
remove_na <- function(df, cols = NULL) {
  idx <- if (is.null(cols)) df else df[, cols, drop = FALSE]
  df[stats::complete.cases(idx), , drop = FALSE]
}

#' Remove rows which contain missing values in every column
#'
#' @param df Data frame
#' @param ... Columns to be searched for missing values. Enter using tidyselect syntax. If empty, all columns searched
#' @returns Data frame. Removes rows containing exclusively NA in the columns specified by `...`
#' @export
remove_empty_rows <- function(df, ...) {
  df_subset <- if (n_dots(...) > 0) dplyr::select(df, ...) else df
  rows_all_na <- apply(df_subset, 1, function(x) all(is.na(x)))
  df[!rows_all_na, , drop = FALSE]
}
