#' Rename variables in data frame, list, or vector using a lookup table
#'
#' @param x Data frame, list, or vector
#' @param lookup Lookup data frame or named character vector. If a named character vector, `names(lookup)` must match `names(x)` (old column names) and values of `lookup` are the replacements (new column names). If `lookup` is a data frame, must specify column containing old names `old` (`lookup[[old]]` must match `names(x)`) and replacements (`lookup[[new]]`)
#' @param old,new Column names of `lookup` containing old/current column names (`names(x)`) and corresponding replacement names, respectively. Enter as length 1 character vectors
#' @returns Same class as input
#' @export
rename_by_lookup <- function(x, lookup, old = NULL, new = NULL) {
  names(x) <- update_values(names(x), lookup = lookup, old = old, new = new)
  x
}

#' Rename variables in data frame, list, or vector, using a transformation function
#'
#' @param .x Data frame, list, or vector
#' @param .fn Function to transform `names(.x)`
#' @param ... Column names in `.x` to transform. Enter using tidyselect syntax. Default uses all columns in `.x`. Only relevant when `.x` is a data frame
#' @returns Same class as input
#' @export
rename_by_fn <- function(.x, .fn = function(x) tolower(clean_names(x)), ...) {
  idx <- if (n_dots(...) == 0L) TRUE else names(.x) %in% names(dplyr::select(.x, ...))
  names(.x)[idx] <- .fn(names(.x)[idx])
  .x
}

# Rowwise -----------------------------------------------------------------

#' Row-wise transformation
#'
#' @param .df Data frame
#' @param .fn Function used to transform row values. Must be vectorized
#' @param ... Columns to transform. Enter using tidyselect syntax
#' @returns Data frame with same dimensions as input
#' @export
transform_rowwise <- function(.df, .fn, ...) {
  vars <- vars_numeric(Select(.df, ...))
  .df[vars] <- matrix_to_df(t.default(apply(.df[vars], 1, .fn)))
  .df
}

# Create new variable -----------------------------------------------------

#' Add column for row number
#'
#' @param .df Data frame
#' @param .new_colname Column name for id variable. Default is `"id"`
#' @returns Data frame with new column for id variable (`.new_colname`). If `.df` is a grouped data frame, rows will be numbered by group
#' @export
add_id_var <- function(.df, .new_colname = "id") {
  .new_colname <- .safe_name(.new = .new_colname, .old = names(.df), .sep = "")
  if (inherits(.df, "grouped_df")) {
    .df <- dplyr::mutate(.df, "{.new_colname}" := seq_len(dplyr::n()))
    dplyr::ungroup(.df)
  } else {
    .df[[.new_colname]] <- seq_nrow(.df)
    .df
  }
}

#' Add column containing row number within groups
#'
#' @param .df Data frame
#' @param ... Columns used to form groups. Passed to `dplyr::group_by`
#' @param .arrange_by Columns used to order `.df` prior to grouping. Enter as character vector
#' @param .new_colname Name for newly created column containing group number. Enter as quoted or unquoted column name. Default is `"n"`.
#' @returns Ungrouped data frame with integer column (`.new_colname`) containing group number
#' @export
add_group_id <- function(.df, ..., .arrange_by = NULL, .new_colname = "n") {
  .new_colname <- .safe_name(.new = .new_colname, .old = names(.df), .sep = "")
  if (!is.null(.arrange_by)) {
    .df <- .df[do.call("order", as.list(.df[.arrange_by])), ]
  }
  if (inherits(.df, "grouped_df")) {
    if (n_dots(...) != 0L) {
      group_vars <- Setdiff(names(attr(.df, "groups")), ".rows")
      z <- dots_as_quoted(...)
      if (!Setequal(group_vars, z)) {
        Warning(sprintf("Input to 'add_group_id' is a 'grouped_df' with groups %s. Will regroup using the following: %s", .quote_collapse(group_vars), .quote_collapse(z)))
        .df <- dplyr::ungroup(.df)
        .df <- dplyr::group_by(.df, !!!rlang::syms(z))
      }
    }
  } else {
    .df <- dplyr::group_by(.df, ...)
  }
  .df <- dplyr::mutate(.df, "{.new_colname}" := seq_len(dplyr::n()))
  dplyr::ungroup(.df)
}

#' Add new factor variable to data frame by pasting values from existing columns together
#'
#' @param df Data frame
#' @param ... Columns to paste together. Enter using tidyselect syntax
#' @param add_colname_prefix If `TRUE` (default), values in each column are prefixed by column names
#' @param sep_colname_value Symbol used to separate column name and corresponding values. Default is `" = "`. Only relevant when `add_colname_prefix = TRUE`
#' @param sep_cols Symbol used to separate columns. Default is `", "`
#' @param new_colname Column name for new variable. Default is `"group"`
#' @param reverse If `TRUE`, levels of output variable are reversed in `create_levels()`
#' @param as_fct If `TRUE` (default), `new_colname` will be a factor. If `TRUE`, `new_colname` will be a character
#' @returns Data frame with new variable for combination of values in columns defined in `...`
#' @export
fct_cross <- function(
    df,
    ...,
    add_colname_prefix = TRUE,
    sep_colname_value = " = ",
    sep_cols = ", ",
    new_colname = "group",
    reverse = FALSE,
    as_fct = TRUE) {
  df_cols <- dplyr::select(df, ...)
  if (add_colname_prefix) {
    for (i in names(df_cols)) {
      df_cols[[i]] <- sprintf("%s%s%s", i, sep_colname_value, .subset2(df_cols, i))
    }
  }
  z <- do.call(paste, c(df_cols, sep = sep_cols))
  name <- .safe_name(.new = new_colname, .old = names(df), .sep = "")
  if (new_colname != name) {
    Warning(sprintf("In 'fct_cross', new_colname = '%s' but this column already exists in df.\n\nSetting new column name to '%s'", new_colname, name))
  }
  df[[name]] <- if (as_fct) factor(z, levels = create_levels(z, reverse = reverse)) else z
  df
}

#' Add binary variables for each level of grouping variable
#'
#' @param df Data frame
#' @param ... Categorical columns to create binary variables. Enter using tidyselect syntax. Default uses all columns in `df`
#' @param .sep Separator between column name and value for categorical variables. Default is `"_"`
#' @returns Data frame with new binary variables coded as 0 (feature absent) or 1 (feature present).
#' @export
dummy <- function(df, ..., .sep = "_") {
  vars <- Select(df, ...)
  vars <- names(vars)
  z <- df[vars]
  z[] <- lapply(z, as_fct)
  levels <- vector(mode = "list", length = length(vars))
  form <- paste0(vars, "-1")
  names(form) <- names(levels) <- vars
  for (i in vars) {
    terms <- stats::model.frame.default(formula = create_formula(NULL, form[i]), data = z, na.action = stats::na.pass)
    df_temp <- stats::model.matrix.default(object = terms, data = terms)
    lvls <- gsub(paste0("^\\`?", i, "\\`?"), "", colnames(df_temp))
    #colnames(df_temp) <- paste(i, make.names(lvls), sep = .sep)
    colnames(df_temp) <- paste(i, gsub(" ", "_", lvls, fixed = TRUE), sep = .sep)
    z <- cbind(z, df_temp)
  }
  cbind(df, z[, Setdiff(colnames(z), vars), drop = FALSE])
}

# Rownames ----------------------------------------------------------------

#' Remove row names from data frame
#'
#' @param df Data frame
#' @returns Data frame
#' @noRd
remove_rownames <- function(df) {
  attr(df, "row.names") <- .set_row_names(Nrow(df))
  df
}

#' Create new column in data frame using row names
#'
#' @param df Data frame
#' @param new_colname Name for new column. Enter as length 1 character vector. If already present in `df`, will edit to make unique
#' @param remove_rownames If `TRUE` (default), row names will be removed in output
#' @returns Data frame
#' @noRd
rownames_to_col <- function(df, new_colname, remove_rownames = TRUE) {
  new_colname <- .safe_name(new_colname, names(df))
  df[[new_colname]] <- rownames(df)
  if (remove_rownames) {
    df <- remove_rownames(df)
  }
  df
}

#' Set row names using column data
#'
#' @param df Data frame
#' @param col Column in `df` that will form row names of output. Enter as length 1 character vector
#' @param remove_col If `TRUE` (default), `col` will be removed from output
#' @returns Data frame
#' @noRd
col_to_rownames <- function(df, col, remove_col = TRUE) {
  rownames(df) <- .subset2(df, col)
  if (remove_col) {
    df[[col]] <- NULL
  }
  df
}

# Other -------------------------------------------------------------------

#' Transpose a data frame
#'
#' @param df Data frame
#' @param col_names Character vector of column names for output data frame
#' @returns Data frame. If any column in `df` is a character, all output columns will be characters
#' @noRd
transpose_df <- function(df, col_names = "variable") {
  df_names <- names(df)
  # Don't use df_to_matrix as rownames will be removed
  df <- matrix_to_df(t.data.frame(df))
  df$variable <- df_names
  df <- df[c("variable", Setdiff(names(df), "variable"))]
  names(df)[seq_along(col_names)] <- col_names
  df
}

#' Split data frame by unique values across 1 or more columns
#'
#' @param df Data frame
#' @param by Columns in `df` to split `df`. Enter as character, integer, or logical vector
#' @param sep Character to separate values. Only relevant if `length(by) > 1L`. Enter as length 1 character vector. Default is `"_"`
#' @param drop Handling of unused levels of grouping variable created by `by`. A factor variable is created in which levels (groups) are formed for every possible combination of values across columns defined in `by`. `drop` is relevant when 1 or more of these potential combinations is not actually present in `df`. If `drop = TRUE`, such combinations (levels) will be removed from output list. If `FALSE` (default), unused combinations will create empty data frames (0 rows) in output list
#' @returns List of data frames for each unique combination of values for columns in `by`
#' @export
split_df <- function(df, by, sep = "_", drop = FALSE) {
  idx <- split.default(seq_nrow(df), f = as.list(df[by]), drop = drop, sep = sep)
  lapply(idx, function(i) df[i, , drop = FALSE])
}
