# Other class to data frame -----------------------------------------------

#' Convert vectors to data.frame
#'
#' @param ... Vectors or list of vectors. Can have different lengths (lengths do not need to share a common multiple which is in contrast to vector to data frame conversion by data.frame() function)
#' @param .prefix Prefix for newly created column names. Default is `"V"`. Only relevant when `.col_names = NULL`
#' @param .col_names,.row_names Column and row names for output data frame
#' @returns Data frame. Each input vector forms a column in output data frame. If vectors do not share the same length, shorter vectors are recycled to match the length of the longest input vector (see examples)
#' @export
vec_to_df <- function(..., .col_names = NULL, .prefix = "V", .row_names = NULL) {
  x <- if (is.list(x <- c(...))) x else list(...)
  z <- lengths(x, use.names = FALSE)
  if (length(z) == 0L) return(empty_df())
  zero_idx <- z == 0
  if (any(zero_idx)) {
    x[zero_idx] <- NULL
    z <- z[z != 0]
  }
  n <- max(z)
  idx <- z != n
  if (any(idx)) {
    for (i in which(idx)) {
      vals <- .subset2(x, i)
      x[[i]] <- rep_len(vals, length.out = n)
    }
  }
  if (is.null(.col_names) || length(.col_names) != length(x)) {
    .col_names <- .force_names(x, prefix = .prefix)
  }
  if (is.null(.row_names) || length(.row_names) != n) {
    .row_names <- c(NA_integer_, -n)
  }
  names(x) <- .col_names
  class(x) <- "data.frame"
  attr(x, "row.names") <- .row_names
  x
}

#' Combine list of vectors column-wise to create data frame
#'
#' Opposite of `as.list.data.frame`
#' @param x List of vectors. Each vector must have same length or must share a common multiple (see examples for details). To convert a list of vectors to rows of a data frame, use `rbind_list()`
#' @returns Data frame with elements of input list (vectors) forming columns of output data frame
#'
#' @examples
#' \dontrun{
#' ## Identical to data.frame(a = 1:3, b = 4:6)
#' # cbind_list(list(a = 1:3, b = 4:6))
#' ## Identical to data.frame(a = 1:3, b = 4:9)
#' # cbind_list(list(a = 1:3, b = 4:9))
#' ## Results in error: same as data.frame(a = 1:3, b = 4:8)
#' # cbind_list(list(a = 1:3, b = 4:8))
#' }
#'
#' @export
cbind_list <- function(x) {
  x <- remove_null(x)
  if (length(x) == 0L) return(NULL)
  names(x) <- .force_names(x)
  z <- lengths(x, use.names = FALSE)
  n <- max(z)
  idx <- z != n
  if (any(idx)) {
    for (i in which(idx)) {
      x[[i]] <- rep_len(.subset2(x, i), length.out = n)
    }
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -n)
  x
}

#' Combine list of vectors row-wise to create data frame
#'
#' Similar to `Reduce(rbind, list)`
#' @param ... Vectors or list of vectors used to create rows in output data frame. If the lengths of list components are not equal, shorter elements will be recycled to the length of the longest element (i.e. for a single row, values will be recycled across the leftover columns)
#' @param rownames_to_col If `TRUE`, rownames of `x` are used to create a new column in output data frame. Default is `FALSE`
#' @param colname_rownames Name of column in output data frame containing rownames of `x`. Only relevant when `rownames_to_col = TRUE`. Default is `"rowname"`
#' @param prefix Prefix for newly created column names. Default is `"V"`
#' @returns Data frame with elements of input combined to form rows of data frame
#'
#' @examples
#' \dontrun{
#' # rbind_list(list(a = 1:5, b = 6:10)) # Data frame with 2 rows and 5 columns
#' # rbind_list(list(a = 1, b = 6:10)) # Data frame with 2 rows (1st row contains all 1)
#' }
#'
#' @export
rbind_list <- function(..., rownames_to_col = FALSE, colname_rownames = "rowname", prefix = "V") {
  x <- if (is.list(x <- c(...))) x else list(...)
  matrix_to_df(do.call(rbind, x), rownames_to_col = rownames_to_col, prefix = prefix, colname_rownames = colname_rownames)
}

#' Convert a matrix to a data frame
#'
#' @param x Matrix
#' @param rownames_to_col If `TRUE`, rownames of `x` are used to create a new column in output data frame. Default is `FALSE`
#' @param colname_rownames Name of column in output data frame containing rownames of `x`. Only relevant when `rownames_to_col = TRUE`. Default is `"rowname"`
#' @param prefix Prefix for newly created column names. Default is `"V"`
#' @returns Data frame
#'
#' @examples
#' \dontrun{
#' # matrix_to_df(matrix(1:20, nrow = 2))
#' }
#'
#' @export
matrix_to_df <- function(x, rownames_to_col = FALSE, colname_rownames = "rowname", prefix = "V") {
  # Dimensions of input matrix
  dims <- dim(x)
  n_rows <- dims[[1L]]
  n_cols <- dims[[2L]]
  col_idx <- seq_len(n_cols)

  # Row/column names of input matrix
  x_names <- dimnames(x)
  col_names <- x_names[[2L]]
  if (is.null(col_names)) {
    col_names <- paste0(prefix, col_idx)
  } else {
    # Identify any columns with names
    empty_col_names <- !nzchar(col_names)
    if (any(empty_col_names)) {
      # Create new column names with a numeric component that reflects column number in input matrix
      col_names[empty_col_names] <- paste0(prefix, col_idx)[empty_col_names]
    }
  }

  # Split matrix into list of columns
  df <- lapply(col_idx, function(i) x[, i])
  #df <- vector("list", n_cols)
  #for (i in col_idx) {
  #  df[[i]] <- x[, i]
  #}
  # Convert list of columns to data frame
  class(df) <- "data.frame"
  attr(df, "row.names") <- c(NA_integer_, -n_rows)
  names(df) <- col_names
  if (rownames_to_col && !is.null(row_names <- x_names[[1L]])) {
    df[[colname_rownames]] <- row_names
    df[c(colname_rownames, col_names)]
  } else {
    df
  }
}

#' Convert components of matrix to data frame
#'
#' Code from stackoverflow.com/questions/18127476/extract-one-triangle-of-a-correlation-matrix-with-attributes
#' Primary use is to convert pairwise matrix to a data frame containing columns for row name, column name, and value
#' @param x Matrix. Must have row names and column names
#' @param upper,lower,diag Component of matrix to include in output. Enter each as a length 1 logical
#' @returns Data frame with columns "row", "col", "value"
#' @noRd
.matrix_component_to_df <- function(x, upper = TRUE, lower = FALSE, diag = FALSE) {
  z <- dimnames(x)
  idx <- if (upper) {
    upper.tri(x, diag = diag)
  } else if (lower) {
    lower.tri(x, diag = diag)
  } else if (diag) {
    dims <- dim(x)
    idx_diag <- 1L + seq.int(from = 0L, to = min(dims) - 1L)*(dims[1L] + 1L)
    idx <- matrix(NA, nrow = dims[1L], ncol = dims[2L])
    idx[idx_diag] <- TRUE
    idx
  } else {
    if (!diag) return(empty_df())
    dims <- dim(x)
    matrix(TRUE, nrow = dims[1L], ncol = dims[2L])
  }
  idx <- which(idx, arr.ind = TRUE)
  vec_to_df(
    row = z[[1L]][idx[, 1L]],
    col = z[[2L]][idx[, 2L]],
    value = x[idx]
  )
}


#' Convert named vector to data frame with 2 columns (1 column for values, 1 column for named)
#'
#' @param x Named vector
#' @param colname_names,colname_values Column names for names and values, respectively. Enter as length 1 character vector
#' @returns Data frame with 2 columns
#' @export
named_vector_to_df <- function(x, colname_names = "names", colname_values = "values") {
  vec_to_df(names = names(x), values = unname(x), .col_names = c(colname_names, colname_values))
}

# Data frame to other class -----------------------------------------------

#' Convert data frame to matrix
#'
#' Functionality from `base::data.matrix`
#' @param df Data frame
#' @param rownames Row names for output matrix. Enter as character vector with length equal to `nrow(df)`. If specified, `col_to_rownames` must be `NULL`
#' @param col_to_rownames Column in `df` containing row names for output matrix. Note: column containing row names will not be included in output matrix. Enter as length 1 character (column name) or integer (column number) vector. If specified, `rownames` must not be specified
#' @returns Matrix. If `col_to_rownames = NULL`, dimensions of output matrix will be identical to dimensions of `df`. If `col_to_rownames` is specified, output matrix will have `nrow(df)` rows and `ncol(df) - 1L` columns
#'
#' @examples
#' \dontrun{
#' # df_to_matrix(mtcars)
#' }
#'
#' @export
df_to_matrix <- function(df, rownames = NULL, col_to_rownames = NULL) {
  if (!is.null(col_to_rownames)) {
    if (!missing(rownames)) {
      Stop("In 'df_to_matrix', row names for output matrix can be specified using either 'rownames' argument or 'col_to_rownames' argument, but not both")
    }
    if (is.numeric(col_to_rownames)) {
      col_to_rownames <- names(df)[col_to_rownames]
    }
    rownames <- .subset2(df, col_to_rownames)
    df <- df[Setdiff(names(df), col_to_rownames)]
  }
  d <- dim(df)
  n_col <- d[2L]
  n_row <- d[1L]
  idx_col <- seq_len(n_col)
  for (i in idx_col) {
    xi <- .subset2(df, i)
    if (is.numeric(xi)) {
      next
    }
    if (inherits(xi, c("factor", "character", "logical"))) {
      df[[i]] <- as_numeric_factor(xi)
      next
    }
    df[[i]] <- as.numeric(xi)
  }
  x <- matrix(if (all(vapply(df, is.integer, logical(1)))) NA_integer_ else NA_real_, nrow = n_row, ncol = n_col, dimnames = list(rownames, names(df)))
  for (i in idx_col) {
    x[, i] <- .subset2(df, i)
  }
  x
}

#' Convert 2 columns of data frame to a named vector
#'
#' @param df Data frame
#' @param values,names Columns in `df` containing values and names of output vector, respectively. Enter as string (column name) or integer (column position)
#' @returns Named vector
#' @noRd
df_to_named_vector <- function(df, values, names) {
  out <- .subset2(df, values)
  names(out) <- .subset2(df, names)
  out
}
