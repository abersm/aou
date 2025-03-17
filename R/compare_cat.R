#' Compare categorical variable across 2 or more groups
#'
#' @param df Data frame in long format
#' @param grouping_var Column in `df` that contains groups (i.e., columns in output table). Enter as quoted or unquoted column name
#' @param split_by Character vector of column names in `df` that will be used to split data to perform separate analysis. No yet incorporated
#' @param exclude_vars Character vector of column names in `df` to exclude from analyses
#' @param na_rm If `TRUE` (default), missing values for outcome, grouping, and subgrouping variables are removed from analyses
#' @param max_n_unique Maximum number of factor levels for a non-grouping variable to be included in output. Enter as length 1 integer. Default is `20`
#' @param as_df If `TRUE` (default), output will be a data frame. If `FALSE`, output will be a list of tables
#' @returns Data frame (if `as_df = TRUE`) or list of tables (if `as_df = FALSE`)
#' @export
compare_cat <- function(
    df,
    grouping_var,
    split_by = NULL,
    exclude_vars = NULL,
    na_rm = TRUE,
    as_df = TRUE,
    max_n_unique = 20) {
  grouping_var <- get_input(grouping_var)
  split_by <- get_input(split_by)
  if (!is.null(split_by)) Stop("In 'compare_cat', 'split_by' has not yet been incorporated")
  df <- df[Setdiff(names(df), exclude_vars)]
  df_names <- names(df)
  if (length(grouping_var) > 1L) {
    if (!all(grouping_var %in% df_names)) return(NULL)
    for (i in grouping_var) {
      df[[i]] <- sprintf("%s%s%s", i, " = ", .subset2(df, i))
    }
    df[["___GROUPS"]] <- do.call(paste, c(df[grouping_var], sep = ", "))
    df_names <- c(Setdiff(df_names, grouping_var), "___GROUPS")
    df <- df[df_names]
    grouping_var <- "___GROUPS"
  } else if (grouping_var %!in% df_names) {
    return(NULL)
  }
  df_names <- vars_which(df[Setdiff(df_names, grouping_var)], function(x) {
    n_unique(x) <= max_n_unique && (!is.numeric(x) || all(floor(x) == x, na.rm = TRUE))
  })
  df_names <- vars_continuous(df[Setdiff(df_names, grouping_var)], invert = TRUE)
  if (length(df_names) == 0L) return(NULL)
  if (na_rm) {
    df <- remove_na(df, grouping_var)
    y <- .subset2(df, grouping_var)
    y_levels <- if (is.factor(y)) attr(y, "levels") else unique(y)
    #y_levels <- unique(y)
    n_y_levels <- length(y_levels)
    if (n_y_levels < 2L) return(NULL)
    y_idx <- match(y, y_levels)
    out <- lapply(df[df_names], function(x) {
      idx <- !is.na(x)
      x <- x[idx]
      if (length(x) == 0L) return(NULL)
      x_levels <- unique(x)
      n_x_levels <- length(x_levels)
      out <- match(x, x_levels) + n_x_levels*(y_idx[idx]) - n_x_levels
      out <- tabulate(out, n_x_levels*n_y_levels)
      array(out, dim = c(n_x_levels, n_y_levels), dimnames = list(x_levels, y_levels))
    })
  } else {
    y <- .subset2(df, grouping_var)
    if (anyNA(y)) {
      df[[grouping_var]][is.na(y)] <- "NA"
      y <- .subset2(df, grouping_var)
    }
    y_levels <- unique(y)
    n_y_levels <- length(y_levels)
    if (n_y_levels < 2L) return(NULL)
    y_idx <- match(y, y_levels)
    out <- lapply(df[df_names], function(x) {
      x_levels <- unique(x)
      n_x_levels <- length(x_levels)
      out <- match(x, x_levels) + n_x_levels*y_idx - n_x_levels
      out <- tabulate(out, n_x_levels*n_y_levels)
      array(out, dim = c(n_x_levels, n_y_levels), dimnames = list(x_levels, y_levels))
    })
  }
  out <- remove_null(out)
  if (!as_df) return(out)
  n <- vapply(out, function(x) dim(x)[1L], integer(1))
  out <- do.call(rbind, out)
  total <- rowSums(out)
  out <- matrix_to_df(out, rownames_to_col = TRUE, colname_rownames = "group")
  out$n_total <- total
  out_names <- names(out)
  out$var <- rep(names(n), times = n)
  out[c("var", out_names)]
}
