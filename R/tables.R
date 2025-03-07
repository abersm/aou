# Tables ------------------------------------------------------------------

#' Contingency table
#'
#' @param df Data frame
#' @param ... Variables to create table. Enter as character vector, comma separated list of unquoted or unquoted column names, or formula
#' @param incl_na If `FALSE` (default), NA values are not included in table. If `TRUE`, NA is included in table
#' @returns Table
#'
#' @examples
#' \dontrun{
#' # covid |> xtab(death, gender)
#' # covid |> x_table(death, gender) # Output same as above
#' # table(covid$death, covid$gender) # Output same as above but names(dimnames(x)) is NULL
#' }
#'
#' @export
xtab <- function(df, ..., incl_na = FALSE) {
  dots <- dots_as_quoted(...)
  n <- n_dots(...)
  vars <- if (n == 0L) {
    names(df)
  } else if (n == 1L) {
    if (startsWith(dots[[1L]], "~") && is.list(vars <- c(...))) {
      all.vars(as_formula(as.character(vars)))
    } else {
      tryCatch({
        vars <- c(...)
        if (is.character(vars) && all(vars %in% names(df))) vars else dots
      }, error = function(e) dots)
    }
  } else {
    dots
  }
  df <- lapply(df[vars], function(x) {
    if (!is.factor(x)) {
      x <- factor(x, exclude = if (!incl_na) c(NA, NaN))
    }
    if (incl_na && anyNA(x)) {
      x_levels <- attr(x, "levels")
      if (!anyNA(x_levels)) {
        x_levels <- c(x_levels, NA)
      }
      x <- factor(x, levels = x_levels, exclude = NULL)
    }
    x
  })
  table(df, dnn = names(df))
}

#' Cross table with or without row/column totals
#'
#' Same as `xtab` but option to include row/column totals
#' @rdname xtab
#' @param df Data frame
#' @param ... Columns in `df` used to generate contingency table. Enter using tidyselect syntax
#' @param total If `FALSE` (Default), no columns or rows for totals in output table. If `TRUE`, row and column totals are included in output table
#' @param incl_na If `TRUE` (default), missing values are included in output
#' @param title_row_totals Title of column containing row totals. Default is `"Total"`
#' @param title_col_totals Title of row containing column totals. Default is `"Total"`
#' @returns Cross table +/- rows and columns for totals
#'
#' @examples
#' \dontrun{
#' # covid |> x_table(death, gender)
#' # covid |> xtab(death, gender) # Output same as above
#' # table(covid$death, covid$gender) # Output same as above but names(dimnames(x)) is NULL
#' }
#'
#' @export
x_table <- function(df, ..., total = FALSE, incl_na = TRUE, title_row_totals = "Total", title_col_totals = "Total") {
  vars <- names(dplyr::select(df, ...))
  df <- xtab(df = df, vars, incl_na = incl_na)
  if (total) {
    add_totals(df, title_row_totals = title_row_totals, title_col_totals = title_col_totals)
  } else {
    df
  }
}

#' Add column and row totals to table or matrix
#'
#' @param x Table or array/matrix
#' @param title_row_totals Title of column containing row totals. Default is `"Total"`
#' @param title_col_totals Title of row containing column totals. Default is `"Total"`
#' @returns Matrix containing row and column sums as well as raw numbers in `x`. Number in bottom right hand corner of matrix contains overall total
#'
#' @examples
#' \dontrun{
#' # table(covid$death, covid$gender) |> add_totals()
#' }
#'
#' @export
add_totals <- function(x, title_row_totals = "Total", title_col_totals = "Total") {
  dims <- dim(x)
  n_rows <- dims[1L]
  n_cols <- dims[2L]
  other_dims <- dims[-c(1L, 2L)]
  z <- matrix(x, nrow = n_rows)
  z <- rbind(z, .colSums(z, n_rows, n_cols))
  z <- matrix(t.default(z), nrow = n_cols)
  z <- rbind(z, .colSums(z, n_cols, n_rows + 1L))
  z <- t.default(matrix(t.default(z), nrow = prod(other_dims)))
  z <- array(z, c(dims + 1, other_dims))
  rownames(z) <- c(rownames(x, do.NULL = FALSE), title_col_totals)
  colnames(z) <- c(colnames(x, do.NULL = FALSE), title_row_totals)
  x_names <- names(dimnames(x))
  if (!is.null(x_names)) {
    names(dimnames(z)) <- x_names
  }
  z
}

#' Convert counts in contingency table to column proportions
#'
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `x` is a data frame, `incl_na` determines whether missing values form distinct groups
#' @returns Table containing column proportions (i.e., column proportions sum to 1)
#'
#' @examples
#' \dontrun{
#' # table(covid$death, covid$gender) |> col_prop() |> colSums() # Output: c(1, 1)
#' # covid |> col_prop(death, gender) |> colSums() # Output same as above
#' }
#'
#' @export
col_prop <- function(x, ..., incl_na = FALSE) {
  x <- if (is.data.frame(x)) {
    xtab(df = x, names(dplyr::select(x, ...)), incl_na = incl_na)
  } else {
    as.table(x)
  }
  sweep(x, 2, marginSums(x, 2), "/", check.margin = FALSE)
}

#' Convert counts in contingency table to row proportions
#'
#' @rdname col_prop
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `TRUE` (default), missing values are included in output
#' @returns Table containing row proportions (i.e., row proportions sum to 1)
#'
#' @examples
#' \dontrun{
#' # table(covid$death, covid$gender) |> row_prop() |> rowSums() # Output: c(1, 1)
#' # covid |> row_prop(death, gender) |> rowSums() # Output same as above
#' }
#'
#' @export
row_prop <- function(x, ..., incl_na = FALSE) {
  x <- if (is.data.frame(x)) {
    xtab(df = x, names(dplyr::select(x, ...)), incl_na = incl_na)
  } else {
    as.table(x)
  }
  sweep(x, 1, marginSums(x, 1), "/", check.margin = FALSE)
}

#' Convert counts in contingency table to column percentages
#'
#' @rdname col_prop
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `x` is a data frame, `incl_na` determines whether missing values form distinct groups
#' @param digits Number of digits to include after decimal. Default is `1`
#' @returns Table containing column percentages (i.e., column percentages sum to 100)
#'
#' @examples
#' \dontrun{
#' # table(covid$death, covid$gender) |> col_perc() |> colSums() # Output: c(100, 100)
#' # covid |> col_perc(death, gender) |> colSums() # Output same as above
#' }
#'
#' @export
col_perc <- function(x, ..., incl_na = FALSE, digits = 1) {
  x <- col_prop(x, ..., incl_na = incl_na)
  round_up(x*100, digits = digits)
}

#' Convert counts in contingency table to row percentages
#'
#' @rdname col_prop
#' @param x Contingency table or data frame
#' @param ... If `x` is a data frame, enter column names using tidyselect syntax
#' @param incl_na If `x` is a data frame, `incl_na` determines whether missing values form distinct groups
#' @param digits Number of digits to include after decimal. Default is `1`
#' @returns Table containing row percentages (i.e., row percentages sum to 100)
#'
#' @examples
#' \dontrun{
#' # table(covid$death, covid$gender) |> row_perc() |> rowSums() # Output: c(100, 100)
#' # covid |> row_perc(death, gender) |> rowSums() # Output same as above
#' }
#'
#' @export
row_perc <- function(x, ..., incl_na = FALSE, digits = 1) {
  x <- row_prop(x, ..., incl_na = incl_na)
  round_up(x*100, digits = digits)
}

# 2 x 2 tables ------------------------------------------------------------

#' Create 2 x 2 table for a broad range of input types
#'
#' @param x Data frame of counts, matrix, table, or length 4 numeric vector of counts. Enter by column (i.e., row 1/col 1, then row 2/col 1, then row 1/col 2, then row 2/col 2)
#' @param ... Arguments passed to class-specific `tab_2_by_2` function
#' @returns 2 x 2 table
#' @export
tab_2_by_2 <- function(x, ...) UseMethod("tab_2_by_2")

#' tab_2_by_2 - default method
#'
#' @rdname tab_2_by_2
#' @export
tab_2_by_2.default <- function(x, ...) {
  Stop(sprintf("In 'tab_2_by_2', not sure how to handle input of class %s", .quote_collapse(class(x))))
}

#' tab_2_by_2 - numeric
#'
#' @param x Numeric vector
#' @param ... Counts or arguments passed to table
#' @export
tab_2_by_2.numeric <- function(x, ...) {
  x_counts <- c(x, ...)
  if (length(x) != 4L) {
    x <- table(x, ...)
    dims <- dim(x)
    if (!all(dims == 2L)) {
      Stop(sprintf("In 'tab_2_by_2', numeric vector input must consist of either a length 4 numeric vector or 2 binary vectors that generate a 2 x 2 table.\nCurrent input has length %s and table(x, ...) generates a %s x %s table", length(x_counts), dims[1L], dims[2L]))
    }
    return(x)
  }
  as.table(matrix(x_counts, nrow = 2))
}

#' tab_2_by_2 - logical
#'
#' @param x Logical vector
#' @param ... Arguments passed to table
#' @export
tab_2_by_2.logical <- function(x, ...) {
  x <- table(x, ...)
  dims <- dim(x)
  if (!all(dims == 2L)) {
    Stop(sprintf("In 'tab_2_by_2', logical vector input must generate a 2 x 2 table.\ntable(x, ...) applied to current input generates a %s x %s table", dims[1L], dims[2L]))
  }
  x
}

#' tab_2_by_2 - character
#'
#' @param x Character vector
#' @param ... Arguments passed to table
#' @export
tab_2_by_2.character <- function(x, ...) {
  x <- table(x, ...)
  dims <- dim(x)
  if (!all(dims == 2L)) {
    Stop(sprintf("In 'tab_2_by_2', character vector input must generate a 2 x 2 table, not a %s x %s table", dims[1L], dims[2L]))
  }
  x
}

#' tab_2_by_2 - factor vector
#'
#' @rdname tab_2_by_2.character
#' @export
tab_2_by_2.factor <- tab_2_by_2.character

#' tab_2_by_2 - matrix
#'
#' @param x 2 x 2 matrix
#' @param ... Not used
#' @export
tab_2_by_2.matrix <- function(x, ...) {
  dims <- dim(x)
  if (!all(dims == 2L)) {
    Stop(sprintf("In 'tab_2_by_2', matrix input must be 2 x 2, not %s x %s", dims[1L], dims[2L]))
  }
  as.table(x)
}

#' tab_2_by_2 - table
#'
#' @param x 2 x 2 table
#' @param ... Not used
#' @export
tab_2_by_2.table <- function(x, ...) {
  dims <- dim(x)
  if (!all(dims == 2L)) {
    Stop(sprintf("In 'tab_2_by_2', table input must be 2 x 2, not %s x %s", dims[1L], dims[2L]))
  }
  x
}

#' tab_2_by_2 - data.frame
#'
#' @param x Data frame of counts
#' @param ... Columns to use in `x`. Enter as formula (y ~ x or ~ y + x. outcome ~ predictor), comma separated list of quoted or unquoted column names not wrapped in `c()`, or tidyselect syntax
#' @export
tab_2_by_2.data.frame <- function(x, ...) {
  # Attempt to create 2 x 2 table using xtab
  x_xtab <- tryCatch(xtab(x, ...), error = function(e) NULL)
  if (!is.null(x_xtab)) {
    dims <- dim(x_xtab)
    if (all(dims == 2L)) return(x_xtab)
  }

  # If x is the output of count() or tab_to_counts()
  # Remove NA if x is the output of count()
  x_counts <- x[stats::complete.cases(x), , drop = FALSE]
  dims <- dim(x_counts)
  # If x contains a column of counts, number of rows should be 4
  if (dims[1L] == 4L) {
    if (dims[2L] == 1L) {
      # If x contains 1 column, create table of counts
      as.table(matrix(unlist(x_counts), nrow = 2))
    } else if (dims[2L] %in% c(2, 3)) {
      # If x contains 2 or 3 columns, find column containing counts (i.e. non-binary numeric variable)
      counts_var <- vars_which(x_counts, function(y) !is_binary(y) && is.numeric(y))
      # Ensure x_count has appropriate row order
      if (length(counts_var) == 1L) {
        # x_counts has only 1 column that possibly contains count data
        group_vars <- Setdiff(names(x_counts), counts_var)
        # Arrange rows in order of other columns
        row_order <- lapply(group_vars, function(y) x_counts[[y]])
        x_counts <- x_counts[do.call(order, row_order), , drop = FALSE]
        if (length(group_vars) == 2L) {
          # If 2 grouping variables in x_counts, create dimnames
          tab_rows <- unique.default(x_counts[group_vars[1L]][[1L]])
          tab_cols <- unique.default(x_counts[group_vars[2L]][[1L]])
          dim_names <- list(tab_rows, tab_cols)
          names(dim_names) <- group_vars
        } else {
          # If > 2 grouping variables in x_counts, don't attempt to set dimnames
          dim_names <- list(NULL)
        }
        as.table(matrix(unlist(x_counts[[counts_var]]), nrow = 2, byrow = TRUE, dimnames = dim_names))
      } else {
        # Send message if x_counts contains multiple columns that possibly contains count data
        message("In 'tab_2_by_2', data frame input has dimensions of ", paste(dims, collapse = " x "), "\nUnclear which column contains count data")
      }
    } else {
      Stop("In 'tab_2_by_2, not able to handle data frame input with dimensions of  ", paste(dims, collapse = " x "))
    }
  } else {
    # x is not a data frame of counts. Determine which columns were selected in dots
    dots_class <- vapply(dots_as_unquoted(...), class, character(1), USE.NAMES = FALSE)
    vars <- if (dots_class[1L] == "call") all.vars(...) else names(dplyr::select(x, ...))
    n_vars <- length(vars)
    if (n_vars == 2L) {
      x[stats::complete.cases(x[, vars, drop = FALSE]), , drop = FALSE]
    } else {
      Stop(sprintf("In 'tab_2_by_2', when 'x' is a data frame, 2 columns can be selected in '...'\nCurrent input to '...' selects %s columns (%s)", n_vars, paste0(vars, collapse = ", ")))
    }
  }
}

#' tab_2_by_2 - glm
#'
#' @param x glm object
#' @param ... Not used
#' @export
tab_2_by_2.glm <- function(x, ...) {
  x <- xtab(x$model)
  dims <- dim(x)
  if (!all(dims == 2L)) {
    Stop("In 'tab_2_by_2', when input is a glm, table generated by predictor and outcome variables must be 2 x 2, not", paste0(dims, collapse = " x "))
  }
  x
}

# Binary ------------------------------------------------------------------

#' Summarize all binary variables stratified by 1 binary grouping variable
#'
#' @param df Data frame
#' @param grouping_var Binary grouping variable used to define columns in output table. Enter as quoted variable name
#' @param binary_variables Binary variables to be compared between groups. Enter as comma separated list of quoted variable names wrapped in `c()`
#' @param n_format Format for sample size entry in column names. Default is `n`
#' @param group_levels Levels for `grouping_var` Enter as length 2 character vector with lower level 1st
#' @param digits_or Number of digits used to print odds ratios. Default is `2`
#' @param digits_perc Number of digits for percentages. Default is `1`
#' @param incl_denominator If `TRUE` (default), format for count is n/total. If `FALSE`, format is n
#' @param perc_first If `TRUE` (default), format is % (n) or % (n/n_total). If `FALSE`, format is n (%) or n/total (%)
#' @param n_min_chisq Minimum group size that will be allowed for Chi-squared test. Default is `5`
#' @param ci Confidence interval. Default is `0.95`
#' @param total_col If `TRUE` (default), column for totals included in output table
#' @returns Data frame
#' @export
summary_table_binary <- function(
    df,
    grouping_var = NULL,
    binary_variables = NULL,
    n_format = "n",
    group_levels = NULL,
    digits_or = 2,
    digits_perc = 1,
    incl_denominator = TRUE,
    perc_first = TRUE,
    n_min_chisq = 5,
    ci = 0.95,
    total_col = TRUE) {
  # Function to create OR summary
  z <- stats::qnorm(0.5 + ci/2)*c(-1, 1)
  formatted_or <- function(x, z = z, digits_or = digits_or) {
    if (any(x == 0)) {
      return("Unable to calculate OR")
    }
    or <- x[1L]*x[4L]/(x[3L]*x[2L])
    or <- c(or, or*exp(z*sqrt(sum(1/x))))
    format_num_range(or[1L], or[2L], or[3L], digits = digits_or)
  }

  # Variables to be compared across groups
  vars <- binary_variables %||% vars_which(df, is_binary)
  grouping_var <- get_input(grouping_var)
  if (is.null(grouping_var)) Stop("In 'summary_table_binary', must specify 'grouping_var'")
  vars <- Setdiff(vars, grouping_var)
  df <- df[c(grouping_var, vars)]
  idx <- vars_which(df[vars], function(x) !is_binary_01(x) && !is.logical(x))
  var_names_new <- vapply(idx, function(x) {
    y <- df[[x]]
    levels <- if (inherits(y, "factor")) {
      y_levels <- attr(y, "levels")
      attr(factor(y, exclude = if (anyNA(y_levels)) NULL else NA), "levels")
    } else if (is_genotype(y)) {
      levels <- sort.int(unique.default(y))
      dko <- grepl("dko", levels, ignore.case = TRUE)
      if (any(dko)) {
        levels <- c(levels[dko], levels[!dko])
      }
      ko <- grepl("ko", levels, ignore.case = TRUE)
      if (any(ko)) {
        levels <- c(levels[ko], levels[!ko])
      }
      cre_negative <- levels == "Cre-"
      if (any(cre_negative)) {
        levels <- c(levels[cre_negative], levels[!cre_negative])
      }
      wt <- grepl("wt", levels, ignore.case = TRUE)
      if (any(wt)) {
        levels <- c(levels[wt], levels[!wt])
      }
      levels
    } else {
      unique.default(y)
    }
    paste(x, levels[2L], sep = "___")
  }, character(1))
  names(df)[names(df) %in% idx] <- var_names_new

  # Remove missing values
  df <- df[stats::complete.cases(df[grouping_var]), ]

  # Grouping variable
  group_levels <- group_levels %||% create_levels(df[[grouping_var]])
  group_labels <- if (is_binary_01(df[[grouping_var]])) {
    c(paste("No", grouping_var), str_capitalize(grouping_var))
  } else {
    group_levels
  }
  n_cases <- sum(df[[grouping_var]] == group_levels[2L])
  n_controls <- sum(df[[grouping_var]] == group_levels[1L])
  df$g_ <- factor(df[[grouping_var]], levels = group_levels)
  df <- df[names(df) %!in% grouping_var]

  # Long format
  out <- dplyr::mutate(df, dplyr::across(dplyr::everything(), function(x) as_numeric_factor(x) - 1))
  out <- tidyr::pivot_longer(out, cols = -g_, names_to = "variable", values_to = "value", values_drop_na = TRUE)
  names(out)[names(out) == "g_"] <- "group"

  # Contingency tables for each binary variable
  out <- dplyr::group_by(out, variable)
  out <- dplyr::summarize(out,
                         case_pos = sum(value == 1 & group == 1),
                         case_neg = sum(value == 0 & group == 1),
                         control_pos = sum(value == 1 & group == 0),
                         control_neg = sum(value == 0 & group == 0),
                         .groups = "drop")
  out <- dplyr::rowwise(out)
  out <- dplyr::mutate(out,
                      cases = format_n_perc(case_pos, case_pos + case_neg, digits = digits_perc, incl_denominator = incl_denominator, perc_first = perc_first),
                      controls = format_n_perc(control_pos, control_pos + control_neg, digits = digits_perc, incl_denominator = incl_denominator, perc_first = perc_first),
                      Method = ifelse(min(case_pos, case_neg, control_pos, control_neg) < n_min_chisq, "Fisher's exact test", "Chi-squared test"),
                      P = ifelse(Method == "Fisher's exact test", p_fisher(c(control_neg, case_neg, control_pos, case_pos)), p_chi(c(control_neg, case_neg, control_pos, case_pos))),
                      OR = formatted_or(c(control_neg, case_neg, control_pos, case_pos), z, digits_or))
  out <- dplyr::ungroup(out)
  out$total <- format_n_perc(out$case_pos + out$control_pos, out$case_pos + out$case_neg + out$control_pos + out$control_neg, digits = digits_perc, incl_denominator = incl_denominator, perc_first = perc_first)
  out <- out[c("variable", "cases", "controls", "total", "OR", "P", "Method")]
  out$variable <- gsub("___", " = ", out$variable, fixed = TRUE)
  names(out) <- c("Characteristic", paste(group_labels[2L], sprintf("(%s = %s)", n_format, n_cases)), paste(group_labels[1L], sprintf("(%s = %s)", n_format, n_controls)), paste("Total", sprintf("(%s = %s)", n_format, n_cases + n_controls)), paste0("OR (", ci*100, "% CI)"), "P-value", "Method")
  if (total_col) {
    out
  } else {
    out[-4L]
  }
}

#' Summarize all categorical variables stratified by grouping variable
#'
#' @param df Data frame in long format
#' @param grouping_var Column in `df` that contains groups (i.e., columns in output table). Enter as quoted or unquoted column name
#' @param split_by Character vector of column names in `df` that will be used to split data to perform separate analysis. No yet incorporated
#' @param exclude_vars Character vector of column names in `df` to exclude from analyses
#' @param na_rm If `TRUE` (default), missing values for outcome, grouping, and subgrouping variables are removed from analyses
#' @param max_n_unique Maximum number of factor levels for a non-grouping variable to be included in output. Enter as length 1 integer. Default is `20`
#' @param digits_perc Number of digits past decimal point for percentages. Default is `1`
#' @param incl_denominator If `TRUE` (default), format for count is n/total. If `FALSE`, format is n
#' @param perc_first If `TRUE` (default), format is % (n) or % (n/n_total). If `FALSE`, format is n (%) or n/total (%)
#' @param compare_fn Function to compare count variables. Options: `p_chi` (default), `p_fisher`, `p_prop`
#' @param simplify_binary If `TRUE` (default), output includes 1 row for each binary variable. If `FALSE` each value for binary variable included in output
#' @param var_once If `TRUE`, output is formatted such that each variable appears once (included in first row for a given variable. Other rows contain NA). Useful when exporting. If `FALSE` (default), each row in output has variable
#' @param format_p_fn Function to format P values. Must accept numeric vector of P values as input. Default is `format_p_value`
#' @returns Either a data frame (if `split_by = NULL`) or a list of data frames (if `split_by` is specified)
#' @export
summary_table_cat <- function(
    df,
    grouping_var,
    split_by = NULL,
    exclude_vars = NULL,
    na_rm = TRUE,
    max_n_unique = 20,
    digits_perc = 1,
    incl_denominator = TRUE,
    compare_fn = p_chi,
    simplify_binary = TRUE,
    var_once = FALSE,
    format_p_fn = format_p_value) {
  grouping_var <- get_input(grouping_var)
  split_by <- get_input(split_by)
  if (!is.null(split_by)) {
    out <- lapply(split_df(df, split_by), function(x) {
      out <- summary_table_cat(
        x,
        grouping_var = grouping_var,
        split_by = NULL,
        exclude_vars = split_by,
        na_rm = na_rm,
        max_n_unique = max_n_unique,
        digits_perc = digits_perc,
        incl_denominator = incl_denominator,
        compare_fn = compare_fn,
        simplify_binary = simplify_binary,
        var_once = var_once,
        format_p_fn = format_p_fn
      )
      if (Nrow(out) == 0L) return(NULL)
      cbind(x[1L, split_by, drop = FALSE], out)
    })
    return(out)
  }

  # List of contingency tables
  ## Counts
  tab_list <- compare_cat(df = df, grouping_var = grouping_var, split_by = split_by, exclude_vars = exclude_vars, na_rm = na_rm, max_n_unique = max_n_unique, as_df = FALSE)
  var_names <- names(tab_list)

  ## Column percentages
  tab_perc <- lapply(tab_list, function(x) {
    col_sum <- colSums(x)
    z <- sweep(x, 2, col_sum, "/", check.margin = FALSE)
    round_up(z*100, digits = digits_perc)
  })
  n_rows <- vapply(tab_list, function(x) dim(x)[1L], integer(1))
  if (sum(n_rows) == 0L) return(NULL)

  # Counts
  tab_n <- do.call(rbind, tab_list)
  row_total <- rowSums(tab_n)
  tab_n <- matrix_to_df(tab_n, rownames_to_col = TRUE, colname_rownames = "Group")
  col_names <- names(tab_n)
  tab_n$`Total (row)` <- row_total
  tab_n$Variable <- rep(var_names, times = n_rows)

  # Column percentages
  tab_perc <- do.call(rbind, tab_perc)
  tab_perc <- matrix_to_df(tab_perc, rownames_to_col = TRUE, colname_rownames = "Group")

  # Column total
  tab_p <- rbind_list(lapply(tab_list, colSums))

  # Formatted values
  out <- tab_n
  format_fn <- if (incl_denominator) {
    function(x) {
      perc <- format_number(.subset2(tab_perc, x), digits = digits_perc)
      idx <- is.na(perc)
      out <- paste0(perc, "% (", .subset2(tab_n, x), "/", rep(.subset2(tab_p, x), times = n_rows), ")")
      out[idx] <- NA_character_
      out
    }
  } else {
    function(x) {
      perc <- format_number(.subset2(tab_perc, x), digits = digits_perc)
      idx <- is.na(perc)
      out <- paste0(perc, "% (", .subset2(tab_n, x), ")")
      out[idx] <- NA_character_
      out
    }
  }
  for (i in Setdiff(col_names, "Group")) {
    out[[i]] <- format_fn(i)
  }

  # Merge P value and column total
  for (i in seq_along(tab_p)) {
    tab_p[[i]] <- sprintf("n = %d", .subset2(tab_p, i))
  }
  tab_p$Variable <- var_names
  tab_p$P <- vapply(tab_list, compare_fn, numeric(1), USE.NAMES = FALSE)
  idx <- !is.na(tab_p$P)
  tab_p$P[idx] <- format_p_fn(tab_p$P[idx])
  tab_p$Method <- if (identical(compare_fn, p_chi)) {
    "Chi-squared test"
  } else if (identical(compare_fn, p_fisher)) {
    "Fisher's exact test"
  } else if (identical(compare_fn, p_prop)) {
    ifelse(vapply(tab_list, min, numeric(1), USE.NAMES = FALSE) < 5, "Fisher's exact test", "Chi-squared test")
  } else {
    get_input(compare_fn)
  }

  # Add row for each variable to display P values and column totals
  out <- dplyr::bind_rows(out, tab_p)
  idx <- do.call(order, list(factor(out$Variable, levels = unique(out$Variable)), out$Group, na.last = FALSE))
  out <- out[idx, , drop = FALSE]
  out <- out[c("Variable", Setdiff(names(out), "Variable"))]
  group_col_names <- Setdiff(names(out), c("Variable", "Group", "Total (row)", "P", "Method"))
  n <- n_per_group(df[grouping_var])
  if (Nrow(n) == length(group_col_names)) {
    for (i in group_col_names) {
      names(out)[names(out) == i] <- paste0(i, " (n = ", n$Freq[n$group == i], ")")
    }
    names(out)[names(out) == "Total (row)"] <- paste0("Total per row (n = ", Sum(n$Freq), ")")
  }
  if (simplify_binary) {
    idx <- rle(out$Variable)
    idx <- idx$values[idx$lengths == 3L]
    idx <- !is.na(out$Group) & duplicated(out$Variable, fromLast = TRUE) & out$Variable %in% idx
    out <- out[!idx, , drop = FALSE]
  }
  if (var_once) {
    idx <- !is.na(out$Group)
    out$Variable[idx] <- NA_character_
  }
  out
}
