#' Calculate correlation coefficient and P value for each pair of columns in data frame
#'
#' @param df Data frame in wide format (correlation will be calculated between columns) or grouped data frame
#' @param ... Columns in `df` to include in correlation analysis. Enter using tidyselect syntax
#' @param method Method of comparison. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param ci Confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @returns Data frame with columns "col_1", "col_2", "cor", "cor_lower", "cor_upper", "p", "label", "method", "n", "n_nna", "perc_na"
#'
#' @examples
#' # cor_pairs(covid)
#' # cor_pairs(dplyr::group_by(covid, severity))
#'
#' @export
cor_pairs <- function(df, ..., method = c("spearman", "pearson", "kendall"), ci = 0.95) {
  method <- match.arg(arg = method, choices = c("spearman", "pearson", "kendall"))
  df <- Select(df, ...)
  if (inherits(df, "grouped_df")) {
    df <- tidyr::nest(df)
    df <- dplyr::mutate(df, cor = lapply(data, .cor_pairs, method = method, ci = ci))
    df <- df[names(df) != "data"]
    df <- tidyr::unnest(df, cor)
    df <- dplyr::ungroup(df)
  } else {
    df <- .cor_pairs(df, method = method, ci = ci)
  }
  df$label <- sig_stars(df$p)
  df$perc_na <- 100 - 100*df$n_nna/df$n
  core_cols <- c("col_1", "col_2", "cor", "cor_lower", "cor_upper", "p", "label", "method", "n", "n_nna", "perc_na")
  other_cols <- Setdiff(names(df), core_cols)
  df[c(other_cols, core_cols)]
}

#' Helper function to calculate pairwise correlation coefficients and P values
#'
#' @param df Data frame
#' @param method Method of comparison. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param ci Confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @returns Data frame with columns "col_1", "col_2", "cor", "cor_lower", "cor_upper", "p", "method", "n", "n_nna"
#' @noRd
.cor_pairs <- function(df, method, ci = 0.95) {
  df <- df[vars_numeric(df)]
  df_nna <- crossprod(!is.na(df))
  df_nna[upper.tri(df_nna, diag = TRUE)] <- Inf
  df_nna <- matrix_to_df(df_nna, rownames_to_col = TRUE, colname_rownames = "col_1")
  df_nna <- tidyr::pivot_longer(df_nna, cols = -col_1, names_to = "col_2", values_to = "n_nna")
  df_nna <- df_nna[is.finite(df_nna$n_nna), , drop = FALSE]
  z <- suppress(stats::cor(x = df, y = NULL, use = "pairwise.complete.obs", method = method))
  z[upper.tri(z, diag = TRUE)] <- Inf

  # Don't use rownames_to_col in next line because z is a matrix
  z <- matrix_to_df(z, rownames_to_col = TRUE, colname_rownames = "col_1")
  z <- tidyr::pivot_longer(z, cols = -col_1, names_to = "col_2", values_to = "cor")
  z <- z[is.finite(z$cor), , drop = FALSE]
  z$method <- method
  z <- dplyr::left_join(z, df_nna, by = c("col_1", "col_2"))
  z$se <- suppress(1/sqrt(z$n_nna - 3))
  z$n <- Nrow(df)
  z$p <- 2*stats::pnorm(-abs(z$cor/z$se))
  l <- atanh(z$cor)
  k <- stats::qnorm(0.5 + ci/2)*z$se
  z$cor_lower <- tanh(l - k)
  z$cor_upper <- tanh(l + k)
  z[order(z$p), c("col_1", "col_2", "cor", "cor_lower", "cor_upper", "p", "method", "n", "n_nna"), drop = FALSE]
}

#' P value for correlation test
#'
#' @param df Data frame
#' @param formula Formula in format y ~ x (same as x ~ y)
#' @param x,y Column names containing values to correlate. Enter as length 1 character vectors. Can also enter numeric inputs for `x` and `y` (in which case, `df` must be `NULL`)
#' @param method Method for correlation. Options: `"spearman"` (default), `"pearson"`, `"kendall"`
#' @param ci Confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param otherwise Value to return if unable to perform correlation test
#' @param hypothesis_type Type of hypothesis testing to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as length 1 character vector
#' @returns Length 1 numeric vector containing P value
#' @export
p_cor <- function(df = NULL, formula = NULL, x = NULL, y = NULL, method = c("spearman", "pearson", "kendall"), ci = 0.95, hypothesis_type = c("two.sided", "less", "greater"), otherwise = NA_real_) {
  tryCatch(cor_test(df, formula = formula, x = x, y = y, method = method, ci = ci, hypothesis_type = hypothesis_type)$p, error = function(e) otherwise)
}

#' Calculate correlation coefficient and 95% CI
#'
#' @rdname p_cor
#' @returns List of 4 numeric vectors (each length 1): "cor_coeff", "cor_lower", "cor_upper", "p"
#' @export
cor_test <- function(
    df = NULL,
    formula = NULL,
    x = NULL,
    y = NULL,
    method = c("spearman", "pearson", "kendall"),
    ci = 0.95,
    hypothesis_type = c("two.sided", "less", "greater")) {
  method <- match.arg(method, choices = c("spearman", "pearson", "kendall"))
  hypothesis_type <- match.arg(hypothesis_type, choices = c("two.sided", "less", "greater"))
  if (is.data.frame(df)) {
    if (!is.null(formula)) {
      y <- all.vars(formula)
      x <- y[2L]
      y <- y[1L]
    }
    df <- remove_na(df, c(x, y))
    x <- .subset2(df, x)
    y <- .subset2(df, y)
  }
  .cor_test(x = x, y = y, method = method, ci = ci, hypothesis_type = hypothesis_type)
}

#' Calculate correlation coefficient and 95% CI
#'
#' Numeric vector input
#' @param x,y Numeric vectors to correlate. Order must be the same for `x` and `y`. Missing values are allowed
#' @param method Method for correlation. Options: `"spearman"`, `"pearson"`, `"kendall"`
#' @param ci Confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param hypothesis_type Type of hypothesis testing to perform. Options: `"two.sided"` (default), `"less"`, `"greater"`. Enter as length 1 character vector
#' @returns List of 4 numeric vectors (each length 1): "cor_coeff", "cor_lower", "cor_upper", "p"
#' @noRd
.cor_test <- function(x, y, method, ci = 0.95, hypothesis_type = "two.sided") {
  out <- suppress(stats::cor.test(x, y, method = method, conf.level = ci, alternative = hypothesis_type))
  list(
    cor_coeff = unname(out$estimate),
    cor_lower = out$conf.int[1L],
    cor_upper = out$conf.int[2L],
    p = out$p.value
  )
}
