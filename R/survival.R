# Data --------------------------------------------------------------------

#' Set maximum duration of follow up time
#'
#' @param df Data frame
#' @param max_time Maximum duration of follow up. Enter as length 1 numeric vector
#' @param time_var Variable containing follow up time. Enter as length 1 character vector. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as `1` (event) or `0` (no event/censored). Enter as length 1 character vector. Default is `"death"`
#' @returns Data frame with updated time in `time_var` column and updated outcome variable in `outcome_var` column
#' @export
set_max_follow_up_time <- function(df, max_time, time_var = "time", outcome_var = "death") {
  if (!is_binary_01(df[[outcome_var]])) {
    Stop("In 'set_max_follow_up_time', 'outcome_var' must be a binary variable coded as 1 for event and 0 for no event")
  }
  df <- remove_na(df, c(time_var, outcome_var))
  idx <- df[[time_var]] > max_time
  df[[outcome_var]][idx] <- 0
  df[[time_var]][idx] <- max_time
  df
}

# Survival functions ------------------------------------------------------

#' coxph wrapper
#'
#' @param df Data frame
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector
#' @param time_var Variable containing follow up time. Enter as length 1 character vector. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as length 1 character vector. Default is `"death"`
#' @param ... Arguments passed to `survival::coxph`
#' @returns Output from `survival::coxph` (list) as well as "outcome_var", "time_var", "predictor_var", "univariate" (logical), "outcome_binary" (logical. `TRUE` if outcome is binary and both outcomes occur in all groups)
#' @export
Coxph <- function(df, predictor_var, time_var = "time", outcome_var = "death", ...) {
  out <- .survival_fn(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, survival_fn = survival::coxph, ...)
  out$outcome_var <- outcome_var
  out$time_var <- time_var
  out$predictor_var <- predictor_var
  outcomes <- .subset2(df, outcome_var)
  outcome_binary <- length(unique(outcomes)) == 2L
  predictor_cat <- length(predictor_var) == 1L && (inherits(df[[predictor_var]], c("factor", "ordered", "logical", "character", "integer", "labelled")) || is_integerish(df[[predictor_var]]))
  out$outcome_binary <- outcome_binary && predictor_cat && all(vapply(split.default(outcomes, f = factor(df[[predictor_var]])), function(x) length(unique(x)) == 2L, logical(1), USE.NAMES = FALSE))
  out$univariate <- length(predictor_var) < 2L
  out
}

#' survfit wrapper
#'
#' @rdname Coxph
#' @export
Survfit <- function(df, predictor_var, time_var = "time", outcome_var = "death", ...) {
  out <- .survival_fn(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, survival_fn = survival::survfit, ...)
  out$outcome_var <- outcome_var
  out$time_var <- time_var
  out$predictor_var <- predictor_var
  out$univariate <- length(predictor_var) < 2L
  out
}

#' survdiff wrapper
#'
#' @rdname Coxph
#' @param method Method for determining P value. Options: `"MH"` (default, Mantel-Haenszel test), `"PP"` (Peto and Peto modification of Gehan-Wilcoxon test)
#' @export
Survdiff <- function(df, predictor_var, time_var = "time", outcome_var = "death", method = "MH", ...) {
  rho <- if (method == "MH") 0 else 1
  numeric_predictors <- vars_which(df[predictor_var], function(x) is.numeric(x) && n_unique(x) > 10L)
  if (length(numeric_predictors) > 0L) {
    message("Only categorical variables should be entered into 'predictor_var' argument of Survdiff()\nThe following variables are likely continuous: ", paste0(numeric_predictors, collapse = ", "))
  }
  out <- .survival_fn(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, survival_fn = survival::survdiff, rho = rho)
  out$outcome_var <- outcome_var
  out$time_var <- time_var
  out$predictor_var <- predictor_var
  out$univariate <- length(predictor_var) < 2L
  out
}

# P values ----------------------------------------------------------------

#' Log-rank p value
#'
#' @param df Data frame
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector
#' @param time_var Variable containing follow up time. Enter as length 1 character vector. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as length 1 character vector. Default is `"death"`
#' @param method Method for determining P value. Options: `"MH"` (default, Mantel-Haenszel test), `"PP"` (Peto and Peto modification of Gehan-Wilcoxon test)
#' @param otherwise Value to return if unable to perform log-rank test
#' @returns Length 1 numeric vector containing P value
#' @export
p_log_rank <- function(df, predictor_var, time_var = "time", outcome_var = "death", method = "MH", otherwise = NA_real_) {
  tryCatch({
    z <- Survdiff(df = df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var, method = method)
    stats::pchisq(z$chisq, length(z$n) - 1, lower.tail = FALSE)
    }, error = function(e) otherwise)
}

#' P value for log rank test
#'
#' @rdname p_log_rank
#' @param x coph or survdiff object
#' @export
p_surv <- function(x) {
  if (inherits(x, "coxph")) {
    stats::pchisq(x$score, sum(!is.na(x$coefficients)), lower.tail = FALSE)
  } else {
    stats::pchisq(x$chisq, length(x$n) - 1L, lower.tail = FALSE)
  }
}

#' Test Cox model for proportional hazards
#'
#' @param df Data frame or coxph object
#' @inheritParams Coxph
#' @returns Data frame with columns "predictor_var" (1 row for global model) and "p". If P < 0.05, proportional hazards assumption is violated
#' @export
p_prop_hazards <- function(df, predictor_var, time_var = "time", outcome_var = "death") {
  if (is.data.frame(df)) {
    df <- Coxph(df, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
  }
  df <- survival::cox.zph(df)
  df <- matrix_to_df(df$table, rownames_to_col = TRUE, colname_rownames = "predictor_var")
  df[c("predictor_var", "p")]
}

# Broom tidiers -----------------------------------------------------------

#' Summary of coxph object
#'
#' Inspired by `broom::tidy`
#' @param x coxph object
#' @param ci Confidence interval. Enter as numeric 0-1. Default is `0.95`
#' @param clean_predictor_names If `TRUE` (default), predictor column of output is cleaned and output contains separate columns for predictor_var ("variable" in output) and level of predictor variable ("predictor" in output)
#' @returns Data frame with columns "outcome_var", "time_var", "predictor_var", "predictor", "hr", "hr_lower", "hr_upper", "p_wald", "n", "n_events", "univariate", "covariates"
#' @export
surv_tidy <- function(x, ci = 0.95, clean_predictor_names = TRUE) {
  beta <- x$coefficients
  label <- names(beta)
  beta <- as.vector(beta)
  se <- sqrt(diag(x$var))
  X <- beta/se
  z <- stats::qnorm(0.5 + ci/2, 0, 1)*se
  out <- vec_to_df(
    outcome_var = x$outcome_var,
    label = gsub("TRUE|FALSE", "", label),
    time_var = x$time_var,
    hr = exp(beta),
    hr_lower = exp(beta - z),
    hr_upper = exp(beta + z),
    p_wald = stats::pchisq(X*X, 1, lower.tail = FALSE),
    n = x$n,
    n_events = x$nevent,
    univariate = x$univariate)
  out$covariates <- list(x$predictor_var)
  out <- dplyr::left_join(out, .regression_var_lookup(x), by = "label")
  out[c("outcome_var", "time_var", "predictor_var", "predictor", "hr", "hr_lower", "hr_upper", "p_wald", "n", "n_events", "univariate", "covariates")]
}

#' Summarize coxph model
#'
#' Inspired by `broom::glance`
#' @param x coxph object
#' @returns Data frame with 1 row and columns for "outcome_var", "time_var", "covariates", "n", "n_events", "p_wald_model" (Wald test P value  for global model), "p_log_rank_coxph_model" (P value for log-rank test for global model), "p_log_likelihood_model" (log likelihood ratio test P value for global model), "concordance", "concordance_se", "r_sq", "r_sq_max", "aic", "bic", "univariate"
#' @export
surv_glance <- function(x) {
  beta <- as.vector(x$coefficients)
  deg_free <- length(beta[!is.na(beta)])
  loglik <- x$loglik
  loglik_1 <- loglik[1L]
  loglik_2 <- loglik[2L]
  logtest <- 2*(loglik_2 - loglik_1)
  n <- x$n
  concordance <- as.vector(x$concordance[c(6L, 7L)])
  vars <- .regression_vars(x)
  out <- vec_to_df(
    outcome_var = vars$outcome_var,
    time_var = vars$time_var,
    n = n,
    n_events = x$nevent,
    p_log_rank_coxph_model = stats::pchisq(x$score, deg_free, lower.tail = FALSE),
    p_wald_model = stats::pchisq(as.vector(x$wald.test), deg_free, lower.tail = FALSE),
    p_log_likelihood_model = stats::pchisq(logtest, deg_free, lower.tail = FALSE),
    concordance = concordance[1L],
    concordance_se = concordance[2L],
    r_sq = 1 - exp(-logtest/n),
    r_sq_max = 1 - exp(2*loglik_1/n),
    aic = stats::AIC(x),
    bic = stats::BIC(x),
    univariate = vars$univariate)
  out$covariates <- list(vars$predictor_var)
  move_cols(out, "outcome_var", "time_var", "covariates")
}

# Summary functions -------------------------------------------------------

#' Pairwise HR, CI, P values
#'
#' @param df Data frame
#' @param predictor_var Column in `df` containing grouping variable. Pairwise comparisons will be made across unique values of `df[[predictor_var]]`. Enter as character vector
#' @param time_var Column in `df` containing follow up time. Enter as length 1 character vector. Default is `"time"`
#' @param outcome_var Column in `df` containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as length 1 character vector. Default is `"death"`
#' @param adjust_method Method of adjustment for multiple comparisons. Default is `"holm"`
#' @returns Data frame with columns "Group1", "Group2", "hr", "hr_lower", "hr_upper", "p_lrt", "p_lrt_adj", "label", "p_wald", "p_wald_adj", "n", "n_events"
#' @export
surv_pairwise <- function(df, predictor_var, time_var = "time", outcome_var = "death", adjust_method = "holm") {
  df <- df[c(time_var, predictor_var, outcome_var)]
  df <- remove_na(df)
  predictor <- .subset2(df, predictor_var)
  groups <- unique(predictor)
  group_pairs <- combos_list(groups, n = 2, na.rm = TRUE)
  out <- lapply(group_pairs, function(x) {
    df1 <- df[predictor %in% x, , drop = FALSE]
    coxph_model <- Coxph(df1, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
    out <- tryNULL(surv_tidy(coxph_model, clean_predictor_names = FALSE))
    if (is.null(out)) {
      out <- coxph_model
      out$n <- Nrow(df1)
    } else {
      out$n <- coxph_model$n
      out$n_events <- coxph_model$nevent
      out$p_lrt <- p_log_rank(df1, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
    }
    out$Group1 <- x[1L]
    out$Group2 <- x[2L]
    out
  })
  #out <- dplyr::bind_rows(out)
  out <- do.call(rbind, out)
  if (!any(names(out) == "n_events")) {
    out$predictor <- out$Group2
    out$n_events <- 0L
  } else {
    out$p_lrt_adj <- p_adjust(out$p_lrt, method = adjust_method)
    out <- out[order(out$p_lrt_adj), , drop = FALSE]
    out$p_wald_adj <- p_adjust(out$p_wald, method = adjust_method)
    out$label <- sig_stars(out$p_lrt_adj)
    out$adjust_method <- adjust_method
    out$n_events <- as.integer(out$n_events)
    out$n_events[is.na(out$n_events)] <- 0L
  }
  cols <- Intersect(c("outcome_var", "time_var", "predictor_var", "predictor", "Group1", "Group2", "hr", "hr_lower", "hr_upper", "p_lrt", "p_lrt_adj", "label", "p_wald", "p_wald_adj", "n", "n_events", "univariate", "adjust_method"), names(out))
  out[cols]
}

#' Extract sample sizes, number of events, and P value from survdiff object
#'
#' @param x survdiff object
#' @returns List containing "p_lrt" and "groups" (data frame with information about grouping variables, sample size (n), number of events (n_events))
#' @export
survdiff_summary <- function(x) {
  group_sizes <- x$n
  df_groups <- strsplit(names(group_sizes), ", ", fixed = TRUE)
  df_groups <- Reduce(rbind, df_groups)
  df_names <- gsub("=.*", "", df_groups)[1L, ]
  df_groups <- matrix_to_df(gsub(".*=", "", df_groups))
  names(df_groups) <- df_names
  df_groups$n <- as.vector(group_sizes)
  df_groups$n_events <- x$obs
  list(p_lrt = stats::pchisq(x$chisq, length(x$n) - 1L, lower.tail = FALSE), groups = df_groups)
}

# Cutpoints ---------------------------------------------------------------

#' Determine optimal cutpoint for a continuous variable as a predictor of survival
#'
#' @param df Data frame
#' @param time_var Variable containing follow up time. Enter as length 1 character vector. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as length 1 character vector. Default is `"death"`
#' @param predictor_var Continuous predictor variable(s). Enter as character vector
#' @param min_prop Minimum proportion of all subjects in smaller of 2 groups when stratifying by cutpoint. Enter as length 1 numeric 0-1. Default is `0.1`
#' @returns Data frame with columns "outcome_var", "time_var", "predictor_var", "cutpoint"
#' @export
cutpoint_survival <- function(df, predictor_var, time_var = "time", outcome_var = "death", min_prop = 0.1) {
  outcome_var <- get_input(outcome_var)
  time_var <- get_input(time_var)
  df <- remove_na(df, c(outcome_var, time_var, predictor_var))
  if (Nrow(df) == 0L) return(NULL)
  time <- .subset2(df, time_var)
  outcome <- .subset2(df, outcome_var)
  predictor <- .subset2(df, predictor_var)
  time_ordered <- order(time)
  time_rank <- as.integer(rank(time))
  z <- outcome/(length(outcome) - time_rank + 1L)
  z <- outcome - cumsum(z[time_ordered])[time_rank]
  z_length <- length(z)
  y <- z[order(predictor)]
  x <- sort.int(predictor, method = "quick")
  z <- which(!duplicated(x)) - 1L
  if (all(z < floor(z_length*min_prop))) Stop("In 'cutpoint_survival', 'min_prop' is too large")
  z <- z[z >= max(1, floor(z_length*min_prop))]
  z <- z[z <= floor(z_length*(1 - min_prop))]
  v <- sum(y)
  a <- z/z_length*v
  b <- z*(z_length - z)/(z_length*z_length*(z_length - 1L))*(z_length*sum(y*y) - v*v)
  a <- abs((cumsum(y)[z] - a)/sqrt(b))
  vec_to_df(
    outcome_var = outcome_var,
    time_var = time_var,
    predictor_var = predictor_var,
    cutpoint = x[z[min(which(a == max(a)))]]
  )
}

#' Evaluate specific cutpoint
#'
#' @param df Data frame
#' @param predictor_var Continuous predictor variable. Enter as quoted or unquoted variable name
#' @param cp,cutpoint Cutpoint to be evaluated. Enter as length 1 numeric vector
#' @param time_var Variable containing follow up time. Enter as quoted or unquoted variable name. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as `1` (event) or `0` (no event/censored). Enter as quoted or unquoted variable name. Default is `"death"`
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of predictor_var greater than cutpoint
#' @returns Data frame with 1 row for each cutpoint and columns "predictor_var", "cutpoint", "hr", "hr_lower", "hr_upper", "p_lrt", "label", "n", "n_events", "pos", "neg", "outcome_var", "time_var", "perc_pos", "perc_neg", "type", "p_wald"
#' @export
eval_cutpoint_survival <- function(
    df,
    predictor_var,
    cp,
    time_var = "time",
    outcome_var = "death",
    type = ">=",
    cutpoint = cp) {
  predictor_var <- get_input(predictor_var)
  outcome_var <- get_input(outcome_var)
  time_var <- get_input(time_var)
  df <- remove_na(df, c(predictor_var, outcome_var, time_var))
  predictor <- df[[predictor_var]]
  # df$pos <- as.integer(eval(str2lang(paste("predictor", type, cutpoint))))
  df$pos <- .cont_to_binary_01(predictor, cutpoint = cutpoint, type = type)
  out <- tryNULL(surv_tidy(Coxph(df = df, predictor_var = "pos", time_var = time_var, outcome_var = outcome_var)))
  if (is.null(out)) return(NULL)
  out$predictor_var <- predictor_var
  out$type <- type
  out$cutpoint <- cutpoint
  out$pos <- sum(df$pos, na.rm = TRUE)
  out$perc_pos <- out$pos/out$n
  out$neg <- out$n - out$pos
  out$perc_neg <- out$neg/out$n
  out$p_lrt <- p_log_rank(df = df, predictor_var = "pos", time_var = time_var, outcome_var = outcome_var)
  out$label <- sig_stars(out$p_lrt)
  out[c("predictor_var", "cutpoint", "hr", "hr_lower", "hr_upper", "p_lrt", "label", "n", "n_events", "pos", "neg", "outcome_var", "time_var", "perc_pos", "perc_neg", "type", "p_wald")]
}

#' Evaluate all possible cutpoints
#'
#' Positive refers to patients with predictor_var > cp (not >= cp)
#' @param df Data frame
#' @param predictor_var Name of variable in `df` containing continuous predictor. Enter as quoted or unquoted variable name
#' @param time_var Name of variable in `df` containing follow up time. Enter as quoted or unquoted variable name. Default is `"time"`
#' @param outcome_var Name of variable in `df` containing outcome coded as `1` (event) or `0` (no event/censored). Enter as quoted or unquoted variable name. Default is `"death"`
#' @param type If `>=` (default), positivity is determined by values greater than or equal to cutpoint.  If `>`, positivity is determined by values of predictor_var greater than cutpoint
#' @param min_prop Minimum proportion of all subjects in smaller of 2 groups created when stratifying by cutpoint to positive and negative groups. Enter as length 1 numeric 0-1. Default is `0.1`
#' @returns Data frame with 1 row for each unique value of predictor variable and columns for "outcome_var", "time_var", "predictor_var", "type", "cutpoint", "hr", "hr_lower", "hr_upper", "p_lrt", "label", "p_wald", "n", "n_events", "pos", "perc_pos", "neg", "perc_neg"
#' @export
eval_all_cutpoints_survival <- function(df, predictor_var, time_var = "time", outcome_var = "death", type = ">=", min_prop = 0.1) {
  predictor_var <- get_input(predictor_var)
  outcome_var <- get_input(outcome_var)
  time_var <- get_input(time_var)
  unique_cps <- unique(df[[predictor_var]])
  min_max <- Quantile(df[[predictor_var]], probs = c(min_prop, 1 - min_prop))
  unique_cps <- unique_cps[unique_cps >= min_max[1L] & unique_cps <= min_max[2L]]
  out <- remove_null(lapply(unique_cps, function(x) eval_cutpoint_survival(df = df, cutpoint = x, predictor_var = predictor_var, outcome_var = outcome_var, time_var = time_var, type = type)))
  if (length(out) == 0L) return(NULL)
  out <- do.call(rbind, out)
  out[order(out$p_lrt), , drop = FALSE]
}

# Earliest significant time point -----------------------------------------

#' Evaluate all time points to identify time point when curves first separate
#'
#' @param df Data frame or grouped data frame
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector
#' @param time_var Name of variable in `df` containing follow up time as numeric. Enter as length 1 character vector. Default is `"time"`
#' @param outcome_var Name of variable in `df` containing outcome coded as 1 (event) or 0 (no event/censored). Enter as length 1 character vector. Default is `"death"`
#' @param time_by Interval between time points. Enter as length 1 integer vector. Default is `1L`
#' @returns Data frame with columns "time_var, "time", "variable", "predictor", "hr", "hr_lower", "hr_upper", "p_lrt", "p_wald", "n_events", "n", "n_events_ever", "cfd_events" (proportion of events that occurred by a given time as as a proportion of total events that ever occurred)
#' @export
earliest_sig_surv <- function(df, predictor_var, time_var = "time", outcome_var = "death", time_by = 1L) {
  time_range <- ceiling(range(.subset2(df, time_var)))
  time_eval <- seq.int(time_range[1L], time_range[2L], by = time_by)
  surv_names <- c("time_var", "time", "predictor_var", "predictor", "hr", "hr_lower", "hr_upper", "p_lrt", "p_wald", "n", "n_events")
  out <- lapply(time_eval, function(x) {
    df1 <- set_max_follow_up_time(df = df, outcome_var = outcome_var, time_var = time_var, max_time = x)
    z <- surv_tidy(Coxph(df = df1, outcome_var = outcome_var, time_var = time_var, predictor_var = predictor_var))
    z$p_lrt <- p_log_rank(df = df1, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
    z$time <- x
    z[Intersect(surv_names, names(z))]
  })
  out <- do.call(rbind, out)
  out$n_events_ever <- sum(df[[outcome_var]], na.rm = TRUE)
  out$cfd_events <- out$n_events/out$n_events_ever
  out
}

# hr_ci -------------------------------------------------------------------

#' Hazard ratio, confidence interval, P values
#'
#' @param x Data frame, grouped data frame, coxph
#' @param predictor_var Name of column(s) in `x` that contains covariates in model. Enter as character vector
#' @param time_var Name of column in `x` that contains follow up time. Enter as length 1 character vector. Default is `"time"`
#' @param outcome_var Name of column in `x` that contains outcome variable coded as 1 (event) or 0 (no event/censored). Enter as length 1 character vector. Default is `"death"`
#' @param ci Confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @param ... Arguments passed to class-specific `hr_ci` function
#' @returns Data frame with columns "predictor_var", "predictor", "hr", "hr_lower", "hr_upper", "p_lrt", "p_wald", "n", "n_events"
#' @export
hr_ci <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  UseMethod("hr_ci", x)
}

#' `hr_ci` - default method
#'
#' @rdname hr_ci
#' @export
hr_ci.default <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  .stop_input_class(x, "hr_ci")
}

#' `hr_ci` - coxph
#'
#' @rdname hr_ci
#' @export
hr_ci.coxph <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  out <- surv_tidy(x, ci = ci)
  coxph_env <- attr(x$terms, ".Environment")
  df <- coxph_env$df
  if (is.null(df)) return(out)
  predictor_var <- coxph_env$predictor_var
  time_var <- coxph_env$time_var
  outcome_var <- coxph_env$outcome_var
  binary_predictors <- vars_binary(df[predictor_var])
  out$p_lrt <- NA_real_
  if (length(binary_predictors) > 0L && any(names(out) == "predictor_var")) {
    out$p_lrt[match(binary_predictors, out$predictor_var, nomatch = 0L)] <- vapply(binary_predictors, function(y) {
      p_log_rank(df = df, predictor_var = y, time_var = time_var, outcome_var = outcome_var)
    }, numeric(1), USE.NAMES = FALSE)
  }
  out <- move_cols(out, "p_lrt", .before = "p_wald")
  out[order(out$p_lrt, out$p_wald), , drop = FALSE]
}

#' `hr_ci` - data frame
#'
#' @rdname hr_ci
#' @export
hr_ci.data.frame <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  hr_ci.coxph(Coxph(x, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var))
}

#' `hr_ci` - grouped_dr
#'
#' @rdname hr_ci
#' @export
hr_ci.grouped_df <- function(x, predictor_var = NULL, time_var = "time", outcome_var = "death", ci = 0.95, ...) {
  out <- dplyr::mutate(
    tidyr::nest(x),
    coxph_model = lapply(data, function(.x) surv_tidy(Coxph(df = .x, predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var))),
    p_lrt = vapply(data, function(.x) p_log_rank(df = .x, outcome_var = outcome_var, time_var = time_var, predictor_var = predictor_var), numeric(1), USE.NAMES = FALSE)
  )
  out <- tidyr::unnest(out, coxph_model)
  out <- dplyr::ungroup(out)
  out <- out[Setdiff(names(out), "data")]
  #out <- dplyr::select(out, -data)
  out[order(out$p_lrt), , drop = FALSE]
}

# Helpers -----------------------------------------------------------------

#' Create survival formula
#'
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector. Default includes all variables
#' @param time_var Variable containing follow up time. Enter as quoted variable name. Default is `"time"`
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as quoted variable name. Default is `"death"`
#' @returns Formula
#' @noRd
create_surv_formula <- function(predictor_var = ".", time_var = "time", outcome_var = "death") {
  as_formula(paste0(paste(sprintf("survival::Surv(time = %s, event = %s)", time_var, outcome_var), paste(predictor_var, collapse = " + "), sep = " ~ ")), env = parent.frame())
}

#' Survival function capable of taking quoted variable names
#'
#' @param df Data frame
#' @param predictor_var Variable(s) to use as covariates in model. Enter as character vector
#' @param time_var Variable containing follow up time. Enter as character vector
#' @param outcome_var Variable containing outcome variable coded as 1 (event) or 0 (no event/censored). Enter as character vector
#' @param survival_fn Function from survival package. Options: `survival::coxph` (default), `survival::survdiff`, `survival::survfit`
#' @param otherwise Output if unable to run survival model. Default is `NULL`
#' @param ... Arguments passed to `survival_fn`
#' @returns Output from running `survival_fn`. Used by `Survdiff`, `Survfit`, `Coxph`
#' @noRd
.survival_fn <- function(df, predictor_var, time_var = "time", outcome_var = "death", survival_fn = survival::coxph, otherwise = NULL, ...) {
  df <- df[c(time_var, outcome_var, predictor_var)]
  df <- remove_na(df)
  factor_vars <- vars_which(df, function(x) n_unique(x, na.rm = TRUE) <= 2L && is.numeric(x))
  vars_recode <- Setdiff(factor_vars, c(time_var, outcome_var))
  if (length(vars_recode) > 0L) {
    df[vars_recode] <- lapply(df[vars_recode], function(x) factor(x, levels = create_levels(x)))
  }
  formula <- create_surv_formula(predictor_var = predictor_var, time_var = time_var, outcome_var = outcome_var)
  tryCatch(suppressWarnings(eval(bquote(survival_fn(formula = .(formula), data = df, ...)))), error = function(e) otherwise)
}
