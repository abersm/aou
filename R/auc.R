#' Area under the curve
#'
#' @param df Data frame containing dependent variable (`y`) and independent variable (`x`)
#' @param formula Formula entered as y ~ x
#' @param x,y Column names in `df` for x and y axis respectively. Enter as quoted or unquoted variable name
#' @param interval Minimum and maximum x values. Range is inclusive (i.e. []). Enter as length 2 numeric vector
#' @param method Method to calculate area under the curve. Options: `"trapezoidal"` (default), `"step"`, `"spline"`, `"all"`
#' @returns Length 1 numeric vector except when `method = "all"`, in which case output is a data frame with columns "method" and "auc"
#' @export
auc <- function(
    df,
    formula = NULL,
    x = NULL,
    y = NULL,
    interval = NULL,
    method = c("trapezoidal", "step", "spline", "all")) {
  vars <- formula2vars(formula = formula, x = get_input(x), y = get_input(y), parent_fn = "auc")
  x <- vars$x
  y <- vars$y
  df <- df[c(x, y)]
  df <- remove_na(df)
  df <- df[order(.subset2(df, x)), ]
  interval <- interval %||% range(.subset2(df, x))
  df <- df[is_between(.subset2(df, x), interval[1L], interval[2L]), , drop = FALSE]
  y <- .subset2(df, y)
  x <- .subset2(df, x)
  n <- length(y)
  method <- match.arg(method, choices = c("trapezoidal", "step", "spline", "all"))
  switch(
    method,
    trapezoidal = {
      n_1 <- seq_len(n - 1L)
      n <- seq.int(from = 2L, to = n)
      sum((x[n] - x[n_1])*(y[n_1] + y[n]))/2
    },
    step = {
      sum(y[-n]*(x[-1L] - x[-n]))
    },
    spline = {
      stats::integrate(stats::splinefun(x, y, method = "natural"), lower = min(x), upper = max(x))$value
    },
    all = {
      idx_1 <- seq_len(n - 1L)
      idx <- seq.int(from = 2L, to = n)
      auc_trap <- sum((x[idx] - x[idx_1])*(y[idx_1] + y[idx]))/2
      auc_step <- sum(y[-n]*(x[-1L] - x[-n]))
      auc_spline <- stats::integrate(stats::splinefun(x, y, method = "natural"), lower = min(x), upper = max(x))$value
      vec_to_df(method = c("trapezoid", "step", "spline"), auc = c(auc_trap, auc_step, auc_spline))
    })
}

# AUC + CI ----------------------------------------------------------------

#' AUC and confidence interval
#'
#' @param x Data frame or regression output
#' @param continuous_var Continuous (predictor) variable. Enter as quoted or unquoted variable name. Only required when `x` is a data frame
#' @param outcome_var Binary (outcome) variable. Enter as quoted or unquoted variable name. Only required when `x` is a data frame
#' @param wilcox If `TRUE` (default), `wilcox.test` is used to calculate confidence interval. If `FALSE`, Hanley-McNeil method is used to calculate confidence interval (used by Prism)
#' @param ci Confidence level. Default is `0.95`
#' @param ... Arguments passed to class-specific `auc_ci` function
#' @returns List containing "auc", "auc_lower", "auc_upper". If `x` is a data frame, "positive" and "negative" outcomes are also included
#' @export
auc_ci <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  UseMethod("auc_ci")
}

#' auc_ci - default method
#'
#' @rdname auc_ci
#' @export
auc_ci.default <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  .stop_input_class(x, fn = "auc_ci")
}

#' auc_ci - data frame
#'
#' @rdname auc_ci
#' @export
auc_ci.data.frame <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  continuous_var <- get_input(continuous_var)
  outcome_var <- get_input(outcome_var)
  x <- x[c(continuous_var, outcome_var)]
  x <- remove_na(x)
  outcome <- .subset2(x, outcome_var)
  unique_outcomes <- create_levels(outcome)
  n_outcomes <- length(unique_outcomes)
  if (n_outcomes != 2L) {
    if (n_outcomes > 2L) {
      Stop(sprintf("In 'auc_ci', 'outcome_var' must be a binary variable.\nCurrently, 'outcome_var' has %s unique groups: %s", n_outcomes, paste0(unique_outcomes, collapse = ", ")))
    } else {
      Warning(sprintf("In 'auc_ci', 'outcome_var' has %s unique groups. Unable to compute auc", n_outcomes))
      return(NULL)
    }
  }
  y <- .subset2(x, continuous_var)
  cases <-  unique_outcomes[2L]
  controls <- unique_outcomes[1L]
  cases <- y[outcome == cases]
  controls <- y[outcome == controls]
  out <- list(predictor_var = continuous_var, outcome_var = outcome_var, positive = cases, negative = controls)
  n_cases <- length(cases)
  n_controls <- length(controls)
  if (wilcox) {
    ranks <- rank(c(cases, controls))
    auroc <- (sum(ranks[seq_len(n_cases)]) - n_cases*(n_cases + 1L)/2)/(n_cases*n_controls)
    case_ranks <- rep(NA, n_cases)
    control_ranks <- rep(NA, n_controls)
    for (i in seq_len(n_cases)) {
      case_ranks[i] <- mean.default(controls < cases[i] + 0.5*(controls == cases[i]))
    }
    for (j in seq_len(n_controls)) {
      control_ranks[j] <- mean.default(cases > controls[j] + 0.5*(cases == controls[j]))
    }
    auc_se <- sqrt(Var(case_ranks)/n_controls + Var(control_ranks)/n_cases)
  } else {
    outcome_var <- c(rep(1L, n_cases), rep(0L, n_controls))
    auroc <- auc(.create_roc_data(c(cases, controls), outcome_var), controls ~ cases)
    auc_squared <- auroc*auroc
    auc_se <- sqrt((auroc*(1 - auroc) + (n_cases - 1L)*(auroc/(2 - auroc) - auc_squared) + (n_controls - 1)*(2*auc_squared/(1 + auroc) - auc_squared))/(n_cases*n_controls))
  }
  #z_score <- (auroc - 0.5)/sqrt((0.25 + n_cases + n_controls - 12)/(12*n_cases*n_controls))
  auc_ci <- auroc + stats::qnorm(0.5 + ci/2)*c(-1, 1)*auc_se
  out$auc <- auroc
  out$auc_lower <- auc_ci[1L]
  out$auc_upper <- auc_ci[2L]
  out
}

#' auc_ci - lm
#'
#' @rdname auc_ci
#' @export
auc_ci.lm <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  z <- stats::qnorm(0.5 + ci/2)*c(-1, 1)
  auroc <- survival::concordance(x)
  auc_se <- sqrt(auroc$var)
  auc_ci <- auroc$concordance + auc_se*z
  list(auc = auroc$concordance, auc_lower = auc_ci[1L], auc_upper = auc_ci[2L])
}

#' auc_ci - glm
#'
#' @rdname auc_ci
#' @export
auc_ci.glm <- auc_ci.lm

#' auc_ci - coxph
#'
#' @rdname auc_ci
#' @export
auc_ci.coxph <- function(x, continuous_var = NULL, outcome_var = NULL, wilcox = TRUE, ci = 0.95, ...) {
  z <- stats::qnorm(0.5 + ci/2)*c(-1, 1)
  auroc <- as.vector(x$concordance[c(6, 7)])
  auc_ci <- auroc[1L] + auroc[2L]*z
  list(auc = auroc[1L], auc_lower = auc_ci[1L], auc_upper = auc_ci[2L])
}
