# lm ----------------------------------------------------------------------

#' Linear regression
#'
#' Modified version of `stats::lm`
#' @param df Data frame
#' @param formula Formula
#' @param y,x Variables. Enter as quoted variable names. `x` can be multiple variable names
#' @param na.rm If `TRUE` (default), `NA` will be removed
#' @returns Output from lm
#' @export
Lm <- function(df, formula = NULL, y = NULL, x = NULL, na.rm = TRUE) {
  formula <- formula %||% create_formula(lhs = y, rhs = x)
  df <- df[all.vars(formula)]
  if (na.rm) {
    df <- remove_na(df)
  }
  mf <- stats::model.frame(formula = formula, data = df, drop.unused.levels = TRUE)
  mt <- attr(mf, "terms")
  y <- stats::model.response(mf, "numeric")
  x <- stats::model.matrix(mt, data = mf, contrasts.arg = NULL)
  x <- stats::lm.fit(x, y)
  class(x) <- "lm"
  x$model <- mf
  x$terms <- mt
  x$xlevels <- .regression_factor_levels(mt, mf)
  x
}

#' Tidy lm output
#'
#' @param x lm object
#' @param ci Confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @returns Data frame with columns "variable", "predictor", "or", "or_lower", "or_upper", "p"
#' @export
lm_tidy <- function(x, ci = 0.95) {
  #var_lookup <- .regression_var_lookup(x)
  x_coefficients <- x$coefficients
  pnames <- names(x_coefficients)
  x_residuals <- x$residuals
  rdf <- x$df.residual
  x_rank <- x$rank
  rss <- sum(x_residuals*x_residuals)
  idx_na <- is.na(x_coefficients)
  p1 <- seq_len(x_rank)
  cov_unscaled <- chol2inv(x$qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(cov_unscaled)*rss/rdf)
  estimate <- x_coefficients[x$qr$pivot[p1]]
  p_est <- cbind(or = estimate, p = 2*stats::pt(abs(estimate/se), rdf, lower.tail = FALSE))
  p_est <- matrix_to_df(p_est, rownames_to_col = TRUE, colname_rownames = "label")
  z <- rss/rdf*cov_unscaled
  P <- length(idx_na)
  x_vcov <- if (NROW(z) < P && any(idx_na)) {
    cn <- names(idx_na)
    VC <- matrix(NA_real_, nrow = P, ncol = P, dimnames = list(cn, cn))
    j <- which(!idx_na)
    VC[j, j] <- z
    VC
  } else {
    z
  }
  ses <- sqrt(diag(x_vcov))
  names(ses) <- pnames
  a <- (1 - ci)/2
  a <- c(a, 1 - a)
  fac <- stats::qt(a, rdf)
  cont_int <- array(NA_real_, dim = c(length(pnames), 2L), dimnames = list(pnames, c("or_lower", "or_upper")))
  cont_int[] <- x_coefficients[pnames] + ses[pnames] %o% fac
  cont_int <- matrix_to_df(cont_int, rownames_to_col = TRUE, colname_rownames = "label")
  out <- dplyr::left_join(p_est, cont_int, by = "label")
  out <- out[out$label != "(Intercept)", ]
  out <- dplyr::left_join(out, .regression_var_lookup(x), by = "label")
  vars <- .regression_vars(x)
  out$univariate <- vars$univariate
  out$method <- "lm"
  out$outcome_var <- vars$outcome_var
  out$covariates <- list(vars$predictor_var)
  out <- out[c("outcome_var", "predictor_var", "predictor", "or", "or_lower", "or_upper", "p", "univariate", "method", "covariates")]
  dplyr::mutate(out, dplyr::across(.cols = c(or, or_lower, or_upper), .fns = function(z) exp(z)))
}

#' Glance lm output
#'
#' @rdname lm_tidy
#' @param x lm object
#' @returns Data frame with columns "n", "r_sq", "r_sq_adj", "p", "aic", "bic", "univariate", "covariates"
#' @export
lm_glance <- function(x) {
  p <- x$rank
  rdf <- x$df.residual
  n <- NROW(x$qr$qr)
  residual <- x$residuals
  fitted_values <- x$fitted.values
  mss <- sum((fitted_values - mean(fitted_values))^2)
  rss <- sum(residual*residual)
  resvar <- rss/rdf
  r_sq <- mss/(mss + rss)
  vars <- .regression_vars(x)
  out <- vec_to_df(
    outcome_var = vars$outcome_var,
    n = n,
    r_sq = r_sq,
    r_sq_adj = 1 - (1 - r_sq)*((n - 1L)/rdf),
    p = stats::pf(mss/(p - 1L)/resvar, p - 1L, rdf, lower.tail = FALSE),
    aic = stats::AIC(x),
    bic = stats::BIC(x),
    univariate = vars$univariate
  )
  out$covariates <- list(vars$predictor_var)
  out
}

# glm ---------------------------------------------------------------------

#' Logistic regression
#'
#' Modified version of `stats::glm`
#' @param df Data frame
#' @param formula Enter in outcome ~ predictor (y ~ x) format. Set `formula = NULL` (default) if using `x` and `y`
#' @param y Outcome variable Enter as character vector. Length must be 1. Ignored if `formula` is not `NULL`
#' @param x Predictor variable(s). Length can be > 1 (multi-variate). Ignored if `formula` is not `NULL`
#' @param family Function passed to family argument of glm. Default is `stats::binomial`
#' @param na.rm If `TRUE` (default), `NA` will be removed
#' @param ... Not used
#' @returns glm object
#' @export
Glm <- function(df, formula = NULL, y = NULL, x = NULL, family = stats::binomial, na.rm = TRUE, ...) {
  formula <- formula %||% create_formula(lhs = y, rhs = x)
  vars <- all.vars(formula)
  df <- df[vars]
  if (na.rm) {
    df <- remove_na(df)
  }
  df <- .droplevels_df(df)
  y <- .subset2(df, vars[1L])
  if (!is.numeric(y) || !all(unique(y) %in% c(0, 1), na.rm = TRUE)) {
    df[[vars[1L]]] <- as_numeric_factor(y) - 1L
  }
  family <- match_fun(family)
  family <- family()
  mf <- stats::model.frame(formula = formula, data = df, drop.unused.levels = TRUE)
  mt <- attr(mf, "terms")
  y <- stats::model.response(mf, "any")
  if (length(dim(y)) == 1L) {
    nm <- rownames(y)
    dim(y) <- NULL
    if (!is.null(nm)) {
      names(y) <- nm
    }
  }
  x <- stats::model.matrix(mt, mf)
  control <- list(epsilon = 1e-8, maxit = 25, trace = FALSE)
  fit <- eval(call("glm.fit", x = x, y = y, weights = NULL, offset = NULL, family = family, control = control, intercept = attr(mt, "intercept") > 0L))
  fit$model <- mf
  fit$xlevels <- .regression_factor_levels(mt, mf)
  structure(c(fit, list(formula = formula, terms = mt, offset = NULL, contrasts = NULL, control = control)), class = c(fit$class, c("glm", "lm")))
}

#' Tidy glm output
#'
#' @param x glm object
#' @param ci Confidence interval. Enter as length 1 numeric 0-1. Default is `0.95`
#' @returns Data frame with columns "outcome", "variable", "predictor", "or", "or_lower", "or_upper", "p", "type" (Univariate or Multivariate), "method" (glm), "covariates"
#' @export
glm_tidy <- function(x, ci = 0.95) {
  n <- x$rank
  idx <- seq_len(n)
  x_coefs <- x$coefficients[x$qr$pivot[idx]]
  var_names <- names(x_coefs)
  out <- chol2inv(x$qr$qr[idx, idx, drop = FALSE])
  std_err <- sqrt(diag(out))
  tval <- x_coefs/std_err
  out <- vec_to_df(
    label = var_names,
    or = as.vector(x_coefs),
    se = std_err,
    statistic = as.vector(tval),
    p = as.vector(2*stats::pnorm(-abs(tval)))
  )
  a <- 1 - ci
  utils::flush.console()

  # profile
  alpha <- a/4
  idx_nna <- !is.na(x_coefs)
  pv0 <- t.default(as.matrix(x_coefs))
  names(std_err) <- var_names
  std_err <- as.matrix(std_err)
  mf <- stats::model.frame(x)
  mr <- stats::model.response(mf)
  mr_nrow <- NROW(mr)
  mo <- rep(0, mr_nrow)
  mw <- rep(1, mr_nrow)
  x_deviance <- x$deviance
  mm <- stats::model.matrix(x)
  x_family <- x$family
  zmax <- sqrt(stats::qchisq(1 - alpha, 1))
  prof <- vector("list", length = n)
  names(prof) <- var_names
  for (i in idx) {
    if (!idx_nna[i]) next
    zi <- 0
    pvi <- pv0
    idx_a <- idx_nna
    idx_a[i] <- FALSE
    Xi <- mm[, idx_a, drop = FALSE]
    pi <- var_names[i]
    for (j in c(-1, 1)) {
      step <- 0
      zs <- 0
      LP <- mm[, idx_nna, drop = FALSE] %*% x_coefs[idx_nna] + mo
      while ((step <- step + 1) < 10 && abs(zs) < zmax) {
        bi <- x_coefs[i] + j*step*(zmax/5)*std_err[var_names[i], 1L]
        o <- mo + mm[, i]*bi
        fm <- suppressWarnings(stats::glm.fit(x = Xi, y = mr, weights = mw, etastart = LP, offset = o, family = x_family, control = x$control))
        coefs <- fm$coefficients
        LP <- Xi %*% coefs + o
        ri <- pv0
        ri[, names(coefs)] <- coefs
        ri[, pi] <- bi
        pvi <- rbind(pvi, ri)
        zz <- fm$deviance - x_deviance
        if (zz > -0.001) {
          zz <- max(zz, 0)
        }
        zs <- j*sqrt(zz)
        zi <- c(zi, zs)
      }
    }
    si <- order(zi)
    prof[[pi]] <- structure(data.frame(zi[si]), names = "zs")
    prof[[pi]]$par.vals <- pvi[si, , drop = FALSE]
  }
  x_profile <- structure(prof, original.fit = x)
  class(x_profile) <- c("profile.glm", "profile")
  a <- a/2
  z <- stats::qnorm(c(a, 1 - a))
  ci <- array(NA, dim = c(n, 2L), dimnames = list(var_names, c("or_lower", "or_upper")))
  for (i in idx) {
    pro <- .subset2(x_profile, var_names[i])
    if (is.null(pro)) next
    sp <- stats::spline(x = pro[, "par.vals"][, i], y = pro[, 1L])
    ci[var_names[i], ] <- tryCatch(suppressWarnings(stats::approx(sp$y, sp$x, xout = z)$y), error = function(e) c(NA_real_, NA_real_))
  }
  ci <- matrix_to_df(ci, rownames_to_col = TRUE, colname_rownames = "label")
  out <- dplyr::left_join(out, ci, by = "label")
  out <- dplyr::mutate(out, dplyr::across(.cols = c(or, or_lower, or_upper), .fns = function(z) exp(as.vector(z))))
  out <- out[out$label != "(Intercept)", ]
  vars <- .regression_vars(x)
  out$univariate <- vars$univariate
  out$method <- sprintf("glm (%s)", x$family$family)
  out$outcome_var <- vars$outcome_var
  out$covariates <- list(vars$predictor_var)
  out <- dplyr::left_join(out, .regression_var_lookup(x), by = "label")
  out[c("outcome_var", "predictor_var", "predictor", "or", "or_lower", "or_upper", "p", "univariate", "method", "covariates")]
}

# Helper functions --------------------------------------------------------

#' Create regression formula
#'
#' @param x lm object
#' @param digits Number of digits after decimal to include in formula. Default is `2`
#' @param bracket_type Type of bracket to surround variables in model. Default uses brace
#' @param mult_sign String used to indicate multiplication in output. Default is `"×"`
#' @param subtract_sign String used to indicate multiplication in output. Default is `"–"`
#' @returns Length 1 character vector
#' @noRd
lm_reg_line_equation <- function(
    x,
    digits = 2,
    bracket_type = "{",
    mult_sign = "\u00d7",
    subtract_sign = "\u2013") {
  if (length(bracket_type) == 1L) {
    bracket_type <- switch(bracket_type,
           "}" = ,
           "{" = c("{", "}"),
           ")" = ,
           "(" = c("(", ")"),
           "]" = ,
           "[" = c("[", "]"),
           c("", "")
    )
  }
  #else bracket_type <- bracket_type[seq_len(2)]
  z <- x$coefficients
  vars <- names(z)[-1L]
  z <- round_up(z, digits = digits)
  intercept <- z[1L]
  intercept <- if (intercept < 0) paste("", subtract_sign, abs(intercept)) else paste(" ", "+", intercept)
  z <- paste(z[-1L], mult_sign, vars)
  z <- paste0(bracket_type[1L], z, bracket_type[2L])
  rhs <- paste0(paste0(z, collapse = " + "), intercept)
  paste(all.vars(x$terms)[1L], rhs, sep = " = ")
}

#' Extract information about variable names and univariate vs. multivariate from regression output
#'
#' @param x lm, glm, or coxph object
#' @returns List containing "outcome_var", "predictor_var", "univariate." If `x` is a coxph object, "time" also included
#' @noRd
.regression_vars <- function(x) {
  z <- attributes(x$terms)
  predictors <- z$term.labels
  if (inherits(x, "coxph")) {
    out <- list(
      outcome_var = Setdiff(names(z$dataClasses), predictors),
      time_var = NA,
      predictor_var = predictors,
      univariate = length(predictors) < 2L
    )
    z <- gsub("Surv\\(time = | event = |\\)", "", out$outcome_var)
    z <- strsplit(z, ",", fixed = TRUE)[[1L]]
    out$time_var <- z[1L]
    out$outcome_var <- z[2L]
    out
  } else {
    list(
      outcome_var = Setdiff(names(z$dataClasses), predictors),
      predictor_var = predictors,
      univariate = length(predictors) < 2L
    )
  }
}

#' Get factor levels from a regression model
#'
#' Functionality from `stats::.getXlevels`
#' @param model_terms Terms from lm or glm object
#' @param model_frame Model frame from lm or glm object
#' @noRd
.regression_factor_levels <- function(model_terms, model_frame) {
  model_terms <- attributes(model_terms)
  x <- vapply(model_terms$variables, function(z) {
    paste(deparse(z, width.cutoff = 500L, backtick = !is.symbol(z) && is.language(z)), collapse = " ")
  }, "")[-1L]
  y <- model_terms$response
  if (y > 0) {
    x <- x[-y]
  }
  if (length(x) != 0L) {
    x_unique <- lapply(model_frame[x], function(x) {
      if (is.factor(x)) {
        attr(x, "levels")
      } else if (is.character(x)) {
        attr(as.factor(x), "levels")
      }
    })
    remove_null(x_unique)
  }
}

#' Create lookup table containing variables, levels, and labels
#'
#' @param x lm, glm, or coxph object
#' @returns Lookup data frame containing columns "predictor", "predictor_var", "label". Can be left joined to data frame summary of regression model
#' @noRd
.regression_var_lookup <- function(x) {
  # All predictor variables (order matches coefficient vector)
  predictor_var <- attr(x$terms, "term.labels")

  # List of variables coded as characters or factors
  cat_levels <- x$xlevels
  if (length(cat_levels) == 0L) {
    if (inherits(x, "coxph") && any(attr(x$terms, "dataClasses") %in% c("character", "factor", "ordered", "integer"))) {
      pattern <- paste0("^", predictor_var)
      label <- names(x$coefficients)
      predictor_var <- unlist(lapply(pattern, function(z) str_extract(label, z)), use.names = FALSE)
      predictor <- vapply(seq_along(label), function(z) {
        if (predictor_var[z] == label[z]) label[z] else sub(predictor_var[z], "", label[z])
      }, character(1), USE.NAMES = FALSE)
      return(vec_to_df(predictor_var = predictor_var, predictor = predictor, label = label))
    } else {
      return(vec_to_df(predictor_var = predictor_var, predictor = predictor_var, label = predictor_var))
    }
  }
  n_levels <- lengths(cat_levels)
  cat_predictors <- names(n_levels)

  # Other predictors
  other_predictors <- Setdiff(predictor_var, cat_predictors)

  # Convert variable/level pairs to names used by regression object to create variable labels
  var_labels <- unlist(lapply(seq_along(cat_predictors), function(i) paste0(cat_predictors[i], cat_levels[[i]])), use.names = FALSE)

  # Lookup table
  vec_to_df(
    predictor_var = c(rep(cat_predictors, times = n_levels), other_predictors),
    predictor = c(unlist(cat_levels, use.names = FALSE), other_predictors),
    label = c(var_labels, other_predictors))
}
