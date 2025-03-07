#' Check if input is a formula
#'
#' @param x Input
#' @returns Length 1 logical vector
#' @export
is.formula <- function(x) {
  tryCatch(inherits(x, "formula") || (is.call(x) && x[[1L]] == "~"), error = function(e) FALSE)
}

#' Coerce string to formula
#'
#' @param x String containing formula
#' @param env Formula environment. Default is `parent.frame()`
#' @returns Formula
#' @export
as_formula <- function(x, env = parent.frame()) {
  x <- str2lang(x)
  class(x) <- "formula"
  environment(x) <- env
  x
}

#' Create formula from character vectors for left and right hand side variables
#'
#' @rdname as_formula
#' @param lhs,rhs Variables for left and right hand side of formula. Enter as character vectors
#' @returns Formula
#' @export
create_formula <- function(lhs, rhs) {
  formula <- paste(paste(lhs, collapse = " + "), paste(rhs, collapse = " + "), sep = " ~ ")
  as_formula(formula, env = parent.frame())
}

#' #' Create formula from user input
#'
#' @param formula Formula entered in y ~ x format
#' @param x,y Character vector specifying variables on left (`y`) and right (`x`) hand side of a formula
#' @param parent_fn Character vector specifying function that called `vars2formula`
#' @returns Formula
#' @noRd
vars2formula <- function(formula = NULL, x = NULL, y = NULL, parent_fn = "") {
  if (!is.null(x) || !is.null(y)) {
    if (!is.null(formula)) {
      Stop(sprintf("In '%s', must specify variables using either 'formula' (y ~ x format) or 'x' and 'y', but not both.\n\nCurrent input: formula = %s, x = %s, y = %s", parent_fn, deparse(formula, width.cutoff = 500L), x, y))
    }
    formula <- create_formula(y, x)
  }
  formula
}


#' #' Extract x and y variables from user input
#'
#' @param formula Formula in y ~ x format. Left hand side cannot contain > 1 variable
#' @param x,y Character vectors of variables used for right and left hand side of formula, respectively
#' @param parent_fn Parent function. Enter as length 1 character vector
#' @returns List containing "x" and "y", each a character vector of variable names
#' @noRd
formula2vars <- function(formula = NULL, x = NULL, y = NULL, parent_fn = "") {
  if (!is.null(formula)) {
    if (!is.null(x) || !is.null(y)) {
      Stop(sprintf("In '%s', must specify variables using either 'formula' (y ~ x format) or 'x' and 'y', but not both.\n\nCurrent input: formula = %s, x = %s, y = %s", parent_fn, deparse(formula, width.cutoff = 500L), x, y))
    }
    vars <- all.vars(formula)
    y <- vars[1L]
    x <- vars[-1L]
  }
  list(x = x, y = y)
}

#' #' Extract formula and variables from user input
#'
#' @param formula Formula entered in y ~ x format
#' @param x,y Character vector specifying variables on left (`y`) and right (`x`) hand side of a formula
#' @param parent_fn Character vector specifying function that called `get_vars_formula`
#' @returns List with components "formula", "x", "y"
#' @noRd
get_vars_formula <- function(formula = NULL, x = NULL, y = NULL, parent_fn = "") {
  if (!is.null(x) || !is.null(y)) {
    if (!is.null(formula)) {
      Stop(sprintf("In '%s', must specify variables using either 'formula' (y ~ x format) or 'x' and 'y', but not both.\n\nCurrent input: formula = %s, x = %s, y = %s", parent_fn, deparse(formula, width.cutoff = 500L), x, y))
    }
    formula <- create_formula(y, x)
  } else {
    vars <- all.vars(formula)
    y <- vars[1L]
    x <- vars[-1L]
  }
  list(formula = formula, x = x, y = y)
}
