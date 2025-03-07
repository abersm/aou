# get_input ---------------------------------------------------------------

#' Get input to function if evaluation is possible, otherwise get input as character
#'
#' @param .input Input. Can be piped
#' @returns If `.input` is `NULL` or can be evaluated to a vector, output is same as input. Otherwise, output is a character representing the typed input. Objects in global environment are ignored
#' @export
get_input <- function(.input) {
  input <- tryCatch(suppressWarnings(force(.input)), error = function(e) "try-error")
  if (is.null(input)) return(NULL)
  if (is.vector(input) && !identical(input, "try-error") && !is.list(input)) return(input)
  input_substituted <- substitute(.input)
  for (i in seq_len(sys.nframe())) {
    if (typeof(input_substituted) == "language") return(deparse(input_substituted, width.cutoff = 500L))
    input_substituted <- do.call("substitute", list(as.name(input_substituted), parent.frame(i)))
  }
  as.character(input_substituted)
}

# Dots --------------------------------------------------------------------

#' Get input dots
#'
#' @param ... Input
#' @returns List of inputs
#' @noRd
get_input_dots <- function(...) {
  lapply(eval(substitute(alist(...))), function(x) {
    input <- tryNULL(suppressWarnings(eval(x)))
    if (length(input) == 0L) return(input)
    for (i in seq_len(sys.nframe())) {
      if (typeof(x) == "language") return(deparse1(x))
      x <- do.call("substitute", list(as.name(x), parent.frame(i)))
    }
    as.character(x)
  })
}

#' Collect dots as quoted terms
#'
#' @param ... Comma separated list of quoted or unquoted terms
#' @param .use_names If `TRUE`, output includes any names entered in `...`
#' @returns Character vector (terms entered in dots are not evaluated or interpreted)
#' @export
dots_as_quoted <- function(..., .use_names = FALSE) {
  unlist(lapply(eval(substitute(alist(...))), function(x) {
    gsub("\"", "", deparse(x, width.cutoff = 500L))
  }), use.names = .use_names)
}

#' Collect dots as unquoted terms
#'
#' @rdname dots_as_quoted
#' @returns List of unquoted terms. Input to `...` is not evaluated or interpreted
#' @export
dots_as_unquoted <- function(..., .use_names = FALSE) {
  # Alternative 1: eval(substitute(alist(...)), envir = parent.frame())
  # Alternative 2: as.list(substitute(list(...)))[-1L]
  unlist(rlang::eval_bare(substitute(alist(...))), use.names = .use_names)
}

#' Determine number of terms entered as dots
#'
#' @param ... Input
#' @returns Length 1 integer vector
#' @noRd
n_dots <- function(...) ...length()

# Error handling ----------------------------------------------------------

#' Alias for `Try(x, otherwise = NULL)`
#'
#' @param x Code to execute
#' @export
tryNULL <- function(x) tryCatch(suppressWarnings(x), error = function(e) NULL)

#' Wrap a function in `tryCatch`
#'
#' @param .f Expression
#' @param .otherwise Return value if `.f` results in an error. Default is `NULL`
#' @param .silent If `TRUE` (default), warnings are suppressed
#' @returns Function
#' @export
as_try_fn <- function(.f, .otherwise = NULL, .silent = TRUE) {
  force(.otherwise)
  # Next line needed so that ".f" is defined in function environment
  .f <- match.fun(.f)
  if (.silent) {
    function(...) tryCatch(suppressWarnings(.f(...)), error = function(e) .otherwise)
  } else {
    function(...) tryCatch(.f(...), error = function(e) .otherwise)
  }
}

#' Suppress messages and warnings
#'
#' @param ... Expression to evaluate
#' @returns Expression entered into `...` run without messages or warnings
#' @export
suppress <- function(...) suppressMessages(suppressWarnings(...))

#' Custom version of `warning`
#'
#' @param ... Warning message. Enter as comma separated list of strings. Entries will be concatenated using `paste`
#' @param n Number of line breaks between "Warning:" and message. Default is `1`
#' @returns Warning issued
#' @noRd
Warning <- function(..., n = 1) warning(paste0(strrep("\n", n)), ..., call. = FALSE)

#' Custom version of `stop`
#'
#' @param ... Error message. Enter as comma separated list of strings. Entries will be concatenated using `paste`
#' @param n Number of line breaks between "Error:" and message. Default is `1`
#' @returns Error issued
#' @noRd
Stop <- function(..., n = 1) stop(paste0(strrep("\n", n)), ..., call. = FALSE)

#' Stop function if package is unavailable
#'
#' @param x Package name. Enter as length 1 character vector
#' @noRd
pkg_required <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    Stop("Must install package ", x)
  }
}

#' Remove all objects from global environment
#'
#' @param all_names If `TRUE`, object names beginning with "." will be removed from global environment. Default is `FALSE`
#' @param except Objects to keep in global environment. Enter as character vector of object names. Default is `".Random.seed"`
#' @returns Objects in global environment (except those specified in `except`) silently removed
#' @export
RM <- function(all_names = FALSE, except = ".Random.seed") {
  obj <- ls(envir = globalenv(), all.names = all_names)
  if (!is.null(except)) {
    obj <- obj[obj %!in% except]
  }
  rm(list = obj, envir = globalenv())
}
