#' purrr
#'
#' @name purrr
#' @importFrom purrr map_dfr map2_dfr pmap pmap_dfr reduce flatten
NULL

#' rlang
#'
#' sym: converts string -> symbol
#' syms: converts character vector -> list of symbols
#'
#' enquo: returns an unevaluated expression that can be run later
#' enquos: returns a list of unevaluated expressions that can be run later
#'
#' expr: run expression that was previously wrapped in !!enquo
#' exprs: run expression that was previously wrapped in !!!enquos
#'
#' as_name: converts variable name -> string
#' as_label: same as as_name but input can be quoted function call or vector
#'
#' @name tidyeval
#' @importFrom rlang expr enquo enquos sym syms ensym as_name as_label as_function eval_bare last_error last_trace .data :=
NULL

#' where function from tidyselect package
#'
#' @param fn Predicate function
#' @noRd
where <- function(fn)  {
  predicate <- as_function(fn)
  function(x, ...) {
    out <- predicate(x, ...)
    out
  }
}
