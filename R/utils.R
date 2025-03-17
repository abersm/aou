#' Move columns
#'
#' @noRd
move_cols <- function(.df, ..., .before = NULL, .after = NULL) {
  cols <- names(Select(.df, ...))
  idx <- seq_along(.df)
  names(idx) <- all_cols <- names(.df)
  if (!is.null(.before)) {
    if (!is.null(.after))
      Stop("In 'move_cols', either '.before' or '.after' can be specified, but not both")
    .before <- Setdiff(.before, cols)
    first_part <- all_cols[idx < idx[.before]]
    first_part <- Setdiff(first_part, cols)
    .df[c(first_part, cols, .before, Setdiff(all_cols, c(first_part, cols, .before)))]
  } else if (!is.null(.after)) {
    .after <- Setdiff(.after, cols)
    last_part <- all_cols[idx > idx[.after]]
    last_part <- Setdiff(last_part, cols)
    .df[c(Setdiff(all_cols, c(.after, cols, last_part)), .after, cols, last_part)]
  } else {
    .df[c(cols, Setdiff(all_cols, cols))]
  }
}

#' Determine if a variable if constant
#'
#' @noRd
is_constant <- function(x, na.rm = FALSE) {
  x <- unique(x)
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  length(x) < 2L
}

#' Empty data frame
#'
#' @noRd
empty_df <- function(..., df_template = NULL) {
  if (!is.null(df_template))
    return(df_template[FALSE, ])
  if (n_dots(...) == 0L) {
    structure(list(), class = "data.frame", row.names = integer(), names = character())
  } else {
    df <- list(...)
    class(df) <- "data.frame"
    attr(df, "row.names") <- integer()
    df
  }
}

#' Remove file extension
#'
#' @noRd
str_remove_ext <- function(x) sub("\\.[^.]+$", "", x)

#' Create a safe file path
#'
#' @noRd
.safe_file_path <- function(file_name, ext, directory = desktop_path()) {
  file_name <- str_remove_ext(file_name)
  file_name <- gsub("(\\\\|/|\\$|\\?|%)+", " ", file_name)
  file_name <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$|\\(|\\)|\\[|\\]", "", file_name)
  if (file_name == "") {
    today <- floor(unclass(Sys.time())/86400)
    class(today) <- "Date"
    file_name <- sprintf("file_%s", format(today, "%Y_%m_%d"))
  }
  dir_files <- str_remove_ext(list.files(path = directory, pattern = sprintf("\\.%s$", ext), ignore.case = TRUE))
  file_name <- make.unique(c(dir_files, file_name), sep = "_")
  file_name <- file_name[length(file_name)]
  paste(gsub("/+$", "", directory), paste(file_name, ext, sep = "."), sep = "/")
}

#' Remove NULL values from a list
#'
#' @param x List
#' @returns List without `NULL` or other length 0 elements
#' @export
remove_null <- function(x) x[lengths(x, use.names = FALSE) > 0L]

#' Update list
#'
#' Functionality from `utils::modifyList`
#' @param old,new Lists containing old and new values respectively
#' @returns Updated list in which named elements shared by both lists will be updated by replacing the named element in `old` with the matching named element in `new`. If element named is present in both lists and is `NULL` in `new`, it will be removed from updated list. Named elements in `new` that do not have a matching named element in `old` will be added to `new`
#' @export
update_list <- function(old, new) {
  old_names <- names(old)
  new_names <- names(new)
  new_names <- new_names[nzchar(new_names)]
  for (i in new_names) {
    old[[i]] <- if (i %in% old_names && is.list(.subset2(old, i)) && is.list(.subset2(new, i))) {
      update_list(.subset2(old, i), .subset2(new, i))
    } else {
      .subset2(new, i)
    }
  }
  old
}

#' Not in operator
#'
#' @param lhs,rhs left and right hand side of operator, respectively
#' @name notin
#' @rdname not-in
#' @export
`%!in%` <- function(lhs, rhs) !`%in%`(lhs, rhs)

#' Waiver operator
#'
#' Functionality from Hadley Wickham's excellent package ggplot2
#' @param x Object possibly of class "waiver"
#' @param y Value to return if `x` is a waiver
#' @returns Output is `x` unless it is a "waiver" object in which case `y` is returned
#' @noRd
`%W%` <- function(x, y) if (is_waiver(x)) y else x

#' Display specified number of rows of data frame as tibble
#'
#' @param df Data frame
#' @param n Number of rows to display. Default is `100`
#' @returns Print first `n` rows of `df`
#' @export
top <- function(df, n = 100) print(df, n = n)

#' Template to create silent version of `lapply`
#'
#' @param X Input to iterate over
#' @param FUN Function to apply to each element of `X`. Can also be a name (character) or index (integer)
#' @param ... Arguments passed to `FUN`
#' @param USE.NAMES If `TRUE`, names of `X` will be applied to names of output
#' @param OTHERWISE Value to be returned if output of `FUN` results in an error. Default is `NA`
#' @param SILENT If `TRUE` (default), warnings are suppressed
#' @returns List with same length as `X`
#' @export
try_map <- function(X, FUN, ..., USE.NAMES = FALSE, OTHERWISE = NULL, SILENT = TRUE) {
  FUN <- as_try_fn(if (is.function(FUN)) FUN else function(x) .subset2(x, FUN), .otherwise = OTHERWISE, .silent = SILENT)
  out <- lapply(X = X, FUN = FUN, ...)
  if (USE.NAMES && length(out) == length(X)) {
    names(out) <- names(X)
  }
  out
}

#' Read rds file
#'
#' Functionality from readr package
#' @param x Path to rds file
#' @returns Contents of rds file
#' @export
read_rds <- function(x) {
  con <- file(x)
  on.exit(close(con))
  readRDS(con, refhook = NULL)
}

#' Import rda file
#'
#' @rdname read_rds
#' @export
read_rda <- function(x) {
  env <- new.env()
  load(file = x, envir = env)
  get(x = sub("\\.[^.]+$", "", basename(x)), envir = env)
}

#' Export rda file
#'
#' @rdname read_rds
#' @param x Name of object in global environment to save
#' @param file_name Name of rda file
#' @returns rda file silently exported
#' @export
write_rda <- function(x, file_name = NULL, directory = getwd()) {
  file_name <- file_name %||% deparse(substitute(x))
  file_name <- str_remove_ext(file_name)
  file_path <- .safe_file_path(file_name, ext = "rda", directory = directory)
  assign(file_name, value = x)
  save(list = file_name, file = file_path)
  invisible(NULL)
}

#' Convert corticosteroid dosing
#'
#' @param x Dose of drug `from`
#' @param from Starting drug. Dose in `x` for drug `from`. Options: `"methylprednisolone"` (default), `"prednisone"`, `"prednisolone"`, `"hydrocortisone"`, `"dexamethasone"`, `"betamethasone"`, `"cortisone"`, `"triamcinolone"`
#' @param to Desired output drug equivalent. Same options as `from`. Default is `"prednisone"`
#' @returns Numeric vector with same length and units as input
#' @noRd
steroid_conversion <- function(
    x,
    from = c("methylprednisolone", "prednisone", "prednisolone", "hydrocortisone", "dexamethasone", "betamethasone", "cortisone", "triamcinolone"),
    to = "prednisone") {
  z <- c(4, 5, 5, 20, 0.75, 0.75, 25, 4)/5
  rx <- c("methylprednisolone", "prednisone", "prednisolone", "hydrocortisone", "dexamethasone", "betamethasone", "cortisone", "triamcinolone")
  names(z) <- rx
  from <- match.arg(from, choices = rx)
  to <- match.arg(to, choices = rx)
  z <- x/z[from]*z[to]
  names(z) <- NULL
  z
}

#' Stop a function due to unrecognized input class
#'
#' @noRd
.stop_input_class <- function(x, fn = .fn_called(sys.nframe() - 1)) {
  Stop(sprintf("Unable to run '%s' with input of class ", fn), .quote_collapse(class(x)))
}

#' Single quote input and collapse with separator
#'
#' @noRd
.quote_collapse <- function(x, sep = ", ") paste(shQuote(x), collapse = sep)

#' Parent function called
#'
#' @noRd
.fn_called <- function(n = 1) deparse(sys.call(n)[[1L]])[1L]

is_odd <- function(x) x%%2 != 0

is_even <- function(x) x%%2 == 0

match_fun <- function(fn) {
  if (is.function(fn)) return(fn)
  if (!(is.character(fn) && length(fn) == 1L || is.symbol(fn))) {
    fn <- eval.parent(substitute(substitute(fn)))
  }
  env <- parent.frame(2)
  get(as.character(fn), mode = "function", envir = env)
}

tryElse <- function(x, otherwise = NULL, silent = TRUE) {
  if (silent) {
    tryCatch(suppressWarnings(x), error = function(e) otherwise)
  } else {
    tryCatch(x, error = function(e) otherwise)
  }
}

#' Load packages from cran
#'
#' @rdname install_if_missing_then_load
#' @export
Lib <- function(
    ...,
    lib.loc = NULL,
    warn.conflicts,
    mask.ok,
    exclude,
    include.only,
    attach.required = missing(include.only)) {
  x <- unlist(lapply(eval(substitute(alist(...))), function(x) {
    gsub("\"", "", deparse(x, width.cutoff = 500L))
  }), use.names = FALSE)
  idx <- x[!(paste0("package:", x) %in% search())]
  for (i in idx) {
    z <- tryCatch(suppressWarnings(suppressMessages(library(i, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, warn.conflicts = warn.conflicts, quietly = TRUE, mask.ok = mask.ok, exclude = exclude, include.only = include.only, attach.required = attach.required))), error = function(e) FALSE)
    if (!z) {
      suppressWarnings(suppressMessages(install.packages(x, quiet = TRUE, verbose = FALSE)))
      tryCatch(suppressWarnings(suppressMessages(library(x, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, warn.conflicts = warn.conflicts, quietly = TRUE, mask.ok = mask.ok, exclude = exclude, include.only = include.only, attach.required = attach.required))), error = function(e) FALSE)
    }
  }
}
