# Edit lists --------------------------------------------------------------

#' Remove NULL values from a list
#'
#' @param x List
#' @returns List without `NULL` or other length 0 elements
#' @export
remove_null <- function(x) {
  #x[!vapply(x, function(z) length(z) == 0, FUN.VALUE = logical(1), USE.NAMES = FALSE)]
  x[lengths(x, use.names = FALSE) > 0L]
}

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

#' Transpose a list
#'
#' @param x List
#' @returns List
#' @noRd
transpose <- function(x) {
  # do.call(mapply, c(FUN = c, x, USE.NAMES = TRUE, SIMPLIFY = simplify))
  do.call(Map, c(c, x, USE.NAMES = TRUE))
}

#' Silently load 1 or more packages
#'
#' @param ... Enter comma separated list of unquoted or quoted package names without `c()`
#' @export
lib <- function(...) {
  pkg_required("cli")
  pkg_required("crayon")
  pkgs <- dots_as_quoted(...)
  not_installed <- pkgs[pkgs %!in% pkg_installed()]
  if (length(not_installed) == 0L) {
    lapply(pkgs, function(x) tryNULL(suppressWarnings(suppressPackageStartupMessages(library(x, character.only = TRUE)))))
    z <- if (length(pkgs) == 1L) sprintf("%s already installed!", pkgs) else "All packages already installed!"
    return(cli::cli_alert_success(crayon::green(z)))
  }
  if (any(idx_github <- grepl("/", not_installed, fixed = TRUE))) {
    pkg_required("devtools")
    lapply(not_installed[idx_github], function(x) invisible(devtools::install_github(x)))
    not_installed <- not_installed[!idx_github]
  }
  if (length(not_installed) > 0) {
    z <- tryCatch(utils::install.packages(not_installed, dependencies = TRUE), warning = function(e) TRUE, error = function(e) TRUE)
    if (!is.null(z) && z) {
      pkg_required("BiocManager")
      tryCatch(suppress(BiocManager::install(not_installed)), error = function(e) TRUE)
    }
  }
  lapply(pkgs, function(x) tryNULL(suppressWarnings(suppressPackageStartupMessages(library(x, character.only = TRUE)))))
  if (any(idx <- pkgs %!in% pkg_installed())) {
    cli::cli_warn(crayon::green(sprintf("Unable to install the following packages:\n%s", .quote_collapse(pkgs[idx], sep = "\n"))))
  } else {
    return(cli::cli_alert_success(crayon::green("All new packages installed!")))
  }
  invisible(pkgs)
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
#' @param n Number of rows to display. Default is `15`
#' @returns Print first `n` rows of `df`
#' @export
top <- function(df, n = 15) print.data.frame(df, n = n)

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
#' @rdname read_r
#' @returns Contents of rds file
#' @export
read_rds <- function(file_name, ...) {
  file_name <- get_input(file_name)
  if (!file.exists(file_name)) {
    dots <- dots_as_quoted(...)
    file_name <- .get_file_path(x = file_name, ext = "rds", dots = dots, desktop = TRUE)
  }
  con <- file(file_name)
  on.exit(close(con))
  readRDS(con, refhook = NULL)
}

#' Import rda file
#'
#' @rdname read_r
#' @param file_name Name of rda file or path to file
#' @param ... Arguments passed to `paste_path()`
#' @returns Contents of rda file
#' @export
read_rda <- function(file_name, ...) {
  file_name <- get_input(file_name)
  if (file.exists(file_name)) {
    file_location <- file_name
  } else {
    dots <- dots_as_quoted(...)
    file_location <- .get_file_path(x = file_name, ext = "rda", dots = dots, desktop = TRUE)
    if (is.null(file_location)) {
      file_location <- .get_file_path(x = file_name, ext = "Rdata", dots = dots, desktop = TRUE)
      if (is.null(file_location)) return(invisible())
    }
  }
  env <- new.env()
  load(file = file_location, envir = env)
  get(x = str_remove_ext(basename(file_name)), envir = env)
}

#' Export rda file
#'
#' @rdname read_r
#' @param x Name of object in global environment to save
#' @param file_name Name of rda file
#' @param directory Directory where rda object will be saved. Default is `desktop_path()`
#' @returns rda file silently exported
#' @export
write_rda <- function(x, file_name = NULL, directory = desktop_path()) {
  file_name <- get_input(file_name) %||% get_input(x)
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
