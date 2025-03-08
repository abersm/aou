#' Detect pattern in string
#'
#' @rdname Grepl
#' @param ignore_case If `FALSE` (default), search is case sensitive (i.e., output is only `TRUE` if the exact input to `pattern` is occurs is found in `x)
#' @param ... Arguments passed to `grepl`
#' @export
str_contains <- function(x, pattern, ignore_case = FALSE, invert = FALSE, ...) {
  out <- grepl(pattern, x, ignore.case = ignore_case, ...)
  if (invert) {
    out <- !out
  }
  out
}

#' Is string coercible to numeric
#'
#' @rdname str_contains
#' @returns Logical vector with same length as input (unless `x` is logical in which case, output is `FALSE`)
#' @export
can_be_numeric <- function(x) {
  if (is.logical(x)) return(FALSE)
  is.na(x) | !is.na(suppressWarnings(as.numeric(x)))
}

#' Count number of occurrences of substring
#'
#' @rdname str_contains
#' @returns Integer vector with same length as input
#' @export
str_count <- function(x, pattern, ignore_case = FALSE, ...) {
  vapply(gregexpr(pattern, x, ignore.case = ignore_case, ...), function(y) sum(y > 0, na.rm = TRUE), integer(1), USE.NAMES = FALSE)
}

#' Count number of words in a string
#'
#' @rdname str_count
#' @export
str_count_words <- function(x) str_count(x, "\\w+")

# Replace -----------------------------------------------------------------

#' Substitute multiple pattern/replacement pairs in a string
#'
#' @param x Character vector
#' @param pattern Character vector of search patterns to replace in `x`
#' @param replacement Character vector containing replacement patterns. Will be recycled to `length(pattern)`
#' @param ... Arguments passed to substitution function (either `sub` or `gsub`)
#' @param .sub_fn String substitution function. Default is `gsub`
#' @export
mgsub <- function(x, pattern, replacement, ..., .sub_fn = gsub) {
  n_pattern <- length(pattern)
  n_replacement <- length(replacement)
  if (n_pattern == 1L) {
    if (n_replacement != 1L) {
      Stop(sprintf("In 'mgsub', length(pattern) is 1L, but length(replacement) is %iL", n_replacement))
    }
    return(.sub_fn(pattern, replacement, x = x, ...))
  }
  if (n_replacement == 1L) {
    replacement <- rep(replacement, length.out = n_pattern)
  }
  for (i in seq_len(n_pattern)) {
    x <- .sub_fn(pattern[i], replacement[i], x = x, ...)
  }
  x
}

# Remove ------------------------------------------------------------------

#' Remove a pattern from a string
#'
#' @param x Character vector
#' @param pattern Pattern to remove from `x`. Enter as length 1 character vector
#' @param all If `TRUE` (default), all occurrences of `pattern` will be removed. If `FALSE`, only first occurrence of `pattern` will be removed
#' @param ... Arguments passed to `sub` or `gsub`
#' @returns Character vector with same length as input
#' @export
str_remove <- function(x, pattern, all = TRUE, ...) {
  sub_fn <- if (all) gsub else sub
  sub_fn(pattern, "", x = x, ...)
}

#' Remove numbers from a string
#'
#' @rdname str_remove
#' @export
str_remove_numerics <- function(x, all = TRUE) {
  fn <- if (all) gsub else sub
  fn("[0-9.]", "", x)
}

#' Remove letters from string
#'
#' @rdname str_remove
#' @param case Case of letters to be removed. Options: `"both"` (default), `"lower"`, `"upper"`
#' @export
str_remove_letters <- function(x, case = "both", all = TRUE) {
  pattern <- switch(case, lower = "[a-z]", upper = "[A-Z]", all = , "[[:alpha:]]")
  str_remove(x, pattern = pattern, all = all)
}

#' Wrapper for trimws
#'
#' @rdname str_remove
#' @param side Side of string to remove spaces. Options: `"both"` (default), `"left"`, `"right"`
#' @export
str_trimws <- function(x, side = c("both", "left", "right")) {
  side <- match.arg(side, choices = c("both", "left", "right"))
  switch(
    side,
    left = sub("^[ \t\r\n]+", "", x, perl = TRUE),
    right = sub("[ \t\r\n]+$", "", x, perl = TRUE),
    both = sub("[ \t\r\n]+$", "", sub("^[ \t\r\n]+", "", x, perl = TRUE), perl = TRUE)
  )
}

#' Remove all spaces (not just at ends of string)
#'
#' @rdname str_remove
#' @export
str_remove_space <- function(x) gsub("\\s+", "", x)

#' Convert multiple spaces to single space
#'
#' @rdname str_remove
#' @export
str_remove_multi_space <- function(x) gsub("[ \t\r\n]+", " ", x)

#' Remove substring before a given pattern (i.e. pattern retained in output)
#'
#' @rdname str_remove
#' @param name description
#' @param fixed If `TRUE` (default), `escape_regex` will be applied to `pattern` before running `sub` or `gsub`
#' @export
str_remove_before <- function(x, pattern, fixed = TRUE, all = FALSE) {
  if (fixed) {
    pattern <- escape_regex(pattern)
  }
  sub_fn <- if (all) gsub else sub
  sub_fn(paste0(".*(", pattern, ")"), "\\1", x, perl = TRUE)
}

#' Remove substring before a given pattern (i.e. pattern retained in output)
#'
#' @rdname str_remove
#' @param fixed If `TRUE` (default), `escape_regex` will be applied to `pattern` before running `sub` or `gsub`
#' @export
str_remove_after <- function(x, pattern, fixed = TRUE, all = FALSE) {
  if (fixed) {
    pattern <- escape_regex(pattern)
  }
  sub_fn <- if (all) gsub else sub
  sub_fn(paste0("(", pattern, ").*"), "\\1", x, perl = TRUE)
}

# Extract -----------------------------------------------------------------

#' Extract text following a particular pattern
#'
#' @param x Character vector
#' @param pattern Pattern to extract from `x`. Enter as length 1 character vector
#' @param all If `TRUE` (default), all occurrences of `pattern` will be extracted. If `FALSE`, only first occurrence of `pattern` will be extracted
#' @param ... Arguments passed to `gregexpr` (if `all = TRUE`) or `regexpr` (if `all = FALSE`)
#' @returns List of character vectors
#' @export
str_extract <- function(x, pattern, all = TRUE, ...) {
  regex_fn <- if (all) gregexpr else regexpr
  regmatches(x, regex_fn(pattern, x, ...))
}

#' Extract text that occurs before a specified pattern
#'
#' @rdname str_extract
#' @returns Character vector with same length as input. If no match is found, output is `""`. If multiple matches are found, only the first is returned. Names of input are retained in output
#' @export
str_extract_before <- function(x, pattern) {
  # gsub(paste0("(?=", pattern, ").*"), "", x, perl = TRUE)
  x_names <- names(x)
  pattern <- paste0(".*(?=", pattern, ")")
  x <- vapply(x, function(z) {
    z <- regmatches(z, gregexpr(pattern = pattern, text = z, perl = TRUE))[[1L]]
    if (length(z) == 0L) "" else z
  }, character(1), USE.NAMES = FALSE)
  names(x) <- x_names
  x
}

#' Extract text that follows a specified pattern
#'
#' @rdname str_extract
#' @export
str_extract_after <- function(x, pattern) {
  #str_remove(x, pattern = paste0(pattern, "*."), all = all)
  x_names <- names(x)
  pattern <- paste0("(?<=", pattern, ").*")
  x <- vapply(x, function(z) {
    z <- regmatches(z, gregexpr(pattern = pattern, text = z, perl = TRUE))[[1L]]
    if (length(z) == 0L) "" else z
  }, character(1), USE.NAMES = FALSE)
  names(x) <- x_names
  x
}

#' Extract text that occurs between two specified patterns
#'
#' @rdname str_extract
#' @param before,after Pattern before and after text to be extracted. Enter each as length 1 character vectors. Can't use fixed (i.e., exact/literal) matching
#' @export
str_extract_between <- function(x, before, after) {
  x_names <- names(x)
  pattern <- sprintf("(?<=%s).*?(?=%s)", before, after)
  # alternative 1: pattern <- paste0("(?<=", after, ").*(?=", before, ")")
  # alternative 2: pattern <- sprintf("(%s)(.*?)(%s)", before, after)
  x <- vapply(x, function(z) {
    z <- regmatches(z, gregexpr(pattern = pattern, text = z, perl = TRUE))[[1L]]
    if (length(z) == 0L) "" else z
  }, character(1), USE.NAMES = FALSE)
  names(x) <- x_names
  x
}

#' Extract all numbers from each element of a character vector
#'
#' @param x Character vector
#' @param single_number If `FALSE` (default), output is a list (same length as input) of numbers. If `TRUE`, all numbers in a string are pasted together using `paste0()`
#' @param incl_decimal If `TRUE` (default), decimals can be included in numbers. If `FALSE`, values will be split at decimals. Only relevant when `single_number = FALSE`
#' @param incl_comma If `FALSE` (default), values will be split at commas. If `TRUE`, commas can be included in numbers. Only relevant when `single_number = FALSE`
#' @param incl_neg If `TRUE`, negative sign can be included in output. If `FALSE` (default), negative sign not included in output. Only relevant when `single_number = FALSE`
#' @param as_numeric If `FALSE` (default), output is a character. If `TRUE`, output is numeric
#' @returns If `single_number = TRUE`, output is a vector (character vector if `as_numeric = FALSE`, numeric vector if `as_numeric = TRUE`). If `single_number = FALSE`, output is a list of vectors, each containing all of the individual numbers in a given string (character vector if `as_numeric = FALSE`, numeric vector if `as_numeric = TRUE`)
#' @export
str_numerics <- function(x, single_number = FALSE, incl_decimal = TRUE, incl_comma = FALSE, incl_neg = FALSE, as_numeric = FALSE) {
  if (single_number) {
    x <- gsub("[^0-9.-]+", "", as.character(x))
    if (as_numeric) as.numeric(x) else x
  } else {
    decimal <- if (incl_decimal) "." else NULL
    neg <- if (incl_neg) "-" else NULL
    comma <- if (incl_comma && !as_numeric) "," else NULL
    split_pattern <- paste0("[^", comma, "0-9", decimal, neg, "]+")
    lapply(strsplit(as.character(x), split_pattern), function(y) {
      y <- sub("\\.$", "", y)
      y <- y[nzchar(y)]
      y <- unlist(lapply(y, function(z) {
        if (grepl(",", z, fixed = TRUE)) {
          z_split <- strsplit(z, ",", fixed = TRUE)[[1L]]
          z_digits <- nchar(strsplit(z_split[-1L], ".", fixed = TRUE)[[1L]][1L])
          z <- if (all(z_digits == 3)) z else z_split
          z[z != ","]
        } else {
          z
        }
      }), use.names = FALSE)
      if (as_numeric) as.numeric(y) else y
    })
  }
}

#' Extract nth number from string
#'
#' @rdname str_numerics
#' @param n nth occurrence of a number in each element of `x`. Options: numeric, `"first"` (equivalent to `n = 1`), `"last"` (equivalent to `n = -1`). For second to last number, enter `n = -2`
#' @returns Vector (character vector if `as_numeric = FALSE`, numeric vector if `as_numeric = TRUE`) with same length as input
#' @export
str_nth_number <- function(x, n = 1, incl_decimal = TRUE, incl_comma = FALSE, incl_neg = FALSE, as_numeric = FALSE) {
  n <- if (n == "first") 1L else if (n == "last") -1L else n
  n_neg <- n < 0
  num_list <- str_numerics(x = x, single_number = FALSE, incl_decimal = incl_decimal, incl_comma = incl_comma, incl_neg = incl_neg, as_numeric = as_numeric)
  num_list <- remove_null(num_list)
  output_type <- if (as_numeric) numeric(1) else character(1)
  if (length(num_list) == 0L) return(output_type)
  vapply(num_list, function(y) {
    if (n_neg) {
      y <- Rev(y)
      n <- abs(n)
    }
    y[n]
  }, output_type, USE.NAMES = FALSE)
}

#' Extract nth word
#'
#' @rdname str_extract
#' @param n Nth word in `x`. Enter as length 1 integer vector. Default is `1L` (first word)
#' @returns Character vector with same length as input
#' @export
str_nth_word <- function(x, n = 1L) vapply(strsplit(x, split = " "), function(z) z[n], character(1), USE.NAMES = FALSE)

# Case --------------------------------------------------------------------

#' Capitalize first letter of each string in character vector
#'
#' @param x Character vector
#' @param other_letters_lower If `FALSE` (default), subsequent letters left as is. If `TRUE`, subsequent letters converted to lower case
#' @returns Character vector with same length as input
#' @export
str_capitalize <- function(x, other_letters_lower = FALSE) {
  if (other_letters_lower) {
    x <- tolower(x)
  }
  substring(x, 1L, 1L) <- toupper(substring(x, 1L, 1L))
  x
}

#' Capitalize each word in a string
#'
#' Inspired by github.com/raredd/regex
#' @rdname str_capitalize
#' @export
str_capitalize_words <- function(x, other_letters_lower = FALSE) {
  if (other_letters_lower) {
    x <- tolower(x)
  }
  gsub("(?<=\\b)([a-z])", "\\U\\1", x, perl = TRUE)
}

# Order -------------------------------------------------------------------

#' Sort vector (or order of column names in a data frame) alphabetically by names
#'
#' @param x Named vector, named list, or data frame
#' @param blank_last If `TRUE` (default), elements without names will be placed at the end
#' @param reverse If `TRUE`, names ordered reverse alphabetically. Default is `FALSE`
#' @returns Same class and length as input
#' @export
sort_by_name <- function(x, blank_last = TRUE, reverse = FALSE) {
  if (length(x) == 0L) return(x)
  x_names <- names(x)
  x_names[!nzchar(x_names)] <- NA
  x[order(x_names, na.last = blank_last, decreasing = reverse)]
}

#' Sort strings by string length (number of characters)
#'
#' @param x Vector
#' @param desc If `FALSE` (default), `x` output order is shortest to longest. If `TRUE`, output order is longest to shortest
#' @returns Same class and length as input
#' @export
sort_by_nchar <- function(x, desc = FALSE) x[order(nchar(x), decreasing = desc)]

#' Pad numbers with leading zeros
#'
#' @param x Integer or character vector that is coercible to an integer vector
#' @param n Number of digits to add. If `NULL` (default), determined by longest element in `x`
#' @returns Character vector with same length as input
#' @export
pad_leading_zero <- function(x, n = NULL) {
  x <- as.integer(x)
  idx_na <- is.na(x)
  #n <- n %||% max(nchar(x), na.rm = TRUE)
  #x <- sprintf(paste0("%0", n, "d"), x)
  x <- gsub(" ", "0", format(x), fixed = TRUE)
  x[idx_na] <- NA_character_
  x
}

# Regular expressions -----------------------------------------------------

#' Escape regular expressions
#'
#' @param x Character vector
#' @returns Character vector with same length as input
#' @noRd
escape_regex <- function(x) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
