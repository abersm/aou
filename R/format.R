#' Round up for 0.5 or higher
#'
#' Functionality from Tyler Rinker's excellent package numform
#' @param x Numeric vector
#' @param digits Number of digits after decimal place to include in output
#' @returns Numeric vector with length equal to input
#' @export
round_up <- function(x, digits = 2) {
  m <- 10^digits
  z <- abs(x)*m + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  sign(x)*z/m
}

#' Format numbers for printing
#'
#' @param x Numeric vector (can also be character or factor)
#' @param digits Number of digits to include after decimal. Default is `2`
#' @param max_width Maximum number of characters (including decimal places) allowed in output (including digits before decimal, decimal, and digits after decimal). Default is `10`
#' @param big_mark Character to separate thousands. Default is `""`
#' @returns Character vector with length equal to input
#' @noRd
format_number <- function(x, digits = 2, max_width = 10, big_mark = "") {
  if (inherits(x, "factor")) {
    x <- as.character(x)
  }
  .format_number <- function(.x) {
    if (is.na(.x)) return(NA_character_)
    if (!is.numeric(.x)) {
      if (can_be_numeric(.x)) {
        .x <- as.numeric(.x)
      } else if (grepl("[a-z]", .x, ignore.case = TRUE)) {
        .x <- gsub("[a-z]", "", .x, ignore.case = TRUE)
        if (can_be_numeric(.x)) {
          .x <- as.numeric(.x)
        } else {
          return(NA_character_)
        }
      } else {
        return(NA_character_)
      }
    }
    x_rounded <- round_up(.x, digits = digits)
    x_char <- sprintf(paste0("%.", digits, "f"), x_rounded)
    if ((nchar(x_char) > max_width) || (as.numeric(x_char) == 0 && .x != 0)) {
      .format_number_extreme(.x, digits = digits)
    } else {
      x_char
    }
  }
  vapply(x, .format_number, character(1), USE.NAMES = FALSE)
}

#' Add thousands commas to numbers
#'
#' @param x Numeric or character vector
#' @param big_mark Mark for thousands. Default is `","`
#' @param include_decimal If `FALSE` (default), number is rounded and decimal removed
#' @returns Character vector with length equal to input
#' @noRd
add_comma <- function(x, big_mark = ",", include_decimal = FALSE) {
  x_numeric <- is.numeric(x)
  if (!include_decimal || x_numeric) {
    if (!x_numeric) {
      x <- sub(",", "", x, fixed = TRUE)
      x <- as.numeric(x)
    }
    return(format(round_up(x, digits = 0), big.mark = big_mark, scientific = FALSE))
  }
  x_decimal_split <- strsplit(x, ".", fixed = TRUE)
  pre_decimal <- vapply(x_decimal_split, `[`, "", 1L)
  post_decimal <- vapply(x_decimal_split, `[`, "", 2L)
  if (any(idx_no_decimal <- is.na(post_decimal))) {
    post_decimal[idx_no_decimal] <- ""
  }
  post_decimal[!idx_no_decimal] <- paste0(".", post_decimal[!idx_no_decimal])
  str_reverse <- function(x) vapply(lapply(strsplit(x, NULL), rev), paste, "", collapse = "")
  if (nzchar(big_mark) && length(idx_big_mark <- grep("[0-9]{4,}", pre_decimal))) {
    pre_decimal[idx_big_mark] <- str_reverse(gsub("([0-9]{3})\\B", paste0("\\1", big_mark), str_reverse(pre_decimal[idx_big_mark])))
  }
  paste0(pre_decimal, post_decimal)
}

#' Format n (or n/total) and %
#'
#' @param n Numerator for percentage calculation. Enter as numeric or character
#' @param total Denominator for percentage calculation. Enter as numeric or character
#' @param digits Number of digits after decimal in output. Only used for percentage. Default is `1`
#' @param incl_denominator If `TRUE` (default), format for count is n/total. If `FALSE`, format is n
#' @param perc_first If `TRUE` (default), format is % (n) or % (n/n_total). If `FALSE`, format is n (%) or n/total (%)
#' @param bracket_type Type of bracket to surround range. Default is `"("`
#' @returns Character vector with length equal to input
#' @noRd
format_n_perc <- function(n, total, digits = 1, incl_denominator = TRUE, perc_first = TRUE, bracket_type = "(") {
  brackets <- if (bracket_type %in% c("(", ")")) c("(", ")") else c("[", "]")
  perc <- paste0(format_number(n/total*100, digits = digits), "%")
  n <- if (incl_denominator) paste0(n, "/", total) else n
  if (perc_first) {
    part1 <- perc
    part2 <- paste0(brackets[1L], n, brackets[2L])
  } else {
    part1 <- n
    part2 <- paste0(brackets[1L], perc, brackets[2L])
  }
  paste(part1, part2)
}

#' Format number and range
#'
#' @param x Risk ratio. Enter as numeric
#' @param x_lower,x_upper Lower and upper confidence intervals of risk ratio. Enter as numeric
#' @param sep Separator character between `x_lower` and `x_upper.` Default is `"-"`
#' @param bracket_type Type of bracket to surround range. Default is `"("`
#' @param digits Number of digits to display after decimal point. Default is `2`
#' @returns Character vector with length equal to input
#' @noRd
format_num_range <- function(x, x_lower, x_upper, sep = "-", bracket_type = "(", digits = 2) {
  brackets <- if (bracket_type %in% c("(", ")")) c("(", ")") else c("[", "]")
  z <- format_number(c(x, x_lower, x_upper), digits = digits)
  paste(z[1L], paste0(brackets[1L], z[2L], sep, z[3L], brackets[2L]))
}

#' Helper function for processing very small or very large numbers
#'
#' @param x Numeric
#' @param digits Number of digits to include after decimal
#' @returns Character vector with same length as input
#' @noRd
.format_number_extreme <- function(x, digits) {
  x_sci <- strsplit(format(x, scientific = TRUE), split = "e")[[1L]]
  x_base <- sprintf(paste0("%.", digits, "f"), round_up(as.numeric(x_sci[1L]), digits = digits))
  paste0(x_base, "e", as.integer(x_sci[2L]))
}
