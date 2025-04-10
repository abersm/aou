% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{str_extract}
\alias{str_extract}
\alias{str_extract_before}
\alias{str_extract_after}
\alias{str_extract_between}
\alias{str_nth_word}
\title{Extract text following a particular pattern}
\usage{
str_extract(x, pattern, all = TRUE, ...)

str_extract_before(x, pattern)

str_extract_after(x, pattern)

str_extract_between(x, before, after)

str_nth_word(x, n = 1L)
}
\arguments{
\item{x}{Character vector}

\item{pattern}{Pattern to extract from \code{x}. Enter as length 1 character vector}

\item{all}{If \code{TRUE} (default), all occurrences of \code{pattern} will be extracted. If \code{FALSE}, only first occurrence of \code{pattern} will be extracted}

\item{...}{Arguments passed to \code{gregexpr} (if \code{all = TRUE}) or \code{regexpr} (if \code{all = FALSE})}

\item{before, after}{Pattern before and after text to be extracted. Enter each as length 1 character vectors. Can't use fixed (i.e., exact/literal) matching}

\item{n}{Nth word in \code{x}. Enter as length 1 integer vector. Default is \code{1L} (first word)}
}
\value{
List of character vectors

Character vector with same length as input. If no match is found, output is \code{""}. If multiple matches are found, only the first is returned. Names of input are retained in output

Character vector with same length as input
}
\description{
Extract text following a particular pattern

Extract text that occurs before a specified pattern

Extract text that follows a specified pattern

Extract text that occurs between two specified patterns

Extract nth word
}
