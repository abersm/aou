% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{summary_table_cat}
\alias{summary_table_cat}
\title{Summarize all categorical variables stratified by grouping variable}
\usage{
summary_table_cat(
  df,
  grouping_var,
  split_by = NULL,
  exclude_vars = NULL,
  na_rm = TRUE,
  max_n_unique = 20,
  digits_perc = 1,
  incl_denominator = TRUE,
  compare_fn = p_chi,
  simplify_binary = TRUE,
  var_once = FALSE,
  format_p_fn = format_p_value
)
}
\arguments{
\item{df}{Data frame in long format}

\item{grouping_var}{Column in \code{df} that contains groups (i.e., columns in output table). Enter as quoted or unquoted column name}

\item{split_by}{Character vector of column names in \code{df} that will be used to split data to perform separate analysis. No yet incorporated}

\item{exclude_vars}{Character vector of column names in \code{df} to exclude from analyses}

\item{na_rm}{If \code{TRUE} (default), missing values for outcome, grouping, and subgrouping variables are removed from analyses}

\item{max_n_unique}{Maximum number of factor levels for a non-grouping variable to be included in output. Enter as length 1 integer. Default is \code{20}}

\item{digits_perc}{Number of digits past decimal point for percentages. Default is \code{1}}

\item{incl_denominator}{If \code{TRUE} (default), format for count is n/total. If \code{FALSE}, format is n}

\item{compare_fn}{Function to compare count variables. Options: \code{p_chi} (default), \code{p_fisher}, \code{p_prop}}

\item{simplify_binary}{If \code{TRUE} (default), output includes 1 row for each binary variable. If \code{FALSE} each value for binary variable included in output}

\item{var_once}{If \code{TRUE}, output is formatted such that each variable appears once (included in first row for a given variable. Other rows contain NA). Useful when exporting. If \code{FALSE} (default), each row in output has variable}

\item{format_p_fn}{Function to format P values. Must accept numeric vector of P values as input. Default is \code{format_p_value}}

\item{perc_first}{If \code{TRUE} (default), format is \% (n) or \% (n/n_total). If \code{FALSE}, format is n (\%) or n/total (\%)}
}
\value{
Either a data frame (if \code{split_by = NULL}) or a list of data frames (if \code{split_by} is specified)
}
\description{
Summarize all categorical variables stratified by grouping variable
}
