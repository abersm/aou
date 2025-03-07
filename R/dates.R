#' Today's date
#'
#' @returns Date vector (length 1) containing today's date in YYYY-MM-DD format
#' @export
today <- function() {
  out <- floor(unclass(Sys.time())/86400)
  class(out) <- "Date"
  out
}

#' Time interval between 2 dates
#'
#' @param t0,t1 Dates for beginning and end of time period, respectively. Can be character or date vector
#' @param units Units for time interval. Default is `"days"`
#' @param date_fn Function used to convert `t0` and `t1` to dates. Default is `as.Date.` Other options: `lubridate::ymd`
#' @returns Numeric vector with units of time determined by `units`
#' @export
time_interval <- function(
    t0,
    t1,
    units = "days",
    date_fn = function(x) as.Date(x, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))) {
  if (units == "years") {
    as.numeric(difftime(date_fn(t1), date_fn(t0), units = "days"))/365
  } else {
    as.numeric(difftime(date_fn(t1), date_fn(t0), units = units))
  }
}

#' Test whether vector contains date object
#'
#' @param x Vector
#' @returns Logical
#' @noRd
is_date <- function(x) inherits(x, c("Date", "POSIXct"))

#' Convert input to date
#'
#' @param x Vector containing year, month, day
#' @returns Date vector in YYYY-MM-DD format
#' @export
as_date <- function(x) {
  if (is_date(x)) return(as.Date(x))
  x <- str_trimws(x)
  x <- strsplit(x, "\\/|-")
  x <- vapply(x, function(y) {
    if (length(y) != 3L) return(NA_character_)
    n <- nchar(y)
    idx <- n == 1L
    y[idx] <- paste0("0", y[idx])
    idx_yr <- n == 4
    if (any(idx_yr)) {
      paste(y[idx_yr], paste(y[!idx_yr], collapse = "-"), sep = "-")
    } else {
      y[3L] <- paste0(if (as.numeric(y[3L]) > 30) "19" else "20", y[3L])
      paste(y[3L], paste(y[c(1L, 2L)], collapse = "-"), sep = "-")
    }
  }, character(1), USE.NAMES = FALSE)
  as.Date(x)
}

#' Create time series data frame
#'
#' @param df Data frame
#' @param time Index variable containing time. Enter as length 1 character vector. Only required if `t0` and `t1` are not specified
#' @param id Key variable containing subject/individuals. Enter as length 1 character vector. Default is `"id"`
#' @param t0,t1 Dates for beginning and end of time period, respectively. Enter as length 1 character vector. If provided, must also provide `t1`
#' @param units Units for time variable in output. Enter as length 1 character vector. Options: `"days"` (default), `"weeks"`, `"months"`, `"years"`, `"none"`
#' @param min_time,max_time Minimum and maximum values for time. Enter as length 1 numeric vector with units matching `units` argument
#' @param new_time_col Name for new time variable. Enter as length 1 character vector
#' @returns tsibble object
#' @export
tsibble <- function(
    df,
    t0 = NULL,
    t1 = NULL,
    time = NULL,
    id = "id",
    units = c("days", "weeks", "months", "years", "none"),
    min_time = -Inf,
    max_time = Inf,
    new_time_col = "time") {
  pkg_required("tsibble")
  time_empty <- is.null(time)
  units <- match.arg(units, choices = c("days", "weeks", "months", "years", "none"))
  time_units <- switch(units, none = , days = 1, weeks = 7, months = 30, years = 365)
  if (missing(new_time_col)) {
    if (time_empty) {
      if (units != "days") {
        new_time_col <- paste0(new_time_col, "_", units)
      }
    } else {
      new_time_col <- time
    }
  }
  if (time_empty) {
    if (is.null(t1) || is.null(t0)) {
      stop("In 'tsibble', must specify either 'time' (name of column in 'df' containing time as numeric variable) or both 't0' (name of column in 'df' containing the start date) and 't1' (name of column in 'df' containing date of interest")
    }
    df[[new_time_col]] <- time_interval(t0 = .subset2(df, t0), t1 = .subset2(df, t1), units = "days")
  } else {
    names(df)[names(df) == time] <- new_time_col
  }
  df[[new_time_col]] <- .subset2(df, new_time_col)/time_units
  df <- remove_na(df, new_time_col)
  df <- dplyr::distinct(df, id, .data[[new_time_col]], .keep_all = TRUE)
  df <- tsibble::as_tsibble(df, key = id, index = new_time_col, regular = FALSE)
  z <- .subset2(df, new_time_col)
  df[!is.na(z) & is_between(z, min_time, max_time), , drop = FALSE]
}
