# Merge -------------------------------------------------------------------

#' Left join with information about number of rows before and after join
#'
#' @param df1,df2 Primary and secondary data frames
#' @param by Key variables shared between `df1` and `df2`. Enter as character vector
#' @param ... Arguments passed to `dplyr::left_join()`
#' @returns Merged data frame with message containing number of rows in `df1` and output data frame
#' @export
lj <- function(df1, df2, by = NULL, ...) {
  .safe_join(.df1 = df1, .df2 = df2, .by = by, .type = "left", ...)
}

#' Right join with information about number of rows before and after join
#'
#' @rdname lj
#' @param df1,df2 Primary and secondary data frames
#' @param by Key variables shared between `df1` and `df2`
#' @param ... Arguments passed to `dplyr::right_join()`
#' @returns Merged data frame with message containing number of rows in `df2` and output data frame
#' @export
rj <- function(df1, df2, by = NULL, ...) {
  .safe_join(.df1 = df1, .df2 = df2, .by = by, .type = "right", ...)
}

#' Semi join with information about number of rows before and after join
#'
#' @rdname lj
#' @param df1,df2 Primary and secondary data frames
#' @param by Key variables shared between `df1` and `df2`
#' @param ... Arguments passed to `dplyr::semi_join()`
#' @returns Merged data frame with message containing number of rows in input and output data frames
#' @export
semi_join_safe <- function(df1, df2, by = NULL, ...) {
  .safe_join(.df1 = df1, .df2 = df2, .by = by, .type = "semi", ...)
}

#' Inner join with information about number of rows before and after join
#'
#' @rdname lj
#' @param df1,df2 Primary and secondary data frames
#' @param by Key variables shared between `df1` and `df2`
#' @param ... Arguments passed to `dplyr::inner_join()`
#' @returns Merged data frame with message containing number of rows in input and output data frames
#' @export
inner_join_safe <- function(df1, df2, by = NULL, ...) {
  .safe_join(.df1 = df1, .df2 = df2, .by = by, .type = "inner", ...)
}

#' Anti join with information about number of rows before and after join
#'
#' @rdname lj
#' @param df1,df2 Primary and secondary data frames
#' @param by Key variables shared between `df1` and `df2`
#' @param ... Arguments passed to `dplyr::anti_join()`
#' @returns Merged data frame with message containing number of rows in input and output data frames
#' @export
anti_join_safe <- function(df1, df2, by = NULL, ...) {
  .safe_join(.df1 = df1, .df2 = df2, .by = by, .type = "anti", ...)
}

#' Join > 2 data frames
#'
#' @param ... Comma separated list of data frames
#' @param join_fn Function to join data frames. Unless `full_join` is used, order of data frames in `...` influences the resulting data frame. Default uses `left_join.` Enter as quoted or unquoted function name
#' @param by Variable shared by data frames used to join all data frames. Must be the same in all data frames. Enter as quoted variable name
#' @returns Single data frame a result of joining all data frames
#' @export
join_many <- function(..., join_fn = dplyr::left_join, by = NULL) {
  dfs <- list(...)
  join_fn <- match.fun(join_fn)
  purrr::reduce(dfs, join_fn, by = by)
}

# Helpers -----------------------------------------------------------------

#' Helper function for performing joins
#'
#' @param .df1,.df2 Data frames to join
#' @param .by Key variables shared between `.df1` and `.df2`
#' @param .type Type of join to perform. Options: `"left"`, `"right"`, `"semi"`, `"inner"`, `"anti"`
#' @param ... Arguments passed to join function
#' @returns Data frame
#' @noRd
.safe_join <- function(.df1, .df2, .by, .type, ...) {
  switch(.type,
         left = {
           join_fn <- dplyr::left_join
           m <- sprintf("Rows in input df: %s", Nrow(.df1))
         },
         right = {
           join_fn <- dplyr::right_join
           m <- sprintf("Rows in input df: %s", Nrow(.df2))
         },
         semi = {
           join_fn <- dplyr::semi_join
           m <- sprintf("Rows in input df1: %s\nRows in input df2: %s", Nrow(.df1), Nrow(.df2))
         },
         inner = {
           join_fn <- dplyr::inner_join
           m <- sprintf("Rows in input df1: %s\nRows in input df2: %s", Nrow(.df1), Nrow(.df2))
         },
         anti = {
           join_fn <- dplyr::anti_join
           m <- sprintf("Rows in input df1: %s\nRows in input df2: %s", Nrow(.df1), Nrow(.df2))
         },
         Stop("In '.safe_join', argument 'type' must be one of:/n", paste0(c("left", "right", "semi", "inner", "anti"), collapse = ", ")))
  df_merged <- join_fn(.df1, .df2, by = .by, ...)
  message(sprintf("%s\nRows in output df: %s", m, Nrow(df_merged)))
  df_merged
}
