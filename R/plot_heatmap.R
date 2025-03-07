#' Heatmap of summarized numeric
#'
#' @param df,mat Data frame with column containing rownames
#' @param row_var Variable used for rows. Enter as quoted or unquoted row name
#' @param scale_by Scaling of rows and columns. Options: `"rows" `(default), `"columns"`, `"none"`
#' @param cluster_cols If `TRUE`, columns are clustered. Default is `FALSE`
#' @param cluster_rows If `TRUE` (default), rows are clustered
#' @param size,cellheight,cellwidth Height and width of heatmap squares in pts. Default is `15`
#' @param colors Colors used to generate heatmap colors. Enter 2-3 colors. Default is blue and red
#' @param n_colors Number of unique colors. Default is `100`
#' @param border_colors Color of line surrounding cells in heatmap. Default is `"black"`
#' @param show_colnames If `TRUE` (default), column names are displayed on heatmap
#' @param show_rownames If `TRUE` (default), row names are displayed on heatmap
#' @param font_size,fontsize,font_size_rownames,fontsize_row,font_size_colnames,fontsize_col Font size for row and column labels in heatmap. Default is `18`
#' @param rowname_location Location of row labels. Options include `"left"` (default), `"right"`, `"none"`
#' @param rowname_align Horizontal alignment of row labels. Options include `"right"` (default), `"left"`, `"center"`
#' @param colname_location Location of column labels. Options include `"top"` (default), `"bottom"`, `"none"`
#' @param colname_angle,angle_col Angle for column labels. Enter as length 1 numeric. Default is `90`
#' @param colname_align Vertical alignment of column labels. Options include `"bottom"` (default), `"top"`, `"center"`
#' @param row_gaps,gaps_row,col_gaps,gaps_col Row numbers to separate heatmap rows and columns, respectively. Enter as length 1 numeric. Default uses no gaps for either
#' @param treeheight_row,treeheight_col Size of dendrogram for rows and columns, respectively. Default is `0` for both
#' @param show_legend,legend If `TRUE` (default), legend is displayed
#' @returns pheatmap object with ggplot object added
#' @export
plot_heatmap <- function(
    df,
    row_var = NULL,
    col_var = NULL,
    scale_by = c("rows", "columns", "none"),
    cluster_cols = FALSE,
    cluster_rows = TRUE,
    size = 15,
    cellheight = size,
    cellwidth = size,
    colors = c("#0072B5", "white", "#BC3C29"),
    n_colors = 100,
    border_colors = "black",
    show_colnames = TRUE,
    show_rownames = TRUE,
    font_size = 18,
    font_size_rownames = font_size,
    font_size_colnames = font_size,
    rowname_location = "right",
    rowname_align = "right",
    colname_location = "bottom",
    colname_angle = 90,
    colname_align = "right",
    col_gaps = 0,
    row_gaps = 0,
    treeheight_col = 0,
    treeheight_row = 0,
    show_legend = TRUE,
    mat = df,
    legend = show_legend,
    fontsize = font_size,
    fontsize_row = font_size_rownames,
    fontsize_col = font_size_colnames,
    angle_col = colname_angle,
    gaps_col = col_gaps,
    gaps_row = row_gaps) {
  pkg_required("pheatmap")
  plot_fn <- "plot_heatmap"
  scale_by <- match.arg(scale_by, choices = c("rows", "columns", "none"))
  scale_by <- switch(scale_by, rows = "row", columns = "column", none = "none")
  p <- pheatmap::pheatmap(
    mat = .as_heatmap_matrix(mat, row_var = get_input(row_var)),
    color = clr_continuous(colors, n = n_colors),
    scale = scale_by,
    border_colors = border_colors,
    cluster_cols = cluster_cols,
    cluster_rows = cluster_rows,
    gaps_col = gaps_col,
    gaps_row = gaps_row,
    cellwidth = cellwidth,
    cellheight = cellheight,
    show_rownames = show_rownames,
    show_colnames = show_colnames,
    treeheight_col = treeheight_col,
    treeheight_row = treeheight_row,
    legend = legend,
    fontsize = fontsize,
    fontsize_row = fontsize_row,
    fontsize_col = fontsize_col,
    angle_col = angle_col
  )
  p$ggplot <- as_ggplot(p$gtable)
  p
}

# Helpers -----------------------------------------------------------------

#' Print pheatmap
#'
#' @param x pheatmap object
#' @param ... Not used
#' @returns pheatmap object with ggplot appended
#' @export
print.pheatmap <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  invisible(x)
}

#' Convert data frame to matrix that can be passed to heatmap plotting functions
#'
#' @param df Data frame or matrix
#' @param row_var Variable used for row name. Enter as length 1 character vector
#' @returns Matrix
#' @noRd
.as_heatmap_matrix <- function(df, row_var = NULL) {
  if (is.matrix(df)) return(df)
  df_names <- names(df)
  numeric_vars <- vars_numeric(df)
  if (is.null(row_var) || row_var %!in% df_names) {
    row_var <- Setdiff(df_names, numeric_vars)
    if (length(row_var) == 0L) {
      return(df_to_matrix(df))
    }
    row_var <- row_var[1L]
  }
  df_to_matrix(df[Setdiff(numeric_vars, row_var)], rownames = .subset2(df, row_var))
}
