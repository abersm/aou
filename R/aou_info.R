#' List contents of bucket
#'
#' @export
ls_bucket <- function(..., bucket = Sys.getenv("WORKSPACE_BUCKET")) {
  allofus::aou_ls_bucket(..., bucket = bucket)
}

#' List files in workspace
#'
#' @export
ls_workspace <- function(...) {
  allofus::aou_ls_workspace(...)
}
