#' Using SQL, create data frame and export to workspace
#'
#' @param sql String containing SQL query
#' @param filename String containing name of file
#' @param dir Path to directory where `filename` will be stored
#' @param workspace,bucket,email,billing Workspace, bucket, email, and billing specific to users environment
#' @param return_df If `TRUE` (default), data frame is returned
#' @param destination_format File type for exported data. Default is `"CSV"`
#' @returns Data silently exported. If `return_df = TRUE`, output is a data frame, otherwise output is path to file
#' @export
write_raw_data <- function(
  sql,
  filename,
  workspace = Sys.getenv("WORKSPACE_CDR"),
  bucket = Sys.getenv("WORKSPACE_BUCKET"),
  email = Sys.getenv("OWNER_EMAIL"),
  billing = Sys.getenv("GOOGLE_PROJECT"),
  destination_format = "CSV") {
  file_path <- file.path(
    bucket,
    "bq_exports",
    email,
    filename,
    paste0(filename, "_*.", tolower(destination_format))
  )
  bigrquery::bq_table_save(
    bigrquery::bq_dataset_query(workspace, query = sql, billing = billing),
    file_path,
    destination_format = destination_format
  )
  if (return_df) {

  } else {
    file_path
  }
}
