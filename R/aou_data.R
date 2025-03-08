#' Load data from workbench using SQL
#'
#' @param patients,pneumonia,cv_complications If `TRUE` (default), data frame is loaded
#' @param other Other data frames to load. Enter as character vector. Options: `"demographics"`, `"comorbidities"`, `"substance_use"`. Default includes all
#' @param background If `TRUE` (default), data loaded as background job
#' @returns List of data frames
#' @export
load_df <- function(
    patients = TRUE,
    pneumonia = TRUE,
    cv_complications = TRUE,
    other = c("demographics", "comorbidities", "substance_use"),
    background = TRUE) {
  fn <- if (background) job::job else identity
  names <- c(
    if (patients) "patients",
    if (pneumonia) "pneumonia",
    if (cv_complications) "cv_complications",
    other
  )
  lookup <- c(
    patients = "person",
    pneumonia = "conditions",
    cv_complications = "conditions",
    comorbidities = "surveys",
    substance_use = "surveys"
  )
  sql <- paste0("sql_", names)
  types <- unname(lookup[names])
  fn({
    out <- lapply(seq_along(names), function(i) aou::write_raw_data(eval(sql[i]), filename = names[i], type = types[i]))
    names(out) <- names
    out
  })
}

#' Move object from workspace to bucket
#'
#' @param x Object in global environment
#' @param filename Name for exported Rdata file
#' @returns Rdata file silently exported
#' @export
transfer_to_bucket <- function(x, filename = deparse(substitute(x))) {
  save(x, file = sprintf("%s.Rdata", filename))
  system2("bucket='$WORKSPACE_BUCKET'")
  system2(sprintf("gsutil cp /home/rstudio/%s.Rdata '$bucket'", filename))
}

#' Read CSV file from workspace
#'
#' @param file_path Path to CSV file
#' @param type Type of data from All of Us. Options: `"conditions"` (default), `"person"`, `"surveys"`, `"procedures"`, `"measurements"`, `"drugs"`, `"procedures"`
#' @export
read_raw_data <- function(file_path, type = c("conditions", "person", "surveys", "procedures", "measurements", "drugs", "procedures")) {
  col_types <- col_types(match.arg(type, choices = c("conditions", "person", "surveys", "procedures", "measurements", "drugs", "procedures")))
  dplyr::bind_rows(
    lapply(system2("gsutil", args = c("ls", file_path), stdout = TRUE, stderr = TRUE),
           function(csv) {
             #chunk <- readr::read_csv(pipe(glue::glue("gsutil cat {csv}")), col_types = col_types, show_col_types = FALSE)
             chunk <- readr::read_csv(pipe(sprintf("gsutil cat %s", csv)), col_types = col_types, show_col_types = FALSE)
             if (is.null(col_types)) {
               col_types <- attr(chunk, "spec")
             }
             chunk
           })
  )
}


#' Using SQL, create data frame and export to workspace
#'
#' @param sql String containing SQL query
#' @param filename String containing name of file
#' @param dir Path to directory where `filename` will be stored
#' @param type Type of sql query. If `NULL` (default), type will be guessed from `sql`. Only relevant when `return_df = TRUE`
#' @param workspace,billing Workspace and billing specific to user's environment
#' @param destination_format File type for exported data. Default is `"CSV"`
#' @param return_df If `TRUE` (default), data frame is returned
#' @returns Data silently exported. If `return_df = TRUE`, output is a data frame, otherwise output is path to file
#' @export
write_raw_data <- function(
    sql,
    filename,
    dir = NULL,
    type = NULL,
    workspace = Sys.getenv("WORKSPACE_CDR"),
    billing = Sys.getenv("GOOGLE_PROJECT"),
    destination_format = "CSV",
    return_df = TRUE) {
  dir <- dir %||% file.path(Sys.getenv("WORKSPACE_BUCKET"), "bq_exports", Sys.getenv("OWNER_EMAIL"), filename)
  file_path <- paste0(dir, "/", filename, "_*.", tolower(destination_format))
  bigrquery::bq_table_save(
    bigrquery::bq_dataset_query(workspace, query = sql, billing = billing),
    file_path,
    destination_format = destination_format
  )
  if (return_df) {
    type <- type %||% guess_sql_query_type(sql)
    read_raw_data(file_path, type)
  } else {
    file_path
  }
}

#' Set columns specifications when reading data from csv file
#'
#' @param x Type of data from All of Us. Options: `"conditions"` (default), `"person"`, `"surveys"`, `"procedures"`, `"measurements"`, `"drugs"`, `"procedures"`
#' @noRd
col_types <- function(x) {
  chr <- readr::col_character()
  switch(
    x,
    conditions = {
      readr::cols(
        standard_concept_name = chr,
        standard_concept_code = chr,
        standard_vocabulary = chr,
        condition_type_concept_name = chr,
        stop_reason = chr,
        visit_occurrence_concept_name = chr,
        condition_source_value = chr,
        source_concept_name = chr,
        source_concept_code = chr,
        source_vocabulary = chr,
        condition_status_source_value = chr,
        condition_status_concept_name = chr
      )
    },
    measurements = {
      readr::cols(
        standard_concept_name = chr,
        standard_concept_code = chr,
        standard_vocabulary = chr,
        measurement_type_concept_name = chr,
        operator_concept_name = chr,
        value_as_concept_name = chr,
        unit_concept_name = chr,
        visit_occurrence_concept_name = chr,
        measurement_source_value = chr,
        source_concept_name = chr,
        source_concept_code = chr,
        source_vocabulary = chr,
        unit_source_value = chr,
        value_source_value = chr
      )
    },
    drugs = {
      readr::cols(
        standard_concept_name = chr,
        standard_concept_code = chr,
        standard_vocabulary = chr,
        drug_type_concept_name = chr,
        stop_reason = chr,
        sig = chr,
        route_concept_name = chr,
        lot_number = chr,
        visit_occurrence_concept_name = chr,
        drug_source_value = chr,
        source_concept_name = chr,
        source_concept_code = chr,
        source_vocabulary = chr,
        route_source_value = chr,
        dose_unit_source_value = chr
      )
    },
    procedures = {
      readr::cols(
        standard_concept_name = chr,
        standard_concept_code = chr,
        standard_vocabulary = chr,
        procedure_type_concept_name = chr,
        modifier_concept_name = chr,
        visit_occurrence_concept_name = chr,
        procedure_source_value = chr,
        source_concept_name = chr,
        source_concept_code = chr,
        source_vocabulary = chr,
        modifier_source_value = chr
      )
    },
    surveys = {
      readr::cols(
        survey = chr,
        question = chr,
        answer = chr,
        survey_version_name = chr
      )
    },
    person = {
      readr::cols(
        gender = chr,
        race = chr,
        ethnicity = chr,
        sex_at_birth = chr,
        self_reported_category = chr
      )
    }
  )
}
