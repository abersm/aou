#' Load data from workbench using SQL
#'
#' @param patients,pneumonia,cv_complications If `TRUE` (default), data frame is loaded
#' @param other Other data frames to load. Enter as character vector. Options: `"demographics"`, `"comorbidities"`, `"substance_use"`, `"inflammatory_markers"`, `"lipids_a1c"`, `"bcx"`, `"sputum_uag"`. Default includes 1st 3
#' @returns List of data frames
#' @export
load_df <- function(
    patients = TRUE,
    pneumonia = TRUE,
    cv_complications = TRUE,
    other = c("demographics", "comorbidities", "substance_use")) {
  names <- c(
    if (patients) "patients",
    if (pneumonia) "pneumonia",
    if (cv_complications) "cv_complications",
    other
  )
  types <- c(
    patients = "person",
    pneumonia = "conditions",
    cv_complications = "conditions",
    demographics = "surveys",
    comorbidities = "conditions",
    substance_use = "surveys",
    inflammatory_markers = "measurements",
    lipids_a1c = "measurements",
    bcx = "measurements",
    sputum_uag = "measurements"
  )
  sql <- c(
    patients = sql_patients,
    pneumonia = sql_pneumonia,
    cv_complications = sql_cv_complications,
    demographics = sql_demographics,
    comorbidities = sql_comorbidities,
    substance_use = sql_substance_use,
    inflammatory_markers = sql_inflammatory_markers,
    lipids_a1c = sql_lipids_a1c,
    bcx = sql_bcx,
    sputum_uag = sql_sputum_uag
  )
  out <- lapply(names, function(x) tryElse(aou::write_raw_data(sql[x], filename = x, type = types[x]), otherwise = "Error"))
  names(out) <- names
  out
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

#' Move file from workspace to bucket for permanent storage
#'
#' @param file File in workspace
#' @returns File silently exported
#' @export
aou_workspace_to_bucket <- function(file, directory = FALSE, bucket = getOption("aou.default.bucket")) {
  file <- gsub(" ", "_", file)
  tmp <- tempdir()
  tmp_log <- file.path(tmp, "cp.log")
  gsutil_args <- paste("-L", tmp_log)
  if (directory) {
    gsutil_args <- paste("-r", gsutil_args)
  }
  for (i in seq_along(file)) {
    system(paste("gsutil cp", gsutil_args, file[i], bucket), intern = TRUE)
  }
  if (length(read.csv(tmp_log)$Destination) == 0L) {
    cli::cli_inform(c(`!` = "Oops! No files were copied"))
  } else {
    cli::cli_inform(c(v = "Saved to bucket:", paste(gsub(paste0(bucket, "/"), "", read.csv(tmp_log)$Destination), collapse = "\n")))
  }
  invisible(file.remove(tmp_log))
}

#' Move file from bucket to workspace
#'
#' @param file File in bucket
#' @returns File silently exported
#' @export
aou_bucket_to_workspace <- function(file, directory = FALSE, bucket = getOption("aou.default.bucket")) {
  bucket_files <- allofus::aou_ls_bucket(silent = TRUE)
  missing_files <- list()
  if (directory) {
    file <- paste0(file, "/:")
    gs_args <- "gsutil cp -r "
  } else {
    gs_args <- "gsutil cp "
  }
  for (i in seq_along(file)) {
    if (!(file[i] %in% bucket_files)) {
      missing_files <- append(missing_files, file[i])
    } else {
      system(paste0(gs_args, bucket, "/", file[i], " ."), intern = TRUE)
      cli::cli_inform(c(v = "Retrieved ", file[i], " from bucket."))
    }
  }
  if (length(missing_files) != 0L) {
    missing <- paste0(unlist(missing_files), collapse = ", ")
    cli::cli_inform(c(`!` = paste0(missing, " not found in bucket.")))
  }
}

