# Functionality from aou.reader package

.download_query <- function(sql, date_column, dest, anchor_date_table, before, after) {
  .window_data(.download_big_data(sql, dest), date_column, anchor_date_table, before, after)
}

.window_data <- function(dat, date_column, anchor_date_table, before, after) {
  if (!is.null(anchor_date_table)) {
    dat <- data.table::as.data.table(merge(dat, anchor_date_table, by = "person_id", allow.cartesian = TRUE))
    dat[, min_window_date := anchor_date + before]
    dat[, max_window_date := anchor_date + after]
    dat <- dat[get(date_column) >= min_window_date]
    dat <- dat[get(date_column) <= max_window_date]
    dat[, min_window_date := NULL]
    dat[, max_window_date := NULL]
    dat[, anchor_date := NULL]
  }
  dat
}

#' Download big data
#'
#' @param query SQL query
#' @param dest Name of csv file
#' @param rm_csv Whether to delete the csv after downloading. Default is `TRUE`
#' @returns If `rm_csv = TRUE`, it will return a data.table corresponding to the query only.  If `rm_csv = FALSE`, then in addition to the above, a csv file will also be saved to a folder called aou_reader in the workspace bucket
#' @noRd
.download_big_data <- function(query, dest, rm_csv = TRUE) {
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  output_folder <- stringr::str_glue("{bucket}/{dest}/")
  dest <- paste0(output_folder, gsub(".csv", "_*.csv", dest))
  bq_table <- bigrquery::bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), query, billing = Sys.getenv("GOOGLE_PROJECT"))
  bigrquery::bq_table_save(bq_table, dest, destination_format = "CSV")
  res <- data.table::as.data.table(aou.bucket::read_bucket(dest))
  if (rm_csv) {
    system(stringr::str_glue("gsutil rm {output_folder}*"), intern = TRUE)
  }
  res
}

.download_query <- function(sql, dest, date_column, anchor_date_table, before, after) {
  .window_data(.download_big_data(sql, dest), date_column, anchor_date_table, before, after)
}

#' Concept code query
#'
#' Example: concept_code_query(c(441641, 4014295))
#' @param x Character vector or string containing medication names
#' @param anchor_date_table Data frame with 2 columns: person_id, anchor_date. A time window can be defined around the anchor date using the `before` and `after` arguments
#' @param before integer >= 0. Dates prior to anchor_date + before will be excluded
#' @param after integer >= 0. Dates after anchor_date + after will be excluded
#' @returns data.table containing the following columns: person_id, condition_start_date, concept_name, condition_concept_id
#' @export
concept_code_query <- function(x, anchor_date_table = NULL, before = NULL, after = NULL, dest = "concept_code_query_result.csv") {
  date_column <- "condition_start_date"
  codes <- paste0("(", paste0(x, collapse = ","), ")")
  query <- stringr::str_glue("
        SELECT person_id, condition_start_date, concept_name, condition_concept_id
        FROM condition_occurrence co
        INNER JOIN concept c ON (co.condition_concept_id = c.concept_id)
        WHERE condition_concept_id IN {codes}
  ")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Condition query
#'
#' Example: condition_query(concept_ids = c(710706,705076), source_values = "U09.9")
#' @param concept_ids Numeric vector with condition concept ids
#' @param source_values Character vector with condition source values
#' @rdname concept_code_query
#' @returns data.table containing the following columns: person_id, condition_start_date, condition_concep_id, condition_source_value
#' @export
condition_query <- function(concept_ids = NULL, source_values = NULL, anchor_date_table = NULL, before = NULL, after = NULL, dest = "condition_query_result.csv") {
  if (is.null(concept_ids) && is.null(source_values)) {
    stop("Both concept ids and source values can't be null")
  }
  date_column <- "condition_start_date"
  if (is.null(concept_ids)) {
    dx_values <- paste("c.condition_source_value LIKE ", "'", source_values, "'", collapse = " OR ", sep = "")
    query <- stringr::str_glue("
            SELECT person_id, condition_start_date, condition_source_value, condition_concept_id
            FROM `condition_occurrence` c
            WHERE ({dx_values})")
  } else if (is.null(source_values)) {
    dx_ids <- paste(concept_ids, collapse = ", ")
    query <- stringr::str_glue("
            SELECT person_id, condition_start_date, condition_source_value, condition_concept_id
            FROM `condition_occurrence` c WHERE condition_concept_id IN ({dx_ids})")
  } else {
    dx_ids <- paste(concept_ids, collapse = ", ")
    dx_values <- paste("c.condition_source_value LIKE ", "'", source_values, "'", collapse = " OR ", sep = "")
    query <- stringr::str_glue("
            SELECT person_id, condition_start_date, condition_source_value, condition_concept_id
            FROM `condition_occurrence` c WHERE ({dx_values}) OR condition_concept_id IN ({dx_ids})")
  }
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Demographics query
#'
#' @returns data.table with the following columns: person_id, date_of_birth, race, ethnicity, sex
#' @rdname concept_code_query
#' @export
demographics_query <- function(dest = "demographics_query_result.csv",  anchor_date_table = NULL, before = NULL, after = NULL) {
  query <- stringr::str_glue("
    SELECT
        person.person_id,
        person.birth_datetime as date_of_birth,
        p_race_concept.concept_name as race,
        p_ethnicity_concept.concept_name as ethnicity,
        p_sex_at_birth_concept.concept_name as sex
    FROM
        `person` person
    LEFT JOIN
        `concept` p_race_concept
            ON person.race_concept_id = p_race_concept.concept_id
    LEFT JOIN
        `concept` p_ethnicity_concept
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id
    LEFT JOIN
        `concept` p_sex_at_birth_concept
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id", sep = "")
  .download_query(query, dest, "", NULL, before, after)
}

#' Lab concept query
#'
#' Example: lab_concept_query(c(586520,586523,586525))
#' @param x Character vector or string containing the labs concepts to query
#' @returns data.table containing the following columns: person_id, measurement_date, value_as_number, value_as_concept
#' @rdname concept_code_query
#' @export
lab_concept_query <- function(x, anchor_date_table = NULL, before = NULL, after = NULL, dest = "lab_concept_query_result.csv") {
  date_column <- "measurement_date"
  lab_concepts <- paste(x, collapse = ", ")
  query <- stringr::str_glue("
        SELECT person_id, measurement_date, value_as_number, value_as_concept_id
        FROM `measurement` m
        INNER JOIN `concept` c ON (m.measurement_concept_id = c.concept_id)
        WHERE c.concept_id IN ({lab_concepts})
        ")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Lab query
#'
#' Example: lab_query(c("Triglyceride [Mass/volume] in Serum or Plasma","Triglyceride [Mass/volume] in Blood"))
#' @param x Character vector or string containing the labs to query
#' @returns data.table containing the following columns: person_id, measurement_date, value_as_number
#' @rdname concept_code_query
#' @export
lab_query <- function(x, anchor_date_table = NULL, before = NULL, after = NULL, dest = "lab_query_result.csv") {
  date_column <- "measurement_date"
  lab_terms <- paste("c.concept_name LIKE ", "'", x, "'", collapse = " OR ", sep = "")
  query <- stringr::str_glue("
        SELECT person_id, measurement_date, value_as_number
        FROM `measurement` m
        INNER JOIN `concept` c ON (m.measurement_concept_id = c.concept_id)
        WHERE
        ({lab_terms})
        ")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Lab query with extended columns
#'
#' Example: lab_query(c("Triglyceride [Mass/volume] in Serum or Plasma","Triglyceride [Mass/volume] in Blood"), ext_cols = c("unit_source_value"))
#' @param x Character vector or string containing the labs to query
#' @param ext_cols Extra columns to return on top of the date and value of the lab
#' @returns data.table containing the following columns: person_id, measurement_date, value_as_number, ext_cols
#' @export
lab_query_extended <- function(x, ext_cols = NULL, anchor_date_table = NULL, before = NULL, after = NULL, dest = "lab_query_result.csv") {
  date_column <- "measurement_date"
  lab_terms <- paste("c.concept_name LIKE ", "'", x, "'", collapse = " OR ", sep = "")
  if (is.null(ext_cols)) {
    query <- stringr::str_glue("
        SELECT person_id, measurement_date, value_as_number
        FROM `measurement` m
        INNER JOIN `concept` c ON (m.measurement_concept_id = c.concept_id)
        WHERE
        ({lab_terms})
        ")
  } else {
    extr_cols <- paste(ext_cols, collapse = ", ")
    query <- stringr::str_glue("
        SELECT person_id, measurement_date, value_as_number, {extr_cols}
        FROM `measurement` m
        INNER JOIN `concept` c ON (m.measurement_concept_id = c.concept_id)
        WHERE
        ({lab_terms})
        ")
  }
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Medication query
#'
#' @param x Character vector or string containing medication names
#' @returns data.table containing the following columns: person_id, drug_exposure_start_date
#' @rdname concept_code_query
#' @export
rx_query <- function(x, anchor_date_table = NULL, before = NULL, after = NULL, dest = "med_query_result.csv") {
  date_column <- "drug_exposure_start_date"
  rx_terms <- paste("lower(c.concept_name) LIKE ", "'%", x, "%'", collapse = " OR ", sep = "")
  query <- stringr::str_glue("
       SELECT DISTINCT d.person_id,d.drug_exposure_start_date
        FROM
        drug_exposure d
        INNER JOIN
        concept c
        ON (d.drug_concept_id = c.concept_id)
        WHERE
        {rx_terms}
    ")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Medication with record source information query
#'
#' @param x Character vector or string containing medication names
#' @returns data.table containing the following columns: person_id, drug_exposure_start_date, record_source
#' @rdname concept_code_query
#' @export
rx_with_record_source_query <- function(x, anchor_date_table = NULL, before = NULL, after = NULL, dest = "med_with_drug_type_query_result.csv") {
  date_column <- "drug_exposure_start_date"
  rx_terms <- paste("lower(c.concept_name) LIKE ", "'%", x, "%'", collapse = " OR ", sep = "")
  query <- stringr::str_glue("
       SELECT DISTINCT d.person_id, d.drug_exposure_start_date, c2.concept_name AS record_source
        FROM
        drug_exposure d
        INNER JOIN
        concept c
        ON (d.drug_concept_id = c.concept_id)
        INNER JOIN
        concept c2
        ON (d.drug_type_concept_id = c2.concept_id)
        WHERE
        {rx_terms}
    ")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' BMI query
#'
#' @returns data.table with the following columns: person_id, measurement_date, bmi
#' @rdname concept_code_query
#' @export
bmi_query <- function(anchor_date_table = NULL, before = NULL, after = NULL, dest = "bmi_query_result.csv") {
  date_column <- "measurement_date"
  query <- stringr::str_glue("
        SELECT
            measurement.person_id,
            measurement.measurement_date AS measurement_date,
            measurement.value_as_number AS bmi
        FROM
            ( SELECT
                *
            FROM
                `measurement` measurement
            WHERE
                (
                    measurement_concept_id IN  (
                        SELECT
                            DISTINCT c.concept_id
                        FROM
                            `cb_criteria` c
                        JOIN
                            (
                                select
                                    cast(cr.id as string) as id
                                FROM
                                    `cb_criteria` cr
                                WHERE
                                    concept_id IN (
                                        3038553
                                    )
                                    AND full_text LIKE '%_rank1]%'
                            ) a
                                ON (
                                    c.path LIKE CONCAT('%.',
                                a.id,
                                '.%')
                                OR c.path LIKE CONCAT('%.',
                                a.id)
                                OR c.path LIKE CONCAT(a.id,
                                '.%')
                                OR c.path = a.id)
                            WHERE
                                is_standard = 1
                                AND is_selectable = 1
                            )
                    )
                ) measurement")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Death
#'
#' @returns data.table with the following columns: person_id, death_entry_date
#' @rdname concept_code_query
#' @export
death_query <- function(anchor_date_table = NULL, before = NULL, after = NULL, dest = "death_query_result.csv") {
  date_column <- "death_entry_date"
  query <- stringr::str_glue("
            SELECT
                person_id,
                death_date AS death_entry_date
            FROM
                death")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' Survey query
#'
#' Example: survey_query("1585860")
#' @param x Character vector or string of survey codes
#' @returns data.table with the following columns: person_id, survey_response, survey_date
#' @rdname concept_code_query
#' @export
survey_query <- function(x, anchor_date_table = NULL, before = NULL, after = NULL, dest = "survey_query.csv") {
  date_column <- "survey_date"
  survey_codes <- paste0(x, collapse = ",")
  query <- stringr::str_glue("
        SELECT
            survey.person_id,
            survey.answer AS survey_response,
            CAST(survey.survey_datetime AS DATE) AS survey_date
        FROM
            `ds_survey` survey
        WHERE
            (
                question_concept_id IN (
                          {survey_codes}
                )
            )")
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' WGS
#'
#' @returns data.table with the following columns: person_id
#' @rdname concept_code_query
#' @export
wgs_query <- function(dest = "wgs_query_result.csv") {
  query <- paste("
        SELECT
            person.person_id
        FROM
            `person` person
        WHERE
            person.PERSON_ID IN (
                SELECT
                    distinct person_id
                FROM
                    `cb_search_person` cb_search_person
                WHERE cb_search_person.person_id IN (
                        SELECT
                            person_id
                        FROM
                            `cb_search_person` p
                        WHERE
                            has_whole_genome_variant = 1
                    )
                )", sep = "")
  .download_query(query, dest, "", NULL, before, after)
}

#' ICD-10
#'
#' Example: icd10_query(c("I21","I21.%"))
#' @param x Character vector of ICD10 codes. String can contain wildcards using % (e.g. "410.%")
#' @returns data.table with the following columns: person_id, condition_start_date, condition_source_value
#' @rdname concept_code_query
#' @export
icd10_query <- function(x = NULL, anchor_date_table = NULL, before = NULL, after = NULL, dest = "icd10_query_result.csv") {
  date_column <- "condition_start_date"
  icd10_terms <- paste("co.CONDITION_SOURCE_VALUE LIKE ", "'", x, "'", collapse = " OR ", sep = "")
  query <- if (is.null(x)) {
    stringr::str_glue("
          SELECT DISTINCT co.person_id,
            MIN(co.condition_start_date) AS condition_start_date,
            co.condition_source_value AS condition_source_value
        FROM
            condition_occurrence co
            INNER JOIN
            concept c
            ON (co.condition_source_concept_id = c.concept_id)
        WHERE
            c.VOCABULARY_ID LIKE 'ICD10CM'
            GROUP BY person_id, condition_source_value
      ")
  } else {
    stringr::str_glue("
          SELECT DISTINCT co.person_id,co.condition_start_date,co.condition_source_value
          FROM
          condition_occurrence co
          INNER JOIN
          concept c
          ON (co.condition_source_concept_id = c.concept_id)
      WHERE
          c.VOCABULARY_ID LIKE 'ICD10CM' AND
          ({icd10_terms})
      ")
  }
  .download_query(query, dest, date_column, anchor_date_table, before, after)
}

#' SNOMED
#'
#' @param x Character vector or character string containing snomed codes. String can contain wildcards using % (e.g. "410.%")
#' @returns data.table with the following columns: person_id, condition_start_date, condition_source_value
#' @rdname concept_code_query
#' @export
snomed_query <- function(x = NULL, anchor_date_table = NULL, before = NULL, after = NULL, dest = "snomed_query_result.csv") {
  snomed_terms <- paste("co.CONDITION_SOURCE_VALUE LIKE ", "'", x, "'", collapse = " OR ", sep = "")
  query <- if (is.null(x)) {
    stringr::str_glue("
       SELECT DISTINCT co.person_id,
        MIN(co.condition_start_date) AS condition_start_date,
        co.condition_source_value AS condition_source_value
    FROM
        condition_occurrence co
        INNER JOIN
        concept c
        ON (co.condition_source_concept_id = c.concept_id)
    WHERE
        c.VOCABULARY_ID LIKE 'SNOMED'
    GROUP BY person_id, condition_source_value
  ")
  } else {
    stringr::str_glue("
      SELECT DISTINCT co.person_id, co.condition_start_date,co.condition_source_value
      FROM
          condition_occurrence co
          INNER JOIN
          concept c
          ON (co.condition_source_concept_id = c.concept_id)
      WHERE
          c.VOCABULARY_ID LIKE 'SNOMED' AND
          ({snomed_terms})
      ")
  }
  result_all <- .download_big_data(query,dest)
  result_all$condition_source_value <- as.character(result_all$condition_source_value)
  result_all <- .window_data(result_all,"condition_start_date",anchor_date_table,before,after)
}
