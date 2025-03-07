# Lookup tables
pneumonia_lookup <- readxl::read_xlsx(system.file("inst/lookup/pneumonia_lookup.xlsx", package = "aou"), na = c("", "NA", "N/A", "na", "n/a", "?"), trim_ws = TRUE)
comorb_lookup <- readxl::read_xlsx(system.file("inst/lookup/comorb_lookup.xlsx", package = "aou"), na = c("", "NA", "N/A", "na", "n/a", "?"), trim_ws = TRUE)

usethis::use_data(pneumonia_lookup, overwrite = TRUE)
usethis::use_data(comorb_lookup, overwrite = TRUE)

## Next step takes 4 minutes
#job::job({
#  # Patients
#  path_patients <- build_csv_path("patients")
#  write_csv_ws(sql_patient, path_patients)
#  df_patient <- read_csv_ws(path_patients, type = "person")
#
#  # Pneumonia
#  path_pneumonia <- build_csv_path("pneumonia")
#  write_csv_ws(sql_pneumonia, path_pneumonia)
#  df_pneumonia <- read_csv_ws(path_pneumonia, type = "conditions")
#
#  # CV complications
#  path_cv <- build_csv_path("cv_complications")
#  write_csv_ws(sql_cv_complications, path_cv)
#  df_complications <- read_csv_ws(path_cv, type = "conditions")
#
#  # Demographics
#  path_demog <- build_csv_path("demographics")
#  write_csv_ws(sql_demog, path_demog)
#  df_demog <- read_csv_ws(path_demog, type = "surveys")
#
#  # Comorbidities
#  path_comorb <- build_csv_path("comorbidities")
#  write_csv_ws(sql_comorb, path_comorb)
#  df_comorb <- read_csv_ws(path_comorb, type = "conditions")
#
#  # Substance use
#  path_substance <- build_csv_path("substance_use")
#  write_csv_ws(sql_substance_use, path_substance)
#  df_substance <- read_csv_ws(path_substance, type = "surveys")
#})
