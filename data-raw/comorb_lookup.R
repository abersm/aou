## code to prepare `comorb_lookup` dataset goes here
comorb_lookup <- readxl::read_xlsx(system.file("inst/lookup/comorb_lookup.xlsx", package = "aou"), na = c("", "NA", "N/A", "na", "n/a", "?"), trim_ws = TRUE)
usethis::use_data(comorb_lookup, overwrite = TRUE)
