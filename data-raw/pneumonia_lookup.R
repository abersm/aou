## code to prepare `pneumonia_lookup` dataset goes here
pneumonia_lookup <- readxl::read_xlsx(system.file("lookup/pneumonia_lookup.xlsx", package = "aou"), na = c("", "NA", "N/A", "na", "n/a", "?"), trim_ws = TRUE)
usethis::use_data(pneumonia_lookup, overwrite = TRUE)
