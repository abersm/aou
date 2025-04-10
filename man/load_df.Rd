% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aou_data.R
\name{load_df}
\alias{load_df}
\title{Load data from workbench using SQL}
\usage{
load_df(
  patients = TRUE,
  pneumonia = TRUE,
  cv_complications = TRUE,
  other = c("demographics", "comorbidities", "substance_use")
)
}
\arguments{
\item{patients, pneumonia, cv_complications}{If \code{TRUE} (default), data frame is loaded}

\item{other}{Other data frames to load. Enter as character vector. Options: \code{"demographics"}, \code{"comorbidities"}, \code{"substance_use"}, \code{"inflammatory_markers"}, \code{"lipids_a1c"}, \code{"bcx"}, \code{"sputum_uag"}. Default includes 1st 3}
}
\value{
List of data frames
}
\description{
Load data from workbench using SQL
}
