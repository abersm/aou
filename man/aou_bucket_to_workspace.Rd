% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aou_data.R
\name{aou_bucket_to_workspace}
\alias{aou_bucket_to_workspace}
\title{Move file from bucket to workspace}
\usage{
aou_bucket_to_workspace(
  file,
  directory = FALSE,
  bucket = Sys.getenv("WORKSPACE_BUCKET")
)
}
\arguments{
\item{file}{File in bucket}
}
\value{
File silently exported
}
\description{
Move file from bucket to workspace
}
