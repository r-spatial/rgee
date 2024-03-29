% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ee_clean.R
\name{ee_clean_container}
\alias{ee_clean_container}
\title{Delete files from either a Folder (Google Drive) or a Bucket (GCS)}
\usage{
ee_clean_container(name = "rgee_backup", type = "drive", quiet = FALSE)
}
\arguments{
\item{name}{Character. Name of the folder (Google Drive) or bucket (GCS)
to delete all files.}

\item{type}{Character. Name of the file storage web service. 'drive'
and 'gcs' are supported.}

\item{quiet}{logical. Suppress info message}
}
\value{
No return value, called for cleaning Google Drive or Google
Cloud Storage container.
}
\description{
Delete all files from a folder (Google Drive) or a bucket
(Google Cloud Storage). Caution: this action will permanently delete any backup
files that were generated using \code{ee_as_stars} and \code{ee_as_sf}.
}
\seealso{
Other ee_clean functions: 
\code{\link{ee_clean_pyenv}()},
\code{\link{ee_clean_user_credentials}()}
}
\concept{ee_clean functions}
