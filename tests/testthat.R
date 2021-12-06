library(testthat)
library(raster)
library(stars)
library(rgee)
library(sf)

Sys.setenv(EARTHENGINE_PYTHON="/usr/bin/python3")
Sys.setenv(RETICULATE_PYTHON="/usr/bin/python3")

# Pre-checking ------------------------------------------------------
# Google credentials were loaded in the system?
skip_if_no_credentials <- function() {
  ee_path <- ee_get_earthengine_path()
  credentials <- list.files(
    path = ee_path,
    pattern = "@gmail.com|credentials|GCS_AUTH_FILE.json"
  )
  if (length(credentials) != 3) {
    skip("All google credentials were not found")
  }
}

# Necessary Python packages were loaded?
skip_if_no_pypkg <- function() {
  have_ee <- reticulate::py_module_available("ee")
  have_numpy <- reticulate::py_module_available("numpy")
  if (isFALSE(have_ee)) {
    skip("ee not available for testing")
  }
  if (isFALSE(have_numpy)) {
    skip("numpy not available for testing")
  }
}

# Define your Drive folder to save intermediate files
# ALERT!!: After tests finished all the files inside the folder
# will be deleted.
drive_folder_f <- function(){
  "rgee_backup"
}

# Define your own GCS bucket with fine-grained access to save 
# intermediate files. ALERT!!: After test finished all the files 
# inside the bucket will be deleted.
gcs_bucket_f <- function(){
  "rgee_dev"
}

# Define your own GCS bucket with fine-grained access to save 
# intermediate files. ALERT!!: After test finished all the files 
# inside the bucket will be deleted.
gcs_bucket_uniform_f <- function(){
  "rgee_dev_uniform"
}

# Initialize credentials
# If you do not count with GCS credentials the test will be skipped
have_ee <- reticulate::py_module_available("ee")
have_numpy <- reticulate::py_module_available("numpy")
if (have_ee & have_numpy) {
ee_Initialize(drive = TRUE, gcs = TRUE)
}

test_check("rgee")
