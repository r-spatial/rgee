context("rgee: ee_check test")

# Pre-checking ------------------------------------------------------
# Google credentials were loaded in the system?
skip_if_no_credentials <- function() {
  ee_path <- path.expand("~/.config/earthengine")
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", ee_path)
  if (isFALSE(file.exists(sessioninfo))) {
    skip("google credentials were not found")
  }
}

# Neccesary Python packages were loaded?
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

# Init Earth Engine just if it is necessary
init_rgee <- function() {
  ee_reattach()
  tryCatch(
    expr = ee$data$get_persistent_credentials()$client_id,
    error = function(e) {
      ee_reattach()
      ee_Initialize(
        email = 'data.colec.fbf@gmail.com',
        drive = TRUE,
        gcs = TRUE
      )
    }
  )
}

skip_if_no_credentials()
skip_if_no_pypkg()
init_rgee()

# -------------------------------------------------------------------------


test_that("simple ee_check ",{
  expect_true(ee_check_python())
  expect_null(ee_check_rgee_python_packages())
  expect_null(ee_check_credentials())
})

test_that("ee_check ",{
  expect_null(ee_check())
})


