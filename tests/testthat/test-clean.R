context("rgee: ee_clean_credentials test")

# Pre-checking ------------------------------------------------------
# Google credentials were loaded in the system?
skip_if_no_credentials <- function(user) {
  ee_path <- path.expand(sprintf("~/.config/earthengine/%s", user))
  credentials <- list.files(
    path = ee_path,
    pattern = "@gmail.com|credentials|GCS_AUTH_FILE.json"
  )
  if (length(credentials) != 3) {
    skip("All google credentials were not found")
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
  tryCatch(
    expr = ee$Image()$getInfo(),
    error = function(e) {
      ee_Initialize(
        email = 'data.colec.fbf@gmail.com',
        drive = TRUE,
        gcs = TRUE
      )
    }
  )
}

user <- "data.colec.fbf"
skip_if_no_credentials(user)
skip_if_no_pypkg()
init_rgee()

# -------------------------------------------------------------------------
test_that("ee_clean_credentials", {
  result_True <- ee_clean_credentials('test')
  expect_true(result_True)
})

test_that("ee_clean_credentials not-defined", {
  result_True <- ee_clean_pyenv()
  expect_true(result_True)
})

ee_Initialize(
  email = user,
  drive = TRUE,
  gcs = TRUE
)
