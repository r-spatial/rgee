context("rgee: ee_help test")

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


# clean TRUE
test_that("simple ee_help - py class", {
  return_message <- ee$Image %>% ee_help()
  expect_true(return_message)
})

test_that("simple ee_help - py function", {
  return_message <- ee$batch$Export$image$toDrive %>% ee_help()
  expect_true(return_message)
})


test_that("complex query ee_help - py function", {
  return_message <- ee$batch$Export$image$toCloudStorage %>% ee_help()
  expect_true(return_message)
})


test_that("complex query ee_help + browser - py function", {
  return_message <- ee$batch$Export$image$toCloudStorage %>%
    ee_help(browser = TRUE)
  expect_true(return_message)
})

test_that("eequery_scope null", {
  ret <- ee_help(ee$Image(0))
  expect_true(ret)
})


test_that("real_name null", {
  expect_equal(
    rgee:::ee_real_name("ee$Image(0)$clip"),
    "ee$Image$clip"
  )
})

test_that("ee_html_head_rstudio", {
  expect_type(
    rgee:::ee_html_head_rstudio("ee"),
    "character"
  )
  expect_type(
    rgee:::ee_html_description_rstudio("ee"),
    "character"
  )
  expect_type(
    rgee:::ee_html_arguments_rstudio("ee"),
    "character"
  )
  expect_type(
    rgee:::ee_html_details_rstudio("ee"),
    "character"
  )
  expect_type(
    rgee:::ee_html_returns_rstudio("ee"),
    "character"
  )
  expect_type(
    rgee:::ee_css_h2_rstudio(),
    "character"
  )
  expect_type(
    rgee:::ee_css_h3_rstudio(),
    "character"
  )
})
