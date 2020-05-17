context("rgee: ee_Initialize() test")

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
  ee_reattach()
  tryCatch(
    expr = ee$Image()$getInfo(),
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

user <- "data.colec.fbf"
skip_if_no_credentials(user)
skip_if_no_pypkg()
init_rgee()
# -------------------------------------------------------------------------


test_that("get_authorization_url is working well?",{
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- rgee:::ee_source_python(oauth_func_path)
  code_challenge <- ee_utils_py_to_r(utils_py$create_codes())
  ee_url <- ee$oauth$get_authorization_url(code_challenge[[2]])
  expect_type(ee_url,'character')
})

test_that("complex init",{
  out <- tryCatch(
    expr = ee_Initialize(email = 'data.colec.fbf@gmail.com',
                         drive = TRUE,
                         gcs = TRUE),
    error = function(e) e)
  expect_equal(any("error" %in% class(out)), FALSE)
})

test_that("get EE path I",{
  expect_type(
    object = ee_get_earthengine_path(),
    type = 'character'
    )
})

test_that("ee_source_python - fn",{
  oauth_func_path <- system.file("python/sf_as_ee.py",
                                 package = "rgee")
  module_name <- gsub("\\.py$", "", basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  sf_as_ee_py <- reticulate::import_from_path(module_name,
                                              path = module_path,
                                              convert = F)
  expect_type(class(sf_as_ee_py),'character')
})

test_that("ee_users",{
  counts <- ee_users()
  expect_true(counts)
})

test_that("ee_users",{
  counts <- ee_user_info()
  expect_true(counts)
})
