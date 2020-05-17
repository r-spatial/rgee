context("rgee: ee_install test")

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


### Instalation module
test_that("ee_install_create_pyenv ",{
  result <- ee_install_create_pyenv('earthengine_test')
  expect_type(result,"character")
})

test_that("ee_install_discover_pyenvs",{
  python_envs <- ee_install_discover_pyenvs()
  expect_equal(class(python_envs), 'character')
})

test_that("ee_install_set_pyenv",{
  python_envs <- ee_install_discover_pyenvs()
  fmsg <- ee_install_set_pyenv(
    py_path = python_envs[1],
    py_env = 'earthengine_test',
    install = FALSE,
    confirm = FALSE)
  expect_true(fmsg)
})

# test_that("ee_install_rgee_python_packages",{
#   fmsg <- ee_install_python_packages()
#   expect_true(fmsg)
# })
#
# test_that("ee_install_earthengine_upgrade",{
#   fmsg <- ee_install_earthengine_upgrade()
#   expect_true(fmsg)
# })
