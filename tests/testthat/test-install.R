context("rgee: ee_install test")

if (isFALSE(exists('ee'))) {
  ee_reattach()
  ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
}

### Instalation module
test_that("ee_create_pyenv ",{
  result <- ee_create_pyenv('earthengine_test')
  expect_type(result,"character")
})

test_that("ee_discover_pyenvs",{
  python_envs <- ee_discover_pyenvs()
  expect_equal(class(python_envs), 'character')
})

test_that("ee_set_pyenv",{
  python_envs <- ee_discover_pyenvs()
  fmsg <- ee_set_pyenv(
    python_path = python_envs[1],
    python_env = 'earthengine_test',
    install = FALSE,
    confirm = FALSE)
  expect_true(fmsg)
})

# test_that("ee_install_rgee_python_packages",{
#   fmsg <- ee_install_python_packages()
#   expect_true(fmsg)
# })
#
# test_that("ee_earthengine_upgrade",{
#   fmsg <- ee_earthengine_upgrade()
#   expect_true(fmsg)
# })
