context("rgee: ee_install test")

ee_path <- path.expand("~/.config/earthengine")
sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", ee_path)

user <- tryCatch(
  expr = read.table(sessioninfo,header = TRUE,stringsAsFactors = FALSE),
  error = function(e) ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
)

if (anyNA(user)) {
  ee_reattach()
  ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
}

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
