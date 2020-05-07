context("rgee: ee_check test")


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

test_that("simple ee_check ",{
  expect_true(ee_check_python())
  expect_null(ee_check_rgee_python_packages())
  expect_null(ee_check_credentials())
})

test_that("ee_check ",{
  expect_null(ee_check())
})


