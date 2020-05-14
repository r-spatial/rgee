context("rgee: ee_clean_credentials test")

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

test_that("ee_clean_credentials",{
  result_True <- ee_clean_credentials('test')
  expect_true(result_True)
})

test_that("ee_clean_credentials not-defined",{
  result_True <- ee_clean_pyenv()
  expect_true(result_True)
})
