context("rgee: ee_help test")
skip_if_no_pypkg()
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
