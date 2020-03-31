context("rgee: ee_help test")

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
