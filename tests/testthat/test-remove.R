test_that("ee_remove_credentials",{
  result_True <- ee_remove_credentials('test')
  expect_true(result_True)
})


