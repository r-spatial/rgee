test_that("ee_remove_credentials",{
  result_True <- ee_remove_credentials('test')
  expect_true(result_True)
})

test_that("ee_remove_driver",{
  result_True <- ee_remove_ChromeDriver()
  expect_true(result_True)
})



