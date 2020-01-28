test_that("ee_remove_credentials",{
  result_True <- ee_remove_credentials()
  expect_true(result_True)
})


test_that("ee_remove_driver",{
  result_True <- ee_remove_driver()
  expect_true(result_True)
})



