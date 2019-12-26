context("rgee: ee_install test")

test_that("ee_install_drivers() error",{
  expect_error(ee_install_drivers())
})

test_that("ee_install_drivers()",{
  expect_true(ee_install_drivers(73))
})

test_that("ee_install_rgee_python_packages()",{
  ee_install_rgee_python_packages(restart_session = FALSE,
                                  envname='rgee_test')
})


