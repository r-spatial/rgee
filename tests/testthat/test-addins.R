context("rgee: sf_as_ee test")

# -------------------------------------------------------------------------
# test_that("ee_help_addins 1", {
#   img <- ee$Image(0)
#   test01 <- expect_warning(ee_help_addins(debug = FALSE, content = "ee$Image$clip"))
#   test02 <- expect_warning(ee_help_addins(debug = FALSE, content = "img$clip"))
#   expect_equal((test01 + test02), 2)
# })


test_that("ee_space_removed", {
  start_to_look <- rgee:::ee_space_removed("  cesar  ", 4)
  expect_equal(start_to_look, 2)

  move_forward <- rgee:::forward("lesly", 1)
  expect_equal(move_forward, 1)

  move_backward <- rgee:::forward("lesly", 4)
  expect_equal(move_backward, 3)
})



test_that("ee_detect_os", {
  expect_equal("linux", rgee:::ee_detect_os())
  expect_equal(FALSE, rgee:::is_windows())
  expect_equal(FALSE, rgee:::ee_get_python_path(""))
})
