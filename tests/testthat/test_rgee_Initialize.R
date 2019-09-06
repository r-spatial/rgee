context("rgee: ee_Initialize() test")

test_that("ee_quota",{
 out <- tryCatch(ee_Initialize(), error = function(e) e)
 expect_equal(any("error" %in% class(out)), FALSE)
})


