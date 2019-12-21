context("rgee: ee_Initialize() test")

test_that("simple init",{
  out <- tryCatch(ee_Initialize(), error = function(e) e)
  expect_equal(any("error" %in% class(out)), FALSE)
})

# test_that("complex init",{
#   out <- tryCatch(
#     ee_Initialize(
#       user_gmail = 'aybar1994@gmail.com',
#       drive = TRUE,
#       gcs = TRUE,
#       assethome = 'users/aybar1994',
#       checkpy = FALSE,
#       quiet=TRUE),
#     error = function(e) e)
#   expect_equal(any("error" %in% class(out)), FALSE)
# })

#ee_remove_credentials()
#ee_remove_drivers()
