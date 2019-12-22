context("rgee: ee_Initialize() test")

test_that("GCS",{
expect_error(
  object = ee_create_credentials_gcs(
    assethome = "/",
    quiet = TRUE
      )
    )
  }
)

test_that("complex init",{
  out <- tryCatch(
    expr = ee_Initialize(user_gmail = 'aybar1994@gmail.com',
                         drive = TRUE,
                         gcs = TRUE,
                         checkpy = FALSE,
                         assethome = 'users/aybar1994'),
    error = function(e) e)
  expect_equal(any("error" %in% class(out)), FALSE)
})

test_that("simple init - error",{
  expect_type(
    object = ee_get_earthengine_path(),
    type = 'character'
    )
})

test_that("ee_source_python - fn",{
  oauth_func_path <- system.file("python/sf_as_ee.py",
                                 package = "rgee")
  sf_as_ee_py <- ee_source_python(oauth_func_path)
  expect_type(class(sf_as_ee_py),'character')
})

#ee_remove_credentials()
#ee_remove_drivers()
