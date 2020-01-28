context("rgee: ee_Initialize() test")

test_that("complex init",{
  out <- tryCatch(
    expr = ee_Initialize(email = 'aybar1994@gmail.com',
                         drive = TRUE,
                         gcs = TRUE),
    error = function(e) e)
  expect_equal(any("error" %in% class(out)), FALSE)
})

test_that("get EE path I",{
  expect_type(
    object = ee_get_earthengine_path(),
    type = 'character'
    )
})

test_that("ee_source_python - fn",{
  oauth_func_path <- system.file("python/sf_as_ee.py",
                                 package = "rgee")
  module_name <- gsub("\\.py$", "", basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  sf_as_ee_py <- reticulate::import_from_path(module_name,
                                              path = module_path,
                                              convert = F)
  expect_type(class(sf_as_ee_py),'character')
})
