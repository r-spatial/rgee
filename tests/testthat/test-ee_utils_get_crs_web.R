context("rgee: ee_utils_get_crs_web test")

# -------------------------------------------------------------------------
ee_utils_get_crs_web <- rgee:::ee_utils_get_crs_web
ee_utils_get_crs <- rgee:::ee_utils_get_crs

test_that("ee_utils_get_crs handles different code types", {
    
    # Test EPSG code
    epsg_result <- ee_utils_get_crs("EPSG:4326")
    expect_true(grepl("GEOGCS", epsg_result))
    expect_true(grepl("WGS 84", epsg_result))

    # Test ESRI code
    esri_result <- ee_utils_get_crs("ESRI:54009")
    expect_true(grepl("PROJCS", esri_result))
    expect_true(grepl("World_Mollweide", esri_result))

    # Test SR-ORG code
    sr_org_result <- ee_utils_get_crs("SR-ORG:6864")
    expect_true(grepl("PROJCS", sr_org_result))
    expect_true(grepl("Pseudo-Mercator", sr_org_result))

    # Test that ee_utils_get_crs correctly uses ee_utils_get_crs_web for SR-ORG codes
    sr_org_direct <- ee_utils_get_crs_web("SR-ORG:6864")
    expect_equal(sr_org_result, rgee:::ee_utils_py_to_r(sr_org_direct))
})

test_that("ee_utils_get_crs_web handles different scenarios correctly", {
  # Test EPSG code
  epsg_result <- ee_utils_get_crs_web("EPSG:3857")
  expect_true(grepl("PROJCS", epsg_result))
  expect_true(grepl("WGS 84 / Pseudo-Mercator", epsg_result))

  # Test SR-ORG code
  sr_org_result <- ee_utils_get_crs_web("SR-ORG:7483")
  expect_true(grepl("PROJCS", sr_org_result))
  expect_true(grepl("WGS 84 / Pseudo-Mercator", sr_org_result))

  # Test non-existent SR-ORG code
  expect_error(result <- ee_utils_get_crs_web("SR-ORG:99999"))
})

test_that("ee_utils_get_crs_web handles network errors gracefully", {
  # Test with an invalid URL to simulate a network error
  expect_error(ee_utils_get_crs_web("INVALID:1234"))
})