context("rgee: ee_as_sf test")

test_that("sfg",{
  # Load and filter watersheds from a data table.
  sheds <- ee$FeatureCollection('USGS/WBD/2017/HUC06')$
    filterBounds(ee$Geometry$Rectangle(-127.18, 19.39, -62.75, 51.29))$
    map(function(feature) {
      num <- ee$Number$parse(feature$get('areasqkm'))
      return(feature$set('areasqkm', num))
    })
  mysheds <- ee_as_sf(sheds$first())
  expect_equal(mysheds$areaacres, "1064898.31")
})
