context("rgee: ee_as_thumbnail test")

# data --------------------------------------------------------------------
dem_palette <- c("#008435", "#1CAC17", "#48D00C", "#B3E34B", "#F4E467",
                 "#F4C84E", "#D59F3C", "#A36D2D", "#C6A889", "#FFFFFF")

nc = st_read(system.file("shp/arequipa.shp", package="rgee"))
sheds <- ee$FeatureCollection('USGS/WBD/2017/HUC06')$
  filterBounds(ee$Geometry$Rectangle(-127.18, 19.39, -62.75, 51.29))$
  map(function(feature) {
    num <- ee$Number$parse(feature$get('areasqkm'))
    return(feature$set('areasqkm', num))
  })

image <- ee$Image("CGIAR/SRTM90_V4")

# just one band -----------------------------------------------------------
test_that("sfg",{
  # PNG images
  region <- nc$geometry[[1]]
  arequipa_dem <- ee_as_thumbnail(x = image, region = region, vizparams = list(min = 0, max = 5000))
  arequipa_dem <- arequipa_dem * 5000
  expect_equal(max(arequipa_dem$G), 5000,tolerance=1)
})

test_that("sfg",{
  # JPEG images
  mysheds <- ee_as_sf(sheds$first())
  region <- mysheds$geometry[[1]]
  shed_dem <- ee_as_thumbnail(x = image,
                              region = region,
                              vizparams = list(min = 0, max = 500))
  expect_equal(max(shed_dem$G), 0.4470588,tolerance = .002)
})


# RGB band -----------------------------------------------------------
test_that("sfg",{
  # PNG images
  region <- nc$geometry[[1]]
  arequipa_dem <- ee_as_thumbnail(x = image,
                                  region = region,
                                  vizparams = list(palette=dem_palette, min = 0, max = 5000))
  arequipa_dem <- arequipa_dem * 5000
  expect_equal(max(arequipa_dem$X), 5000,tolerance=1)
})
