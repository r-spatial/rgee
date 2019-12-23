context("rgee: upload test")

# test_that("ee_upload works", {
#   ee_check_drivers()
#   filename <- "users/aybar1994/rgee_upload/"
#   ee_manage_create(filename)
#   tif <- system.file("tif/geomatrix.tif", package = "stars")
#   geomatrix <- read_stars(tif) %>% st_warp(crs=st_crs(4326))
#   delta_geomatrix <- c(attr(geomatrix,'dimensions')$x$delta,attr(geomatrix,'dimensions')$y$delta*-1)
#   ee_upload(x = geomatrix,filename = paste0(filename,"geomatrix"))
#
#   ee_monitoring()
#   ee_geomatrix <- ee$Image(paste0(filename,"geomatrix"))
#   expect_equal(class(ee_geomatrix)[1],"ee.image.Image")
#   ee_manage_delete(filename)
# })
