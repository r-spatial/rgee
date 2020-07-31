#' rgee Demo #4: Kriging with rgee and gstat
#' @author Cesar Aybar

library(automap)
library(raster)
library(gstat)
library(sp)
library(sf)

# 1. Fit the variogram
data(meuse)
coordinates(meuse) <- ~ x+y
meuse <- meuse["zinc"]
variogram <- autofitVariogram(zinc~1,meuse, model = "Sph")

# 2. Load the neccesary data (meuse)
meuse %>%
  st_as_sf() %>%
  `st_crs<-`(28992) %>%
  sf_as_ee(proj = 28992) -> ee_meuse

# 3. Make predictions using Earth Engine
ee_meuse$kriging(
  shape = "spherical",
  propertyName = "zinc",
  range = variogram$var_model$range[2],
  sill = variogram$var_model$psill[2],
  nugget = variogram$var_model$psill[1],
  maxDistance = 4500
) -> ee_meuse_grid


band_viz <- list(
  min = 100,
  max = 1000,
  palette = c(
    '0D0887', '5B02A3',
    '9A179B', 'CB4678',
    'EB7852', 'FBB32F',
    'F0F921'
  )
)
ee$FeatureCollection4
Map$centerObject(ee_meuse$geometry()$bounds())
Map$addLayer(ee_meuse_grid, band_viz)

## Save results
ee_raster <- ee_as_raster(
  image = ee_meuse_grid,
  region = ee_meuse$geometry()$bounds()$buffer(1000),
  dsn = "/home/aybarpc01/ee_kriging_zinc.tif",
  scale = 40,
  via = "drive",
  crs = "EPSG:28992"
)

# 4. Make predictions using gstats
data(meuse.grid)
gridded(meuse.grid) = ~x+y
x <- krige(zinc~1, meuse, meuse.grid, model = variogram$var_model)

# 5. Compare results
ee_raster <- mask(crop(ee_raster, meuse.grid), meuse.grid)
plot(ee_raster, main = "Kriging - Earth Engine")
plot(mask(raster(x), meuse.grid), main = "gstat - Earth Engine")
