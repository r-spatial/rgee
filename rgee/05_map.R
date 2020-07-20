library(rgee)
library(sf)

ee_Initialize(drive = TRUE)
ee_user_info()

# Case 1: Display an FeatureCollection
roi <- ee$Geometry$Point(c(-122.2575, 37.8795)) %>%
  ee$Geometry$buffer(10000)
blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
subset <- blocks$filterBounds(roi)
Map$centerObject(roi)
Map$addLayer(subset)

# Case 2: Display an Image
image <- ee$Image("CGIAR/SRTM90_V4")
band_viz = list(
  min = 0,
  max = 4000,
  palette = c(
    '0D0887', '5B02A3',
    '9A179B', 'CB4678',
    'EB7852', 'FBB32F',
    'F0F921'
  )
)
Map$setCenter()
Map$addLayer(image, band_viz)

# Case 3: Display an ImageCollection
ee_search_display("COPERNICUS/S2")
ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc) %>%
  ee_get(0:5)

rgbVis <- list(
  min = 0,
  max = 3000,
  bands = c('B4', 'B3', 'B2')
)
Map$centerObject(nc)
Map$addLayers(ee_s2, rgbVis)

# Case 4: Edit
library(mapedit)
m1 <- Map$addLayer(image, band_viz)
my_geometry <- m1 %>% ee_as_mapview() %>% editMap()
