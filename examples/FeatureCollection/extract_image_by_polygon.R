library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Extract Landsat image based on individual polygon
extract_landsat <- function(feature) {
  geom <- ee$Feature(feature)$geometry()
  ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$
    filterDate('2019-01-01', '2019-12-31')$
    filterBounds(geom)$
    median()$
    clip(geom)
}

# Select the first five U.S. counties
counties <- ee$FeatureCollection('TIGER/2018/Counties') %>%
  ee$FeatureCollection$toList(5)

Map$setCenter(-97, 38, 3)
Map$addLayer(
  eeObject = ee$FeatureCollection(counties),
  visParams = list(palette = 'red'),
  name =  "Selected Counties"
)

# Extract Landsat image for each county
# help(ee_pyfunc)
images <- counties$map(ee_pyfunc(extract_landsat))

# Add images to map
vis <- list(
  bands = c('B5', 'B4', 'B3'),
  min = 0,
  max = 3000,
  gamma = 1.4
)

ee_map <- list()
for (index in seq_len(5)) {
  image <- ee$Image(images$get(index - 1))
  Map$centerObject(image)
  obj_map <- Map$addLayer(
    eeObject = image,
    visParams = vis,
    name =  paste0('Image ', index)
  )
  ee_map[[index]] <- obj_map
}

Reduce('+',ee_map)
