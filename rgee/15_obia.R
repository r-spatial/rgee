#' rgee Demo #3: OBIA + RandomForest
#' @author Joao Otavio Nascimento Firigato
#' https://medium.com/@joaootavionf007/object-based-image-analysis-on-google-earth-engine-1b80e9cb7312
#' https://code.earthengine.google.com/eb2890d3ddf09633dfaa13e41bafd2fc
#' @adapted from  Javascript to R: Cesar Aybar

library(raster)
library(rgee)
library(sf)

ee_Initialize(drive = TRUE)

AOI <- ee$Geometry$Polygon(
  list(
    c(-91.1028433068419, 40.82444972968615),
    c(-91.1028433068419, 40.810549087005384),
    c(-91.0761499627257, 40.810549087005384),
    c(-91.0761499627257, 40.82444972968615))
)

dataset <- "https://gist.githubusercontent.com/csaybar/90fc35de6677f8dbfd874bb18d944c32/raw/4d79fd9d6760583444d8b9c27969793687cadf3b/points.geojson"
geojson_file <- tempfile()
download.file(dataset, geojson_file)
train_points <- read_sf(geojson_file) %>% sf_as_ee()

dataset = ee$ImageCollection('USDA/NAIP/DOQQ')$
  filter(ee$Filter$date("2017-01-01", "2018-12-31"))$
  filterBounds(AOI)

img <- dataset$mean()$clip(AOI)
trueColorVis <- list(
  min = 0,
  max = 255,
  bands = "R,G,B"
)

Map$centerObject(AOI)
Map$addLayer(img, trueColorVis, 'True Color')

ndvi <- img$normalizedDifference(c("N", "R"))$rename('NDVI')
Map$addLayer(
  eeObject = ndvi,
  visParams = list(
    min = -1,
    max = 0.8,
    palette = c("red", "orange", "yellow", "green")
  ),
  name = "NDVI"
)

Map$addLayer(
  eeObject = ndvi$gt(c(0, 0.2, 0.40, 0.60, 0.80))$reduce('sum'),
  visParams =  list(
    min = 0,
    max = 4,
    palette = c("red", "orange", "yellow", "green", "darkgreen")
  ),
  name = "NDVI steps"
)

ndviGradient <- ndvi$gradient()$pow(2)$reduce("sum")$sqrt()$rename("NDVI_GRAD")
Map$addLayer(ndviGradient, list(min = 0, max = 0.01), "NDVI gradient")

# Compute entropy and display.
square <- ee$Kernel$square(radius = 4)
entropy <- img$select('N')$toByte()$entropy(square)
Map$addLayer(
  eeObject = entropy,
  visParams = list(min = 1, max = 5, palette = c("0000CC", "CC0000")),
  name = "entropy"
)

glcm <- img$select("N")$toByte()$glcmTexture(size = 4)
contrast <- glcm$select("N_contrast")
Map$addLayer(
  eeObject = contrast,
  visParams = list(min = 0, max = 1500, palette = c("0000CC", "CC0000")),
  name = "contrast"
)

asm <- glcm$select('N_asm')
Map$addLayer(asm, {}, 'asm')

bands <- c("R", "G", "B", "N")
FullImage <- img$select(bands)$float()$divide(255)

# Segmentation -----------------------------------------------------------------------------
seeds <- ee$Algorithms$Image$Segmentation$seedGrid(35)

snic <- ee$Algorithms$Image$Segmentation$SNIC(
  image = FullImage,
  compactness = 0,
  connectivity = 4,
  neighborhoodSize = 128,
  size = 2,
  seeds = seeds
)
clusters_snic <- snic$select("clusters")

vectors <- clusters_snic$reduceToVectors(
  geometryType = 'polygon',
  reducer = ee$Reducer$countEvery(),
  scale = 1,
  maxPixels = 1e13,
  geometry = AOI
)

empty <- ee$Image()$byte()
outline <- empty$paint(
  featureCollection = vectors,
  color = 1,
  width = 1
)
Map$addLayer(outline, list(palette = "FF0000"), "segments")

# Select train polygons from points -------------------------------------------------------
FullImage <- FullImage$addBands(ndvi)$addBands(ndviGradient)$
  addBands(glcm$select(c("N_contrast","N_asm","N_corr"))$float())$
  addBands(entropy)

train_polys <- vectors$map(function(feat){
  feat <- ee$Feature(feat)
  point <- feat$geometry()
  train_points$map(function(poly){
    cls <- poly$get("Class")
    intersects <- poly$intersects(point, ee$ErrorMargin(1))
    property <- ee$String(ee$Algorithms$If(intersects, "TRUE", "FALSE"))
    feat$set("belongsTo",  property)$set("Class", cls)
  })
})$flatten()$filter(ee$Filter$neq("belongsTo", "FALSE"))

# extract features from train polygons ---------------------------------------------
train_areas <- train_polys$reduceToImage(
  properties = list("Class"),
  reducer = ee$Reducer$first()
)$rename('Class')$toInt()


# extract features from train polygons ---------------------------------------------
predict_image <- vectors$reduceToImage(
  properties = list("label"),
  reducer = ee$Reducer$first()
)$rename("id")$toInt()

FullImage <- FullImage$addBands(predict_image)

FullImage_mean <- FullImage$reduceConnectedComponents(
  reducer = ee$Reducer$mean(),
  labelBand = 'id'
)

FullImage_std <- FullImage$reduceConnectedComponents(
  reducer = ee$Reducer$stdDev(),
  labelBand = 'id'
)

FullImage_median <- FullImage$reduceConnectedComponents(
  reducer = ee$Reducer$median(),
  labelBand = 'id'
)

FullImage_area <- ee$Image$pixelArea()$
  addBands(FullImage$select("id"))$
  reduceConnectedComponents(ee$Reducer$sum(), "id")

FullImage_sizes <- ee$Image$pixelLonLat()$
  addBands(FullImage$select("id"))$
  reduceConnectedComponents(ee$Reducer$minMax(), "id")

FullImage_width <- FullImage_sizes$select("longitude_max")$
  subtract(FullImage_sizes$select("longitude_min"))$
  rename("width")

FullImage_height <- FullImage_sizes$select("latitude_max")$
  subtract(FullImage_sizes$select("latitude_min"))$
  rename("height")

# join features in an image
Pred_bands <- ee$Image$cat(
  FullImage_mean,
  FullImage_std,
  FullImage_median,
  FullImage_area,
  FullImage_width,
  FullImage_height
)$float()

clip_Image <- Pred_bands$clip(train_polys)
train_areas <- train_areas$addBands(clip_Image)
predictionBands <- Pred_bands$bandNames()

classifierTraining <- train_areas$
  select(predictionBands)$
  sampleRegions(
    collection = train_polys,
    properties = list("Class"),
    scale = 2
  )

RF <- ee$Classifier$randomForest(50)$
  train(
    features = classifierTraining,
    classProperty = "Class",
    inputProperties = predictionBands
  )

classified_RF <- Pred_bands$select(predictionBands)$classify(RF)

vis_RF <- list(
  min = 0,
  max = 4,
  palette = list(
    'blue', # 0
    'red', # 1
    'orange', #2
    'green', #3
    'yellow'#4
  )
)

Map$addLayer(classified_RF, vis_RF, "OBIA_RF")
rf_obia <- ee_as_raster(classified_RF, AOI, "obia_rf.tif", scale = 5)
plot(rf_obia)
