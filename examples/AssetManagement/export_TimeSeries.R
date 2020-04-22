library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# define the geometry
geometry <- ee$Geometry$Polygon(
  list(
    c(116.49078369140625, 39.82219623803342),
    c(116.49456024169922, 39.763105626443306),
    c(116.57455444335938, 39.76336953661037),
    c(116.57421112060547, 39.8211414937017),
    c(116.49078369140625, 39.82219623803342)
  )
)

geometry <- geometry$bounds()

# mask out cloud covered regions
maskBadData <- function(image) {
  # Bits 3 and 5 are cloud shadow and cloud, respectively.
  cloudShadowBitMask <- bitwShiftL(1, 3)
  cloudsBitMask <- bitwShiftL(1, 5)

  # Get the pixel QA band.
  qa <- image$select("pixel_qa")

  # Both flags should be set to zero, indicating clear conditions.
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudsBitMask)$eq(0))$
    rename("cfmask")

  # Return the masked image, scaled to reflectance, without the QA bands.
  image$updateMask(mask)$divide(10000)$
    select("B[0-9]*")$
    addBands(mask)$
    copyProperties(image, list("system:time_start"))
}

# get the image collection
LC8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")
LC8_clean <- LC8$filterDate("2015-01-01", "2015-12-31")$
  filterBounds(geometry)$
  map(maskBadData)
# get image informaiton
count <- LC8_clean$size()$getInfo()
sceneList <- LC8_clean$aggregate_array("system:index")$getInfo()

# Loop to output each image
LC8_clean_list <- LC8_clean$toList(count)

for (index in seq_len(count)) {
  scene <- ee$Image(LC8_clean_list$get(index - 1))
  scenename <- sceneList[index]
  valid <- scene$select("cfmask")$clip(geometry)
  meanStat <- valid$reduceRegion(reducer = ee$Reducer$mean(), maxPixels = 1e9)$getInfo()
  cat(scenename, ":", meanStat[["cfmask"]], "\n")
  if (meanStat[["cfmask"]] > 0) {
    cat(scenename, "is valid\n")
    layer <- scene$select(
      opt_selectors = c("B2", "B3", "B4", "B5", "B6", "B7"),
      opt_names = c("B1", "B2", "B3", "B4", "B5", "B7")
    )
    layerClip <- layer$clip(geometry)
    # visualize
    # Image(url=layer$getThumbUrl())

    # export
    exportname <- paste0("segID_0_", sceneList[index])
    task <- ee$batch$Export$image$toDrive(
      image = layerClip,
      description = exportname,
      scale = 30
    )
    task$start()
    # ee_monitoring(task)
  } else {
    cat(scenename, " is invalid \n")
  }
}
