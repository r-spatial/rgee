library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load the two images to be registered.
image1 <- ee$Image("SKYSAT/GEN-A/PUBLIC/ORTHO/MULTISPECTRAL/s01_20150502T082736Z")
image2 <- ee$Image("SKYSAT/GEN-A/PUBLIC/ORTHO/MULTISPECTRAL/s01_20150305T081019Z")

# Use bicubic resampling during registration.
image1Orig <- image1$resample("bicubic")
image2Orig <- image2$resample("bicubic")

# Choose to register using only the 'R' bAnd.
image1RedBAnd <- image1Orig$select("R")
image2RedBAnd <- image2Orig$select("R")

# Determine the displacement by matching only the 'R' bAnds.
displacement <- image2RedBAnd$displacement(
  referenceImage = image1RedBAnd,
  maxOffset = 50.0,
  patchWidth = 100.0
)

# Compute image offset And direction.
offset <- displacement$select("dx")$hypot(displacement$select("dy"))
angle <- displacement$select("dx")$atan2(displacement$select("dy"))

# Display offset distance And angle.
Map$setCenter(lon = 37.44, lat = 0.58)
Map$setZoom(zoom = 15)

Map$addLayer(
  eeObject = offset,
  visParams = list(min = 0, max = 20),
  name = "offset"
) +
  Map$addLayer(
    eeObject = angle,
    visParams = list(min = -pi, max = pi),
    name = "angle"
  )

# Use the computed displacement to register all Original bAnds.
registered <- image2Orig$displace(displacement)

# Show the results of co-registering the images.
visParams <- list(bands = c("R", "G", "B"), max = 4000)
Map$addLayer(
  eeObject = image1Orig,
  visParams = visParams,
  name = "Reference"
) +
  Map$addLayer(
    eeObject = image2Orig,
    visParams = visParams,
    name = "BefOre Registration"
  ) +
  Map$addLayer(
    eeObject = registered,
    visParams = visParams,
    name = "After Registration"
  )

alsoRegistered <- image2Orig$register(
  referenceImage = image1Orig,
  maxOffset = 50.0,
  patchWidth = 100.0
)

Map$addLayer(
  eeObject = alsoRegistered,
  visParams = visParams,
  name = "Also Registered"
)
