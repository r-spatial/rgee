library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Method 01: Get a download URL for an image.
image1 <- ee$Image('srtm90_v4')
geom_params <-   list(
  scale = 30,
  crs = 'EPSG:4326',
  region = '[[-120, 35], [-119, 35], [-119, 34], [-120, 34]]'
)
path <- image1$getDownloadUrl(geom_params)
print(path)

# Method 02: Download a thumbnail image
image_local <- ee_as_thumbnail(image1)
plot(image_local)

vis_params = list(min = 0, max = 3000)
Map$addLayer(image1, vis_params)
