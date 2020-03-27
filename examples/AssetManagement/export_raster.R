library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

collection <- ee$ImageCollection("USDA/NAIP/DOQQ")

polys <- ee$Geometry$Polygon(
  list(
    c(-99.29615, 46.72545),
    c(-99.21169, 46.72404),
    c(-99.21443, 46.77203),
    c(-99.30267, 46.77321)
  )
)

centroid <- polys$centroid()$getInfo()[["coordinates"]]
lng <- centroid[1]
lat <- centroid[2]
print(sprintf("lng = %s, lat = %s", lng, lat))

lng_lat <- ee$Geometry$Point(lng, lat)
naip <- collection$filterBounds(polys)
naip_2015 <- naip$filterDate("2015-01-01", "2015-12-31")
ppr <- naip_2015$mosaic()

count <- naip_2015$size()$getInfo()
cat("Count: ", count)

# scale means resolution.
downConfig <- list(
  scale = 30,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

img_lst <- naip_2015$toList(100)

for (r_index in seq_len(count)) {
  index <- r_index - 1
  image <- ee$Image(img_lst$get(index))
  name <- image$get("system:index")$getInfo()
  # print(name)
  task <- ee$batch$Export$image(image, name, downConfig)
  task$start()
}

ee_monitoring(task)
