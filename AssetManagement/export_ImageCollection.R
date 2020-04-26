library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# USDA NAIP ImageCollection
collection <- ee$ImageCollection('USDA/NAIP/DOQQ')

# create an roi
polys <- ee$Geometry$Polygon(
    list(
        c(-99.29615020751953, 46.725459351792374),
        c(-99.2116928100586, 46.72404725733022),
        c(-99.21443939208984, 46.772037733479884),
        c(-99.30267333984375, 46.77321343419932)
    )
)
fc <- ee$FeatureCollection(polys)

# create a FeatureCollection based on the roi and center the map
centroid = polys$centroid()$coordinates()$getInfo()
lng <- centroid[1]; lat <- centroid[2]
print(sprintf("lng = %s, lat = %s", lng, lat))

# filter the ImageCollection using the roi
naip = collection$filterBounds(polys)
naip_2015 = naip$filterDate('2015-01-01', '2015-12-31')
mosaic = naip_2015$mosaic()

# print out the number of images in the ImageCollection
count = naip_2015$size()$getInfo()
cat("Count: ", count)

# add the ImageCollection and the roi to the map
vis = list(bands = c('N', 'R', 'G'))
Map$setCenter(lng, lat, 12)
Map$addLayer(mosaic,vis) +
Map$addLayer(fc)

# export the ImageCollection to Google Drive
# scale means resolution.
downConfig = list(scale = 30, maxPixels = 1.0E13, driveFolder = 'image')
img_lst = naip_2015$toList(10)

# Method - 01
for (index in seq_len(count)) {
    image = ee$Image(img_lst$get(index-1))
    name = image$get('system:index')$getInfo()
    # print(name)
    task = ee$batch$Export$image(image, name, downConfig)
    task$start()
    ee_monitoring()
}

#Method - 02
usda_stars_time <- list()
for (index in seq_len(count)) {
    image = ee$Image(img_lst$get(index-1))
    name = image$get('system:index')$getInfo()
    # print(name)
    usda_stars <- ee_as_stars(
        image = image,
        region = polys,
        scale = downConfig$scale,
        geodesic = FALSE,
        maxPixels = downConfig$maxPixels,
        container = downConfig$driveFolder
    )
    names(usda_stars) <- name
    usda_stars[usda_stars==0] = NA
    usda_stars_time[[index]] <- usda_stars
}
usda_stars_mosaic <- do.call(st_mosaic, usda_stars_time)
