context("rgee: ee_download test")

ee_Initialize(
  email = "aybar1994@gmail.com",
  drive = TRUE,
  gcs = TRUE
)

# Communal Reserve Amarakaeri - Peru
xmin <- -71.132591318
xmax <- -70.953664315
ymin <- -12.892451233
ymax <- -12.731116372
x_mean <- (xmin + xmax) / 2
y_mean <- (ymin + ymax) / 2

ROI <- c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin)
ROI_polygon <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
  list() %>%
  st_polygon() %>%
  st_sfc() %>%
  st_set_crs(4326)
ee_geom <- sf_as_ee(ROI_polygon, check_ring_dir = TRUE)

# Elevation map
ic_srtm <- ee$Image("CGIAR/SRTM90_V4")$reproject(crs = "EPSG:4326", scale = 500)
mean_srtm_Amarakaeri <- ic_srtm$clip(ee_geom)

# Testing parameters ------------------------------------------------------
fc_test <- ee$FeatureCollection(ee$Feature(ee_geom, list("test" = "feature")))
image_test <- mean_srtm_Amarakaeri
imageExportFormatOptions_1 <- list(
  patchDimensions = c(10L, 10L),
  compressed = TRUE
)
imageExportFormatOptions_2 <- list(
  patchDimensions = c(10L, 10L),
  compressed = FALSE
)
vectorExportFormatOptions_1 <- list(compressed = TRUE)
vectorExportFormatOptions_2 <- list(compressed = FALSE)

count <- 1
try_gd_rm <- try(googledrive::drive_rm("rgee_testing"))
while (class(try_gd_rm) == "try-error" & count < 5) {
  try_gd_rm <- try(googledrive::drive_rm("rgee_testing"))
  count <- count + 1
}

try(googledrive::drive_mkdir("rgee_testing"))
googleCloudStorageR::gcs_global_bucket("bag_csaybar")
buckets <- googleCloudStorageR::gcs_list_objects()
gcs_todelete <- buckets$name[grepl("^testing/.*$", buckets$name)]
mapply(googleCloudStorageR::gcs_delete_object, gcs_todelete)

# ### IMAGES
# # 1. GEOTIFF - DRIVE
test_that("GEOTIFF_DRIVE", {
  task_img <- ee$batch$Export$image$toDrive(
    image = image_test,
    folder = "rgee_testing",
    fileFormat = "GEOTIFF",
    fileNamePrefix = "test_image_GEOTIFF"
  )
  task_img$start()
  ee_monitoring(task_img)
  img <- ee_download_drive(task = task_img)
  expect_is(img, "stars")
})

# # 2. CTFRECORD_IMAGE - DRIVE
test_that("CTFRECORD_DRIVE", {
  task_img <- ee$batch$Export$image$toDrive(
    image = image_test,
    folder = "rgee_testing",
    fileFormat = "TFRECORD",
    fileNamePrefix = "test_image_CTFRECORD",
    formatOptions = imageExportFormatOptions_1
  )
  task_img$start()
  ee_monitoring(task_img)
  img <- ee_download_drive(task = task_img)
  expect_equal(img, TRUE)
})

# # 3. TFRECORD_IMAGE - DRIVE
test_that("TFRECORD_DRIVE", {
  task_img <- ee$batch$Export$image$toDrive(
    image = image_test,
    folder = "rgee_testing",
    fileFormat = "TFRECORD",
    fileNamePrefix = "test_image_TFRECORD",
    formatOptions = imageExportFormatOptions_2
  )
  task_img$start()
  ee_monitoring(task_img)
  img <- ee_download_drive(task = task_img)
  expect_equal(img, TRUE)
})
#
# # 4. GEOTIFF - GCS
test_that("GEOTIFF_GCS", {
  task_img <- ee$batch$Export$image$toCloudStorage(
    image = image_test,
    bucket = "bag_csaybar",
    fileFormat = "GEOTIFF",
    fileNamePrefix = "testing/test_image_GEOTIFF"
  )
  task_img$start()
  ee_monitoring(task_img)
  img <- ee_download_gcs(task = task_img)
  expect_is(img, "stars")
})

#
# # 5. CTFRECORD_IMAGE - GCS
# test_that("CTFRECORD_GCS",{
#   task_img <- ee$batch$Export$image$toCloudStorage(
#     image = image_test,
#     bucket = "bag_csaybar",
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "testing/test_image_CTFRECORD",
#     formatOptions = imageExportFormatOptions_1
#   )
#   task_img$start()
#   ee_monitoring(task_img)
#   img <- ee_download_gcs(task = task_img)
#   expect_equal(img, TRUE)
# })
#
# # 6. TFRECORD_IMAGE - GCS
# test_that("TFRECORD_GCS",{
#   task_img <- ee$batch$Export$image$toCloudStorage(
#     image = image_test,
#     bucket = "bag_csaybar",
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "testing/test_image_TFRECORD",
#     formatOptions = imageExportFormatOptions_2
#   )
#   task_img$start()
#   ee_monitoring(task_img)
#   img <- ee_download_gcs(task = task_img)
#   expect_equal(img, TRUE)
# })
#
# ### VECTOR
# # 7. CSV_VECTOR - DRIVE
# test_that("CSV_VECTOR_DRIVE",{
#   task_vector <- ee$batch$Export$table$toDrive(
#     collection = fc_test,
#     folder = "rgee_testing",
#     fileFormat = "CSV",
#     fileNamePrefix = "test_fc_CSV"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_drive(task = task_vector)
#   expect_equal(vector, TRUE)
# })
#
# # 8. SHP_VECTOR - DRIVE
# test_that("SHP_VECTOR_DRIVE",{
#   task_vector <- ee$batch$Export$table$toDrive(
#     collection = fc_test,
#     folder = "rgee_testing",
#     fileFormat = "SHP",
#     fileNamePrefix = "test_fc_SHP"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_drive(task = task_vector)
#   expect_equal(as.character(vector[["test"]]), "feature")
# })
#
# # 9. KML_VECTOR - DRIVE
# test_that("KML_VECTOR_DRIVE",{
#   task_vector <- ee$batch$Export$table$toDrive(
#     collection = fc_test,
#     folder = "rgee_testing",
#     fileFormat = "KML",
#     fileNamePrefix = "test_fc_KML"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_drive(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })
#
# # 10. KMZ_VECTOR - DRIVE
# test_that("KMZ_VECTOR_DRIVE",{
#   task_vector <- ee$batch$Export$table$toDrive(
#     collection = fc_test,
#     folder = "rgee_testing",
#     fileFormat = "KMZ",
#     fileNamePrefix = "test_fc_KMZ"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_drive(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })
#
# # 11. GEOJSON_VECTOR - DRIVE
# test_that("GEOJSON_VECTOR_DRIVE",{
#   task_vector <- ee$batch$Export$table$toDrive(
#     collection = fc_test,
#     folder = "rgee_testing",
#     fileFormat = "GEOJSON",
#     fileNamePrefix = "test_fc_GEOJSON"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_drive(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })
#
# # 12. CTFRECORD_VECTOR - DRIVE
# test_that("CTFRECORD_VECTOR_DRIVE",{
#   task_vector <- ee$batch$Export$table$toDrive(
#     collection = fc_test,
#     folder = "rgee_testing",
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "test_fc_CTFRECORD",
#     formatOptions = vectorExportFormatOptions_1
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_drive(task = task_vector)
#   expect_equal(vector, TRUE)
# })
# # 13. TFRECORD_VECTOR - DRIVE
# test_that("TFRECORD_VECTOR_DRIVE",{
#   task_vector <- ee$batch$Export$table$toDrive(
#     collection = fc_test,
#     folder = "rgee_testing",
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "test_fc_TFRECORD",
#     formatOptions = vectorExportFormatOptions_2
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_drive(task = task_vector)
#   expect_equal(vector, TRUE)
# })
#
# # 14. CSV_VECTOR - GCS
# test_that("CSV_VECTOR_GCS",{
#   task_vector <- ee$batch$Export$table$toCloudStorage(
#     collection = fc_test,
#     bucket = "bag_csaybar",
#     fileFormat = "CSV",
#     fileNamePrefix = "testing/test_fc_CSV"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_gcs(task = task_vector)
#   expect_equal(vector, TRUE)
# })
# # 15. SHP_VECTOR - GCS
# test_that("SHP_VECTOR_GCS",{
#   task_vector <- ee$batch$Export$table$toCloudStorage(
#     collection = fc_test,
#     bucket = "bag_csaybar",
#     fileFormat = "SHP",
#     fileNamePrefix = "testing/test_fc_SHP"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_gcs(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })
#
# # 16. KML_VECTOR - GCS
# test_that("KML_VECTOR_GCS",{
#   task_vector <- ee$batch$Export$table$toCloudStorage(
#     collection = fc_test,
#     bucket = "bag_csaybar",
#     fileFormat = "KML",
#     fileNamePrefix = "testing/test_fc_KML"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_gcs(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })
#
# # 17. KMZ_VECTOR - GCS
# test_that("KMZ_VECTOR_GCS",{
#   task_vector <- ee$batch$Export$table$toCloudStorage(
#     collection = fc_test,
#     bucket = "bag_csaybar",
#     fileFormat = "KMZ",
#     fileNamePrefix = "testing/test_fc_KMZ"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_gcs(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })
#
# # 18. GEOJSON_VECTOR - GCS
# test_that("GEOJSON_VECTOR_GCS",{
#   task_vector <- ee$batch$Export$table$toCloudStorage(
#     collection = fc_test,
#     bucket = "bag_csaybar",
#     fileFormat = "GEOJSON",
#     fileNamePrefix = "testing/test_fc_GEOJSON"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_gcs(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })
# # 19. CTFRECORD_VECTOR - GCS
# test_that("CTFRECORD_VECTOR_GCS",{
#   task_vector <- ee$batch$Export$table$toCloudStorage(
#     collection = fc_test,
#     bucket = "bag_csaybar",
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "testing/test_fc_CTFRECORD",
#     formatOptions = vectorExportFormatOptions_1
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_gcs(task = task_vector)
#   expect_equal(vector, TRUE)
# })
#
# # 20. TFRECORD_VECTOR - GCS
# test_that("TFRECORD_VECTOR_GCS",{
#   task_vector <- ee$batch$Export$table$toCloudStorage(
#     collection = fc_test,
#     bucket = "bag_csaybar",
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "testing/test_fc_TFRECORD",
#     formatOptions = vectorExportFormatOptions_2
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_download_gcs(task = task_vector)
#   expect_equal(vector, TRUE)
# })
#
