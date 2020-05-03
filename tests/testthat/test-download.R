context("rgee: ee_download test")
library(rgee)
library(sf)

ee_Initialize(
  email = "data.colec.fbf@gmail.com",
  drive = TRUE,
  gcs = TRUE
)

drive_folder <- 'rgee_backup'
gcs_bucket <- 'rgee_dev'

# Communal Reserve Amarakaeri - Peru
xmin <- -71.13
xmax <- -70.95
ymin <- -12.89
ymax <- -12.73
x_mean <- (xmin + xmax) / 2
y_mean <- (ymin + ymax) / 2

ROI <- c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin)
ee_geom <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
  list() %>%
  sf::st_polygon() %>%
  sf::st_sfc() %>%
  sf::st_set_crs(4326) %>%
  sf_as_ee(check_ring_dir = TRUE) %>%
  ee$FeatureCollection$geometry()

# Elevation map
ic_srtm <- ee$Image("CGIAR/SRTM90_V4") %>%
  ee$Image$reproject(crs = "EPSG:4326", scale = 1000)
mean_srtm_Amarakaeri <- ic_srtm$clip(ee_geom)

# Testing parameters ------------------------------------------------------
fc_test <- ee_geom %>%
  ee$Feature(list("test" = "feature")) %>%
  ee$FeatureCollection()


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

ee_clean_container(name = drive_folder, type = 'drive')
ee_clean_container(name = gcs_bucket, type = 'gcs')

# ### IMAGES
# # 1. GEOTIFF - DRIVE
test_that("GEOTIFF_DRIVE", {
  task_img <- ee_image_to_drive(
    image = image_test,
    folder = drive_folder,
    fileFormat = "GEOTIFF"
  )
  task_img <- ee_image_to_drive(
    image = image_test,
    folder = drive_folder,
    fileFormat = "GEOTIFF",
    fileNamePrefix = "test_image_GEOTIFF"
  )
  task_img$start()
  ee_monitoring(task_img)
  img <- ee_drive_to_local(
    task = task_img,
    consider = 'last',
    dsn = tempfile()
  )
  expect_is(img, "character")
})

# # 2. CTFRECORD_IMAGE - DRIVE
# test_that("CTFRECORD_DRIVE", {
#   task_img <- ee_image_to_drive(
#     image = image_test,
#     folder = drive_folder,
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "test_image_CTFRECORD",
#     formatOptions = imageExportFormatOptions_1
#   )
#   task_img$start()
#   ee_monitoring(task_img)
#   img <- ee_drive_to_local(
#     task = task_img,
#     consider = 'all'
#   )
#   expect_type(img, 'character')
# })

# # 3. TFRECORD_IMAGE - DRIVE
# test_that("TFRECORD_DRIVE", {
#   task_img <- ee_image_to_drive(
#     image = image_test,
#     folder = drive_folder,
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "test_image_TFRECORD",
#     formatOptions = imageExportFormatOptions_2
#   )
#   task_img$start()
#   ee_monitoring(task_img)
#   img <- ee_drive_to_local(
#     task = task_img,
#     consider = 'all'
#   )
#   expect_type(img, 'character')
# })

# # # 4. GEOTIFF - GCS
test_that("GEOTIFF_GCS", {
  task_img <- ee_image_to_gcs(
    image = image_test,
    bucket = gcs_bucket,
    fileFormat = "GEOTIFF"
  )
  task_img <- ee_image_to_gcs(
    image = image_test,
    bucket = gcs_bucket,
    fileFormat = "GEOTIFF",
    fileNamePrefix = "testing/test_image_GEOTIFF"
  )
  task_img$start()
  ee_monitoring(task_img)
  img <- ee_gcs_to_local(task = task_img, dsn = tempfile())
  img <- ee_gcs_to_local(task = task_img, dsn = tempfile(), quiet = TRUE)
  expect_is(img, "character")
})

# # 5. CTFRECORD_IMAGE - GCS
# test_that("CTFRECORD_GCS",{
#   task_img <- ee_image_to_gcs(
#     image = image_test,
#     bucket = gcs_bucket,
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "testing/test_image_CTFRECORD",
#     formatOptions = imageExportFormatOptions_1
#   )
#   task_img$start()
#   ee_monitoring(task_img)
#   img <- ee_gcs_to_local(task = task_img)
#   expect_type(img, 'character')
# })


#
# # 6. TFRECORD_IMAGE - GCS
# test_that("TFRECORD_GCS",{
#   task_img <- ee_image_to_gcs(
#     image = image_test,
#     bucket = gcs_bucket,
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "testing/test_image_TFRECORD",
#     formatOptions = imageExportFormatOptions_2
#   )
#   task_img$start()
#   ee_monitoring(task_img)
#   img <- ee_gcs_to_local(task = task_img)
#   expect_type(img, 'character')
# })


# ### VECTOR
# # 7. CSV_VECTOR - DRIVE
# test_that("CSV_VECTOR_DRIVE",{
#   task_vector <- ee_table_to_drive(
#     collection = fc_test,
#     folder = drive_folder,
#     fileFormat = "CSV",
#     fileNamePrefix = "test_fc_CSV"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_drive_to_local(
#     task = task_vector,
#     consider = 'last'
#   )
#   expect_type(vector, 'character')
# })

# # 8. SHP_VECTOR - DRIVE
test_that("SHP_VECTOR_DRIVE",{
  task_vector <- ee_table_to_drive(
    collection = fc_test,
    folder = drive_folder,
    fileFormat = "SHP"
  )

  task_vector <- ee_table_to_drive(
    collection = fc_test,
    folder = drive_folder,
    fileFormat = "SHP",
    fileNamePrefix = "test_fc_SHP"
  )
  task_vector$start()
  ee_monitoring(task_vector)
  vector <- ee_drive_to_local(
    task = task_vector,
    consider = 'all'
  )
  expect_is(vector, "character")
})

# # 9. KML_VECTOR - DRIVE
test_that("KML_VECTOR_DRIVE",{
  task_vector <- ee_table_to_drive(
    collection = fc_test,
    folder = drive_folder,
    fileFormat = "KML",
    fileNamePrefix = "test_fc_KML"
  )
  task_vector$start()
  ee_monitoring(task_vector)
  vector <- ee_drive_to_local(
    task = task_vector,
    consider = 'all'
  )
  expect_is(vector, "character")
})

# # 10. KMZ_VECTOR - DRIVE
test_that("KMZ_VECTOR_DRIVE",{
  task_vector <- ee_table_to_drive(
    collection = fc_test,
    folder = drive_folder,
    fileFormat = "KMZ",
    fileNamePrefix = "test_fc_KMZ"
  )
  task_vector$start()
  ee_monitoring(task_vector)
  vector <- ee_drive_to_local(
    task = task_vector,
    consider = 'all'
  )
  expect_is(vector, "character")
})

# # 11. GEOJSON_VECTOR - DRIVE
test_that("GEOJSON_VECTOR_DRIVE",{
  task_vector <- ee$batch$Export$table$toDrive(
    collection = fc_test,
    folder = "rgee_testing",
    fileFormat = "GEOJSON",
    fileNamePrefix = "test_fc_GEOJSON"
  )
  task_vector$start()
  ee_monitoring(task_vector)
  vector <- ee_drive_to_local(
    task = task_vector,
    consider = 'last'
  )
  expect_is(vector, "character")
})

# # 12. CTFRECORD_VECTOR - DRIVE
# test_that("CTFRECORD_VECTOR_DRIVE",{
#   task_vector <- ee_table_to_drive(
#     collection = fc_test,
#     folder = drive_folder,
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "test_fc_CTFRECORD"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_drive_to_local(
#     task = task_vector,
#     consider = 'all'
#   )
#   expect_type(vector, "character")
# })


# # 14. CSV_VECTOR - GCS
# test_that("CSV_VECTOR_GCS",{
#   task_vector <- ee_table_to_gcs(
#     collection = fc_test,
#     bucket = gcs_bucket,
#     fileFormat = "CSV",
#     fileNamePrefix = "testing/test_fc_CSV"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_gcs_to_local(task = task_vector)
#   expect_type(vector, "character")
# })


# # 15. SHP_VECTOR - GCS
# test_that("SHP_VECTOR_GCS",{
#   task_vector <- ee_table_to_gcs(
#     collection = fc_test,
#     bucket = gcs_bucket,
#     fileFormat = "SHP",
#     fileNamePrefix = "testing/test_fc_SHP"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_gcs_to_local(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })

# # 16. KML_VECTOR - GCS
# test_that("KML_VECTOR_GCS",{
#   task_vector <- ee_table_to_gcs(
#     collection = fc_test,
#     bucket = gcs_bucket,
#     fileFormat = "KML",
#     fileNamePrefix = "testing/test_fc_KML"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_gcs_to_local(task = task_vector)
#   expect_equal(as.character(vector[['test']]), "feature")
# })

# # 17. KMZ_VECTOR - GCS
test_that("KMZ_VECTOR_GCS",{
  task_vector <- ee_table_to_gcs(
    collection = fc_test,
    bucket = gcs_bucket,
    fileFormat = "KMZ"
  )
  task_vector <- ee_table_to_gcs(
    collection = fc_test,
    bucket = gcs_bucket,
    fileFormat = "KMZ",
    fileNamePrefix = "testing/test_fc_KMZ"
  )
  task_vector$start()
  ee_monitoring(task_vector)
  vector <- ee_gcs_to_local(task = task_vector)
  expect_is(vector, "character")
})

# # 18. GEOJSON_VECTOR - GCS
test_that("GEOJSON_VECTOR_GCS",{
  task_vector <- ee_table_to_gcs(
    collection = fc_test,
    bucket = gcs_bucket,
    fileFormat = "GEOJSON",
    fileNamePrefix = "testing/test_fc_GEOJSON"
  )
  task_vector$start()
  ee_monitoring(task_vector)
  vector <- ee_gcs_to_local(task = task_vector)
  expect_is(vector, "character")
})
# # 19. CTFRECORD_VECTOR - GCS
# test_that("CTFRECORD_VECTOR_GCS",{
#   task_vector <- ee_table_to_gcs(
#     collection = fc_test,
#     bucket = gcs_bucket,
#     fileFormat = "TFRECORD",
#     fileNamePrefix = "testing/test_fc_CTFRECORD"
#   )
#   task_vector$start()
#   ee_monitoring(task_vector)
#   vector <- ee_gcs_to_local(task = task_vector)
#   expect_type(vector, 'character')
# })

# ASSET
test_that("table to asset",{
  assetid <- paste0(ee_get_assethome(), '/l5_Amarakaeri')
  try(ee_manage_delete(assetid), silent = TRUE)
  task_vector <- ee_table_to_asset(
    collection = fc_test,
    assetId = assetid
  )
  task_vector$start()
  ee_monitoring(task_vector)
  mess <- ee_manage_delete(assetid)
  expect_equal(mess, TRUE)
})

test_that("image to asset",{
  assetid <- paste0(ee_get_assethome(), '/image_test')
  task_img <- ee_image_to_asset(
    image = image_test,
    assetId = assetid
  )
  task_img$start()
  ee_monitoring(task_img)
  mess <- ee_manage_delete(assetid)
  expect_equal(mess, TRUE)
})
