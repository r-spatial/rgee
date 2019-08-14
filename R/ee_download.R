#' Move the results of an Earth Engine task to export from Google Drive to Hard disk
#'
#' Download an EE export task saved in Google Drive.
#'
#' @param task List generated after finished correctly a EE task. See
#' `ee_Export()` for details.
#' @param filename Output filename.
#' @param overwrite A boolean indicating whether "filename" should be overwritten.
#' @importFrom googledrive drive_find drive_download
#' @importFrom stars read_stars
#' @importFrom readr read_csv
#' @export
#' @examples
#' \dontrun{
#'  library(rgee)
#'  library(stars)
#'  library(sf)
#'  library(mapview)
#'
#'  ee_initialize()
#'
#'  # Communal Reserve Amarakaeri - Peru
#'  xmin <- -71.132591318
#'  xmax <- -70.953664315
#'  ymin <- -12.892451233
#'  ymax <- -12.731116372
#'  x_mean <- (xmin + xmax) / 2
#'  y_mean <- (ymin + ymax) / 2
#'
#'  ROI <- c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin)
#'  ROI_polygon <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'    list() %>%
#'    st_polygon() %>%
#'    st_sfc() %>%
#'    st_set_crs(4326)
#'  ee_geom <- ee_as_eegeom(ROI_polygon)
#'
#'  # Get the mean annual NDVI for 2011
#'  cloudMaskL457 <- function(image) {
#'    qa <- image$select("pixel_qa")
#'    cloud <- qa$bitwiseAnd(32L)$
#'      And(qa$bitwiseAnd(128L))$
#'      Or(qa$bitwiseAnd(8L))
#'    mask2 <- image$mask()$reduce(ee_Reducer()$min())
#'    image <- image$updateMask(cloud$Not())$updateMask(mask2)
#'    image$normalizedDifference(list("B4", "B3"))
#'  }
#'
#'  ic_l5 <- ee_ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'    filterBounds(ee_geom)$
#'    filterDate("2011-01-01", "2011-12-31")$
#'    map(cloudMaskL457)
#'  mean_l5 <- ic_l5$mean()$rename("NDVI")
#'  mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#'  mean_l5_Amarakaeri <- mean_l5$clip(ee_geom)
#'
#'  # Download -> ee$Image
#'  task_img <- ee$batch$Export$image$toDrive(
#'    image = mean_l5_Amarakaeri,
#'    description = "Amarakaeri_task_1",
#'    folder = "Amarakaeri",
#'    fileFormat = "GEOTIFF",
#'    fileNamePrefix = "NDVI_l5_2011_Amarakaeri"
#'  )
#'  task_img$start()
#'  ee_download_monitoring(task_img, eeTaskList = TRUE) # optional
#'  img <- ee_download_drive(
#'    task = task_img,
#'    filename = "amarakaeri.tif",
#'    overwrite = TRUE
#'  )
#'  plot(img)
#'
#'  # Download -> ee$FeatureCollection
#'  amk_f <- ee$FeatureCollection(list(ee$Feature(ee_geom, list(name = "Amarakaeri"))))
#'  amk_fc <- ee$FeatureCollection(amk_f)
#'  task_vector <- ee$batch$Export$table$toDrive(
#'    collection = amk_fc,
#'    description = "Amarakaeri_task_2",
#'    folder = "Amarakaeri",
#'    fileFormat = "GEOJSON",
#'    fileNamePrefix = "geometry_Amarakaeri"
#'  )
#'
#'  task_vector$start()
#'  ee_download_monitoring(task_vector, eeTaskList = TRUE) # optional
#'  amk_geom <- ee_download_drive(
#'    task = task_vector,
#'    filename = "amarakaeri.geojson",
#'    overwrite = TRUE
#'  )
#'
#'  plot(amk_geom$geometry, add = TRUE, border = "red", lwd = 3)
#' }
ee_download_drive <- function(task,filename=NULL,overwrite=FALSE) {
  gd_filename <- task$config$driveFileNamePrefix
  query_gd <- sprintf("name contains '%s'",gd_filename)
  files_gd <- try(drive_find(q = query_gd))

  count <- 1
  while (any(class(files_gd) %in% "try-error") & count < 5) {
    files_gd <- try(drive_find(q = query_gd))
    count <- count + 1
  }

  orgl_filename <- files_gd$drive_resource[[1]]$originalFilename
  row_id <- mapply(grepl,task$id,orgl_filename) %>% which()
  to_download <- files_gd[row_id,]

  if(is.null(filename)) filename <- tempfile()
  download_mtd <- drive_download(to_download, filename, overwrite = overwrite)

  fileformat <- toupper(task$config$fileFormat)
  if (fileformat == 'GEOTIFF') {
    fread <- read_stars(download_mtd$local_path)
  } else if(fileformat == 'CSV') {
    fread <- read_csv(download_mtd$local_path)
  } else if(fileformat  %in%  c("GEOJSON","KML","KMZ","SHP")) {
    fread <- st_read(download_mtd$local_path)
  } else{
    print(sprintf("Download completed:%s (%s)",
                  download_mtd$local_path,
                  fileformat))
  }
}

#' Move the results of an Earth Engine task to export from Google Cloud Storage to Hard disk
#'
#' Download an EE export task saved in Google Cloud Storage.
#' @export
ee_download_gcs <- function(){}

#' Monitoring Earth Engine task progress
#'
#' @param task List generated after finished correctly a EE task. See
#' `ee_Export()` for details.
#' @param eeTaskList Logical, if \code{TRUE}, all Earth Engine tasks will listing initially.
#'
#' @export
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_initialize()
#'
#' ee_download_monitoring(eelist=TRUE)
#' }
#' @export
ee_monitoring <- function(task, eeTaskList = FALSE) {
  if (missing(task)) {
    task <-ee$batch$Task$list()[[1]]
  }
  if (eeTaskList) {
    cat("EETaskList:\n")
    task_list <- mapply( function(x) {
        sprintf("<Task %s: %s (%s)>", x$task_type, x$config, x$state)
      }, ee$batch$Task$list())
    cat("", paste0(task_list, "\n"))
  }
  cat("\n")
  while (task$active()){
    print(sprintf('Polling for task (id: %s).',task$id))
    Sys.sleep(5)
  }
  print(sprintf('State: %s',task$status()$state))
  if (!is.null(task$status()$error_message)) {
    print(task$status()$error_message)
  }
}
