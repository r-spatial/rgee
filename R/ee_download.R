#' Move Earth Engine (EE) results from Google Drive to Hard disk
#'
#' Transfer results of an EE task saved in Google Drive to Hard disk
#'
#' @param task List generated after finished correctly a EE task. See
#' `ee_help(ee$batch$Export)` for details.
#' @param filename Output filename.
#' @param overwrite A boolean indicating whether "filename" should be overwritten.
#' @param quiet logical; suppress info message
#' @importFrom googledrive drive_find drive_download
#' @importFrom stars read_stars
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_Initialize()
#'
#' # Communal Reserve Amarakaeri - Peru
#' xmin <- -71.132591318
#' xmax <- -70.953664315
#' ymin <- -12.892451233
#' ymax <- -12.731116372
#' ROI <- c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin)
#' ROI_polygon <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326)
#' ee_geom <- sf_as_ee(ROI_polygon)
#'
#' # Get the mean annual NDVI for 2011
#' cloudMaskL457 <- function(image) {
#'   qa <- image$select("pixel_qa")
#'   cloud <- qa$bitwiseAnd(32L)$
#'     And(qa$bitwiseAnd(128L))$
#'     Or(qa$bitwiseAnd(8L))
#' mask2 <- image$mask()$reduce(ee$Reducer$min())
#' image <- image$updateMask(cloud$Not())$updateMask(mask2)
#' image$normalizedDifference(list("B4", "B3"))
#' }
#'
#' ic_l5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'   filterBounds(ee_geom)$
#'   filterDate("2011-01-01", "2011-12-31")$
#'   map(cloudMaskL457)
#' mean_l5 <- ic_l5$mean()$rename("NDVI")
#' mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#' mean_l5_Amarakaeri <- mean_l5$clip(ee_geom)
#'
#' # Download a EE Image
#' task_img <- ee$batch$Export$image$toDrive(
#'   image = mean_l5_Amarakaeri,
#'   folder = "Amarakaeri",
#'   fileFormat = "GEOTIFF",
#'   fileNamePrefix = "my_image"
#' )
#' task_img$start()
#' ee_monitoring(task_img)
#' img <- ee_download_drive(task_img)
#' plot(img)
#'
#' # Download a EE FeatureCollection
#' amk_fc <- ee$FeatureCollection(list(ee$Feature(ee_geom, list(name = "Amarakaeri"))))
#' task_vector <- ee$batch$Export$table$toDrive(
#'   collection = amk_fc,
#'   folder = "Amarakaeri",
#'   fileFormat = "GEOJSON",
#'   fileNamePrefix = "geom_Amarakaeri"
#' )
#' task_vector$start()
#' ee_monitoring(task_vector) # optional
#' amk_geom <- ee_download_drive(task = task_vector)
#' plot(amk_geom$geometry, border = "red", lwd = 10)
#' }
#' @export
ee_download_drive <- function(task, filename, overwrite = FALSE, quiet = TRUE) {
  if (!requireNamespace('googledrive', quietly = TRUE)) {
    stop('The googledrive package is required to use rgee::ee_download_drive',
         call. = FALSE)
  } else {
    googledrive_credentials_path <- sprintf("%s/googledrive", ee_get_earthengine_path())
    googledrive::drive_auth(token = readRDS(googledrive_credentials_path), email = TRUE) #deprecated
    gd_folder <- basename(task$status()$output_url)
    gd_filename <- task$config$driveFileNamePrefix

    # Selecting a file considering the name
    files_gd <- try(googledrive::drive_find(
      q = sprintf("'%s' in parents", gd_folder),
      q = sprintf("name contains '%s'", gd_filename)
    ))
    count <- 1
    while (any(class(files_gd) %in% "try-error") & count < 5) {
      files_gd <- try(googledrive::drive_find(
        q = sprintf("'%s' in parents", gd_folder),
        q = sprintf("name contains '%s'", gd_filename)
      ))
      count <- count + 1
    }

    # Choose the right file using the driver_resource["originalFilename"]
    orgl_filename <- sapply(files_gd$drive_resource, function(x) x$originalFilename)
    row_id <- grepl(task$id, orgl_filename) %>% which()
    to_download <- files_gd[row_id, ]

    fileformat <- toupper(task$config$fileFormat)
    if (missing(filename)) filename <- tempfile()

    fileformat <- get_fileformat(task)
    file_suffix <- get_format_suffix(fileformat)
    filenames_hd <- create_filenames(filename,file_suffix,fileformat)

    to_download <- sort_drive_files(to_download, fileformat)
    filenames_hd <- sort_harddisk_files(filenames_hd, fileformat)

    for (z in 1:nrow(to_download)) {
      googledrive::drive_download(file = to_download[z,],
                                  path = filenames_hd[z],
                                  overwrite = overwrite,
                                  verbose = !quiet)
    }
    read_filenames(filenames_hd, fileformat,quiet = quiet)
  }
}

#' Move EE results from Google Cloud Storage to Hard disk
#'
#' Transfer results of an Earth Engine (EE) task saved in Google Cloud Storage to Hard disk
#'
#' @param task List generated after finished correctly a EE task. See
#' `ee_help(ee$batch$Export)` for details.
#' @param filename Output filename.
#' @param overwrite A boolean indicating whether "filename" should be overwritten.
#' @param GCS_AUTH_FILE Authentication json file you have downloaded from your Google Cloud Project
#' @param quiet logical; suppress info message
#' @details
#' The best way to use `rgee::ee_download_gcs` is save the Google Cloud Project JSON file into
#' `ee_get_earthengine_path()` with the name GCS_AUTH_FILE.json. It is necessary in order to
#' attain that rgee can read the credentials automatically.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_Initialize()
#'
#' # Communal Reserve Amarakaeri - Peru
#' xmin <- -71.132591318
#' xmax <- -70.953664315
#' ymin <- -12.892451233
#' ymax <- -12.731116372
#' x_mean <- (xmin + xmax) / 2
#' y_mean <- (ymin + ymax) / 2
#'
#' ROI <- c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin)
#' ROI_polygon <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326)
#' ee_geom <- sf_as_ee(ROI_polygon)
#'
#' # Get the mean annual NDVI for 2011
#' cloudMaskL457 <- function(image) {
#'   qa <- image$select("pixel_qa")
#'   cloud <- qa$bitwiseAnd(32L)$
#'     And(qa$bitwiseAnd(128L))$
#'     Or(qa$bitwiseAnd(8L))
#'   mask2 <- image$mask()$reduce(ee$Reducer$min())
#'   image <- image$updateMask(cloud$Not())$updateMask(mask2)
#'   image$normalizedDifference(list("B4", "B3"))
#' }
#'
#' ic_l5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'   filterBounds(ee_geom)$
#'   filterDate("2011-01-01", "2011-12-31")$
#'   map(cloudMaskL457)
#' mean_l5 <- ic_l5$mean()$rename("NDVI")
#' mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#' mean_l5_Amarakaeri <- mean_l5$clip(ee_geom)
#'
#' # Download a EE Image
#' task_img <- ee$batch$Export$image$toCloudStorage(
#'   image = mean_l5_Amarakaeri,
#'   bucket = "bag_csaybar",
#'   fileFormat = "GEOTIFF",
#'   fileNamePrefix = "my_image"
#' )
#' task_img$start()
#' ee_monitoring(task_img)
#' img <- ee_download_gcs(task_img)
#' plot(img)
#'
#' # Download a EE FeatureCollection
#' amk_fc <- ee$FeatureCollection(list(ee$Feature(ee_geom, list(name = "Amarakaeri"))))
#' task_vector <- ee$batch$Export$table$toCloudStorage(
#'   collection = amk_fc,
#'   bucket = "bag_csaybar",
#'   fileFormat = "SHP",
#'   fileNamePrefix = "geom_Amarakaeri"
#' )
#' task_vector$start()
#' ee_monitoring(task_vector) # optional
#' amk_geom <- ee_download_gcs(task = task_vector)
#' plot(amk_geom$geometry, border = "red", lwd = 10)
#' }
#' @export
ee_download_gcs <- function(task, filename, overwrite = FALSE,
                            GCS_AUTH_FILE = getOption("rgee.gcs.auth"), quiet = TRUE) {
  if (!requireNamespace('googleCloudStorageR', quietly = TRUE)) {
    stop('The googleCloudStorageR package is required to use rgee::ee_download_gcs',
         call. = FALSE)
  } else {
    gcs_bucket <- task$config$outputBucket
    gd_filename <- task$config$outputPrefix
    gcs_auth_tk <- try(googleCloudStorageR::gcs_auth(GCS_AUTH_FILE)) #deprecated
    if (any(class(gcs_auth_tk) %in% "try-error")) {
      stop("Authentication is not possible, check the GCS_AUTH_FILE argument")
    }
    if (missing(filename)) filename <- tempfile()

    fileformat <- get_fileformat(task)
    file_suffix <- get_format_suffix(fileformat)
    filenames_gcs <- create_filenames(gd_filename,file_suffix,fileformat)
    filenames_hd <- create_filenames(filename,file_suffix,fileformat)

    filenames_gcs <- sort_harddisk_files(filenames_gcs, fileformat)
    filenames_hd <- sort_harddisk_files(filenames_hd, fileformat)

    for(z in seq_along(filenames_hd)) {
      googleCloudStorageR::gcs_get_object(object_name = filenames_gcs[z],
                                          bucket = gcs_bucket,
                                          saveToDisk = filenames_hd[z],
                                          overwrite = TRUE)
    }
    read_filenames(filenames_hd, fileformat, quiet = quiet)
  }
}


#' Monitoring Earth Engine task progress
#'
#' @param task List generated after finished correctly a EE task, if it is missing the last one will be taken.
#' @param eeTaskList Logical, if \code{TRUE}, all Earth Engine tasks will listing initially.
#'
#' @export
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_initialize()
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


#' get format file suffix - GCS
#' @noRd
get_format_suffix <-function(x) {
  shp_sx <- list("ee_export.dbf","ee_export.shx","ee_export.prj","ee_export.shp")
  #ctf_sx <-  list(".json", sprintf("-%05d.tfrecord.gz",0:(length(gd_folder)-2)))
  image_ctf_sx <-  list(".json", ".tfrecord.gz")
  #tf_sx <-  list(".json", sprintf("-%05d.tfrecord",0:(length(gd_folder)-2)))
  image_tf_sx <-  list(".json", ".tfrecord")
  suffix <- list(".tif","ee_export.csv","ee_export.geojson","ee_export.kml",
                 "ee_export.kmz",shp_sx, image_tf_sx, image_ctf_sx,
                 "ee_export.tfrecord.gz", "ee_export.tfrecord.gz")
  names(suffix) <- c("GEOTIFF" , "CSV", "GEOJSON", "KML",
                     "KMZ","SHP","TFRECORD_IMAGE","CTFRECORD_IMAGE",
                     "TFRECORD_VECTOR","CTFRECORD_VECTOR")
  return(as.character(unlist(suffix[x])))
}

#'  Get the file format
#'  Format available: "GEOTIFF", "CSV", "GEOJSON", "KML", "KMZ",
#'                    "SHP", "TFRECORD_IMAGE", "CTFRECORD_IMAGE",
#'                    "TFRECORD_VECTOR", "CTFRECORD_VECTOR"
#' @noRd
get_fileformat <- function(x) {
  if (x$config$fileFormat == "TFRECORD") {
    if (x$task_type == "EXPORT_FEATURES") {
      if (x$config$formatOptions$compressed == TRUE) {
        return("CTFRECORD_VECTOR")
      } else {
        return("TFRECORD_VECTOR")
      }
    } else {
      if (x$config$tfrecordCompressed == TRUE) {
        return("CTFRECORD_IMAGE")
      } else {
        return("TFRECORD_IMAGE")
      }
    }
  } else {
    return(x$config$fileForma)
  }
}

#' Create file (or files) to save - GCS
#' @noRd
create_filenames <- function(basename, suffix, fileformat) {
  if (fileformat %in% c("GEOTIFF", "TFRECORD_IMAGE", "CTFRECORD_IMAGE")) {
    filename <- sprintf("%s%s", basename, suffix)
  } else {
    filename <- sprintf("%s%s", basename, suffix)
  }
  return(filename)
}

#' Create the download file according to its format
#' @noRd
read_filenames <- function(filename, fileformat, quiet){
if (fileformat == "GEOTIFF") {
  fread <- read_stars(filename, quiet = quiet)
  return(fread)
} else if (fileformat %in% "SHP") {
  fread <- st_read(filename[grep("\\.shp$",filename)], quiet = quiet)
  return(fread)
} else if (fileformat %in% c("GEOJSON", "KML", "KMZ")) {
  fread <- st_read(filename, quiet = quiet)
  return(fread)
} else {
  if (!quiet) print(sprintf(
    "Download completed:%s (%s)",
    filename,
    fileformat
  ))
  return(TRUE)
}
}

#' Sort google drives files
#' @noRd
sort_drive_files <- function(drive_files,fileformat) {
  if (fileformat=='SHP') {
    shp_file <- grep("(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)",drive_files[['name']])
    selected_drive_files <- drive_files[shp_file,]
    drive_files_sort <- selected_drive_files[order(selected_drive_files$name),]
  } else {
    drive_files_sort <- drive_files[order(drive_files[['name']]),]
  }
  drive_files_sort
}

#' Sort hard disk files
#' @noRd
sort_harddisk_files <- function(harddisk_files,fileformat) {
  if (fileformat=='SHP') {
    shp_file <- grep("(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)",harddisk_files)
    shp_file <- harddisk_files[shp_file]
    harddisk_files_sort <- shp_file[order(shp_file)]
  } else {
    harddisk_files_sort <- harddisk_files[order(harddisk_files)]
  }
  return(harddisk_files_sort)
}
