#' Upload local files to Google Cloud Storage
#'
#' Upload images or tables to Google Cloud Storage
#'
#' @param x Character. filename.
#' @param bucket bucket name you are uploading to
#' @param predefinedAcl Specify user access to object. Passed to
#' \code{googleCloudStorageR::gcs_upload}.
#' @param quiet Logical. Suppress info message.
#' @return Character that represents the full path of the object in the GCS
#' bucket specified.
#' @family generic upload functions
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#'
#' # Initialize a specific Earth Engine account and
#' # Google Cloud Storage credentials
#' ee_Initialize(gcs = TRUE)
#'
#' # # Define an image.
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' local_to_gcs(x = tif, bucket = 'rgee_dev')
#' }
#' @export
local_to_gcs <- function(x,
                         bucket = NULL,
                         predefinedAcl = "bucketLevel",
                         quiet = FALSE) {
  # check packages
  ee_check_packages("rgee::ee_download_gcs", "googleCloudStorageR")

  if (is.null(bucket)) {
    stop("Cloud Storage bucket was not defined")
  }

  if (is.na(getOption("rgee.gcs.auth")) || is.null(getOption("rgee.gcs.auth"))) {
    stop(
      "Google Cloud Storage credentials were not loaded.",
      ' Run ee_Initialize(..., gcs = TRUE)',
      " to fix."
    )
  }
  count <- 1

  googleCloudStorageR::gcs_auth(getOption("rgee.gcs.auth"))
  if (isFALSE(quiet)) {
    files_gcs <- try(
      googleCloudStorageR::gcs_upload(
        file = x,
        bucket = bucket,
        name = basename(x),
        predefinedAcl = predefinedAcl),
        silent = TRUE
    )
    while (any(class(files_gcs) %in% "try-error") & count < 5) {
      files_gcs <- try(
        googleCloudStorageR::gcs_upload(
          file = x,
          bucket = bucket,
          name = basename(x),
          predefinedAcl = predefinedAcl),
          silent = TRUE
        )
      if (count == 4) {
        cat(files_gcs)
      }
      count <- count + 1
    }
  } else {
    files_gcs <- try(
      suppressMessages(
        googleCloudStorageR::gcs_upload(
          file = x,
          bucket = bucket,
          name = basename(x),
          predefinedAcl = predefinedAcl
        )
      ),
      silent = TRUE
    )
    while (any(class(files_gcs) %in% "try-error") & count < 5) {
      files_gcs <- try(
        suppressMessages(
          googleCloudStorageR::gcs_upload(
            file = x,
            bucket = bucket,
            name = basename(x),
            predefinedAcl = predefinedAcl
          )
        ),
        silent = TRUE
      )
      count <- count + 1
    }
  }
  sprintf("gs://%s/%s", bucket, basename(x))
}

#' Move a zipped shapefile from GCS to their EE Assets
#'
#' @param manifest Character. manifest upload file. See \code{\link{ee_utils_create_manifest_table}}.
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool (CLT). If NULL, rgee assumes that CLT is set in the system PATH.
#' (ignore if \code{via} is not defined as "gcs_to_asset").
#' @param overwrite Logical. If TRUE, the assetId will be overwritten if
#' it exists.
#' @param quiet Logical. Suppress info message.
#'
#' @return Character. The Earth Engine asset ID.
#'
#' @importFrom processx run
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' # 1. Read dataset and create a output filename
#' x <- st_read(system.file("shape/nc.shp", package = "sf"))
#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'toy_poly_gcs')
#'
#' # 2. From sf to .shp
#' shp_dir <- sprintf("%s.shp", tempfile())
#' geozip_dir <- ee_utils_shp_to_zip(x, shp_dir)
#'
#' # 3. From local to gcs
#' gcs_filename <- local_to_gcs(
#'  x = geozip_dir,
#'  bucket = "rgee_dev" # Insert your own bucket here!
#' )
#'
#' # 4. Create Table Manifest
#' manifest <- ee_utils_create_manifest_table(
#'  gs_uri = gcs_filename,
#'  assetId = assetId
#' )
#'
#' # 5. From GCS to Earth Engine
#' ee_nc <- gcs_to_ee_table(manifest, overwrite = TRUE)
#' ee_monitoring()
#' Map$addLayer(ee$FeatureCollection(ee_nc))
#' }
#' @export
gcs_to_ee_table <- function(manifest,
                            command_line_tool_path = NULL,
                            overwrite = FALSE,
                            quiet = FALSE) {
  # check packages
  ee_check_packages("gcs_to_ee_table", "jsonlite")

  manifest_list <- jsonlite::read_json(manifest)
  assetId <- ee_remove_project_chr(manifest_list$name)

  # If the command_line_tool_path does not exist
  if (is.null(command_line_tool_path)) {
    command_line_tool_path <- ""
  }

  # Command to run in console
  if (command_line_tool_path == "") {
    command <- "earthengine"
    command_msg <- sprintf("earthengine upload table --manifest %s", manifest)
  } else {
    command <- sprintf("%s/earthengine", command_line_tool_path)
    command_msg <- sprintf("%s/earthengine upload table --manifest %s",
                           command_line_tool_path, manifest)
  }

  if (!quiet) {
    message("Running the OS command: ", command_msg)
  }

  if (isTRUE(overwrite)) {
    try(
      expr = ee_manage_delete(assetId, quiet = TRUE),
      silent = TRUE
    )
  }

  upload_state <- run(
    command = command,
    args = c("upload", "table", "--manifest", manifest),
    error_on_status = FALSE
  )

  message(upload_state$stdout)
  message(upload_state$stderr)
  invisible(assetId)
}

#' Move a GeoTIFF image from GCS to their EE assets
#'
#' @param manifest Character. Manifest upload file. See \code{\link{ee_utils_create_manifest_image}}.
#' @param overwrite Logical. If TRUE, the assetId will be overwritten if
#' it exists.
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool (CLT). If NULL, rgee assumes that CLT is set in the system PATH.
#' (ignore if \code{via} is not defined as "gcs_to_asset").
#' @param quiet Logical. Suppress info message.
#'
#' @return Character. The Earth Engine asset ID.
#' @importFrom processx run
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_Initialize("csaybar", gcs = TRUE)
#'
#' # 1. Read GeoTIFF file and create a output filename
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # 2. From local to gcs
#' gs_uri <- local_to_gcs(
#'   x = tif,
#'   bucket = 'rgee_dev' # Insert your own bucket here!
#' )
#'
#' # 3. Create an Image Manifest
#' manifest <- ee_utils_create_manifest_image(gs_uri, assetId)
#'
#' # 4. From GCS to Earth Engine
#' gcs_to_ee_image(
#'   manifest = manifest,
#'   overwrite = TRUE
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_stars_01 <- ee$Image(assetId)
#' ee_stars_01$bandNames()$getInfo()
#'
#' Map$centerObject(ee_stars_01)
#' Map$addLayer(ee_stars_01, list(min = 0, max = 255, bands = c("b3", "b2", "b1")))
#' }
#' @export
gcs_to_ee_image <- function(manifest,
                            overwrite = FALSE,
                            command_line_tool_path = NULL,
                            quiet = FALSE) {
  # check packages
  ee_check_packages("gcs_to_ee_image", "jsonlite")

  manifest_list <- jsonlite::read_json(manifest)
  assetId <- ee_remove_project_chr(manifest_list$name)

  # If the command_line_tool_path does not exist
  if (is.null(command_line_tool_path)) {
    command_line_tool_path <- ""
  }

  #Command to run in console
  if (command_line_tool_path == "") {
    command <- "earthengine"
    command_msg <- sprintf("earthengine upload image --manifest %s", manifest)
  } else {
    command <- sprintf("%s/earthengine", command_line_tool_path)
    command_msg <- sprintf("%s/earthengine upload image --manifest %s",
                           command_line_tool_path, manifest)
  }

  if (!quiet) {
    message("Running the OS command: ", command_msg)
  }

  if (isTRUE(overwrite)) {
    try(
      expr = ee_manage_delete(assetId, quiet = TRUE),
      silent = TRUE
    )
  }

  upload_state <- run(
    command = command,
    args = c("upload", "image", "--manifest", manifest),
    error_on_status = FALSE
  )

  message(upload_state$stdout)
  message(upload_state$stderr)
  invisible(assetId)
}

#' From sf object to Earth Engine FeatureCollection
#' @noRd
ee_sf_to_fc <- function(x, proj, geodesic, evenOdd) {
  # check packages
  ee_check_packages("sf_as_ee", c("sf", "geojsonio"))

  # Load python module
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee_lib <- ee_source_python(oauth_func_path)

  # How much geometry does x have?
  ngeometries <- if (inherits(x, "sf")) nrow(x) else length(x)

  # does x only has one geometry and it is a sfc?,
  # then, return a ee$Geometry$...
  if (ngeometries == 1 & !inherits(x, "sf")) {
    sfc_feature <- sf::st_geometry(x)
    py_geometry <- geojsonio::geojson_json(sfc_feature, type = 'skip')
    sf_obj_class <- class(sfc_feature)
    wkt_type <- sf_obj_class[sf_obj_class %in% ee_sf_comp()]

    if (length(wkt_type) == 0) {
      stop(
        "sf_as_ee does not support objects of class ",
      )
    }
    ee_geometry <- sf_as_ee_lib$sfg_as_ee_py(x = py_geometry,
                                         sfc_class = wkt_type,
                                         opt_proj = proj,
                                         opt_geodesic = geodesic,
                                         opt_evenOdd = evenOdd)
    return(ee_geometry)
  } else {
    # if x is a sf object with more than one geometry.
    fc <- list()
    for (index in seq_len(ngeometries)) {
      if (inherits(x, "sfc")) {
        # if x is a sfc. (it's need it only in sf >= 1.0.0)
        feature <- x[index]
      } else {
        # if x is a sf. (it's need it only in sf >= 1.0.0)
        feature <- x[index,]
      }
      sfc_feature <- sf::st_geometry(feature)
      py_geometry <- geojsonio::geojson_json(sfc_feature, type = 'skip')
      sf_obj_class <- class(sfc_feature)
      wkt_type <- sf_obj_class[sf_obj_class %in% ee_sf_comp()]
      ee_geometry <- sf_as_ee_lib$sfg_as_ee_py(x = py_geometry,
                                           sfc_class = wkt_type,
                                           opt_proj = proj,
                                           opt_geodesic = geodesic,
                                           opt_evenOdd = evenOdd)
      if (isFALSE(ee_geometry)) {
        stop(
          "sf_as_ee only support the upload of geometries of type: ",
          paste0(ee_sf_comp(), collapse = ", "),
          ". If you are using a GEOMETRYCOLLECTION geometry, please simplify ",
          "it first or use the argument ",
          "via = \"gcs_to_asset\"."
        )
      }

      if (any(class(x) %in% "sf")) {
        sf::st_geometry(feature) <- NULL
        fc[[index]] <- ee$Feature(ee_geometry, as.list(feature))
      } else {
        fc[[index]] <- ee$Feature(ee_geometry, list())
      }
    }
    ee$FeatureCollection(fc)
  }
}

#' Pass a character or stars object to stars-proxy
#' @noRd
ee_as_proxystars <- function(x, temp_dir = tempdir()) {
  # check packages
  ee_check_packages("stars_as_ee", "stars")

  if (is.character(x)) {
    stars::read_stars(x, proxy = TRUE)
  } else if (is(x,"stars")) {
    time_format <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)
    tiff_filename <- sprintf("%s/%s.tif", temp_dir, ee_description)
    stars::write_stars(x, tiff_filename)
    stars::read_stars(tiff_filename, proxy = TRUE)
  } else if (is(x,"Raster")) {
    # check packages
    ee_check_packages("raster_as_ee", "raster")
    time_format <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)
    tiff_filename <- sprintf("%s/%s.tif", temp_dir, ee_description)
    raster::writeRaster(x, tiff_filename)
    stars::read_stars(tiff_filename, proxy = TRUE)
  } else {
    stop('x argument not defined properly.')
  }
}

#' sf classes that ee supports
#' @noRd
ee_sf_comp <- function(){
  c("sfc_MULTIPOLYGON", "sfc_POLYGON", "sfc_LINESTRING", "sfc_MULTILINESTRING",
    "sfc_POINT", "sfc_MULTIPOINT", "MULTIPOLYGON", "POLYGON", "LINESTRING",
    "MULTILINESTRING", "POINT", "MULTIPOINT")
}

#' Convert an R list into a JSON file in the temp() file
#' @param x List to convert into a JSON file.
#' @return A JSON file saved in a /tmp dir.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_utils_create_json(list(a=10,b=10))
#' }
#' @export
ee_utils_create_json <- function(x) {
  tmpf <- tempfile()
  json_path <- sprintf("%s/%s_manifest.json", dirname(tmpf), basename(tmpf))

  # Load utils python module
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils <- ee_source_python(oauth_func_path)

  ee_utils$ee_create_json_py(
    towrite = json_path,
    manifest = x
  )
  json_path
}


#' Create a manifest to upload an image
#'
#' Create a manifest to upload a GeoTIFF to Earth Engine asset folder. The
#' "manifest" is simply a JSON file that describe all the upload parameters. See
#' \url{https://developers.google.com/earth-engine/guides/image_manifest} to get more
#' details.
#'
#' @param gs_uri Character. GCS full path of the image to upload to Earth Engine assets,
#' e.g. gs://rgee_dev/l8.tif
#' @param assetId Character. How to call the file once uploaded
#' to the Earth Engine Asset. e.g. users/datacolecfbf/l8.
#' @param properties List. Set of parameters to be set up as properties
#' of the EE object.
#' @param start_time Character. Sets the start time property (system:time_start).
#' It could be a number (timestamp) or a date.
#' @param end_time Character. Sets the end time property (system:time_end).
#' It could be a number (timestamp) or a date.
#' @param pyramiding_policy Character. The pyramid reduction policy to use.
#' @param returnList Logical. If TRUE will return the "manifest" as a list. Otherwise,
#' will return a JSON file.
#' @param quiet Logical. Suppress info message.
#'
#' @return If \code{returnList} is TRUE, a list otherwise a JSON file.
#' @family generic upload functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#'
#' # Return a JSON file
#' ee_utils_create_manifest_image(
#'   gs_uri = "gs://rgee_dev/l8.tif",
#'   assetId = "users/datacolecfbf/l8"
#' )
#'
#' # Return a list
#' ee_utils_create_manifest_image(
#'   gs_uri = "gs://rgee_dev/l8.tif",
#'   assetId = "users/datacolecfbf/l8",
#'   returnList = TRUE
#' )
#' }
#' @export
ee_utils_create_manifest_image <- function(gs_uri,
                                           assetId,
                                           properties = NULL,
                                           start_time = "1970-01-01",
                                           end_time = "1970-01-01",
                                           pyramiding_policy = 'MEAN',
                                           returnList = FALSE,
                                           quiet = FALSE) {
  # Verify is the EE assets path is valid, if not try to fix
  assetId <- ee_verify_filename(
    path_asset = assetId,
    strict = FALSE
  )

  # Create a name
  # name <- sprintf("projects/earthengine-legacy/assets/%s", assetId)
  name <- assetId

  # from R date to JS timestamp: time_start + time_end
  time_start <- rdate_to_eedate(start_time, timestamp = TRUE)
  time_end <- rdate_to_eedate(end_time, timestamp = TRUE)

  # Creating tileset
  tilesets <- list(
    sources = list(
      list(
        uris = gs_uri
      )
    )
  )

  # Putting all together
  manifest <- list(
    name = name,
    tilesets = list(tilesets),
    pyramiding_policy = pyramiding_policy,
    properties = properties,
    start_time = list(seconds = time_start / 1000),
    end_time = list(seconds = time_end / 1000)
  )

  if (is.null(properties)) manifest[["properties"]] <- NULL
  if (returnList) {
    manifest
  } else {
    ee_utils_create_json(manifest)
  }
}

#' Create a manifest to upload a table
#'
#' Create a manifest to upload a zipped shapefile to Earth Engine assets folder. The
#' "manifest" is simply a JSON file that describe all the upload parameters. See
#' \url{https://developers.google.com/earth-engine/guides/image_manifest} to get more
#' details.
#'
#' @param gs_uri Character. GCS full path of the table to upload to Earth Engine assets
#' e.g. gs://rgee_dev/nc.zip
#' @param assetId Character. How to call the file once uploaded
#' to the Earth Engine Asset. e.g. users/datacolecfbf/nc.
#' @param properties List. Set of parameters to be set up as properties
#' of the EE object.
#' @param start_time Character. Sets the start time property (system:time_start).
#' It could be a number (timestamp) or a date.
#' @param end_time Character. Sets the end time property (system:time_end).
#' It could be a number (timestamp) or a date.
#' @param returnList Logical. If TRUE will return the "manifest" as a list otherwise
#' will return a JSON file.
#' @param quiet Logical. Suppress info message.
#'
#' @return If \code{returnList} is TRUE, a list otherwise a JSON file.
#' @family generic upload functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' x <- st_read(system.file("shape/nc.shp", package = "sf"))
#' shp_dir <- sprintf("%s.shp", tempfile())
#' geozip_dir <- ee_utils_shp_to_zip(x, shp_dir)
#'
#' # Return a JSON file
#' manifest <- ee_utils_create_manifest_table(
#'   gs_uri = "gs://rgee_dev/nc.zip",
#'   assetId = "users/datacolecfbf/nc"
#' )
#'
#' # Return a list
#' ee_utils_create_manifest_table(
#'   gs_uri = "gs://rgee_dev/nc.zip",
#'   assetId = "users/datacolecfbf/nc",
#'   returnList = TRUE
#' )
#' }
#' @export
ee_utils_create_manifest_table <- function(gs_uri,
                                           assetId,
                                           start_time = "1970-01-01",
                                           end_time = "1970-01-01",
                                           properties = NULL,
                                           returnList = FALSE,
                                           quiet = FALSE) {
  # Verify is the EE assets path is valid, if not try to fix
  assetId <- ee_verify_filename(
    path_asset = assetId,
    strict = FALSE
  )

  # Create a name
  # name <- sprintf("projects/earthengine-legacy/assets/%s", assetId)
  name <- assetId

  # Creating tileset
  sources <- list(
    uris = list(gs_uri)
  )

  # from R date to JS timestamp: time_start + time_end
  time_start <- rdate_to_eedate(start_time, timestamp = TRUE)
  time_end <- rdate_to_eedate(end_time, timestamp = TRUE)

  # Putting all together
  manifest <- list(
    name = name,
    sources = list(sources),
    properties = properties,
    start_time = list(seconds = time_start / 1000),
    end_time = list(seconds = time_end / 1000)
  )
  if (is.null(properties)) manifest[["properties"]] <- NULL
  if (returnList) {
    manifest
  } else {
    ee_utils_create_json(manifest)
  }
}


#' Convert EPSG, ESRI or SR-ORG code into a OGC WKT
#'
#' @param code The projection code.
#' @return A character which represents the same projection in WKT2 string.
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_utils_get_crs("SR-ORG:6864")
#' ee_utils_get_crs("EPSG:4326")
#' ee_utils_get_crs("ESRI:37002")
#' }
#' @export
ee_utils_get_crs <- function(code) {
  codetype <- tolower(strsplit(code,":")[[1]][1])
  if (codetype == "sr-org") {
    ee_proj_ogcwkt <- ee_utils_get_crs_web(code = code)
  } else {
    ee_proj_ogcwkt <- sf::st_crs(code)$Wkt
  }
  ee_utils_py_to_r(ee_proj_ogcwkt)
}

#' Convert EPSG, ESRI or SR-ORG code into a OGC WKT
#'
#' @param code The projection code.
#' @noRd
ee_utils_get_crs_web <- function(code) {
  codetype <- tolower(strsplit(code, ":")[[1]][1])
  ee_code <- strsplit(code, ":")[[1]][2]

  if (codetype == 'epsg') {
    format <- "wkt"
    link <- sprintf('https://epsg.io/%s.%s', ee_code, format)
    crs_wkt <- suppressWarnings(readLines(link))
  } else {
    format <- "ogcwkt"
    link <- sprintf("https://spatialreference.org/ref/%s/%s/%s/", codetype, ee_code, format)
    crs_wkt <- tryCatch(
      expr = suppressWarnings(readLines(link)),
      error = function(e) {
        message(sprintf("%s is down using %s ...", bold("spatialreference.org"), bold("GitHub backup")))
        sr_org_data <- jsonlite::fromJSON("https://raw.githubusercontent.com/OSGeo/spatialreference.org/master/scripts/sr-org.json")
        match_index <- which(sr_org_data$code == ee_code)
        
        if (length(match_index) == 0) {
          # Instead of stopping, return a warning and a default value or NULL
          warning(paste("SR-ORG code", ee_code, "not found in the dataset. Returning NULL."))
          return(NULL)
        }
        sr_org_data$ogcwkt[match_index]
      }
    )
  }
  return(ee_utils_py_to_r(crs_wkt))
}
