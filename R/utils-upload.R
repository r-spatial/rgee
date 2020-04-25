#' Upload local files to google cloud storage
#'
#' Upload images or tables into Google Cloud Storage
#' for EE asset ingestion tasks.
#'
#' @param x Character. filename.
#' @param bucket bucket name you are uploading to
#' @param quiet Logical. Suppress info message.
#' @return Character which represents the full name of the
#' object in the GCS bucket specified.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_reattach()
#'
#' # Initialize a specific Earth Engine account and
#' # Google Cloud Storage credentials
#' ee_Initialize(
#'   email = "data.colec.fbf@gmail.com",
#'   gcs = TRUE
#' )
#'
#' # Define an image.
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' ee_local_to_gcs(x = tif, bucket = 'rgee_dev')
#' }
#' @export
ee_local_to_gcs <- function(x,
                            bucket = NULL,
                            quiet = FALSE) {
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
    stop(
      "The googleCloudStorageR package is required to use ",
      "rgee::ee_download_gcs",
      call. = FALSE
    )
  } else {
    if (is.null(bucket)) {
      stop("Cloud Storage bucket was not defined")
    }
    ee_user <- ee_exist_credentials()
    if (is.na(ee_user$gcs_cre)) {
      stop(
        "Google Cloud Storage credentials were not loaded.",
        ' Run ee_Initialize(email = "myemail", gcs = TRUE)',
        " to fix it"
      )
    }
    count <- 1
    googleCloudStorageR::gcs_auth(getOption("rgee.gcs.auth"))
    if (isFALSE(quiet)) {
      files_gcs <- try(
        googleCloudStorageR::gcs_upload(file = x,
                                        bucket = bucket,
                                        name = basename(x)),
        silent = TRUE
      )
      while (any(class(files_gcs) %in% "try-error") & count < 5) {
        files_gcs <- try(
          googleCloudStorageR::gcs_upload(file = x,
                                          bucket = bucket,
                                          name = basename(x)),
          silent = TRUE
        )
        count <- count + 1
      }
    } else {
      files_gcs <- try(suppressMessages(
        googleCloudStorageR::gcs_upload(file = x,
                                        bucket = bucket,
                                        name = basename(x))
      ), silent = TRUE)
      while (any(class(files_gcs) %in% "try-error") & count < 5) {
        files_gcs <- try(suppressMessages(
          googleCloudStorageR::gcs_upload(file = x,
                                          bucket = bucket,
                                          name = basename(x))
        ), silent = TRUE)
        count <- count + 1
      }
    }
    sprintf("gs://%s/%s", bucket, basename(x))
  }
}

#' Move a zipped shapefile from GCS to EE asset
#'
#' Pass a zipped shapefile of gcs to Earth Engine Asset
#'
#' @param gs_uri Character. It represents the full name of an
#' zipped shapefile in a GCS bucket.
#' @param assetId Character. What to call the file once uploaded
#' to the Earth Engine Asset
#' @param overwrite Logical. If TRUE, the assetId will be overwritten if
#' it exists.
#' @param quiet Logical. Suppress info message.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' # Create sf object
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'sf_nc')
#'
#' # Method 1
#' # 1. Pass the sf to a zip file
#' zipfile <- ee_create_shp_zip(nc)
#'
#' # 2. From local to gcs
#' gs_uri <- ee_local_to_gcs(x = zipfile, bucket = 'rgee_dev')
#'
#' # 3. Pass the sf to a zip file
#' ee_gcs_to_table(
#'   gs_uri = gs_uri,
#'   assetId = assetId
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_sf_01 <- ee$FeatureCollection(assetId)
#' Map$centerObject(ee_sf_01)
#' Map$addLayer(ee_sf_01)
#'
#' # Method 2
#' ee_sf_02 <- sf_as_ee(x = nc,
#'                      assetId = assetId,
#'                      bucket = "rgee_dev",
#'                      via = 'gcs')
#' Map$centerObject(ee_sf_02)
#' Map$addLayer(ee_sf_02)
#' }
#' @export
ee_gcs_to_table <- function(gs_uri,
                            assetId,
                            overwrite = FALSE,
                            quiet = FALSE) {
  if (isFALSE(quiet)) {
    cat(
      blue('Uploading'),
      green(gs_uri),
      blue('to'),
      green(assetId),
      blue('... please wait\n')
    )
  }

  if (isTRUE(overwrite)) {
    try(
      expr = ee_manage_delete(assetId, quiet = TRUE),
      silent = TRUE
    )
  }

  system(
    command = sprintf(
      "earthengine upload table --assetId %s '%s'",
      assetId, gs_uri
      ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
}

#' Move a GeoTIFF image from GCS to EE asset
#'
#' Pass a GeoTIFF image of gcs to Earth Engine Asset
#'
#' @param x stars object.
#' @param gs_uri Character. It represents the full name of the
#' GeoTIFF file in a GCS bucket.
#' @param assetId Character. What to call the file once uploaded
#' to the Earth Engine Asset. e.g. users/datacolecfbf/mydatacollection.
#' @param overwrite Logical. If TRUE, the assetId will be overwritten if
#' it exists.
#' @param properties List. Set of parameters to be set up as properties
#' of the EE object.
#' @param start_time Character. Sets the start time property to a number
#' or date.
#' @param end_time Character. Sets the end time property to a number
#' or date.
#' @param pyramiding_policy The pyramid reduction policy to use.
#' @param quiet Logical. Suppress info message.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_Initialize(gcs = TRUE)
#'
#' # Get the filename of a image
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # Method 1
#' # 1. Move from local to gcs
#' gs_uri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev')
#'
#' # 2. Pass from gcs to asset
#' ee_gcs_to_image(
#'   x = x,
#'   gs_uri = gs_uri,
#'   assetId = assetId
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_stars_01 <- ee$Image(assetId)
#' Map$centerObject(ee_stars_01)
#' Map$addLayer(ee_stars_01)
#'
#' # Method 2
#' ee_sf_02 <- stars_as_ee(x = x,
#'                         assetId = assetId,
#'                         bucket = "rgee_dev")
#' Map$centerObject(ee_sf_02)
#' Map$addLayer(ee_sf_02)
#' }
#' @export
ee_gcs_to_image <- function(x,
                            gs_uri,
                            assetId,
                            overwrite = FALSE,
                            properties = NULL,
                            start_time = "1970-01-01",
                            end_time = "1970-01-01",
                            pyramiding_policy = 'MEAN',
                            quiet = FALSE) {
  tempdir_gee <- tempdir()

  # Load python module
  oauth_func_path <- system.file(
    "python/ee_utils.py",
    package = "rgee"
  )
  ee_utils <- ee_source_python(oauth_func_path)

  # Creating affine_transform params
  affine_transform <- attr(x, "dimensions")
  shear <- x %>%
    attr("dimensions") %>%
    attr("raster")
  nbands <- (affine_transform$band$to - affine_transform$band$from) + 1L
  if (length(nbands) == 0) nbands <- 1
  band_names <- affine_transform$band$values
  if (is.null(band_names)) band_names <- sprintf("b%s", 1:nbands)
  name <- sprintf("projects/earthengine-legacy/assets/%s", assetId)

  if (is.na(st_crs(x)$wkt)) {
    stop("x does not have a CRS defined first")
  }

  # Creating tileset
  tilesets <- list(
    crs = st_crs(x)$wkt,
    sources = list(
      list(
        uris = gs_uri,
        affine_transform = list(
          scale_x = affine_transform$x$delta,
          shear_x = shear$affine[1],
          translate_x = affine_transform$x$offset,
          shear_y = shear$affine[2],
          scale_y = affine_transform$y$delta,
          translate_y = affine_transform$y$offset
        )
      )
    )
  )

  # from R date to JS timestamp: time_start + time_end
  time_start <- rdate_to_eedate(start_time, timestamp = TRUE)
  time_end <- rdate_to_eedate(end_time, timestamp = TRUE)

  # Adding bands
  bands <- list()
  for (index in seq_len(length(band_names))) {
    bands[[index]] <- list(
      id = band_names[index],
      tileset_band_index = as.integer((index - 1))
    )
  }

  # Putting all together
  manifest <- list(
    name = name,
    tilesets = list(tilesets),
    bands = bands,
    pyramiding_policy = pyramiding_policy,
    properties = properties,
    start_time = list(seconds = time_start / 1000),
    end_time = list(seconds = time_end / 1000)
  )

  if (is.null(properties)) manifest[["properties"]] <- NULL
  json_path <- sprintf("%s/manifest.json", tempdir_gee)
  ee_utils$ee_create_json_py(
    towrite = json_path,
    manifest = manifest
  )

  if (isFALSE(quiet)) {
    cat(
      blue('Uploading'),
      green(gs_uri),
      blue('to'),
      green(assetId),
      blue('... please wait\n')
    )
  }

  if (isTRUE(overwrite)) {
    try(
      expr = ee_manage_delete(assetId, quiet = TRUE),
      silent = TRUE
    )
  }

  system(
    command = sprintf("earthengine upload image --manifest '%s'", json_path),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
}


#' Create a zip file from a sf object
#'
#' @param x sf object
#' @param filename data source name
#' @param SHP_EXTENSIONS file extension of the files to save
#' into the zip file. By default: "dbf", "prj", "shp", "shx".
#' @importFrom utils zip
#' @importFrom sf write_sf
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' # Create sf object
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'sf_nc')
#'
#' # Method 1
#' # 1. Pass the sf to a zip file
#' zipfile <- ee_create_shp_zip(nc)
#'
#' # 2. From local to gcs
#' gs_uri <- ee_local_to_gcs(x = zipfile, bucket = 'rgee_dev')
#'
#' # 3. Pass the sf to a zip file
#' ee_gcs_to_table(
#'   gs_uri = gs_uri,
#'   assetId = assetId
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_sf_01 <- ee$FeatureCollection(assetId)
#' Map$centerObject(ee_sf_01)
#' Map$addLayer(ee_sf_01)
#'
#' # Method 2
#' ee_sf_02 <- sf_as_ee(x = nc,
#'                      assetId = assetId,
#'                      bucket = "rgee_dev")
#' Map$centerObject(ee_sf_02)
#' Map$addLayer(ee_sf_02)
#' }
#' @export
ee_create_shp_zip <- function(x,
                           filename,
                           SHP_EXTENSIONS = c("dbf", "prj", "shp", "shx")) {
  if (missing(filename)) {
    filename <- sprintf("%s%s",tempfile(),'.shp')
  }
  write_sf(obj = x, dsn = filename)
  shp_basename <- gsub("\\.shp$", "", filename)
  shp_filenames <- sprintf("%s.%s", shp_basename, SHP_EXTENSIONS)
  zipname <- sprintf("%s.zip", shp_basename)
  zip(zipfile = zipname, files = shp_filenames, flags = "-j -q")
  zipname
}

#' From sf object to Earth Engine FeatureCollection
#' @importFrom sf st_geometry
#' @noRd
ee_sf_to_fc <- function(sf, proj, geodesic, evenOdd) {
  # Load python module
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  fc <- list()
  for (index in seq_len(nrow(sf))) {
    feature <- sf[index,]
    sfc_feature <- st_geometry(feature)
    py_geometry <- geojson_json(sfc_feature,type = 'skip')
    wkt_type <- class(sfc_feature)[1] # wkt type identifier
    ee_geometry <- sf_as_ee$sfg_as_ee_py(x = py_geometry,
                                         sfc_class = wkt_type,
                                         opt_proj = proj,
                                         opt_geodesic = geodesic,
                                         opt_evenOdd = evenOdd)
    if (isFALSE(ee_geometry)) {
      stop("rgee does not support the upload of GEOMETRYCOLLECTION",
           " (sfg object).")
    }
    st_geometry(feature) <- NULL
    fc[[index]] <- ee$Feature(ee_geometry, as.list(feature))
  }
  ee$FeatureCollection(fc)
}

#' Pass a character, sfg, sfc to sf
#' @noRd
ee_st_read <- function(x, proj = 4326, check_ring_dir = FALSE, quiet = FALSE) {
  if (any(class(x) %in% 'sf')) {
    x$geometry <- st_sfc(x$geometry, check_ring_dir = check_ring_dir)
    x
  } else if (any(class(x) %in% 'sfg')) {
    if (is.null(proj)) {
      proj <- 4326
    }
    st_sf(
      index = 1,
      geometry = st_sfc(
        x,
        crs = 4326,
        check_ring_dir = check_ring_dir
      )
    )
  } else {
    result <- tryCatch(
      expr = st_sf(index = 1,
                   geometry = x,
                   check_ring_dir = check_ring_dir),
      error = function(e) st_read(dsn = x,
                                  stringsAsFactors =  FALSE,
                                  check_ring_dir = check_ring_dir,
                                  quiet = quiet)
    )
    if (ncol(result) == 1) {
      result$index <- seq_len(nrow(result))
    }
    result
  }
}

#' Pass a character or stars object to stars-proxy
#' @noRd
ee_as_proxystars <- function(x, temp_dir = tempdir()) {
  if (is.character(x)) {
    read_stars(x, proxy = TRUE)
  } else if (is(x,"stars")) {
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)
    tiff_filename <- sprintf("%s/%s.tif", temp_dir, ee_description)
    write_stars(x, tiff_filename)
    read_stars(tiff_filename, proxy = TRUE)
  } else if (is(x,"Raster")) {
    if (!requireNamespace("raster", quietly = TRUE)) {
      stop("package raster required, please install it first")
    }
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)
    tiff_filename <- sprintf("%s/%s.tif", temp_dir, ee_description)
    raster::writeRaster(x, tiff_filename)
    read_stars(tiff_filename, proxy = TRUE)
  } else {
    stop('x argument not defined properly.')
  }
}
