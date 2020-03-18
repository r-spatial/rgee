#' Upload local files to google cloud storage
#'
#' Upload images or tables into google cloud storage
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
      stop('The argument bucket was not defined')
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
#' @param gs_uri Character. It represents the full name of an
#' zipped shapefile in a GCS bucket.
#' @param asset_id Character. What to call the file once uploaded
#' to the Earth Engine Asset
#' @param quiet Logical. Suppress info message.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' # Create sf object
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' asset_id <- sprintf("%s/%s",ee_get_assethome(),'sf_nc')
#'
#' # Method 1
#' # 1. Pass the sf to a zip file
#' zipfile <- create_shp_zip(nc)
#'
#' # 2. From local to gcs
#' gs_uri <- ee_local_to_gcs(x = zipfile, bucket = 'rgee_dev')
#'
#' # 3. Pass the sf to a zip file
#' ee_gcs_to_asset_table(
#'   gs_uri = gs_uri,
#'   asset_id = asset_id
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_sf_01 <- ee$FeatureCollection(asset_id)
#' Map$centerObject(ee_sf_01)
#' Map$addLayer(ee_sf_01)
#'
#' # Method 2
#' ee_sf_02 <- sf_as_ee(nc, asset_id,via = 'gcs')
#' Map$centerObject(ee_sf_02)
#' Map$addLayer(ee_sf_02)
#' }
#' @export
ee_gcs_to_asset_table <- function(gs_uri, asset_id, quiet = FALSE) {
  if (isFALSE(quiet)) {
    cat(
      crayon::blue('Uploading'),
      crayon::green(gs_uri),
      crayon::blue('to'),
      crayon::green(asset_id),
      crayon::blue('... please wait\n')
    )
  }
  system(
    command = sprintf(
      "earthengine upload table --asset_id %s '%s'",
      asset_id, gs_uri
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
#' @param asset_id Character. What to call the file once uploaded
#' to the Earth Engine Asset. e.g. users/datacolecfbf/mydatacollection.
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
#' asset_id <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # Method 1
#' # 1. Move from local to gcs
#' gs_uri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev')
#'
#' # 2. Pass from gcs to asset
#' ee_gcs_to_asset_image(
#'   x = x,
#'   gs_uri = gs_uri,
#'   asset_id = asset_id
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_stars_01 <- ee$Image(asset_id)
#' Map$centerObject(ee_stars_01)
#' Map$addLayer(ee_stars_01)
#'
#' # Method 2
#' ee_sf_02 <- stars_as_ee(x = x, assetId = asset_id)
#' Map$centerObject(ee_sf_02)
#' Map$addLayer(ee_sf_02)
#' }
#' @export
ee_gcs_to_asset_image <- function(x,
                                  gs_uri,
                                  asset_id,
                                  properties = NULL,
                                  start_time = "1970-01-01",
                                  end_time = "1970-01-01",
                                  pyramiding_policy = 'MEAN',
                                  quiet = FALSE
                                  ) {
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
  name <- sprintf("projects/earthengine-legacy/assets/%s", asset_id)

  # Creating tileset
  tilesets <- list(
    crs = showWKT(st_crs(x)$proj4string),
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
  time_start <- rdate_to_eedate(start_time, eeobject = FALSE)
  time_end <- rdate_to_eedate(end_time, eeobject = FALSE)

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
      crayon::blue('Uploading'),
      crayon::green(gs_uri),
      crayon::blue('to'),
      crayon::green(asset_id),
      crayon::blue('... please wait\n')
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
#' asset_id <- sprintf("%s/%s",ee_get_assethome(),'sf_nc')
#'
#' # Method 1
#' # 1. Pass the sf to a zip file
#' zipfile <- create_shp_zip(nc)
#'
#' # 2. From local to gcs
#' gs_uri <- ee_local_to_gcs(x = zipfile, bucket = 'rgee_dev')
#'
#' # 3. Pass the sf to a zip file
#' ee_gcs_to_asset_table(
#'   gs_uri = gs_uri,
#'   asset_id = asset_id
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_sf_01 <- ee$FeatureCollection(asset_id)
#' Map$centerObject(ee_sf_01)
#' Map$addLayer(ee_sf_01)
#'
#' # Method 2
#' ee_sf_02 <- sf_as_ee(nc, asset_id,via = 'gcs')
#' Map$centerObject(ee_sf_02)
#' Map$addLayer(ee_sf_02)
#' }
#' @export
create_shp_zip <- function(x,
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
    py_geometry <- geojson_json(st_geometry(feature),type = 'skip')
    ee_geometry <- sf_as_ee$sfg_as_ee_py(x = py_geometry,
                                         opt_proj = proj,
                                         opt_geodesic = geodesic,
                                         opt_evenOdd = evenOdd)
    feature$geometry <- NULL
    fc[[index]] <- ee$Feature(ee_geometry, as.list(feature))
  }
  ee$FeatureCollection(fc)
}

#' Pass a character, sfg, sfc to sf
#' @importFrom sf NA_crs_
#' @noRd
ee_st_read <- function(x, proj = 4326, check_ring_dir = FALSE, quiet = TRUE) {
  if (any(class(x) %in% 'sf')) {
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
      expr = st_sf(x, check_ring_dir = check_ring_dir),
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
  } else {
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)
    tiff_filename <- sprintf("%s/%s.tif", temp_dir, ee_description)
    write_stars(x, tiff_filename)
    tryCatch(
      expr = read_stars(tiff_filename, proxy = TRUE),
      error = function(e) stop('x argument not defined properly.')
    )
  }
}
