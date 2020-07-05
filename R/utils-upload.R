#' Upload local files to Google Cloud Storage
#'
#' Upload images or tables to Google Cloud Storage
#'
#' @param x Character. filename.
#' @param bucket bucket name you are uploading to
#' @param quiet Logical. Suppress info message.
#' @return Character which represents the full path of the object in the GCS
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
#' # tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' # local_to_gcs(x = tif, bucket = 'rgee_dev')
#' }
#' @export
local_to_gcs <- function(x,
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

    if (is.na(getOption("rgee.gcs.auth"))) {
      stop(
        "Google Cloud Storage credentials were not loaded.",
        ' Run ee_Initialize(..., gcs = TRUE)',
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

#' Move a zipped shapefile from GCS to their EE Assets
#'
#' @param gs_uri Character. It represents the full name of an
#' zipped shapefile in a GCS bucket.
#' @param assetId Character. What to call the file once uploaded
#' to their Earth Engine Assets
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool. If NULL, rgee assumes is saved in the same path that your Python
#' environment.
#' @param overwrite Logical. If TRUE, the assetId will be overwritten if
#' it exists.
#' @param quiet Logical. Suppress info message.
#'
#' @return Character. The Earth Engine asset ID.
#'
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
#' zipfile <- ee_utils_shp_to_zip(nc)
#'
#' # # 2. From local to gcs
#' # gs_uri <- local_to_gcs(x = zipfile, bucket = 'rgee_dev')
#' #
#' # # 3. Pass the sf to a zip file
#' # gcs_to_ee_table(
#' #   gs_uri = gs_uri,
#' #   overwrite = TRUE,
#' #   assetId = assetId
#' # )
#' #
#' # # OPTIONAL: Monitoring progress
#' # ee_monitoring()
#' #
#' # # OPTIONAL: Display results
#' # ee_sf_01 <- ee$FeatureCollection(assetId)
#' # Map$centerObject(ee_sf_01)
#' # Map$addLayer(ee_sf_01)
#' #
#' # # Method 2
#' # ee_sf_02 <- sf_as_ee(x = nc,
#' #                      assetId = assetId,
#' #                      overwrite = TRUE,
#' #                      bucket = "rgee_dev",
#' #                      via = 'gcs')
#' # Map$centerObject(ee_sf_02)
#' # Map$addLayer(ee_sf_02)
#' }
#' @export
gcs_to_ee_table <- function(gs_uri,
                            assetId,
                            command_line_tool_path = NULL,
                            overwrite = FALSE,
                            quiet = FALSE) {
  if (is.null(command_line_tool_path)) {
    command_line_tool_path <- dirname(Sys.getenv("EARTHENGINE_PYTHON"))
  }

  if (command_line_tool_path == "") {
    command = sprintf(
      "earthengine upload table --asset_id %s %s",
      assetId, gs_uri
    )
  } else {
    command = sprintf(
      "%s/earthengine upload table --asset_id %s %s",
      command_line_tool_path, assetId, gs_uri
    )
  }

  if (!quiet) {
    message("Running the OS command:", command)
  }

  if (isTRUE(overwrite)) {
    try(
      expr = ee_manage_delete(assetId, quiet = TRUE),
      silent = TRUE
    )
  }

  upload_state <- system(
    command = command,
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )

  if (upload_state != 0) {
    stop(
      sprintf(
        "An error occurs when %s try to upload %s to %s",
        bold("gcs_to_ee_table"), bold(gs_uri), bold(assetId)
      ),
      ". Please make sure that you set the ",
      bold("command_line_tool_path"),
      " argument correctly."
    )
  }
  assetId
}

#' Move a GeoTIFF image from GCS to their EE assets
#'
#' @param x An object of class stars or stars-proxy.
#' @param gs_uri Character. It represents the full name of the
#' GeoTIFF file of \code{x} in a GCS bucket.
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool. If NULL, rgee assumes is saved in the same path that your Python
#' environment.
#' @param assetId Character. How to call the file once uploaded
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
#'
#' @return Character. The Earth Engine asset ID.
#'
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
#' zipfile <- ee_utils_shp_to_zip(nc)
#'
#' # # 2. From local to gcs
#' # gs_uri <- local_to_gcs(x = zipfile, bucket = 'rgee_dev')
#'
#' # # 3. Pass the sf to a zip file
#' # gcs_to_ee_table(
#' #    gs_uri = gs_uri,
#' #    overwrite = TRUE,
#' #    assetId = assetId
#' # )
#'
#' # # OPTIONAL: Monitoring progress
#' # ee_monitoring()
#'
#' # # OPTIONAL: Display results
#' # ee_sf_01 <- ee$FeatureCollection(assetId)
#' # Map$centerObject(ee_sf_01)
#' # Map$addLayer(ee_sf_01)
#'
#' # # Method 2
#' # ee_sf_02 <- sf_as_ee(x = nc,
#' #                       assetId = assetId,
#' #                       overwrite = TRUE,
#' #                       bucket = "rgee_dev",
#' #                       via = 'gcs')
#' # Map$centerObject(ee_sf_02)
#' # Map$addLayer(ee_sf_02)
#' }
#' @export
gcs_to_ee_image <- function(x,
                            gs_uri,
                            assetId,
                            overwrite = FALSE,
                            properties = NULL,
                            command_line_tool_path = NULL,
                            start_time = "1970-01-01",
                            end_time = "1970-01-01",
                            pyramiding_policy = 'MEAN',
                            quiet = FALSE) {
  # Folder to save upload temporary files.
  tempdir_gee <- tempdir()

  # If the command_line_tool_path does not exist
  if (is.null(command_line_tool_path)) {
    command_line_tool_path <- dirname(Sys.getenv("EARTHENGINE_PYTHON"))
  }

  # Verify is the EE assets path is valid, if not try to fix
  assetId <- ee_verify_filename(
    path_asset = assetId,
    strict = FALSE
  )

  # Load utils python module
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

  if (is.na(sf::st_crs(x)$wkt)) {
    stop("x does not have a CRS defined first")
  }

  # Creating tileset
  tilesets <- list(
    crs = sf::st_crs(x)$wkt,
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

  #Command to run in console
  if (command_line_tool_path == "") {
    command <- sprintf("earthengine upload image --manifest '%s'",
                       json_path)
  } else {
    command <- sprintf("%s/earthengine upload image --manifest '%s'",
                       command_line_tool_path, json_path)
  }

  if (!quiet) {
    message("Running the OS command:", command)
  }

  if (isTRUE(overwrite)) {
    try(
      expr = ee_manage_delete(assetId, quiet = TRUE),
      silent = TRUE
    )
  }

  upload_state <- system(
    command = command,
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )

  if (upload_state != 0) {
    stop(
      sprintf(
        "An error occurs when %s try to upload %s to %s",
        bold("gcs_to_ee_image"), bold(gs_uri), bold(assetId)
      ),
      ". Please make sure that you set the ",
      bold("command_line_tool_path"),
      " argument correctly."
    )
  }
  assetId
}

#' From sf object to Earth Engine FeatureCollection
#' @noRd
ee_sf_to_fc <- function(x, proj, geodesic, evenOdd) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  if (!requireNamespace("geojsonio", quietly = TRUE)) {
    stop("package geojsonio required, please install it first")
  }

  # Load python module
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)

  ngeometries <- if (any(class(x) %in% "sf")) nrow(x) else length(x)

  if (ngeometries == 1 & !any(class(x) %in% "sf")) {
    sfc_feature <- sf::st_geometry(x)
    py_geometry <- geojsonio::geojson_json(sfc_feature, type = 'skip')
    sf_obj_class <- class(sfc_feature)
    wkt_type <- sf_obj_class[sf_obj_class %in% ee_sf_comp()]
    if (length(wkt_type) == 0) {
      stop(
        "sf_as_ee does not support objects of class ",
        paste0(sf_obj_class,collapse = ", ")
      )
    }
    ee_geometry <- sf_as_ee$sfg_as_ee_py(x = py_geometry,
                                         sfc_class = wkt_type,
                                         opt_proj = proj,
                                         opt_geodesic = geodesic,
                                         opt_evenOdd = evenOdd)
    return(ee_geometry)
  } else {
    fc <- list()
    for (index in seq_len(ngeometries)) {
      feature <- x[index,]
      sfc_feature <- sf::st_geometry(feature)
      py_geometry <- geojsonio::geojson_json(sfc_feature, type = 'skip')
      sf_obj_class <- class(sfc_feature)
      wkt_type <- sf_obj_class[sf_obj_class %in% ee_sf_comp()]
      ee_geometry <- sf_as_ee$sfg_as_ee_py(x = py_geometry,
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

  if (!requireNamespace("stars", quietly = TRUE)) {
    stop("package stars required, please install it first")
  }

  if (is.character(x)) {
    stars::read_stars(x, proxy = TRUE)
  } else if (is(x,"stars")) {
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)
    tiff_filename <- sprintf("%s/%s.tif", temp_dir, ee_description)
    stars::write_stars(x, tiff_filename)
    stars::read_stars(tiff_filename, proxy = TRUE)
  } else if (is(x,"Raster")) {
    if (!requireNamespace("raster", quietly = TRUE)) {
      stop("package raster required, please install it first")
    }
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
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
