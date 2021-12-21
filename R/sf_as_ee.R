#' Convert an sf object to an EE object
#'
#' Load an sf object to Earth Engine.
#'
#' @param x object of class sf, sfc or sfg.
#' @param via Character. Upload method for sf objects. Three methods are
#' implemented: 'getInfo', 'getInfo_to_asset' and 'gcs_to_asset'. See details.
#' @param monitoring Logical. Ignore if via is not set as
#' \code{getInfo_to_asset} or \code{gcs_to_asset}. If TRUE the exportation task
#' will be monitored.
#' @param proj Integer or character. Coordinate Reference System (CRS) for the
#' EE object, defaults to "EPSG:4326" (x=longitude, y=latitude).
#' @param assetId Character. Destination asset ID for the uploaded file. Ignore
#' if \code{via} argument is "getInfo".
#' @param geodesic Logical. Ignored if \code{x} is not a Polygon or LineString.
#' Whether line segments should be interpreted as spherical geodesics. If
#' FALSE, indicates that line segments should be interpreted as planar lines
#' in the specified CRS. If absent, defaults to TRUE if the CRS is geographic
#' (including the default EPSG:4326), or to FALSE if the CRS is projected.
#' @param evenOdd Logical. Ignored if \code{x} is not a Polygon. If TRUE,
#' polygon interiors will be determined by the even/odd rule, where a point
#' is inside if it crosses an odd number of edges to reach a point at infinity.
#' Otherwise polygons use the left-inside rule, where interiors are on the
#' left side of the shell's edges when walking the vertices in the given order.
#' If unspecified, defaults to TRUE.
#' @param bucket Character. Name of the bucket (GCS) to save intermediate files
#' (ignore if \code{via} is not defined as "gcs_to_asset").
#' @param predefinedAcl Specify user access to object. Passed to
#' \code{googleCloudStorageR::gcs_upload}.
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool (CLT). If NULL, rgee assumes that CLT is set in the system PATH.
#' (ignore if \code{via} is not defined as "gcs_to_asset").
#' @param overwrite A boolean argument that indicates indicating
#' whether "filename" should be overwritten. Ignore if \code{via} argument
#' is "getInfo". By default TRUE.
#' @param quiet Logical. Suppress info message.
#' @param ... \code{\link{ee_utils_create_manifest_table}} arguments might be included.
#'
#' @return
#' When \code{via} is "getInfo" and \code{x} is either an sf or sfc object
#' with multiple geometries will return an \code{ee$FeatureCollection}. For
#' single sfc and sfg objects will return an \code{ee$Geometry$...}.
#'
#' If \code{via} is either "getInfo_to_asset" or "gcs_to_asset" always will
#' return an \code{ee$FeatureCollection}.
#'
#' @details
#' \code{sf_as_ee} supports the upload of \code{sf} objects by three different
#' options: "getInfo" (default), "getInfo_to_asset", and "gcs_to_asset". \code{getInfo}
#' transforms sf objects (sfg, sfc, or sf) to GeoJSON (using \code{geojsonio::geojson_json})
#' and then encrusted them in an HTTP request using the server-side objects that are
#' implemented in the Earth Engine API (i.e. ee$Geometry$...). If the sf object is too
#' large (~ >1Mb) is likely to cause bottlenecks since it is a temporary
#' file that is not saved in your EE Assets (server-side). The second option implemented
#' is 'getInfo_to_asset'. It is similar to the previous one, with the difference
#' that after create the server-side object will save it in your Earth Engine
#' Assets. For dealing with very large spatial objects is preferable to use the
#' third option 'gcs_to_asset'. This option firstly saves the sf object as a *.shp
#' file in the /temp directory. Secondly, using the function \code{local_to_gcs}
#' will move the shapefile from local to Google Cloud Storage. Finally, using
#' the function \code{gcs_to_ee_table} the ESRI shapefile will be loaded
#' to their EE Assets. See \href{https://developers.google.com/earth-engine/guides/table_upload/}{Importing
#' table data} documentation for more details.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#'
#' # 1. Handling geometry parameters
#' # Simple
#' ee_x <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
#'   sf_as_ee()
#'
#' Map$centerObject(eeObject = ee_x)
#' Map$addLayer(ee_x)
#'
#' # Create a right-inside polygon.
#' toy_poly <- matrix(data = c(-35,-10,-35,10,35,10,35,-10,-35,-10),
#'                    ncol = 2,
#'                    byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon()
#'
#' holePoly <- sf_as_ee(x = toy_poly, evenOdd = FALSE)
#'
#' # Create an even-odd version of the polygon.
#' evenOddPoly <- sf_as_ee(toy_poly, evenOdd = TRUE)
#'
#' # Create a point to test the insideness of the polygon.
#' pt <- ee$Geometry$Point(c(1.5, 1.5))
#'
#' # Check insideness with a contains operator.
#' print(holePoly$contains(pt)$getInfo() %>% ee_utils_py_to_r())
#' print(evenOddPoly$contains(pt)$getInfo() %>% ee_utils_py_to_r())
#'
#' # 2. Upload small geometries to EE asset
#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'toy_poly')
#' eex <- sf_as_ee(
#'  x = toy_poly,
#'  overwrite = TRUE,
#'  assetId = assetId,
#'  via = "getInfo_to_asset")

#' # 3. Upload large geometries to EE asset
#' ee_Initialize(gcs = TRUE)

#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'toy_poly_gcs')
#' eex <- sf_as_ee(
#'   x = toy_poly,
#'   overwrite = TRUE,
#'   assetId = assetId,
#'   bucket = 'rgee_dev',
#'   monitoring = FALSE,
#'   via = 'gcs_to_asset'
#' )
#' ee_monitoring()
#' }
#' @export
sf_as_ee <- function(x,
                     via = 'getInfo',
                     assetId = NULL,
                     bucket = NULL,
                     predefinedAcl = "bucketLevel",
                     command_line_tool_path = NULL,
                     overwrite = TRUE,
                     monitoring = TRUE,
                     proj = "EPSG:4326",
                     evenOdd = TRUE,
                     geodesic = NULL,
                     quiet = FALSE,
                     ...) {
  # check packages
  ee_check_packages("sf_as_ee", "sf")

  if (!any(class(x) %in%  c("sf", "sfc", "sfg"))) {
    stop("x needs to be an object of class sf, sfc, sfg")
  }

  # sf_as_ee does not support POSIXlt, POSIXct and POSIXt columns
  df_classes <- as.character(x %>% lapply(class) %>% unlist())
  is_POSIX <- df_classes %in% c("POSIXlt", "POSIXct", "POSIXt")
  if (any(is_POSIX)) {
    posix_column_names <- paste0(names(x)[is_POSIX], collapse = ", ")
    pos_msg <- sprintf(
      "%s does not support %s. Convert the %s: %s to character.",
      "sf_as_ee",
      "POSIXt, POSIXct or POSIXlt",
      if (sum(is_POSIX) == 1) "column" else "columns",
      bold(posix_column_names)
    )
    stop(pos_msg)
  }

  # There is point in the name?
  x_point_condition <- grepl("\\.",colnames(x))
  if (any(x_point_condition)) {
    stop(
      "Invalid column name(s). Earth Engine does not support the following column names:\n",
      bold(paste0(colnames(x)[x_point_condition], collapse = ", "))
    )
  }

  if (any(class(x) %in%  "sfg")) {
    x <- sf::st_sfc(x, crs = proj)
  }

  # geodesic is null?
  if (is.null(geodesic)) {
    is_geodesic <- sf::st_is_longlat(x)
  } else {
    is_geodesic <- geodesic
  }

  if (is.na(sf::st_crs(x)$Wkt)) {
    stop(
      "The x CRS needs to be defined, use sf::st_set_crs to set."
    )
  }


  if (via == "getInfo") {
    # Transform x according to proj argument
    x <- sf::st_transform(x, proj)
    ee_sf_to_fc(
      x = x,
      proj = proj,
      geodesic = is_geodesic,
      evenOdd = evenOdd
    )
  } else if (via == "getInfo_to_asset") {

    # Transform x according to proj argument
    x <- sf::st_transform(x, proj)
    sf_fc <- ee_sf_to_fc(
      x = x,
      proj = proj,
      geodesic = is_geodesic,
      evenOdd = evenOdd
    )

    # Creating description name
    time_format <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
    ee_description <- paste0("ee_as_sf_task_", time_format)

    # Verify if assetId exist and the EE asset path
    if (is.null(assetId)) {
      stop(
        "You must define assetId args in 'getInfo_to_asset' and ",
        "'gcs_to_asset' mode."
      )
    }

    # Verify is the EE assets path is valid
    assetId <- ee_verify_filename(
      path_asset = assetId,
      strict = FALSE
    )

    # geojson to assset
    ee_task <- ee_table_to_asset(
      collection = ee$FeatureCollection(sf_fc),
      overwrite = overwrite,
      description = ee_description,
      assetId = assetId
    )

    if (isTRUE(monitoring)) {
      ee$batch$Task$start(ee_task)
      ee_monitoring(ee_task)
      ee$FeatureCollection(assetId)
    } else {
      ee$batch$Task$start(ee_task)
      ee$FeatureCollection(assetId)
    }
  } else if (via == "gcs_to_asset") {

    # Verify if assetId args exist
    if (is.null(assetId)) {
      stop(
        "You must define assetId args in 'getInfo_to_asset' and ",
        "'gcs_to_asset' mode."
      )
    }

    # Verify if bucket args exist
    if (is.null(bucket)) {
      stop("Cloud Storage bucket was not defined.")
    } else {
      tryCatch(
        expr = googleCloudStorageR::gcs_get_bucket(bucket),
        error = function(e) {
          stop(sprintf("The %s bucket was not found.", bucket))
        }
      )
    }

    if (is.null(command_line_tool_path)) {
      command_line_tool_path <- ""
    }

    shp_dir <- sprintf("%s.shp", tempfile())

    # From sf to .shp
    if (!quiet) {
      message("1. Converting sf object to a compressed ZIP shapefile ",
              "... saving in /tmp")
    }
    geozip_dir <- ee_utils_shp_to_zip(x, shp_dir)

    # From local to gcs
    if (!quiet) {
      message("2. From local to GCS")
    }
    gcs_filename <- local_to_gcs(
      x = geozip_dir,
      bucket = bucket,
      predefinedAcl = predefinedAcl,
      quiet = quiet
    )

    if (!quiet) {
      message("3. Creating the manifest")
    }
    # Verify is the EE assets path is valid
    assetId <- ee_verify_filename(
      path_asset = assetId,
      strict = FALSE
    )
    manifest <- ee_utils_create_manifest_table(
      gs_uri = gcs_filename,
      assetId = assetId,
      ...
    )

    if (!quiet) {
      message("4. From GCS to Earth Engine")
    }
    gcs_to_ee_table(
      manifest = manifest,
      command_line_tool_path = command_line_tool_path,
      overwrite = overwrite,
      quiet = quiet
    )

    if (isTRUE(monitoring)) {
      ee_monitoring()
      ee$FeatureCollection(assetId)
    } else {
      ee$FeatureCollection(assetId)
    }
  } else {
    stop('Invalid via argument')
  }
}
