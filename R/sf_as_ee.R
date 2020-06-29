#' Convert an sf object to an EE object
#'
#' Convert an sf object to an ee$FeatureCollection
#'
#' @param x sf object to be converted into an ee$FeatureCollection.
#' @param via Character. Upload method for sf objects. Three methods are
#'  implemented 'getInfo', 'getInfo_to_asset' and 'gcs_to_asset'. See details.
#' @param monitoring Logical. Ignore if via is not set as
#' \code{getInfo_to_asset} or \code{gcs_to_asset}. If TRUE the exportation task
#' will be monitored.
#' @param assetId Character. Destination asset ID for the uploaded file. Ignore
#' if \code{via} argument is "getInfo".
#' @param proj Integer or character. Coordinate Reference System (CRS) for the
#' EE object, defaults to "EPSG:4326" (x=longitude, y=latitude).
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
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool. If NULL, rgee assumes is saved in the same path that your Python
#' environment (ignore if \code{via} is not defined as "gcs_to_asset").
#' @param overwrite A boolean argument which indicates indicating
#' whether "filename" should be overwritten. By default TRUE.
#' @param quiet Logical. Suppress info message.
#' @param ... \code{st_read} arguments might be included.
#'
#' @return An ee$FeatureCollection object
#'
#' @details
#' \code{sf_as_ee} supports the upload of \code{sf} objects by three different
#' options: "getInfo", "getInfo_to_asset", and "gcs_to_asset".
#' When "getInfo" is set in the \code{via} argument the sf object is
#' transformed to GeoJSON (using \code{geojsonio::geojson_json}) and then
#' encrusted in an HTTP request using the server-side objects that are
#' implemented in the Earth Engine API (ee$Geometry). If the sf object is too
#' large (~ >1Mb) it is likely to cause bottlenecks since it is a temporary
#' file that will save in the request message not in their EE Assets. See
#' \href{https://developers.google.com/earth-engine/client_server}{Client
#' vs Server} documentation for more details. The second method implemented is
#' 'getInfo_to_asset'. It is similar to the previous one, with the difference
#' that the result will be saved in your Earth Engine Asset. For dealing
#' with very large spatial objects is preferable to use the third option
#' 'gcs_to_asset'. This option firstly saves the sf object as a *.shp file
#' in the /temp directory. Secondly, using the function \code{local_to_gcs}
#' will move the shapefile from local to Google Cloud Storage. Finally, using
#' the function \code{gcs_to_ee_table} the ESRI shapefile will be loaded
#' to their EE Assets.
#' See \href{https://developers.google.com/earth-engine/importing}{Importing
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
#'   st_polygon() %>%
#'   st_sfc(crs="EPSG:4326") %>%
#'   st_sf()
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
#' print(holePoly$geometry()$contains(pt)$getInfo() %>% ee_utils_py_to_r())
#' print(evenOddPoly$geometry()$contains(pt)$getInfo() %>% ee_utils_py_to_r())
#'
#' # 2. Upload small geometries to EE asset
#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'toy_poly')
#' eex <- sf_as_ee(
#'   x = toy_poly,
#'   overwrite = TRUE,
#'   assetId = assetId,
#'   via = "getInfo_to_asset")
#'
#' # 3. Upload large geometries to EE asset
#' ee_Initialize("data.colec.fbf", gcs = TRUE)
#'
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
                     command_line_tool_path = NULL,
                     overwrite = TRUE,
                     monitoring = TRUE,
                     evenOdd = TRUE,
                     proj = 4326,
                     geodesic = NULL,
                     quiet = FALSE,
                     ...) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }

  if (!any(class(x) %in%  "sf")) {
    stop("x needs to be an object of class sf")
  }

  # geodesic is null?
  if (is.null(geodesic)) {
    is_geodesic <- sf::st_is_longlat(x)
  } else {
    is_geodesic <- geodesic
  }

  if (is.na(sf::st_crs(x)$epsg)) {
    stop(
      "The x EPSG needs to be defined, use sf::st_set_crs to",
      " set, replace or retrieve."
    )
  }

  # Transform x according to proj argument
  x_proj <- sf::st_crs(proj)$epsg
  x <- sf::st_transform(x, x_proj)
  x_proj <- sprintf("EPSG:%s", sf::st_crs(x)$epsg)

  if (via == "getInfo") {
    # sf to geojson
    ee_sf_to_fc(
      x = x,
      proj = x_proj,
      geodesic = is_geodesic,
      evenOdd = evenOdd
    )
  } else if (via == "getInfo_to_asset") {
    # sf to geojson
    sf_fc <- ee_sf_to_fc(
      x = x,
      proj = x_proj,
      geodesic = is_geodesic,
      evenOdd = evenOdd
    )

    # Creating description name
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_sf_task_", time_format)

    # Verify if assetId exist and the EE asset path
    if (is.null(assetId)) {
      stop('assetId was not defined')
    }

    # Verify is the EE assets path is valid
    assetId <- ee_verify_filename(
      path_asset = assetId,
      strict = FALSE
    )

    # geojson to assset
    ee_task <- ee_table_to_asset(
      collection = sf_fc,
      overwrite = overwrite,
      description = ee_description,
      assetId = assetId
    )
    if (isTRUE(monitoring)) {
      ee_task$start()
      ee_monitoring(ee_task)
      ee$FeatureCollection(assetId)
    } else {
      assetId
    }
  } else if (via == "gcs_to_asset") {
    if (is.null(bucket)) {
      stop("Cloud Storage bucket was not defined")
    }

    if (is.null(command_line_tool_path)) {
      command_line_tool_path <- dirname(Sys.getenv("EARTHENGINE_PYTHON"))
    }

    shp_dir <- sprintf("%s.shp", tempfile())

    # From sf to .shp
    message("1. Converting sf object to shapefile ... saving in /tmp")
    geozip_dir <- ee_utils_shp_to_zip(x, shp_dir)

    # From local to gcs
    message("2. From local to GCS")
    gcs_filename <- local_to_gcs(
      x = geozip_dir,
      bucket = bucket,
      quiet = quiet
    )

    message("3. From GCS to Earth Engine")
    gcs_to_ee_table(
      gs_uri = gcs_filename,
      overwrite = overwrite,
      assetId = assetId,
      command_line_tool_path = command_line_tool_path
    )
    if (isTRUE(monitoring)) {
      ee_monitoring()
      ee$FeatureCollection(assetId)
    } else {
      assetId
    }
  } else {
    stop('Invalid via argument')
  }
}
