#' Convert an sf object to an EE object
#'
#' @param x sf object to be converted into a EE object.
#' @param assetId Destination asset ID for the uploaded file.
#' @param check_ring_dir logical. See \link[sf]{st_read} for details.
#' @param evenOdd If TRUE, polygon interiors will be determined
#' by the even/odd rule, where a point is inside if it crosses
#' an odd number of edges to reach a point at infinity. Otherwise
#' polygons use the left-inside rule, where interiors are on the
#' left side of the shell's edges when walking the vertices in
#' the given order. If unspecified, defaults to TRUE.
#' @param proj An optional projection specification, either as a CRS ID
#' code or as a WKT string. If specified, overrides any CRS found in
#' the GeoJSON parameter. If unspecified and the GeoJSON does not
#' declare a CRS, defaults to "EPSG:4326" (x=longitude, y=latitude).
#' @param geodesic Whether line segments should be interpreted as spherical
#' geodesics. If FALSE, indicates that line segments should be interpreted
#' as planar lines in the specified CRS. If absent, defaults to TRUE if
#' the CRS is geographic (including the default EPSG:4326), or to FALSE
#' if the CRS is projected.
#' @param via Method to download the image. Three methods
#' are implemented 'getInfo', 'toasset' and 'gcs'. See details.
#' @param bucket name you want this session to use by default, or a bucket
#' object.
#' @param monitoring Relevant parameter when the argument via is set as
#' either "toasset" or "gcs". If TRUE the exportation task will be monitored.
#' @param quiet Logical. Suppress info message.
#' @param ... \link[sf]{st_read} arguments might be included.
#' @importFrom sf st_read st_sf st_sfc st_is_longlat
#' @importFrom geojsonio geojson_json
#' @return A ee$FeatureCollection object
#' @details
#' The process to pass a sf object to Earth Engine Asset could be carried
#' out by three different strategies. These are controlled by the "via"
#' parameter. The first method implemented is 'getInfo'. In this method the
#' sf objects are transformed to GeoJSON using \link[geojsonio]{geojson_json}
#' and then encrusted in an HTTP request using the server-side objects that are
#' implemented in the Earth Engine API (e.g. ee$Geometry$*). If the sf object
#' is too large (>1Mb) it is likely to cause bottlenecks and plodding
#' connections. One advantage of this method is that it create temporary files
#' and will not be saved in your Earth Engine Asset. See
#' \href{https://developers.google.com/earth-engine/client_server}{Client
#' vs Server} documentation for more details. The second method implemented is
#' 'toasset'. It is similar to the previous one, with the difference that
#' the spatial object will be saved in your Earth Engine Asset. For dealing
#' with very large spatial objects, it is preferable to use the third method
#' called 'gcs'. In this method, firstly, the sf object will be saved as a
#' *.shp in the  /temp directory. Secondly, using the function ee_local_to_gcs
#' will move the shapefile from local to Google Cloud Storage. Finally, using
#' the function ee_gcs_to_asset_table it will be loaded to the Earth Engine
#' Asset.
#'
#' Earth Engine is strict on polygon ring directions (outer ring
#' counter-clockwise, and the inner one clockwise). If `check_ring_dir` is TRUE,
#' it check every ring, and revert them if necessary, to counter clockwise for
#' outer, and clockwise for inner (hole) ones. By default this is FALSE because
#' it is an expensive operation.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#'
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # 1. Handling geometry parameters
#' # Simple
#' ee_x <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
#'   st_transform(4326) %>%
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
#' holePoly <- sf_as_ee(x = toy_poly, evenOdd = FALSE)
#'
#' # Create an even-odd version of the polygon.
#' evenOddPoly <- sf_as_ee(toy_poly, evenOdd = TRUE)
#'
#' # Create a point to test the insideness of the polygon.
#' pt <- ee$Geometry$Point(c(1.5, 1.5))
#'
#' # Check insideness with a contains operator.
#' print(holePoly$geometry()$contains(pt)$getInfo() %>% ee_py_to_r())   # FALSE
#' print(evenOddPoly$geometry()$contains(pt)$getInfo() %>% ee_py_to_r())# TRUE
#'
#' # 2. Upload small geometries to EE asset
#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'toy_poly')
#' eex <- sf_as_ee(
#'   x = toy_poly,
#'   assetId = assetId,
#'   via = 'toasset')
#'
#' # 3. Upload large geometries to EE asset
#' ee_Initialize(gcs = TRUE)
#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'toy_poly_gcs')
#' eex <- sf_as_ee(
#'   x = toy_poly,
#'   assetId = assetId,
#'   bucket = 'rgee_dev',
#'   monitoring = FALSE,
#'   via = 'gcs')
#' }
#' @export
sf_as_ee <- function(x,
                     check_ring_dir = FALSE,
                     evenOdd = TRUE,
                     proj = NULL,
                     geodesic = NULL,
                     via = 'getInfo',
                     assetId = NULL,
                     bucket = NULL,
                     monitoring = TRUE,
                     quiet = FALSE,
                     ...) {
  # Read geometry
  eex <- ee_st_read(
    x = x,
    proj = proj,
    check_ring_dir = check_ring_dir,
    quiet = TRUE
  )

  # geodesic is null?
  if (is.null(geodesic)) {
    is_geodesic <- st_is_longlat(eex)
  } else {
    is_geodesic <- geodesic
  }

  # proj
  if (is.null(proj)) {
    eex_proj <- paste0('EPSG:',st_crs(eex)$epsg)
  } else {
    eex_proj <- paste0('EPSG:',proj)
  }
  if (is.na(st_crs(eex)$epsg)) {
    stop(
      "The x EPSG needs to be defined, use sf::st_set_crs to",
      " set, replace or retrieve."
    )
  }
  if (via == 'getInfo') {
    # sf to geojson
    ee_sf_to_fc(
      sf = eex,
      proj = eex_proj,
      geodesic = is_geodesic,
      evenOdd = evenOdd
    )
  } else if (via == 'toasset') {
    # sf to geojson
    sf_fc <- ee_sf_to_fc(
      sf = eex,
      proj = eex_proj,
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
    assetId <- ee_verify_filename(
      path_asset = assetId,
      strict = FALSE
    )
    # geojson to assset
    ee_task <- ee_table_to_asset(
      collection = sf_fc,
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
  } else if (via == 'gcs') {
    shp_dir <- sprintf("%s.shp", tempfile())
    geozip_dir <- ee_create_shp_zip(eex, shp_dir)
    gcs_filename <- ee_local_to_gcs(
      x = geozip_dir,
      bucket = bucket,
      quiet = quiet
    )
    ee_gcs_to_asset_table(
      gs_uri = gcs_filename,
      asset_id = assetId
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
