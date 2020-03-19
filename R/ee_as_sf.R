#' Convert an EE table in a sf object
#'
#' @param x EE table to be converted into a sf object.
#' @param container Relevant when the "via" argument is
#' defined as "drive" or "gcs". It is the name of a unique
#' folder ('drive') or bucket ('gcs') to be exported into.
#' @param via Method to download the image. Three methods
#' are implemented 'getInfo', 'drive', and 'gcs'. See details.
#' @param selectors The list of properties to include in
#' the output, as a list of strings or a comma-separated
#' string. By default, all properties are included.
#' @param quiet logical. Suppress info message
#' @importFrom geojsonio geojson_sf
#' @importFrom utils tail
#' @import methods
#' @details
#' The process to pass a ee$FeatureCollection, ee$Feature or ee$Geometry to
#' your local env could be carried out by three different strategies. The
#' first one ('getInfo') use the getInfo method, which fetch and return
#' information about Earth Engine objects, the advantage of use this strategy is
#' a direct and fast download. However, there is a limit of 5000 features
#' that can be transferred by request which makes it not recommendable for
#' large collections. The second ('drive') and third ('gcs') methods are
#' suitable for large images since it uses Google Drive and Google Cloud
#' Storage as intermediate containers.
#' @return An sf object.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(
#'   email = "data.colec.fbf@gmail.com",
#'   drive = TRUE,
#'   gcs = TRUE
#' )
#'
#' # Region of interest
#' roi <- ee$Geometry$Polygon(list(
#'   c(-122.275, 37.891),
#'   c(-122.275, 37.868),
#'   c(-122.240, 37.868),
#'   c(-122.240, 37.891)
#' ))
#'
#' # TIGER: US Census Blocks Dataset
#' blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
#' subset <- blocks$filterBounds(roi)
#' sf_subset <- ee_as_sf(x = subset)
#' plot(sf_subset)
#'
#' # Create Random points in Earth Engine
#' region <- ee$Geometry$Rectangle(-119.224, 34.669, -99.536, 50.064)
#' ee_help(ee$FeatureCollection$randomPoints)
#' ee_randomPoints <- ee$FeatureCollection$randomPoints(region, 100)
#'
#' # Download via GetInfo
#' sf_randomPoints <- ee_as_sf(ee_randomPoints)
#' plot(sf_randomPoints)
#'
#' # Download via drive
#' sf_randomPoints_drive <- ee_as_sf(
#'   x = ee_randomPoints,
#'   via = 'drive'
#' )
#'
#' # Download via GCS
#' sf_randomPoints_gcs <- ee_as_sf(
#'   x = subset,
#'   via = 'gcs',
#'   container = 'rgee_dev'
#' )
#' }
#' @export
ee_as_sf <- function(x,
                     via = "getInfo",
                     container = "rgee_backup",
                     selectors = NULL,
                     quiet = FALSE) {

  sp_eeobjects <- ee_get_spatial_objects('Table')
  if (!any(class(x) %in% sp_eeobjects)) {
    stop("x is not a spatial vector Earth Engine object\n")
  }

  # Load ee_Initialize() session; just for either drive or gcs
  ee_path <- path.expand("~/.config/earthengine")
  ee_user <- read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )

  # Geometry or Feature --> FeatureCollection
  x_fc <- ee$FeatureCollection(x)

  if (via == "getInfo") {
    x_fc <- x_fc$getInfo()
    class(x_fc) <- "geo_list"
    geojson_sf(x_fc)
  } else if (via == "drive") {
    # Creating name for temporal file; just for either drive or gcs
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)

    # Getting table ID if it is exist
    table_id <- tryCatch(
      expr = parse_json(x$serialize())$
        scope[[1]][[2]][["arguments"]][["tableId"]],
      error = function(e) "no_tableid"
    )
    if (is.null(table_id)) {
      table_id <- "no_id"
    }
    file_name <- paste0(table_id, "_", time_format)

    # table to drive
    table_task <- ee_table_to_drive(
      collection = x_fc,
      description = ee_description,
      folder = container,
      fileNamePrefix = file_name,
      fileFormat = "GeoJSON",
      selectors = selectors
    )

    if (!quiet) {
      cat(
        "\n- download parameters (Google Drive)\n",
        "Table ID    :", table_id,"\n",
        "Google user :", ee_user$email,"\n",
        "Folder name :", container, "\n",
        "Date        :", time_format, "\n"
      )
    }
    table_task$start()
    ee_monitoring(task = table_task, quiet = quiet)
    ee_drive_to_local(table_task)
  } else if (via == 'gcs') {
    # Creating name for temporal file; just for either drive or gcs
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)

    # Getting table ID if it is exist
    table_id <- tryCatch(
      expr = parse_json(x$serialize())$
        scope[[1]][[2]][["arguments"]][["tableId"]],
      error = function(e) "no_id"
    )
    if (is.null(table_id)) {
      table_id <- "no_id"
    }

    file_name <- paste0(table_id, "_", time_format)

    # table to drive
    table_task <- ee_table_to_gcs(
      collection = x_fc,
      description = ee_description,
      bucket = container,
      fileNamePrefix = file_name,
      fileFormat = "GeoJSON",
      selectors = selectors
    )

    if (!quiet) {
      cat(
        "\n- download parameters (Google Drive)\n",
        "Table ID    :", table_id, "\n",
        "Google user :", ee_user$email, "\n",
        "Folder name :", container, "\n",
        "Date        :", time_format, "\n"
      )
    }
    table_task$start()
    ee_monitoring(task = table_task, quiet = quiet)
    ee_gcs_to_local(table_task)
  } else {
    stop("type argument invalid.")
  }
}
