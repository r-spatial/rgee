#' Convert an Earth Engine table in a sf object
#'
#' @param x Earth Engine table (ee$FeatureCollection) to be converted into a sf
#' object.
#' @param dsn Character. Output filename; in case \code{dsn} is missing
#' \code{ee_as_sf} will create a shapefile file in tmp() directory.
#' @param crs Integer or character. coordinate reference system
#' for the EE table. If is NULL, \code{ee_as_sf} will take the CRS of
#' the first element.
#' @param maxFeatures Numeric. The maximum allowed number of features to
#' export (ignore if \code{via} is not set as "getInfo"). The task will fail
#' if the exported region covers more features. Defaults to 5000.
#' @param overwrite Logical. Delete data source \code{dsn} before attempting
#' to write?.
#' @param via Character. Method to fetch data about the object. Multiple
#' options supported. See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignore if \code{via} is not defined as "drive" or
#' "gcs").
#' @param selectors The list of properties to include in the output, as a
#' list of strings or a comma-separated string. By default, all properties are
#' included.
#' @param quiet logical. Suppress info message
#' @importFrom methods as setMethod new is setGeneric
#' @details
#' \code{ee_as_sf} supports the download of \code{ee$FeatureCollection},
#' \code{ee$Feature} and \code{ee$Geometry} by three different options:
#' "getInfo", "drive", and "gcs". When "getInfo" is set in the \code{via}
#' argument, \code{ee_as_sf} will make an REST call to retrieve
#' all the known information about the object. The advantage of use
#' "getInfo" is a direct and faster download. However, there is a limitation of
#' 5000 features by request which makes it not recommendable for large
#' collections. Instead of "getInfo", the options: "drive" and "gcs" are
#' suitable for large collections since they use an intermediate container,
#' which may be Google Drive and Google Cloud Storage respectively. For getting
#'  more information about exporting data from Earth Engine, take a look at the
#' \href{https://developers.google.com/earth-engine/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @return An sf object.
#' @family vector download functions
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize(drive = TRUE, gcs = TRUE)
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
#' # sf_randomPoints_gcs <- ee_as_sf(
#' #   x = subset,
#' #   via = 'gcs',
#' #   container = 'rgee_dev' #GCS bucket name
#' # )
#' }
#' @export
ee_as_sf <- function(x,
                     dsn,
                     overwrite = TRUE,
                     crs = NULL,
                     via = "getInfo",
                     maxFeatures = 5000,
                     container = "rgee_backup",
                     selectors = NULL,
                     quiet = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }

  if (!requireNamespace("geojsonio", quietly = TRUE)) {
    stop("package geojsonio required, please install it first")
  }

  sp_eeobjects <- ee_get_spatial_objects('Table')

  if (missing(dsn)) {
    dsn <- paste0(tempfile(),".shp")
  }

  if (!any(class(x) %in% sp_eeobjects)) {
    stop("x is not a Earth Engine table\n")
  }

  # Load ee_Initialize() session; just for either drive or gcs
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  ee_user <- read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )

  # Geometry or Feature --> FeatureCollection
  x_fc <- ee$FeatureCollection(x)

  if (via == "getInfo") {
    fc_size <- 5000
    if (maxFeatures > 5000) {
      if (!quiet) {
        cat("Number of features: Calculating ...")
      }
      fc_size <- x_fc %>%
        ee$FeatureCollection$size() %>%
        ee$Number$getInfo()
      if (!quiet) {
        cat(sprintf("\rNumber of features: %s              \n", fc_size))
      }
      if (maxFeatures < fc_size) {
        stop(
          "Export too large. Specified ",
          fc_size,
          " features (max: ",
          maxFeatures,
          "). Specify a higher maxFeatures value",
          " if you intend to export a large area."
        )
      }
    }

    nbatch <- ceiling(fc_size / 5000)
    if (nbatch >= 3) {
      message(
        "Warning: getInfo is just for small tables (max: ",
        5000*3,
        "). Use 'drive' or 'gcs' instead for faster download."
      )
    }

    if (fc_size > 5000) {
      sf_list <- list()
      for (r_index in seq_len(nbatch)) {
        index <- r_index - 1
        if (!quiet) {
          cat(
            sprintf(
              "Getting data from the patch: %s/%s",
              r_index, nbatch
            ), "\n"
          )
        }
        x_eelist <- x_fc$toList(count = 5000, offset = 5000*index)
        x_fc_batch <- ee$FeatureCollection(x_eelist)
        sf_list[[r_index]] <- ee_fc_to_sf_getInfo(
          x_fc = x_fc_batch,
          overwrite = overwrite,
          maxFeatures = maxFeatures
        )
      }
      x_sf_mosaic <- do.call(rbind, sf_list)
      sf::st_write(x_sf_mosaic, dsn, delete_dsn = overwrite, quiet = TRUE)
      local_sf <- x_sf_mosaic
    } else {
      crs_sf <- x_fc %>%
        ee$FeatureCollection$geometry() %>%
        ee$Geometry$projection() %>%
        ee$Projection$wkt() %>%
        ee$String$getInfo()
      local_sf <- ee_fc_to_sf_getInfo(x_fc, dsn, maxFeatures, overwrite)
      suppressWarnings(sf::st_crs(local_sf) <- crs_sf)
    }
  } else if (via == "drive") {
    # Creating name for temporal file; just for either drive or gcs
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)

    # Getting table ID if it is exist
    table_id <- tryCatch(
      expr = jsonlite::parse_json(x$serialize())$
        scope[[1]][[2]][["arguments"]][["tableId"]],
      error = function(e) "no_tableid"
    )
    if (is.null(table_id)) {
      table_id <- "no_id"
    }
    file_name <- paste0(table_id, "_", time_format)

    # table to drive
    table_format <- ee_get_table_format(dsn)
    if (is.na(table_format)) {
      stop(
        'sf_as_ee(..., via = \"drive\"), only support the ',
        'following output format: "CSV", "GeoJSON", "KML", "KMZ", "SHP"',
        '. Use ee_table_to_drive and ee_drive_to_local to save in a TFRecord format.'
      )
    }

    table_task <- ee_table_to_drive(
      collection = x_fc,
      description = ee_description,
      folder = container,
      fileNamePrefix = file_name,
      fileFormat = table_format,
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

    if (table_task$status()$state != "COMPLETED") {
      stop(table_task$status()$error_message)
    }

    ee_drive_to_local(
      table_task,
      dsn = dsn,
      overwrite = overwrite,
      consider = 'all'
    )

    if (table_format == "CSV") {
      return(read.csv(dsn, stringsAsFactors = FALSE))
    } else {
      local_sf <- sf::read_sf(dsn, quiet = TRUE)
    }
  } else if (via == 'gcs') {
    # Creating name for temporal file; just for either drive or gcs
    time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
    ee_description <- paste0("ee_as_stars_task_", time_format)

    # Getting table ID if it is exist
    table_id <- tryCatch(
      expr = jsonlite::parse_json(x$serialize())$
        scope[[1]][[2]][["arguments"]][["tableId"]],
      error = function(e) "no_id"
    )
    if (is.null(table_id)) {
      table_id <- "no_id"
    }

    file_name <- paste0(table_id, "_", time_format)

    # table to gcs
    table_format <- ee_get_table_format(dsn)
    if (is.na(table_format)) {
      stop(
        'sf_as_ee(..., via = \"gcs\"), only support the ',
        'following output format: "CSV", "GeoJSON", "KML", "KMZ", "SHP"',
        '. Use ee_table_to_drive and ee_drive_to_local to save in a TFRecord format.'
      )
    }

    table_task <- ee_table_to_gcs(
      collection = x_fc,
      description = ee_description,
      bucket = container,
      fileNamePrefix = file_name,
      fileFormat = table_format,
      selectors = selectors
    )

    if (!quiet) {
      cat(
        "\n- download parameters (Google Cloud Storage)\n",
        "Table ID    :", table_id, "\n",
        "Google user :", ee_user$email, "\n",
        "Folder name :", container, "\n",
        "Date        :", time_format, "\n"
      )
    }

    table_task$start()
    ee_monitoring(task = table_task, quiet = quiet)
    if (table_task$status()$state != "COMPLETED") {
      stop(table_task$status()$error_message)
    }
    ee_gcs_to_local(task = table_task,dsn = dsn, overwrite = overwrite)
    if (table_format == "CSV") {
      return(read.csv(dsn, stringsAsFactors = FALSE))
    } else {
      local_sf <- sf::read_sf(dsn, quiet = TRUE)
    }
  } else {
    stop("via argument invalid.")
  }
  local_sf
}


#' Convert a FeatureCollection to sf via getInfo
#' @noRd
ee_fc_to_sf_getInfo <- function(x_fc, dsn, maxFeatures, overwrite = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  x_list <- tryCatch(
    expr = x_fc$getInfo(),
    error = function(e) {
        feature_len <- x_fc$size()$getInfo()
        stop(
          "Specify higher maxFeatures value if you",
          " intend to export a large area via getInfo.",
          "\nEntered: ", feature_len,
          "\nmaxFeatures: ", maxFeatures
        )
    }
  )
  class(x_list) <- "geo_list"
  x_sf <- geojsonio::geojson_sf(x_list, stringsAsFactors = FALSE)
  if (missing(dsn)) {
    x_sf
  } else {
    suppressWarnings(
      sf::write_sf(x_sf, dsn, delete_dsn = overwrite, quiet = TRUE)
    )
    x_sf
  }
}

#' Sync sf and ee drivers
#' @noRd
ee_get_table_format <- function(dsn) {
  table_format <- tolower(sub(".*([.*])", "\\1", basename(dsn)))
  if (length(table_format) != 1) {
    stop("dns must be a single-length character")
  }

  if (table_format == ".shp") {
    "SHP"
  } else if (table_format == ".geojson") {
    "GeoJSON"
  } else if (table_format == ".kml") {
    "KML"
  } else if (table_format == ".kmz") {
    "KMZ"
  } else if (table_format == ".csv") {
    "CSV"
  } else {
    NA
  }
}
