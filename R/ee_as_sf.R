#' Convert an Earth Engine table in an sf object
#'
#' @param x Earth Engine table (ee$FeatureCollection) to be converted in an sf
#' object.
#' @param dsn Character. Output filename. In case \code{dsn} is missing,
#' a shapefile is created in the \code{tmp()} directory.
#' @param overwrite Logical. Delete data source \code{dsn} before attempting
#' to write?.
#' @param via Character. Method to export the image. Three method are
#' implemented: "getInfo", "drive", "gcs". See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignore if \code{via} is not defined as "drive" or
#' "gcs").
#' @param crs Integer or Character. Coordinate Reference System (CRS)
#' for the EE table. If it is NULL, \code{ee_as_sf} will take the CRS of
#' the first element.
#' @param maxFeatures Numeric. The maximum allowed number of features to
#' export (ignore if \code{via} is not set as "getInfo"). The task will fail
#' if the exported region covers more features than the specified in
#' \code{maxFeatures}. Defaults to 5000.
#' @param selectors The list of properties to include in the output, as a
#' list/vector of strings or a comma-separated string. By default, all properties are
#' included.
#' @param lazy Logical. If TRUE, a \code{\link[future:sequential]{
#' future::sequential}} object is created to evaluate the task in the future.
#' Ignore if \code{via} is set as "getInfo". See details.
#' @param public Logical. If TRUE, a public link to the file is created.
#' See details.
#' @param add_metadata Add metadata to the sf object. See details.
#' @param timePrefix Logical. Add current date and time (\code{Sys.time()}) as
#' a prefix to export files. This parameter helps to avoid exported files
#' with the same name. By default TRUE.
#' @param quiet logical. Suppress info message.
#' @importFrom methods as setMethod new is setGeneric
#' @details
#' \code{ee_as_sf} supports the download of \code{ee$Geometry}, \code{ee$Feature},
#' and \code{ee$FeatureCollection} by three different options:
#' "getInfo" (which make an REST call to retrieve the data), "drive"
#' (which use \href{https://CRAN.R-project.org/package=googledrive}{Google Drive})
#' and "gcs" (which use \href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' Google Cloud Storage}). The advantage of use "getInfo" is a
#' direct and faster download. However, there is a limitation of 5000 features by
#' request, making it not recommendable for large FeatureCollection. Instead of
#' "getInfo", the options: "drive" and "gcs" are suitable for large FeatureCollections
#' due to the use of an intermediate container. When via is set as "drive" or "gcs"
#' \code{ee_as_sf} perform the following steps:
#' \itemize{
#'   \item{1. }{A task is started (i.e., \code{ee$batch$Task$start()}) to
#'   move the EE Table from Earth Engine to the file storage system (Google Drive
#'   or Google Cloud Storage) specified in the argument \code{via}.}
#'   \item{2. }{If the argument \code{lazy} is TRUE, the task will not be
#'   monitored. This is useful to lunch several tasks simultaneously and
#'   calls them later using \code{\link{ee_utils_future_value}} or
#'   \code{\link[future:value]{future::value}}. At the end of this step,
#'   the EE Table is stored on the path specified in the argument
#'   \code{dsn}.}
#'   \item{3. }{Finally, if the argument \code{add_metadata} is TRUE, a list
#'   with the following elements is added to the sf object.
#'   \itemize{
#'     \item{\bold{if via is "drive":}}
#'       \itemize{
#'         \item{\bold{ee_id: }}{Name of the Earth Engine task.}
#'         \item{\bold{drive_name: }}{Name of the Table in Google Drive.}
#'         \item{\bold{drive_id: }}{Id of the Table in Google Drive.}
#'         \item{\bold{drive_download_link: }}{Download link to the table.}
#'     }
#'   }
#'   \itemize{
#'     \item{\bold{if via is "gcs":}}
#'       \itemize{
#'         \item{\bold{ee_id: }}{Name of the Earth Engine task.}
#'         \item{\bold{gcs_name: }}{Name of the Table in Google Cloud Storage.}
#'         \item{\bold{gcs_bucket: }}{Name of the bucket.}
#'         \item{\bold{gcs_fileFormat: }}{Format of the table.}
#'         \item{\bold{gcs_public_link: }}{Download link to the table.}
#'         \item{\bold{gcs_URI: }}{gs:// link to the table.}
#'     }
#'   }
#'   Run \code{attr(sf, "metadata")} to get the list.
#'  }
#' }
#'
#' For getting more information about exporting data from Earth Engine, take
#' a look at the
#' \href{https://developers.google.com/earth-engine/guides/exporting}{Google
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
#' sf_randomPoints_gcs <- ee_as_sf(
#'  x = subset,
#'  via = 'gcs',
#'  container = 'rgee_dev' #GCS bucket name
#' )
#' }
#' @export
ee_as_sf <- function(x,
                     dsn,
                     overwrite = TRUE,
                     via = "getInfo",
                     container = "rgee_backup",
                     crs = NULL,
                     maxFeatures = 5000,
                     selectors = NULL,
                     lazy = FALSE,
                     public = TRUE,
                     add_metadata = TRUE,
                     timePrefix = TRUE,
                     quiet = FALSE) {
  #check packages
  ee_check_packages("ee_as_sf", c("sf", "geojsonio"))

  # Is  a geometry, feature, or fc?
  sp_eeobjects <- ee_get_spatial_objects('Table')
  if (!any(class(x) %in% sp_eeobjects)) {
    stop("x is not a Earth Engine table\n")
  }

  # From ee$Geometry or ee$Feature to ee$FeatureCollection
  x_fc <- ee$FeatureCollection(x)

  # Getting image ID if it is exist
  # table_id is the name of the table in the container
  if (missing(dsn)) {
    table_id <- tryCatch(
      expr = {
        x %>%
          ee$FeatureCollection$get("system:id") %>%
          ee$ComputedObject$getInfo() %>%
          basename()
      }, error = function(e) "no_tableid"
    )
    if (is.null(table_id)) {
      table_id <- "no_tableid"
    }
    dsn <- sprintf("%s/%s.geojson",tempdir(), table_id)
  } else {
    table_id <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(dsn))
  }

  # Have you loaded the necessary credentials?
  # Only important for drive or gcs.
  ee_user <- ee_exist_credentials()

  if (via == "getInfo") {
    # Create a sf object using ee$FeatureCollection$getInfo method
    ee_fc_to_sf_getInfo_batch(
      x_fc = x_fc,
      dsn = dsn,
      maxFeatures = maxFeatures,
      overwrite = overwrite,
      quiet = quiet
    )
  } else if (via == "drive") {
    # From Earth Engine to drive
    table_task <- ee_init_task_drive_fc(
      x_fc = x_fc,
      dsn = dsn,
      container = container,
      table_id = table_id,
      ee_user = ee_user,
      selectors =  selectors,
      timePrefix = timePrefix,
      quiet = quiet
    )

    if(lazy) {
      prev_plan <- future::plan(future::sequential, .skip = TRUE)
      on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
      future::future({
        ee_create_credentials_drive(ee_user$email)
        # From googledrive to the client-side
        ee_sf_drive_local(
          table_task = table_task,
          dsn = dsn,
          metadata = add_metadata,
          public = public,
          overwrite = overwrite,
          quiet = quiet
        )
      }, lazy = TRUE)
    } else {
      # From googledrive to the client-side
      ee_sf_drive_local(
        table_task = table_task,
        dsn = dsn,
        metadata = add_metadata,
        public = public,
        overwrite = overwrite,
        quiet = quiet
      )
    }
  } else if (via == 'gcs') {
    # From Earth Engine to gcs
    table_task <- ee_init_task_gcs_fc(
      x_fc = x_fc,
      dsn = dsn,
      container = container,
      table_id = table_id,
      ee_user = ee_user,
      selectors =  selectors,
      timePrefix = timePrefix,
      quiet = quiet
    )

    if(lazy) {
      prev_plan <- future::plan(future::sequential, .skip = TRUE)
      on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
      future::future({
        ee_create_credentials_gcs(ee_user$email)
        # From gcs to the client-side
        ee_sf_gcs_local(
          table_task = table_task,
          dsn = dsn,
          metadata = add_metadata,
          public = public,
          overwrite = overwrite,
          quiet = quiet
        )
      }, lazy = TRUE)
    } else {
      # From gcs to the client-side
      ee_sf_gcs_local(
        table_task = table_task,
        dsn = dsn,
        metadata = add_metadata,
        public = public,
        overwrite = overwrite,
        quiet = quiet
      )
    }
  } else {
    stop("via argument invalid.")
  }
}


#' Convert a FeatureCollection to sf via getInfo (support batch)
#' @noRd
ee_fc_to_sf_getInfo_batch <- function(x_fc, dsn, maxFeatures, overwrite, quiet) {
  # fc_size is the number of elements in the collection
  # If the users does not change the maxFeatures argument
  # by a value greater than 5000 rgee assume a initial value
  # of 5000 for fc_size (5000 is the maximum number of elements to download
  # using getInfo).
  fc_size <- 5000

  # If maxFeatures is greather than 5000 estimate the number of elements.
  if (maxFeatures > 5000) {
    if (!quiet) {
      cat("Number of features: Calculating ...")
    }
    # get the number of features
    fc_size <- x_fc %>%
      ee$FeatureCollection$size() %>%
      ee$Number$getInfo()
    if (!quiet) {
      cat(sprintf("\rNumber of features: %s \n", fc_size))
    }
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

  # Only three batches it is recommended with getInfo. If not display this warning.
  nbatch <- ceiling(fc_size / 5000)
  if (nbatch >= 3) {
    message(
      "Warning: getInfo is just for small tables (max: ",
      5000*3,
      "). Use 'drive' or 'gcs' instead for a faster download."
    )
  }

  if (fc_size > 5000) {
    # If the number of elements is greater than 5000 downloads by batches
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
      if (r_index == 1) {
        # Estimate the CRS for the first element (we suppose that all the
        # features have the same CRS).
        crs_sf <- x_fc %>%
          ee$FeatureCollection$geometry() %>%
          ee$Geometry$projection() %>%
          ee$Projection$wkt() %>%
          ee$String$getInfo()
      }
      # Split the Feature on batches of 5000 elements.
      x_fc_batch <- ee$FeatureCollection(x_fc) %>%
        ee$FeatureCollection$toList(count = 5000, offset = 5000*index) %>%
        ee$FeatureCollection()

      # ee_fc_to_sf_getInfo is a auxiliary function that using geojsonio
      # convert a ee$FeatureCollection -> list -> sf object.
      sf_list[[r_index]] <- ee_fc_to_sf_getInfo(
        x_fc = x_fc_batch,
        overwrite = overwrite,
        maxFeatures = maxFeatures,
        dsn = dsn
      )
    }

    # the final sf object should have the same CRS than the ee$FeatureCollection object.
    local_sf <- do.call(rbind, sf_list)
    suppressWarnings(sf::st_crs(local_sf) <- crs_sf)

    # If dsn is not NULL write the sf object in their system
    if (is.null(dsn)) {
      suppressWarnings(
        sf::st_write(local_sf, dsn, delete_dsn = overwrite, quiet = TRUE)
      )
    }
    local_sf
  } else {
    crs_sf <- x_fc %>%
      ee$FeatureCollection$geometry() %>%
      ee$Geometry$projection() %>%
      ee$Projection$wkt() %>%
      ee$String$getInfo()
    local_sf <- ee_fc_to_sf_getInfo(x_fc, dsn, maxFeatures, dsn = dsn, overwrite)
    suppressWarnings(sf::st_crs(local_sf) <- crs_sf)
    local_sf
  }
}

#' Convert a FeatureCollection to sf via getInfo
#' @noRd
ee_fc_to_sf_getInfo <- function(x_fc, dsn, maxFeatures, dns = NULL, overwrite = TRUE) {
  # check packages
  ee_check_packages("ee_fc_to_sf_getInfo", "sf")

  x_list <- tryCatch(
    expr = ee$FeatureCollection$getInfo(x_fc),
    error = function(e) {
      feature_len <- ee$FeatureCollection$size(x_fc) %>%
        ee$Number$getInfo()
      stop(
        "Specify higher maxFeatures value if you",
        " intend to export a large area via getInfo.",
        "\nEntered: ", feature_len,
        "\nmaxFeatures: ", maxFeatures
      )
    }
  )

  # Remove system:index (id camp)
  x_list$features <-lapply(
    X = x_list$features,
    FUN = function(x) {
      x$id = NULL
      x
    }
  )

  class(x_list) <- "geo_list"
  x_sf <- geojsonio::geojson_sf(x_list, stringsAsFactors = FALSE)
  if (missing(dsn)) {
    x_sf
  } else {
    if (is.null(dns)) {
      suppressWarnings(
        sf::write_sf(x_sf, dsn, delete_dsn = overwrite, quiet = TRUE)
      )
    }
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

#' Create a Export task to GD
#' @noRd
ee_init_task_drive_fc <- function(x_fc, dsn, container, table_id,
                                  ee_user, selectors, timePrefix, quiet) {
  # Create description (Human-readable name of the task)
  # Relevant for either drive or gcs.
  time_format <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  ee_description <- paste0("rgeeTable_", time_format)
  if (timePrefix) {
    file_name <- paste0(table_id, "_", time_format)
  } else {
    file_name <- table_id
  }

  # Are GD credentials loaded?
  if (is.na(ee_user$drive_cre)) {
    drive_credential <- ee_create_credentials_drive(ee_user$email)
    ee_save_credential(pdrive = drive_credential)
    # ee_Initialize(user = ee_user$email, drive = TRUE)
    message(
      "\nNOTE: Google Drive credentials were not loaded.",
      " Running ee_Initialize(user = '", ee_user$email, "', drive = TRUE)",
      " to fix."
    )
  }

  # The file format specified in dsn exist and it is suppoted by GEE?
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
    selectors = selectors,
    timePrefix =  FALSE
  )

  if (!quiet) {
    cat(
      "\n- download parameters (Google Drive)\n",
      "Table ID    :", table_id,"\n",
      "Google user :", ee_user[["email"]],"\n",
      "Folder name :", container, "\n",
      "Date        :", time_format, "\n"
    )
  }
  ee$batch$Task$start(table_task)
  table_task
}

#' from drive to local
#' @noRd
ee_sf_drive_local <- function(table_task, dsn, metadata, public, overwrite, quiet) {
  ee_monitoring(task = table_task, quiet = quiet)

  if (ee$batch$Task$status(table_task)[["state"]] != "COMPLETED") {
    stop(ee$batch$Task$status(table_task)[["error_message"]])
  }

  local_files <- ee_drive_to_local(
    task = table_task,
    dsn = dsn,
    overwrite = overwrite,
    consider = 'all',
    metadata = metadata,
    public = public,
    quiet = quiet
  )
  if (is.character(local_files)) {
    local_files <- list(dsn = local_files)
  }

  # The file format specified in dsn exist and it is supported by GEE?
  table_format <- ee_get_table_format(dsn)
  if (is.na(table_format)) {
    stop(
      'sf_as_ee(..., via = \"gcs\"), only support the ',
      'following output format: "CSV", "GeoJSON", "KML", "KMZ", "SHP"',
      '. Use ee_table_to_drive and ee_drive_to_local to save in a TFRecord format.'
    )
  }

  if (table_format == "CSV") {
    local_files
  } else {
    local_sf <- sf::read_sf(dsn, quiet = TRUE)
    attr(local_sf, "metadata") <- local_files
    local_sf
  }
}

#' Create a Export task to GCS
#' @noRd
ee_init_task_gcs_fc <- function(x_fc, dsn, container, table_id,
                                ee_user, selectors, timePrefix, quiet) {
  # Create description (Human-readable name of the task)
  # Relevant for either drive or gcs.
  time_format <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  ee_description <- paste0("rgeeTable_", time_format)
  if (timePrefix) {
    file_name <- paste0(table_id, "_", time_format)
  } else {
    file_name <- table_id
  }

  # Are GCS credentials loaded?
  if (is.na(ee_user$gcs_cre)) {
    gcs_credential <- ee_create_credentials_gcs(ee_user$email)
    ee_save_credential(pgcs = gcs_credential$path)
    message(
      "\nGoogle Cloud Storage credentials were not loaded.",
      " Running ee_Initialize(user = '", ee_user$email, "', gcs = TRUE)",
      " to fix."
    )
  }


  # The file format specified in dsn exist and it is supported by GEE?
  table_format <- ee_get_table_format(dsn)
  if (is.na(table_format)) {
    stop(
      'sf_as_ee(..., via = \"drive\"), only support the ',
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
    selectors = selectors,
    timePrefix =  FALSE
  )

  if (!quiet) {
    cat(
      "\n- download parameters (Google Drive)\n",
      "Table ID    :", table_id,"\n",
      "Google user :", ee_user[["email"]],"\n",
      "Folder name :", container, "\n",
      "Date        :", time_format, "\n"
    )
  }
  ee$batch$Task$start(table_task)
  table_task
}

#' from GCS to local
#' @noRd
ee_sf_gcs_local <- function(table_task, dsn, metadata, public, overwrite, quiet) {
  ee_monitoring(task = table_task, quiet = quiet)

  if (ee$batch$Task$status(table_task)[["state"]] != "COMPLETED") {
    stop(ee$batch$Task$status(table_task)[["error_message"]])
  }

  local_files <- ee_gcs_to_local(
    task = table_task,
    dsn = dsn,
    metadata = metadata,
    public = public,
    overwrite = overwrite,
    quiet = quiet
  )
  if (is.character(local_files)) {
    local_files <- list(dsn = local_files)
  }

  # The file format specified in dsn exist and it is supported by GEE?
  table_format <- ee_get_table_format(dsn)
  if (is.na(table_format)) {
    stop(
      'sf_as_ee(..., via = \"gcs\"), only support the ',
      'following output format: "CSV", "GeoJSON", "KML", "KMZ", "SHP"',
      '. Use ee_table_to_drive and ee_drive_to_local to save in a TFRecord format.'
    )
  }

  if (table_format == "CSV") {
    local_files
  } else {
    # Read sf and load metadata
    local_sf <- sf::read_sf(dsn, quiet = TRUE)
    attr(local_sf, "metadata") <- local_files
    local_sf
  }
}
