#' Monitoring Earth Engine task progress
#'
#' @param task List generated after an created an EE task.
#' @param eeTaskList Logical. If \code{TRUE}, all Earth Engine tasks will be
#' listed.
#' @param quiet Logical. Suppress info message
#'
#' @family helper functions
#' @export
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#' ee_monitoring(eeTaskList = TRUE)
#' }
#' @export
ee_monitoring <- function(task, eeTaskList = FALSE, quiet = FALSE) {
  if (missing(task)) {
    task <- ee$batch$Task$list()[[1]]
  }
  if (eeTaskList) {
    if (!quiet) {
      cat("EETaskList:\n")
    }
    task_list <- mapply(function(x) {
      sprintf("<Task %s: %s (%s)>", x$task_type, x$config, x$state)
    }, ee$batch$Task$list())
    if (!quiet) {
      cat("", paste0(task_list, "\n"))
      cat("\n")
    }
  }
  while (task$active() & task$state != "CANCEL_REQUESTED") {
    if (!quiet) {
      cat(sprintf("Polling for task (id: %s).\n", task$id))
    }
    Sys.sleep(5)
  }
  if (!quiet) {
    cat(sprintf("State: %s\n", task$status()$state))
  }
  if (task$status()$state != "COMPLETED") {
    message(
      "ERROR in Earth Engine servers: ",
      task$status()$error_message
    )
    stop("ee_monitoring was forced to stop before getting results")
  }
}

#' Sort google drives files
#' @noRd
sort_drive_files <- function(drive_files, fileformat) {
  if (fileformat == "SHP") {
    shp_file <- grep(
      pattern = "(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)",
      x = drive_files[["name"]]
    )
    selected_drive_files <- drive_files[shp_file, ]
    drive_files_sort <- selected_drive_files[order(selected_drive_files$name), ]
  } else {
    drive_files_sort <- drive_files[order(drive_files[["name"]]), ]
  }
  drive_files_sort
}

#' Sort local files
#' @noRd
ee_sort_localfiles <- function(filenames, fileformat) {
  if (fileformat == "SHP") {
    shp_file <- grep("(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)", filenames)
    shp_file <- filenames[shp_file]
    shp_file[order(shp_file)]
  } else {
    filenames[order(filenames)]
  }
}


#' GCS or Google Drive Exist credentials?
#' @noRd
ee_exist_credentials <- function() {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )
}



#' Fix offset of stars object
#' @noRd
ee_fix_offset <- function(img_transform, sf_region) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  if (all(img_transform %in% c(1, 0, 0, 0, 1, 0))) {
    sf::st_bbox(sf_region)
  } else {
    rectangle_coord <- sf::st_coordinates(sf_region)
    # image spatial parameters
    img_x_scale <- img_transform[1][[1]]
    img_x_offset <- img_transform[3][[1]]
    img_y_scale <- img_transform[5][[1]]
    img_y_offset <- img_transform[6][[1]]
    # X offset fixed
    sf_x_min <- min(rectangle_coord[, "X"])
    x_min <- ee_fix_x_coord(img_x_offset, sf_x_min, img_x_scale, option = 'min')
    sf_x_max <- max(rectangle_coord[, "X"])
    x_max <- ee_fix_x_coord(img_x_offset, sf_x_max, img_x_scale, option = 'max')

    # Y offset fixed
    sf_y_min <- min(rectangle_coord[, "Y"])
    y_min <- ee_fix_y_coord(img_y_offset, sf_y_min, img_y_scale, option = 'min')
    sf_y_max <- max(rectangle_coord[, "Y"])
    y_max <- ee_fix_y_coord(img_y_offset, sf_y_max, img_y_scale, option = 'max')
    c(xmin = x_min, ymin = y_min, xmax = x_max, ymax = y_max)
  }
}

#' Fix x coordinates
#' @noRd
ee_fix_x_coord <- function(img_offset, sf_offset, scale, option) {
  # fix the offset
  if (img_offset <= sf_offset) {
    if (option == "min") {
      n <- floor(abs((img_offset - sf_offset)/scale))
    } else if (option == "max") {
      n <- ceiling(abs((img_offset - sf_offset)/scale))
    }
    img_offset + n * scale
  } else {
    n <- ceiling(abs((img_offset - sf_offset)/scale))
    img_offset - n * scale
  }
}


#' Fix y coordinates
#' @noRd
ee_fix_y_coord <- function(img_offset, sf_offset, scale, option) {
  # fix the offset
  if (img_offset > sf_offset) {
    if (option == "min") {
      n <- ceiling(abs((sf_offset - img_offset)/scale))
    } else if (option == "max") {
      n <- floor(abs((sf_offset - img_offset)/scale))
    }
    img_offset + n * scale
  } else {
    n <- ceiling(abs((sf_offset - img_offset)/scale))
    img_offset - n * scale
  }
}

#' Set crs and band names
#' @noRd
set_crs <- function(image_stars, prj_image, band_names) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  if (!requireNamespace("stars", quietly = TRUE)) {
    stop("package stars required, please install it first")
  }
  img_crs <- as.numeric(gsub("EPSG:", "", prj_image$crs))
  sf::st_crs(image_stars) <- img_crs
  if (length(band_names) > 1) {
    stars::st_set_dimensions(image_stars, 3, values = band_names)
  } else {
    image_stars <- stars::st_set_dimensions(image_stars, "bands")
    attr(image_stars, "dimensions")$bands$to <- 1
    stars::st_set_dimensions(image_stars, 3, values = band_names)
  }
}

#' type of an Earth Engine Image
#' @noRd
ee_get_typeimage_size <- function(type) {
  if (type == "int") {
    32
  } else if (type == "INT") {
    32
  } else if (type == "double") {
    64
  } else if (type == "float") {
    64
  } else if (type == "int8") {
    8
  } else if (type == "int16") {
    16
  } else if (type == "int32") {
    32
  } else {
    32
  }
}
