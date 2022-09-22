#' Get the Asset home name
#' @family path utils
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#' ee_get_assethome()
#' }
#' @return Character. The name of the Earth Engine Asset home
#' (e.g. users/datacolecfbf)
#' @export
ee_get_assethome <- function() {
  options('rgee.ee_user')[[1]]
}

#' Get the date of a EE Image
#'
#' @param x ee$Image or ee$ImageCollection object
#' @param time_end Logical. If TRUE, the \code{system:time_end} property is
#' also returned. See details.
#'
#' @details
#' \code{system:time_start} sets the start period of data acquisition while
#' \code{system:time_end} does the same for the end period. See the
#' \href{https://developers.google.com/earth-engine/glossary/}{Earth Engine glossary}
#' for getting more information.
#'
#' @return An List object with the elements: id, time_start and time_end
#' (only if the \code{time_end} argument is TRUE).
#' @family date functions
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' l8 <- ee$Image('LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318')
#' ee_get_date_img(l8)
#' srtm <- ee$Image('CGIAR/SRTM90_V4')
#' ee_get_date_img(srtm, time_end = TRUE)
#' }
#' @export
ee_get_date_img <- function(x, time_end = FALSE) {
  time_start <- tryCatch(
    expr =   x %>%
      ee$Image$get("system:time_start") %>%
      eedate_to_rdate(),
    error = function(e) NA
  )
  if (isTRUE(time_end)) {
    time_end <- tryCatch(
      expr = x %>%
        ee$Image$get("system:time_end") %>%
        eedate_to_rdate(),
      error = function(e) NA
    )
  } else {
    time_end <- NULL
  }
  # get id of the image
  image_id <- x %>%
    ee$Image$get("system:id") %>%
    ee$ComputedObject$getInfo()

  if (is.null(image_id)) {
    image_id <- "no_id"
  }
  if (is.null(time_end)) {
    list(image_id = image_id, time_start = time_start)
  } else {
    list(image_id = image_id, time_start = time_start, time_end = time_end)
  }
}

#' Get the date of a EE ImageCollection
#'
#' @param x ee$ImageCollection object
#' @param time_end Logical. If TRUE, the \code{system:time_end} property is
#' also returned. See details.
#'
#' @details
#' \code{system:time_start} Sets the start period of data acquisition while
#' \code{system:time_end} does the same for the end period. See the
#' \href{https://developers.google.com/earth-engine/glossary/}{Earth Engine glossary}
#' for getting more information.
#'
#' @return A data.frame with the columns: \code{id} (ID of the image),
#' \code{time_start}, and \code{time_end} (only if the argument \code{time_end} is
#' set as TRUE). The number of rows depends on the number of images
#' (\code{ee$ImageCollection$size}).
#'
#' @family date functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
#'   st_transform(4326) %>%
#'   sf_as_ee()
#'
#' ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
#'   filterDate("2016-01-01", "2016-01-31")$
#'   filterBounds(nc)
#'
#' ee_get_date_ic(ee_s2)
#' }
#' @export
ee_get_date_ic <- function(x, time_end = FALSE) {
  # Call Python module
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils <- ee_source_python(oauth_func_path)

  # Fetch the time_start of each Image
  time_start <- ee_utils_py_to_r(
    ee_utils$eedate_to_rdate_ic(x, "system:time_start")
  )

  # Getting ID
  image_id <- x$aggregate_array("system:id")$getInfo()
  if (is.null(image_id)) {
    image_id <- rep("no_id", x$size()$getInfo())
  }
  if (length(image_id) == 0) {
    image_id <- rep("no_id", x$size()$getInfo())
  }


  if (is.null(time_start)) {
    time_start <- NA
  } else {
    time_start <- as.POSIXct(
      x = time_start / 1000,
      origin = "1970-01-01",
      tz = "GMT"
    )
  }

  #  Stop process if time_start and image_id do not have the same length
  if (length(time_start) != length(image_id)) {
    stop(
      "Imposible to create a data.frame with:\n",
      "system:id -> ", length(time_start),
      sprintf(" [%s, ...., %s]", time_start[1], time_start[length(time_start)]),
      "\nsystem:time_stars -> ", length(image_id),
      sprintf(" [%s, ...., %s]", image_id[1], image_id[length(image_id)])
    )
  }
  # Getting time_end
  if (!time_end) {
    return(
      data.frame(
        id = image_id,
        time_start = time_start,
        stringsAsFactors = FALSE
      )
    )
  } else {
    time_end <- ee_utils_py_to_r(
      ee_utils$eedate_to_rdate_ic(x, "system:time_end")
    )
    if (is.null(time_end)) {
      time_end <- rep(NA, length(time_start))
    }
    time_end <- as.POSIXct(
      x = time_end / 1000,
      origin = "1970-01-01",
      tz = "GMT"
    )
    data.frame(
      id = image_id,
      time_start = time_start,
      time_end = time_end,
      stringsAsFactors = FALSE
    )
  }
}

#' Get the path where the credentials are stored
#'
#' @family path utils
#' @return A character that represents the path credential of a specific
#' user
#'
#' @export
ee_get_earthengine_path <- function() {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())

  sessioninfo <- sprintf(
    "%s/rgee_sessioninfo.txt",
    ee_utils_py_to_r(utils_py$ee_path())
  )
  if (file.exists(sessioninfo)) {
    user <- read.table(sessioninfo,
                       header = TRUE,
                       stringsAsFactors = FALSE
    )[[1]]
    if (is.na(user)) {
      stop("rgee_sessioninfo.txt malformed")
    }
  } else {
    stop(
      "rgee_sessioninfo.txt does not exist, ",
      "run rgee::ee_Initialize() to fixed."
    )
  }
  return(sprintf("%s/%s/", ee_path, user))
}



#' Return the element at a specified position in an Earth Engine Image or ImageCollection (See rgeeExtra for an updated version)
#'
#' @param ee_c ImageCollection or FeatureCollection.
#' @param index Numeric. Specified position.
#' @return Depending of \code{ee_c} can return either an \code{ee$FeatureCollection}
#' or \code{ee$ImageCollection}.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#'
#' ee_Initialize()
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
#'   st_transform(4326) %>%
#'   sf_as_ee()
#'
#' ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
#'   filterDate("2016-01-01", "2016-01-31")$
#'   filterBounds(nc)
#'
#' ee_s2$size()$getInfo() # 126
#'
#' # Get the first 5 elements
#' ee_get(ee_s2, index = 0:4)$size()$getInfo() # 5
#' }
#' @noRd
ee_get <- function(ee_c, index = 0) {
  is_consecutive <- all(diff(index) == 1)
  if (any(index < 0)) {
    stop("index must be a positive value")
  }
  if (any(class(ee_c) %in%  c("ee.imagecollection.ImageCollection"))) {
    # Index is a single value?
    if (length(index) == 1) {
      ee_c %>%
        rgee::ee$ImageCollection$toList(count = 1, offset = index) %>%
        rgee::ee$ImageCollection()
    } else {
      # Index is a n-length vector and consecutive?
      if (length(index) > 1 & is_consecutive) {
        ee_c %>%
          rgee::ee$ImageCollection$toList(count = length(index), offset = min(index)) %>%
          rgee::ee$ImageCollection()
      } else {
        ee_c_r <- list()
        counter <- 1
        for (i in index) {
          ee_c_r[[counter]] <- ee_c %>%
            rgee::ee$ImageCollection$toList(count = 1, offset = i) %>%
            rgee::ee$ImageCollection() %>%
            rgee::ee$ImageCollection$first()
          counter <- counter + 1
        }
        ee$ImageCollection(ee_c_r)
      }
    }
  } else  if (any(class(ee_c) %in%  c("ee.featurecollection.FeatureCollection"))) {
    # Index is a single value?
    if (length(index) == 1) {
      ee_c %>%
        rgee::ee$FeatureCollection$toList(count = 1, offset = index) %>%
        rgee::ee$FeatureCollection()
    } else {
      # Index is a n-length vector and consecutive?
      if (length(index) > 1 & is_consecutive) {
        ee_c %>%
          rgee::ee$FeatureCollection$toList(count = length(index), offset = min(index)) %>%
          rgee::ee$FeatureCollection()
      } else {
        ee_c_r <- list()
        counter <- 1
        for (i in index) {
          ee_c_r[[counter]] <- ee_c %>%
            rgee::ee$FeatureCollection$toList(count = 1, offset = i) %>%
            rgee::ee$FeatureCollection() %>%
            rgee::ee$FeatureCollection$first()
          counter <- counter + 1
        }
        ee$FeatureCollection(ee_c_r)
      }
    }
  } else {
    stop("ee_get only support objects of class FeatureCollection and ImageCollection.",
         "\nEnter: ", class(ee_c)[1],
         "\nExpected: ee$ImageCollection or ee$FeatureCollection")
  }
}
