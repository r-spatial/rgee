#' Pass R date object to Earth Engine
#'
#' Function to pass objects of classes "Date",
#' "character", "POSIXt", and "POSIXct" to
#' ee$Date or unix time stamp.
#'
#' @param date R date object
#' @param eeobject Logical. If TRUE, return
#' a ee$object otherwise return the date in milliseconds.
#' @examples
#' library(rgee)
#' ee_reattach()
#' ee_Initialize()
#' rdate_to_eedate('2000-01-01')
#' rdate_to_eedate(315532800000) # float number
#' @export
rdate_to_eedate <- function(date, eeobject = TRUE) {
  condition <- any(class(date) %in% c('Date', 'character', 'POSIXt', 'POSIXct'))
  if (condition) {
    date <- date %>%
      as.POSIXct(origin = "1970-01-01", tz = "GMT") %>%
      as.numeric() %>%
      prod(1000)
  }

  if (isTRUE(eeobject)) {
    ee$Date(date)
  } else {
    date
  }
}

#' Pass Earth Engine date object to R
#'
#' Function to pass objects of class ee$Date
#' to as.POSIXct or numeric (unix time stamp)
#'
#' @param ee_date EE date object (ee$Date)
#' @param js Logical. If TRUE, return a numeric
#' date otherwise return the date as a POSIXct object.
#' @examples
#' library(rgee)
#' ee_reattach()
#' ee_Initialize()
#' eeDate <- ee$Date$fromYMD(1980,1,1)
#' eedate_to_rdate(eeDate)
#' @export
eedate_to_rdate <- function(ee_date, js = FALSE) {
  oauth_func_path <- system.file("python/ee_selenium_functions.py",
                                 package = "rgee")
  ee_selenium_functions <- ee_source_python(oauth_func_path)
  date_numeric <- ee_selenium_functions$eedate_to_rdate(ee$Date(ee_date)) %>%
    ee_py_to_r()
  if (isTRUE(js)) {
    date_numeric
  } else {
    as.POSIXct(x = date_numeric/1000, origin = "1970-01-01", tz = "GMT")
  }
}

#' Get the date of a Earth Engine Image
#'
#' @param image The Earth Engine Image
#' @param time_end Logical. If TRUE, the
#' system:time_end property will also be returned.
#' See details.
#' @details
#' The properties system:time_start and system:time_end represent the
#' image dates based on a timestamp in milliseconds
#' since the UNIX epoch. The system:time_start set the
#' start period while system:time_end the end period. See
#' this \href{https://developers.google.com/earth-engine/glossary}{link}
#' for more information.
#' @examples
#' library(rgee)
#' ee_Initialize()
#' ee_reattach()
#' l8 <- ee$Image('LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318')
#' ee_get_date(l8)
#' srtm <- ee$Image('CGIAR/SRTM90_V4')
#' ee_get_date(srtm, time_end = TRUE)
#' @export
ee_get_date <- function(image, time_end = FALSE) {
  time_start <- tryCatch(
    expr = eedate_to_rdate(image$get('system:time_start')),
    error = function(e) NA
  )
  if (isTRUE(time_end)) {
    time_end <- tryCatch(
      expr = eedate_to_rdate(image$get('system:time_end')),
      error = function(e) NA
    )
  } else {
    time_end <- NULL
  }
  c('system:time_start' = time_start,'system:time_end'=time_end)
}
