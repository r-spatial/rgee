#' Pass an R date object to Earth Engine
#'
#' Pass an R date object ("Date", "Numeric", "character", "POSIXt",
#' and "POSIXct") to Google Earth Engine (ee$Date).
#'
#' @param date R date object
#' @param timestamp Logical. If TRUE, return the date in milliseconds
#' from the Unix Epoch (1970-01-01 00:00:00 UTC) otherwise return a
#' EE date object. By default FALSE.
#' @examples
#' library(rgee)
#' ee_reattach()
#' ee_Initialize()
#' rdate_to_eedate('2000-01-01')
#' rdate_to_eedate(315532800000) # float number
#' @export
rdate_to_eedate <- function(date, timestamp = FALSE) {
  condition <- any(class(date) %in% c('Date', 'character', 'POSIXt', 'POSIXct'))
  if (condition) {
    date <- date %>%
      as.POSIXct(origin = "1970-01-01", tz = "GMT") %>%
      as.numeric() %>%
      prod(1000)
  }
  if (isTRUE(timestamp)) {
    date
  } else {
    ee$Date(date)
  }
}

#' Pass an Earth Engine date object to R
#'
#' @param ee_date EE date object (ee$Date)
#' @param timestamp Logical. If TRUE, return the date in milliseconds
#' from the Unix Epoch (1970-01-01 00:00:00 UTC) otherwise return the
#' date as a POSIXct object. By default FALSE.
#' @details
#' \code{eedate_to_rdate} is essential to avoid potential errors which
#' could appear when users call to retrieve the date. Currently,
#' R integer only support 32 bit signed, such integers can only
#' count up to about 2 billion. This range is extremely insufficient to
#' deal with Google Earth Engine date, which is represent by timestamp in
#' milliseconds since the UNIX epoch. \code{eedate_to_rdate} use Python as a
#' backend to obtain the date and convert it in float before to export to R.
#' @examples
#' library(rgee)
#' ee_reattach()
#' ee_Initialize()
#' eeDate <- ee$Date$fromYMD(2010,1,1)
#' eedate_to_rdate(eeDate,timestamp = TRUE) # good
#' eeDate$getInfo()$value # bad
#' @export
eedate_to_rdate <- function(ee_date, timestamp = FALSE) {
  oauth_func_path <- system.file("python/ee_utils.py",
                                 package = "rgee")
  ee_utils <- ee_source_python(oauth_func_path)
  date_numeric <- ee_utils$eedate_to_rdate(ee$Date(ee_date)) %>%
    ee_py_to_r()
  if (isTRUE(timestamp)) {
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
#' ee_reattach()
#' ee_Initialize()
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
  c('system:time_start' = time_start,'system:time_end' = time_end)
}
