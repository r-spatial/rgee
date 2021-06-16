#' Pass an R date object to Earth Engine
#'
#' Pass an R date object ("Date", "Numeric", "character", "POSIXt",
#' and "POSIXct") to Google Earth Engine (ee$Date).
#'
#' @param date R date object
#' @param timestamp Logical. If TRUE, return the date in milliseconds
#' from the Unix Epoch (1970-01-01 00:00:00 UTC). Otherwise return a
#' EE date object. By default, FALSE.
#' @family date functions
#' @return \code{rdate_to_eedate} will return either a numeric timestamp or
#' an ee$Date depending on the \code{timestamp} argument.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#' rdate_to_eedate('2000-01-01')
#' rdate_to_eedate(315532800000) # float number
#' }
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
#' @param ee_date ee$date object (ee$Date)
#' @param timestamp Logical. If TRUE, return the date in milliseconds
#' from the Unix Epoch (1970-01-01 00:00:00 UTC). Otherwise, return the
#' date as a POSIXct object. By default FALSE.
#'
#' @details
#' \code{eedate_to_rdate} is essential to avoid potential errors that
#' might appear when users call to retrieve dates. Currently,
#' R integer only supports 32 bit signed (such integers can only
#' count up to about 2 billion). This range is notably insufficient for dealing
#' with GEE date objects represented by timestamps in milliseconds since the
#' UNIX epoch. \code{eedate_to_rdate} uses Python in the backend to obtain the
#' date and convert it in float before exporting to R.
#'
#' @return \code{eedate_to_rdate} will return either a numeric timestamp or
#' a POSIXct object depending on the \code{timestamp} argument.
#' @family date functions
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' eeDate <- ee$Date$fromYMD(2010,1,1)
#' eedate_to_rdate(eeDate,timestamp = TRUE) # good
#' eeDate$getInfo()$value # bad
#' }
#' @export
eedate_to_rdate <- function(ee_date, timestamp = FALSE) {
  oauth_func_path <- system.file("python/ee_utils.py",
                                 package = "rgee")
  ee_utils <- ee_source_python(oauth_func_path)
  date_numeric <- suppressWarnings(
    { # ee_utils$eedate_to_rdate could return error,
      # then warning message "restarting interrupted
      # promise evaluation" is invoked.
      ee_utils$eedate_to_rdate(ee$Date(ee_date)) %>%
        ee_utils_py_to_r()
    }
  )
  if (isTRUE(timestamp)) {
    date_numeric
  } else {
    as.POSIXct(x = date_numeric/1000, origin = "1970-01-01", tz = "GMT")
  }
}
