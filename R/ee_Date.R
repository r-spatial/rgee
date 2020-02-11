#' Pass R date object to Earth Engine
#'
#' Function to pass objects of classes "Date",
#' "character", "POSIXt", and "POSIXct" to
#' ee$Date or unix time stamp.
#'
#' @param date R date object
#' @param eeobject Logical. Whether TRUE, it will return
#' a ee$object otherwise return the date in milliseconds.
#' @examples
#' library(rgee)
#' ee_reattach()
#' ee_Initialize()
#' rdate_to_eedate('2000-01-01')
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
#' @param js Logical. Whether TRUE, it will return a numeric
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
