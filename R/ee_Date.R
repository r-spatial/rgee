#' Pass R date object to Earth Engine
#'
#' Pass R date object and return a timestamp as a float value
#'
#' @param date R date object
#' @examples
#' ee_Initialize()
#' r_to_eeDate('2000-01-01')
#' @export
r_to_eeDate <- function(date) {
  oauth_func_path <- system.file("python/ee_selenium_functions.py",
                                 package = "rgee")
  ee_selenium_functions <- ee_source_python(oauth_func_path)
  ee_py_to_r(ee_selenium_functions$r_to_eeDate(date))
}

#' Pass Earth Engine date to R date object
#'
#' Pass Earth Engine and return a R date object
#'
#' @param eedate EE date object (ee$Date)
#' @examples
#' ee_Initialize()
#' eeDate <- ee$Date('1980-01-01')
#' eeDate_to_r(eeDate)
#' @export
eeDate_to_r <- function(eedate) {
  oauth_func_path <- system.file("python/ee_selenium_functions.py",
                                 package = "rgee")
  ee_selenium_functions <- ee_source_python(oauth_func_path)
  ee_py_to_r(ee_selenium_functions$eeDate_to_r(eedate))
}
