#' An object to represent collection filters.
#'
#' @param filter Filter|List, optional
#' @details
#' This constructor accepts the following args:
#' \enumerate{
#'  \item Another filter.
#'  \item A list of filters (which are implicitly AND ed together).
#'  \item A ComputedObject returning a filter. Users should not be making these;
#'  they are produced by the generator functions below.
#' }
#'
#' @return Filter EarthEngine Python class
#' @section Methods:
#' \itemize{
#' \item \strong{always():}
#' \item \strong{And():}
#' \item \strong{aside():}
#' \item \strong{calendarRange():}
#' \item \strong{contains():}
#' \item \strong{date():}
#' \item \strong{dateRangeContains():}
#' \item \strong{dayOfYear():}
#' \item \strong{disjoint():}
#' \item \strong{encode():}
#' \item \strong{eq():}
#' \item \strong{equals():}
#' \item \strong{freeze():}
#' \item \strong{geometry():}
#' \item \strong{getInfo():}
#' \item \strong{greaterThan():}
#' \item \strong{greaterThanOrEquals():}
#' \item \strong{gt():}
#' \item \strong{gte():}
#' \item \strong{hasGeometry():}
#' \item \strong{initialize():}
#' \item \strong{inList():}
#' \item \strong{intersects():}
#' \item \strong{isContained():}
#' \item \strong{isVariable():}
#' \item \strong{lessThan():}
#' \item \strong{lessThanOrEquals():}
#' \item \strong{listContains():}
#' \item \strong{lt():}
#' \item \strong{lte():}
#' \item \strong{maxDifference():}
#' \item \strong{metadata_():}
#' \item \strong{name():}
#' \item \strong{neq():}
#' \item \strong{never():}
#' \item \strong{Not():}
#' \item \strong{notEquals():}
#' \item \strong{notNull():}
#' \item \strong{Or():}
#' \item \strong{predicateCount():}
#' \item \strong{rangeContains():}
#' \item \strong{reset():}
#' \item \strong{serialize():}
#' \item \strong{stringContains():}
#' \item \strong{stringEndsWith():}
#' \item \strong{stringStartsWith():}
#' \item \strong{withinDistance():}
#' }
#' @export
ee_Filter <- function(filter=NULL){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_filter_py(Filter=NULL)
}

#' Soy una bonita funcion que no hace nada 3 :)
#' @param ID holi2
#' @export
ee_Image <- function(ID){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_image_py(ID)
}


#' Soy una bonita funcion que no hace nada 2 :)
#' @param ID holi
#' @export
ee_ImageCollection <- function(ID){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_imagecollection_py(ID)
}

#' Soy una bonita funcion que no hace nada 4 :)
#' @export
ee_Geometry <- function(){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_geometry_py()
}


#' Soy una bonita funcion que no hace nada 3 :)
#' @param ID holi2
#' @export
ee_Feature <- function(ID){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_feature_py(ID)
}

#' Soy una bonita funcion que no hace nada 3 :)
#' @param ID holi2
#' @export
ee_FeatureCollection <- function(ID){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_featurecollection_py(ID)
}


#' Soy una bonita funcion que no hace nada 4 :)
#' @export
ee_Reducer <- function(){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_reducer_py()
}


#' Soy una bonita funcion que no hace nada 4 :)
#' @export
ee_Export <- function(){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_export_py()
}
