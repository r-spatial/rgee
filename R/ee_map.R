#' Adds a given EE object to mapview as a layer.
#'
#' Display a given EE object (ee$Image, ee$Feature, ee$FeatureCollection
#' or ee$ImageCollection) using the \code{\link[mapview]{mapview}} style.
#'
#' @param eeobject ee$Feature,ee$FeatureCollection, ee$Image or ee$ImageCollection.
#' @param vizparams list; visualization parameters. See Details.
#' @param center character vector; the longitude and latitude of the map center.
#' @param  zoom_start zoom level.
#' @param obj_name character vector; name of the map(or maps).
#' @param max_nimage Max number of Image to display (Only relevant for ee$ImageCollection objects).
#' @param ... additional \code{\link[mapview]{mapview}} arguments could be passed.
#' @importFrom mapview mapview
#' @importFrom leaflet addWMSTiles setView
#'
#' @details
#' `ee_map` takes advantage of the `ee$*$getMapId(self,vis_params=NULL)` function for generating a provisial
#' WMS service supported by Google Earth Engine. To achieve desirable visualization effects, you can
#' provide visualization parameters to ee_map by the parameter vizparams. The
#' \href{https://developers.google.com/earth-engine/image_visualization}{parameters} available are:
#'
#' \tabular{lll}{
#'\strong{Parameter}\tab \strong{Description}\tab\strong{Type}\cr
#'\strong{bands}     \tab  Comma-delimited list of three band names to be mapped to RGB                \tab  list                                               \cr
#'\strong{min}       \tab  Value(s) to map to 0                                                        \tab  number or list of three numbers, one for each band \cr
#'\strong{max}       \tab  Value(s) to map to 255                                                      \tab  number or list of three numbers, one for each band \cr
#'\strong{gain}      \tab  Value(s) by which to multiply each pixel value                              \tab  number or list of three numbers, one for each band \cr
#'\strong{bias}      \tab  Value(s) to add to each DN                                                  \tab  number or list of three numbers, one for each band \cr
#'\strong{gamma}     \tab  Gamma correction factor(s)                                                  \tab  number or list of three numbers, one for each band \cr
#'\strong{palette}  \tab  List of CSS-style color strings (single-band images only)                   \tab  comma-separated list of hex strings                \cr
#'\strong{opacity}   \tab  The opacity of the layer (0.0 is fully transparent and 1.0 is fully opaque) \tab  number                                             \cr
#'\strong{format}    \tab  Either "jpg" or "png"                                                       \tab  string \cr
#' }
#'
#' If you add a layer to the map without any additional parameters, by default the
#' `ee_map` assigns the first three bands to red, green and blue, respectively.
#' The default stretch is based on the type of data in the band (e.g. floats are
#' stretched in <0,1>, 16-bit data are stretched to the full range of possible values),
#' which may or may not be suitable.
#'
#' @examples
#' \dontrun{
#'library(rgee)
#'ee_initialize()
#'
#'#Ploting an ee_FeatureColletion
#'eeobject <- ee_FeatureCollection('users/csaybar/DLdemos/train_set')
#'center <- eeobject$geometry()$centroid()$getInfo()$coordinates
#'vizparams = list(color='FF0000', strokeWidth=5)
#'ee_map(eeobject,vizparams,center,obj_name = "Arequipa-landuse")
#'
#'#Ploting an ee_Image
#'collection = ee_ImageCollection('LANDSAT/LC08/C01/T1_TOA')$
#'  filter(ee_Filter()$eq('WRS_PATH', 44))$
#'  filter(ee_Filter()$eq('WRS_ROW', 34))$
#'  filterDate('2014-01-01', '2015-01-01')$
#'  sort('CLOUD_COVER')
#'
#'eeobject = collection$median()
#'vizparams = list(bands=c('B4', 'B3', 'B2'),max=0.3)
#'center = c(-122.3578,37.7726)
#'ee_map(eeobject,vizparams,center,obj_name = 'SF')
#'
#'#Ploting an ee_ImageColection
#'ee_map(collection,vizparams,center,obj_name = c('SF1',"SF2","SF3"),max_nimage = 3)
#' }
#' @export
ee_map <- function(eeobject,
                   vizparams,
                   center,
                   zoom_start = 8,
                   max_nimage = 10,
                   obj_name = "map", ...) {
  oauth_func_path <- system.file("Python/ee_map.py", package = "rgee")
  ee_source_python(oauth_func_path)

  if (eeobject$name() %in% c("Image", "Feature", "FeatureCollection")) {
    tile <- ee_map_py(eeobject, vizparams)
    m <- mapview(...)
    m@map <- m@map %>%
      addWMSTiles(group = obj_name, baseUrl = tile, layers = "0") %>%
      setView(center[1], center[2], zoom = zoom_start) %>%
      ee_mapViewLayersControl(names = c(obj_name))

    m@object$tokens <- tile
    m@object$names <- obj_name
    m@object$eeobject <- eeobject$name()
  } else if (eeobject$name() == "ImageCollection") {
    ee_size <- eeobject$size()$getInfo()
    if (ee_size > max_nimage) ee_size <- max_nimage
    if (length(obj_name) != ee_size) stop("The length of ee_ImageCollection and 'obj_name' are different")

    eeobject_list <- eeobject$toList(ee_size) # collection to list

    m <- mapview(...)
    tokens <- rep(NA, ee_size)
    for (x in 1:ee_size) {
      eeobj <- ee_Image(eeobject_list$get(x - 1)) # indice starts - R(0) vs Python(1)
      tile <- ee_map_py(eeobj, vizparams)
      tokens[x] <- tile
      m@map <- m@map %>%
        addWMSTiles(group = obj_name[x], baseUrl = tile, layers = "0") %>%
        setView(center[1], center[2], zoom = zoom_start) %>%
        ee_mapViewLayersControl(names = c(obj_name[x]))
    }

    m@object$tokens <- tokens
    m@object$names <- obj_name
    m@object$eeobject <- eeobject$name()
  } else {
    stop("Earth Engine object does not support")
  }
  m
}


if ( !isGeneric('+') ) {
  setGeneric('+', function(x, y, ...)
    standardGeneric('+'))
}

#' mapview + mapview; adds data from the second map to the first
#'
#' @author Adapted from \href{https://github.com/r-spatial/mapview/blob/95050618a4eab75c73fea6e50a6aa31dcd152f14/R/plus.R}{tim-salabim code}.
#' @param e1 a mapview map to which e2 should be added.
#' @param e2 a mapview map from which the objects should be added to e1.
#' @examples
#' \dontrun{
#' eeobject <- ee_FeatureCollection('users/csaybar/DLdemos/train_set')
#' center <- eeobject$geometry()$centroid()$getInfo()$coordinates
#' vizparams = list(color='FF0000', strokeWidth=5)
#' m1 <- ee_map(eeobject,vizparams,center,obj_name = "Arequipa-landuse")
#'
#' collection = ee_ImageCollection('LANDSAT/LC08/C01/T1_TOA')$
#'   filter(ee_Filter()$eq('WRS_PATH', 44))$
#'   filter(ee_Filter()$eq('WRS_ROW', 34))$
#'   filterDate('2014-01-01', '2015-01-01')$
#'   sort('CLOUD_COVER')
#' eeobject = collection$median()
#' vizparams = list(bands=c('B4', 'B3', 'B2'),max=0.3)
#' center = c(-122.3578,37.7726)
#' m2 <- ee_map(eeobject,vizparams,center,obj_name = 'SF')
#' m1 + m2
#' }
#'
setMethod("+",
          signature(e1 = "mapview",
                    e2 = "mapview"),
          function(e1, e2) {
            e2_token <- e2@object$tokens
            e2_name <- e2@object$names
            for(x in 1:length(e2_name)) {
              e1@map <- e1@map %>%
                addWMSTiles(group=e2_name[x],baseUrl=e2_token[x],layers = "0") %>%
                ee_mapViewLayersControl(names = e2_name[x])
            }
            return(e1)
          })
