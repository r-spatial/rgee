#' Display a given EE object
#'
#' @param eeobject ee_Feature,ee_FeatureCollection,ee_Image or ee_ImageCollection;
#' the object to add to the map.
#' @param vizparams List; The visualization parameters. See Details
#' @param center Character vector; the longitude and latitude of the map center
#' @param  zoom_start The zoom level.
#' @param obj_name Character vector; Name of the map(or maps).
#' @param max_nimage Specify the max number of Image to display (parameter appropriate just for ee_ImageCollection objects).
#' @param ... additional \link[mapview]{mapview} arguments could be passed.
#' @importFrom mapview mapview
#' @importFrom leaflet addWMSTiles setView
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
