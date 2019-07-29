#' Display a given EE object
#'
#'
#' @param eeobject ee_Feature,ee_FeatureCollection,ee_Image or ee_ImageCollection;
#' the object to add to the map.
#' @param vizparams List; The visualization parameters. See Details
#' @param center Character vector; the longitude and latitude of the map center
#' @param  zoom_start The zoom level.
#' @param obj_name Character vector; Name of the map(or maps).
#' @param max_nimage Specify the max number of Image to display in an ee_ImageCollection.
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
                   obj_name = 'mapobject',...) {

  oauth_func_path <- system.file("Python/ee_map.py", package = "rgee")
  ee_source_python(oauth_func_path)

  if(eeobject$name() %in%  c('Image','Feature','FeatureCollection')) {

    tile = ee_map_py(eeobject,vizparams)
    m <- mapview(...)
    m@map = m@map %>%
      addWMSTiles(group=obj_name,baseUrl=tile,layers = "0") %>%
      setView(center[1], center[2], zoom = zoom_start) %>%
      mapview:::mapViewLayersControl(names = c(obj_name))

  } else if(eeobject$name() == 'ImageCollection') {

    ee_size = eeobject$size()$getInfo()
    if (ee_size > max_nimage) ee_size <- max_nimage
    eeobject_list <- eeobject$toList(ee_size) #collection to list

    m <- mapview(...)

    for (x in 1:ee_size) {
      eeobj <- ee_Image(eeobject_list$get(x-1)) #indice starts - R(0) vs Python(1)
      tile <- ee_map_py(eeobj,vizparams)
      m@map = m@map %>%
        addWMSTiles(group=obj_name[x],baseUrl=tile,layers = "0") %>%
        setView(center[1], center[2], zoom = zoom_start) %>%
        mapview:::mapViewLayersControl(names = c(obj_name[x]))
    }
  }
  m
}
