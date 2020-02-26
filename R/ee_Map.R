#' Function to display map in earthengine
#' @importFrom jsonlite parse_json
#' @export
ee_Map <- function() {
  ee_Map <- new.env(parent = emptyenv())
}

ee_set_methods <- function() {
  ee_Map$setCenter <- ee_setCenter
  ee_Map$centerObject <- ee_centerObject
  ee_Map$addLayer <- ee_addLayer
  # ee_Map$getBounds <- ee_getBounds
  # ee_Map$getScale <- getScale
  # ee_Map$getCenter <- getCenter
  # ee_Map$setZoom <- setZoom
  # ee_Map$getZoom <- getZoom
  ee_Map$mapdisplay <- ee_mapview()
  ee_Map$mapdisplay
  ee_Map$setCenter()
  ee_Map
}

# ee_getBounds <- function(eeObject) {
#   viewer_canvas <- dev.size("px")
#   width <- viewer_canvas[1]
#   height <- viewer_canvas[2]
#   area_box <- ee_getBox(mapview_object = ee_Map$mapdisplay,
#                         width = width,
#                         height = height)
#   rgee:::ee_map(ee$Geometry$Polygon(area_box))
#   m@map$width <- viewer_canvas[1]
#   m@map$height <- viewer_canvas[2]
#   ee_Map$mapdisplay
# }

ee_setCenter <- function(lon = -58, lat = -10, zoom = NULL) {
  ee_Map$lon <- lon
  ee_Map$lat <- lat
  ee_Map$zoom <- zoom
  invisible(ee_Map)
}

ee_centerObject <- function(eeObject, zoom = NULL) {
  if (any(class(eeObject) %in% ee_get_spatial_objects('Simple'))) {
    center <- eeObject$geometry()$centroid()$coordinates()$getInfo()
  } else {
    stop('Spatial Earth Engine Object not supported')
  }
  ee_Map$setCenter(lon = center[1], lat = center[2], zoom = zoom)
}

ee_addLayer <- function(eeObject,
                        visParams,
                        name = NULL,
                        shown = TRUE,
                        opacity = 1.0) {
  if (missing(visParams)) {
    visParams = list()
  }

  image <- NULL

  #Earth Engine Spatial object
  ee_spatial_object <- ee_get_spatial_objects('Simple')

  if (!any(class(eeObject) %in% ee_spatial_object)) {
    stop("The image argument in 'addLayer' function must be an instace of one",
         " of ee.Image, ee.Geometry, ee.Feature or ee.FeatureCollection.")
  }
  if (any(class(eeObject) %in% ee_get_spatial_objects('Table'))) {
    features <- ee$FeatureCollection(eeObject)

    width <- 2
    if (!is.null(visParams[['width']])) {
      width <- visParams[['width']]
    }

    color <- '000000'
    if (!is.null(visParams[['color']])) {
      color <- visParams[['color']]
    }

    image_fill <- features$
      style(fillColor = color)$
      updateMask(ee$Image$constant(0.5))

    image_outline <- features$style(
      color = color,
      fillColor = '00000000',
      width = width)

    image <- image_fill$blend(image_outline)
  } else {
    image <- do.call(eeObject$visualize,visParams)
  }

  if (is.null(name)) {
    name <- tryCatch(
      expr = parse_json(eeObject$id()$serialize())$
        scope[[1]][[2]][["arguments"]][["id"]],
      error = function(e) "untitled"
    )
    if (is.null(name)) name <- "untitled"
  }
  tile <- get_ee_image_url(image)
  ee_addTile(tile, name = name, shown = shown, opacity = opacity)
}


#' Create a base mapview object
#' @noRd
ee_mapview <- function() {
  m <- mapview()
  m@map$x$setView[[1]] <- c(0, 0)
  m@map$x$setView[[2]] <- 1
  m
}

#' Create a mapview object based on a tile_fetcher
#' @importFrom mapview mapview
#' @importFrom leaflet addTiles tileOptions hideGroup
#' @noRd
ee_addTile <- function(tile, name, shown, opacity) {
  m <- ee_mapview()
  m@map <- m@map %>%
    addTiles(urlTemplate = tile,
             group = name,
             options = tileOptions(opacity = opacity)) %>%
    rgee:::ee_mapViewLayersControl(names = name) %>%
    hideGroup(if (!shown) name else NULL)
  m
}

create_basemap <- function(tile, center, objname, zoom_start) {

  m@map <- m@map %>%
    addTiles(urlTemplate = tile, group = objname, layerId = "0") %>%
    setView(center[1], center[2], zoom = zoom_start) %>%
    rgee:::ee_mapViewLayersControl(names = c(objname))

  m@object$tokens <- tile
  m@object$names <- objname
  #m@object$eeobject <- eeobject$name()
  m
}

#' Get the tile_fetcher to display into ee_map
#' @noRd
get_ee_image_url <- function(image) {
  map_id <- ee$data$getMapId(list(image = image))
  url <- map_id[['tile_fetcher']]$url_format
  url
}

#' Return R class for Earth Engine Spatial Objects
#' @noRd
ee_get_spatial_objects <-  function(type='all') {
  if (type == 'Table') {
    ee_spatial_object <- c('ee.geometry.Geometry',
                           'ee.feature.Feature',
                           'ee.featurecollection.FeatureCollection')

  }
  if (type == 'Image') {
    ee_spatial_object <- 'ee.image.Image'
  }
  if (type == 'ImageCollection') {
    ee_spatial_object <- 'ee.imagecollection.ImageCollection'
  }
  if (type == 'Simple') {
    ee_spatial_object <- c('ee.geometry.Geometry',
                           'ee.feature.Feature',
                           'ee.featurecollection.FeatureCollection',
                           'ee.image.Image')
  }
  if (type == 'All') {
    ee_spatial_object <- c('ee.geometry.Geometry',
                           'ee.feature.Feature',
                           'ee.featurecollection.FeatureCollection',
                           'ee.imagecollection.ImageCollection',
                           'ee.image.Image')
  }
  return(ee_spatial_object)
}

#' Estimate the zoom level for a given bounds
#' https://github.com/fitoprincipe/ipygee/
#' https://stackoverflow.com/questions/6048975/
#' @noRd
# ee_getZoom <- function(eeObject) {
#   bounds <- ee_get_boundary(eeObject)
#
#   WORLD_DIM <- list(height = 256, width = 256)
#   ZOOM_MAX = 21
#
#   latRad <- function(lat) {
#     sin <- sin(lat*pi/180)
#     radX2 <- log((1 + sin)/(1 - sin))/2
#     max(min(radX2, pi), -pi)/2
#   }
#
#   zoom <- function(mapPx, worldPx, fraction){
#     floor(log(mapPx/worldPx/fraction)/log(2))
#   }
#
#   latFraction <- (latRad(bounds['ymax']) - latRad(bounds['ymin']))/pi
#   lngDiff <- bounds['xmax'] - bounds['xmin']
#   lngFraction <- if (lngDiff < 0) lngDiff + 360 else lngDiff
#   lngFraction <- lngFraction/360
#
#   latZoom <- zoom(400, WORLD_DIM[['height']], latFraction)
#   lngZoom <- zoom(970, WORLD_DIM[['width']], lngFraction)
#
#   min(latZoom, lngZoom, ZOOM_MAX)
# }

#' Get boundary of a Earth Engine Object
#' @importFrom sf st_polygon st_bbox
#' @noRd
# ee_get_boundary <- function(eeObject) {
#   eeObject$geometry()$bounds()$coordinates()$getInfo() %>%
#     unlist() %>%
#     matrix(ncol = 2,byrow = TRUE) %>%
#     list() %>%
#     st_polygon() %>%
#     st_bbox()
# }

#' Get the box for a specific Device Surface
#' @noRd
# ee_getBox <- function(mapview_object, width , height){
#   view <- mapview_object@map$x$setView
#   sss <- leafem::addMouseCoordinates(map = mapview_object,native.crs = T)
#   sss$x
#   lat <- view[[1]][1]
#   lng <- view[[1]][2]
#   zoom <- view[[2]]
#   zoom_eq <- 256*2^(zoom)
#   lng_width <- width/zoom_eq*360
#   lat_height <- height/zoom_eq*180
#   xmin <- lng - lng_width/2
#   ymin <- lat - lat_height/2
#   xmax <- lng + lng_width/2
#   ymax <- lat + lat_height/2
#   #list(c(xmin,ymin),c(xmax,ymin),c(xmax,ymax),c(xmin,ymax),c(xmin,ymin))
#   list(c(xmin,ymin),c(xmin,ymax),c(xmax,ymax),c(xmax,ymin),c(xmin,ymin))
# }


ee_Map <- ee_Map()
ee_set_methods()
