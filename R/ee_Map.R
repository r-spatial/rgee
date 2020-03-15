#' Module to display Earth Engine (EE) spatial
#' objects
#'
#' Create interactive visualizations of spatial EE objects
#' (ee.Geometry, ee.Image, ee.Feature, and ee.FeatureCollection)
#' through \code{\link[mapview]{mapview}}.
#' @importFrom jsonlite parse_json
#' @format An object of class environment with the
#' following functions:
#' \itemize{
#'   \item  \strong{addLayer(eeObject, visParams, name = NULL, shown = TRUE,
#'   opacity = 1)}: Adds a given EE object to the map as a layer. \cr
#'   \itemize{
#'     \item \strong{eeObject:} The object to add to mapview.\cr
#'     \item \strong{visParams:} List of parameters for visualization.
#'     See details.\cr
#'     \item \strong{name:} The name of the layer.\cr
#'     \item \strong{shown:} A flag indicating whether the
#'     layer should be on by default. \cr
#'     \item \strong{opacity:} The layer's opacity represented as a number
#'      between 0 and 1. Defaults to 1. \cr
#'   }
#'   \item \strong{setCenter(lon = 0, lat = 0, zoom = NULL)}: Centers the map
#'   view at the given coordinates with the given zoom level. If no zoom level
#'   is provided, it uses 1 by default.
#'   \itemize{
#'     \item \strong{lon:} The longitude of the center, in degrees.\cr
#'     \item \strong{lat:} The latitude of the center, in degrees.\cr
#'     \item \strong{zoom:} The zoom level, from 1 to 24.
#'   }
#'   \item \strong{setZoom(zoom = NULL)}: Sets the zoom level of the map.
#'   \itemize{
#'     \item \strong{zoom:} The zoom level, from 1 to 24.
#'   }
#'   \item \strong{ee_centerObject(eeObject, zoom = NULL)}: Centers the
#'   map view on a given object. If no zoom level is provided, it will
#'   be predicted according the bounds of the Earth Engine object specified.
#'   \itemize{
#'     \item \strong{eeObject:} EE object.\cr
#'     \item \strong{zoom:} The zoom level, from 1 to 24.
#'   }
#' }
#'
#' @details
#'
#' `Map` takes advantage of
#' \href{https://developers.google.com/earth-engine/api_docs#ee.data.getmapid}{
#' getMapId} for fetch and return both a mapid and a token suitable
#' to use in a \code{\link[mapview]{mapview}} object. To achieve desirable
#' visualization effect, it will depend on the type of spatial EE object . For
#' Image objects, you can provide visualization parameters to
#' Map$addLayer() by using the argument visParams. The
#' \href{https://developers.google.com/earth-engine/image_visualization}{
#' parameters} available are:
#'
#' \tabular{lll}{
#' \strong{Parameter}\tab \strong{Description}  \tab \strong{Type}\cr
#' \strong{bands}    \tab  Comma-delimited list of three band names to be
#' mapped to RGB     \tab  list \cr
#' \strong{min}      \tab  Value(s) to map to 0 \tab  number or list of three
#' numbers, one for each band \cr
#' \strong{max}      \tab  Value(s) to map to 1 \tab  number or list of three
#' numbers, one for each band \cr
#' \strong{gain}     \tab  Value(s) by which to multiply each pixel value \tab
#' number or list of three numbers, one for each band \cr
#' \strong{bias}     \tab  Value(s) to add to each Digital Number (DN)
#' value \tab number or list of three numbers, one for each band \cr
#' \strong{gamma}    \tab  Gamma correction factor(s) \tab  number or list of
#' three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings
#' (single-band images only) \tab  comma-separated list of hex strings \cr
#' \strong{opacity}   \tab  The opacity of the layer (0.0 is fully transparent
#' and 1.0 is fully opaque) \tab  number \cr
#' }
#'
#' If you add an Image to the map without any additional
#' parameters, by default `Map$addLayer()` assigns the first three bands to red,
#' green, and blue bands, respectively. The default stretch is based on the
#' min-max range.  For Geometry, Feature, and/or FeatureCollection. The available
#' visParams are:
#' \itemize{
#'  \item \strong{color}: A hex string in the format RRGGBB specifying the
#'  color to use for drawing the features. By default 000000.
#'  \item \strong{pointRadius}: The radius of the point markers. By default 3.
#'  \item \strong{strokeWidth}: The width of lines and polygon borders. By
#'  default 3.
#' }
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # Case 1: Geometry*
#' geom <- ee$Geometry$Point(list(-73.53, -15.75))
#' Map$centerObject(geom, zoom = 13)
#' m1 <- Map$addLayer(
#'   eeObject = geom,
#'   visParams = list(
#'     pointRadius = 10,
#'     color = "FF0000"
#'   ),
#'   name = "Geometry-Arequipa"
#' )
#' # Case 2: Feature
#' eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")$
#'   first()
#' m2 <- Map$addLayer(
#'   eeObject = ee$Feature(eeobject_fc),
#'   name = "Feature-Arequipa"
#' )
#' m2 + m1
#'
#' # Case 3: FeatureCollection
#' eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")
#' Map$centerObject(eeobject_fc)
#' m3 <- Map$addLayer(eeObject = eeobject_fc, name = "FeatureCollection")
#' m3 + m2 + m1
#'
#' # Case 4: Image
#' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
#' Map$centerObject(image)
#' m4 <- Map$addLayer(
#'   eeObject = image,
#'   visParams = list(
#'     bands = c("B4", "B3", "B2"),
#'     max = 10000
#'   ),
#'   name = "SF"
#' )
#' m4
#' }
#' @export
Map <- function() {
  Map <- new.env(parent = emptyenv())
}

ee_set_methods <- function() {
  Map$addLayer <- ee_addLayer
  Map$setCenter <- ee_setCenter
  Map$setZoom <- ee_setZoom
  Map$centerObject <- ee_centerObject
  # Map$getBounds <- ee_getBounds
  # Map$getScale <- getScale
  # Map$getCenter <- getCenter
  # Map$getZoom <- getZoom

  # Init environment
  Map$setCenter()
  Map
}

#' Sets the zoom level of the map.
#' @noRd
ee_setZoom <- function(zoom) {
  Map$zoom <- zoom
}

#' Center a mapview
#'
#' Centers the map view at the given coordinates
#' with the given zoom level. If no zoom level is
#' specified, it uses 1.
#'
#' https://developers.google.com/earth-engine/api_docs#map.setcenter
#' @noRd
ee_setCenter <- function(lon = 0, lat = 0, zoom = NULL) {
  Map$lon <- lon
  Map$lat <- lat
  Map$zoom <- zoom
  invisible(Map)
}


#' Center a mapview using an EE object
#'
#' Centers the map view on a given object. If no zoom
#' level is specified, it will be predicted according to the
#' bounds of the specified Earth Engine object.
#'
#' https://developers.google.com/earth-engine/api_docs#map.centerobject
#' @noRd
ee_centerObject <- function(eeObject, zoom = NULL) {
  if (any(class(eeObject) %in% ee_get_spatial_objects("Nongeom"))) {
    center <- tryCatch(
      expr = eeObject$
        geometry()$
        centroid()$
        getInfo() %>%
        '[['('coordinates') %>%
        ee_py_to_r(),
      error = function(e) {
        message(
          "The centroid coordinate was not possible",
          " to estimate, assigning: c(0,0)"
        )
        c(0, 0)
      }
    )
  } else if (any(class(eeObject) %in% "ee.geometry.Geometry")) {
    center <- tryCatch(
      expr = eeObject$
        centroid()$
        coordinates()$
        getInfo() %>%
        ee_py_to_r(),
      error = function(e) {
        message(
          "The centroid coordinate was not possible",
          " to estimate, assigning: c(0,0)"
        )
        c(0, 0)
      }
    )
  } else {
    stop("Spatial Earth Engine Object not supported")
  }

  if (is.null(zoom)) {
    zoom <- ee_getZoom(eeObject)
  }
  Map$setCenter(lon = center[1], lat = center[2], zoom = zoom)
}

#' Adds a given EE object to the map as a layer.
#' https://developers.google.com/earth-engine/api_docs#map.addlaye
#' @noRd
ee_addLayer <- function(eeObject,
                        visParams,
                        name = NULL,
                        shown = TRUE,
                        opacity = 1) {
  if (missing(visParams)) {
    visParams <- list()
  }

  image <- NULL

  # Earth Engine Spatial object
  ee_spatial_object <- ee_get_spatial_objects("Simple")

  if (!any(class(eeObject) %in% ee_spatial_object)) {
    stop(
      "The image argument in 'addLayer' function must be an instace of one",
      " of ee.Image, ee.Geometry, ee.Feature or ee.FeatureCollection."
    )
  }
  if (any(class(eeObject) %in% ee_get_spatial_objects("Table"))) {
    features <- ee$FeatureCollection(eeObject)

    width <- 2
    if (!is.null(visParams[["width"]])) {
      width <- visParams[["width"]]
    }

    color <- "000000"
    if (!is.null(visParams[["color"]])) {
      color <- visParams[["color"]]
    }

    image_fill <- features$
      style(fillColor = color)$
      updateMask(ee$Image$constant(0.5))

    image_outline <- features$style(
      color = color,
      fillColor = "00000000",
      width = width
    )

    image <- image_fill$blend(image_outline)
  } else {
    image <- do.call(eeObject$visualize, visParams)
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


#' Basic base mapview object
#' @noRd
ee_mapview <- function() {
  m <- mapview()
  m@map$x$setView[[1]] <- c(Map$lat, Map$lon)
  m@map$x$setView[[2]] <- if (is.null(Map$zoom)) 1 else Map$zoom
  m
}

#' Add a mapview object based on a tile_fetcher
#' @importFrom mapview mapview
#' @importFrom leaflet addTiles tileOptions hideGroup setView
#' @noRd
ee_addTile <- function(tile, name, shown, opacity) {
  m <- ee_mapview()
  m@map <- m@map %>%
    addTiles(
      urlTemplate = tile,
      group = name,
      options = tileOptions(opacity = opacity)
    ) %>%
    ee_mapViewLayersControl(names = name) %>%
    hideGroup(if (!shown) name else NULL)
  m@object$tokens <- tile
  m@object$names <- name
  m@object$opacity <- opacity
  m@object$shown <- shown
  m
}

if (!isGeneric("+")) {
  setGeneric("+", function(x, y, ...)
    standardGeneric("+"))
}

#' mapview + mapview; adds data from the second map to the first
#'
#' @author Adapted from
#' \href{https://github.com/r-spatial/mapview/blob/develop/R/plus.R}{
#' tim-salabim code}.
#' @param e1 a mapview map to which e2 should be added.
#' @param e2 a mapview map from which the objects should be added to e1.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # Case 1: Geometry*
#' geom <- ee$Geometry$Point(list(-73.53, -15.75))
#' Map$centerObject(geom, zoom = 13)
#' m1 <- Map$addLayer(
#'   eeObject = geom,
#'   visParams = list(
#'     pointRadius = 10,
#'     color = "FF0000"
#'   ),
#'   name = "Geometry-Arequipa"
#' )
#' # Case 2: Feature
#' eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")$
#'   first()
#' m2 <- Map$addLayer(
#'   eeObject = ee$Feature(eeobject_fc),
#'   name = "Feature-Arequipa"
#' )
#' m2 + m1
#'
#' # Case 3: FeatureCollection
#' eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")
#' Map$centerObject(eeobject_fc)
#' m3 <- Map$addLayer(eeObject = eeobject_fc, name = "FeatureCollection")
#' m3 + m2 + m1
#'
#' # Case 4: Image
#' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
#' Map$centerObject(image)
#' m4 <- Map$addLayer(
#'   eeObject = image,
#'   visParams = list(
#'     bands = c("B4", "B3", "B2"),
#'     max = 10000
#'   ),
#'   name = "SF"
#' )
#' m4
#' }
#'
setMethod(
  "+",
  signature(
    e1 = "mapview",
    e2 = "mapview"
  ),
  function(e1, e2) {
    e2_token <- e2@object$tokens
    e2_name <- e2@object$names
    e2_opacity <- e2@object$opacity
    e2_shown <- e2@object$shown

    for (x in seq_len(length(e2_name))) {
      e1@map <- e1@map %>%
        addTiles(
          urlTemplate = e2_token[x],
          group = e2_name[x],
          options = tileOptions(opacity = e2_opacity[x])
        ) %>%
        ee_mapViewLayersControl(names = e2_name[x]) %>%
        hideGroup(if (!e2_shown[x]) e2_name[x] else NULL)
    }
    return(e1)
  }
)

#' Get the tile_fetcher to display into ee_map
#' @noRd
get_ee_image_url <- function(image) {
  map_id <- ee$data$getMapId(list(image = image))
  url <- map_id[["tile_fetcher"]]$url_format
  url
}


#' Return R classes for Earth Engine Spatial Objects
#' @noRd
ee_get_spatial_objects <- function(type = "all") {
  if (type == "Table") {
    ee_spatial_object <- c(
      "ee.geometry.Geometry",
      "ee.feature.Feature",
      "ee.featurecollection.FeatureCollection"
    )
  }
  if (type == "Image") {
    ee_spatial_object <- "ee.image.Image"
  }
  if (type == "ImageCollection") {
    ee_spatial_object <- "ee.imagecollection.ImageCollection"
  }
  if (type == "Nongeom") {
    ee_spatial_object <- c(
      "ee.feature.Feature",
      "ee.featurecollection.FeatureCollection",
      "ee.image.Image"
    )
  }
  if (type == "Simple") {
    ee_spatial_object <- c(
      "ee.geometry.Geometry",
      "ee.feature.Feature",
      "ee.featurecollection.FeatureCollection",
      "ee.image.Image"
    )
  }
  if (type == "All") {
    ee_spatial_object <- c(
      "ee.geometry.Geometry",
      "ee.feature.Feature",
      "ee.featurecollection.FeatureCollection",
      "ee.imagecollection.ImageCollection",
      "ee.image.Image"
    )
  }
  return(ee_spatial_object)
}

#' Estimates the zoom level for a given bounds
#' https://github.com/fitoprincipe/ipygee/
#' https://stackoverflow.com/questions/6048975/
#' @noRd
ee_getZoom <- function(eeObject) {
  bounds <- ee_get_boundary(eeObject)

  WORLD_DIM <- list(height = 256, width = 256)
  ZOOM_MAX <- 18

  latRad <- function(lat) {
    sin <- sin(lat * pi / 180)
    radX2 <- log((1 + sin) / (1 - sin)) / 2
    max(min(radX2, pi), -pi) / 2
  }

  zoom <- function(mapPx, worldPx, fraction) {
    floor(log(mapPx / worldPx / fraction) / log(2))
  }

  latFraction <- (latRad(bounds["ymax"]) - latRad(bounds["ymin"])) / pi
  lngDiff <- bounds["xmax"] - bounds["xmin"]
  lngFraction <- if (lngDiff < 0) lngDiff + 360 else lngDiff
  lngFraction <- lngFraction / 360

  latZoom <- zoom(400, WORLD_DIM[["height"]], latFraction)
  lngZoom <- zoom(970, WORLD_DIM[["width"]], lngFraction)

  min(latZoom, lngZoom, ZOOM_MAX)
}

#' Get boundary of a Earth Engine Object
#' @importFrom sf st_polygon st_bbox
#' @noRd
ee_get_boundary <- function(eeObject) {
  if (any(class(eeObject) %in% "ee.geometry.Geometry")) {
    eeObject <- ee$Feature(eeObject)
  }
  eeObject$geometry()$bounds()$getInfo() %>%
    '[['('coordinates') %>%
    ee_py_to_r() %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    list() %>%
    st_polygon() %>%
    st_bbox()
}

#' Get the box for a specific Device Surface
#' @noRd
# ee_getBox <- function(mapview_object, width , height){
#   view <- mapview_object@map$x$setView
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
#   list(c(xmin,ymin),c(xmin,ymax),c(xmax,ymax),c(xmax,ymin),c(xmin,ymin))
# }


# ee_getBounds <- function(eeObject) {
#   viewer_canvas <- dev.size("px")
#   width <- viewer_canvas[1]
#   height <- viewer_canvas[2]
#   area_box <- ee_getBox(mapview_object = Map$mapdisplay,
#                         width = width,
#                         height = height)
#   m@map$width <- viewer_canvas[1]
#   m@map$height <- viewer_canvas[2]
#   Map$mapdisplay
# }

Map <- Map()
ee_set_methods()
