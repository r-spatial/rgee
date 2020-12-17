#' Module to display Earth Engine (EE) spatial objects
#'
#' Create interactive visualizations of spatial EE objects
#' (ee$Geometry, ee$Image, ee$Feature, and ee$FeatureCollection)
#' using \code{mapview}.
#' @format An object of class environment with the
#' following functions:
#' \itemize{
#'   \item  \strong{addLayer(eeObject, visParams, name = NULL, shown = TRUE,
#'   opacity = 1, legend = FALSE)}: Adds a given EE object to the map as a layer. \cr
#'   \itemize{
#'     \item \strong{eeObject:} The object to add to mapview.\cr
#'     \item \strong{visParams:} List of parameters for visualization.
#'     See details.\cr
#'     \item \strong{name:} The name of the layer.\cr
#'     \item \strong{shown:} A flag indicating whether the
#'     layer should be on by default. \cr
#'     \item \strong{opacity:} The layer's opacity represented as a number
#'      between 0 and 1. Defaults to 1. \cr
#'     \item \strong{legend:} Should a legend be plotted?. Ignore if \code{eeObject}
#'     is not a single-band ee$Image.
#'   }
#'   \item  \strong{addLayers(eeObject, visParams, name = NULL, shown = TRUE,
#'   opacity = 1, legend = FALSE)}: Adds a given ee$ImageCollection to the map
#'   as multiple layers. \cr
#'   \itemize{
#'     \item \strong{eeObject:} The ee$ImageCollection to add to mapview.\cr
#'     \item \strong{visParams:} List of parameters for visualization.
#'     See details.\cr
#'     \item \strong{name:} The name of layers.\cr
#'     \item \strong{shown:} A flag indicating whether
#'     layers should be on by default. \cr
#'     \item \strong{opacity:} The layer's opacity represented as a number
#'      between 0 and 1. Defaults to 1. \cr
#'     \item \strong{legend:} Should a legend be plotted?. Only the legend of
#'     the first image is displayed.
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
#'   \item \strong{centerObject(eeObject, zoom = NULL,
#'    maxError = ee$ErrorMargin(1))}: Centers the
#'   map view on a given object. If no zoom level is provided, it will
#'   be predicted according to the bounds of the Earth Engine object specified.
#'   \itemize{
#'     \item \strong{eeObject:} EE object.\cr
#'     \item \strong{zoom:} The zoom level, from 1 to 24.
#'     \item \strong{maxError:} 	Max error when input
#'     image must be reprojected to an explicitly
#'     requested result projection or geodesic state.
#'   }
#' }
#'
#' @details
#' `Map` use the Earth Engine method
#' \href{https://developers.google.com/earth-engine/api_docs#ee.data.getmapid}{
#' getMapId} to fetch and return an ID dictionary being used to create
#' layers in a \code{mapview} object. Users can specify visualization
#' parameters to Map$addLayer by using the visParams argument. Each Earth
#' Engine spatial object has a specific format. For
#' \code{ee$Image}, the
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
#' If you add an \code{ee$Image} to Map$addLayer without any additional
#' parameters, by default it assigns the first three bands to red,
#' green, and blue bands, respectively. The default stretch is based on the
#' min-max range. On the other hand, the available parameters for
#' \code{ee$Geometry}, \code{ee$Feature}, and \code{ee$FeatureCollection}
#' are:
#'
#' \itemize{
#'  \item \strong{color}: A hex string in the format RRGGBB specifying the
#'  color to use for drawing the features. By default #000000.
#'  \item \strong{pointRadius}: The radius of the point markers. By default 3.
#'  \item \strong{strokeWidth}: The width of lines and polygon borders. By
#'  default 3.
#' }
#' @returns Object of class leaflet, with the following extra parameters: tokens, name,
#' opacity, shown, min, max, palette, and legend. Use the $ method to retrieve
#' the data (e.g. m$rgee$min).
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize()
#'
#' # Case 1: Geometry*
#' geom1 <- ee$Geometry$Point(list(-73.53, -15.75))
#' Map$centerObject(geom1, zoom = 8)
#' m1 <- Map$addLayer(
#'   eeObject = geom1,
#'   visParams = list(
#'     pointRadius = 10,
#'     color = "FF0000"
#'   ),
#'   name = "Geometry-Arequipa"
#' )
#'
#' # Case 2: Feature
#' feature_arq <- ee$Feature(ee$Geometry$Point(list(-72.53, -15.75)))
#' m2 <- Map$addLayer(
#'   eeObject = feature_arq,
#'   name = "Feature-Arequipa"
#' )
#' m2 + m1
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
#'
#' # Case 5: mapview + EarthEnginemap
#' library(mapview)
#' library(sf)
#' nc <- st_read(system.file("shp/arequipa.shp", package="rgee"))
#' mapview(nc, m2)
#'
#' # Case 6: mapedit
#' library(mapedit)
#' # my_geometry <- m4 %>% editMap()
#'
#' # Case 7: ImageCollection
#' nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
#'   st_transform(4326) %>%
#'   sf_as_ee()
#'
#' ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
#'   filterDate("2016-01-01", "2016-01-31")$
#'   filterBounds(nc) %>%
#'   ee_get(0:4)
#' Map$centerObject(nc$geometry())
#' m5 <- Map$addLayers(ee_s2, legend = TRUE)
#' m5
#'
#' # Case 8: Map comparison
#' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
#' Map$centerObject(image)
#' m_ndvi <- Map$addLayer(
#'   eeObject = image$normalizedDifference(list("B5", "B4")),
#'   visParams = list(max = 0.7),
#'   name = "SF_NDVI",
#'   legend = TRUE
#' )
#' m6 <- m4 | m_ndvi
#' m6
#'
#' # Case 9: digging up the metadata
#' m6$rgee$tokens
#' m5$rgee$tokens
#' }
#' @export
Map <- function() {
  Map <- new.env(parent = emptyenv())
}

ee_set_methods <- function() {
  Map$addLayer <- ee_addLayer
  Map$addLayers <- ee_addLayers
  Map$setCenter <- ee_setCenter
  Map$setZoom <- ee_setZoom
  Map$centerObject <- ee_centerObject

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
ee_centerObject <- function(eeObject,
                            zoom = NULL,
                            maxError = ee$ErrorMargin(1)) {
  if (any(class(eeObject) %in% "ee.featurecollection.FeatureCollection")) {
    message("NOTE: Center obtained from the first element.")
    eeObject <- ee$Feature(ee$FeatureCollection$first(eeObject))
  }

  if (any(class(eeObject) %in% ee_get_spatial_objects("Nongeom"))) {
    center <- tryCatch(
      expr = eeObject$
        geometry()$
        centroid(maxError)$
        getInfo() %>%
        '[['('coordinates') %>%
        ee_utils_py_to_r(),
      error = function(e) {
        message(
          "The centroid coordinate was not possible",
          " to estimate, assigning: c(0,0)"
        )
        c(0, 0)
      }
    )

    if (is.null(center)) {
      message(
        "The centroid coordinate was not possible",
        " to estimate, assigning: c(0,0)"
      )
      center <- c(0, 0)
    }
  } else if (any(class(eeObject) %in% "ee.geometry.Geometry")) {
    center <- tryCatch(
      expr = eeObject$
        centroid(maxError)$
        coordinates()$
        getInfo() %>%
        ee_utils_py_to_r(),
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
    zoom <- ee_getZoom(eeObject, maxError = maxError)
  }
  Map$setCenter(lon = center[1], lat = center[2], zoom = zoom)
}

#' Adds a given EE object to the map as a layer.
#' https://developers.google.com/earth-engine/api_docs#map.addlaye
#' @noRd
ee_addLayer <- function(eeObject,
                        visParams = NULL,
                        name = NULL,
                        shown = TRUE,
                        opacity = 1,
                        legend = FALSE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("package jsonlite required, please install it first")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("package leaflet required, please install it first")
  }
  if (is.null(visParams)) {
    visParams <- list()
  }

  # Earth Engine Spatial object
  ee_spatial_object <- ee_get_spatial_objects("Simple")

  if (!any(class(eeObject) %in% ee_spatial_object)) {
    stop(
      "The eeObject argument must be an instance of one",
      " of ee$Image, ee$Geometry, ee$Feature, or ee$FeatureCollection."
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
    image_fill <- features %>%
      ee$FeatureCollection$style(fillColor = color) %>%
      ee$Image$updateMask(ee$Image$constant(0.5))

    image_outline <- features %>%
      ee$FeatureCollection$style(
        color = color,
        fillColor = "00000000",
        width = width
      )
    image <- ee$Image$blend(image_fill, image_outline)
  } else {
    ee_img_viz <- function(...) ee$Image$visualize(eeObject, ...)
    image <- do.call(ee_img_viz, visParams)
  }

  if (is.null(name)) {
    name <- tryCatch(
      expr = ee_get_system_id(eeObject),
      error = function(e) basename(tempfile(pattern = "untitled_"))
    )
    if (is.null(name)) name <- basename(tempfile(pattern = "untitled_"))
  }

  tile <- get_ee_image_url(image)
  map <- ee_addTile(tile = tile, name = name, shown = shown, opacity = opacity)

  if (legend) {
    ee_add_legend(map, eeObject, visParams, name)
  } else {
    map
  }
}

#' Adds a given ee$ImageCollection to the map as a layer.
#' https://developers.google.com/earth-engine/api_docs#map.addlayer
#' @noRd
ee_addLayers <- function(eeObject,
                         visParams = NULL,
                         nmax = 5,
                         name = NULL,
                         shown = TRUE,
                         opacity = 1,
                         legend = FALSE) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("package jsonlite required, please install it first")
  }

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("package leaflet required, please install it first")
  }

  # is an ee.imagecollection.ImageCollection?
  if (!any(class(eeObject) %in% "ee.imagecollection.ImageCollection")) {
    stop("eeObject argument is not an ee$imagecollection$ImageCollection")
  }

  if (is.null(visParams)) {
    visParams <- list()
  }

  # size of objects
  eeObject_size <- eeObject %>%
    ee$ImageCollection$size() %>%
    ee$Number$getInfo()

  m_img_list <- list()

  if (is.null(name)) {
    name <- tryCatch(
      expr = eeObject %>%
        ee$ImageCollection$aggregate_array("system:id") %>%
        ee$List$getInfo() %>%
        ee_utils_py_to_r() %>%
        basename(),
      error = function(e) sprintf("untitled_%02d", seq_len(eeObject_size))
    )
    if (length(name) == 0 | is.null(name)) name <- sprintf("untitled_%02d", seq_len(eeObject_size))
  }

  if (length(name) == 1) {
    name <- sprintf("%s_%02d", name, seq_len(eeObject_size))
  }

  if (length(name) == length(eeObject_size)) {
    stop("name does not have the same length than eeObject$size()$getInfo().")
  }

  for (index in seq_len(eeObject_size)) {
    py_index <- index - 1
    if (py_index == 0) {
      m_img <- Map$addLayer(
        eeObject = ee_get(eeObject, index = py_index)$first(),
        visParams = visParams,
        name = name[index],
        shown = shown,
        opacity = opacity,
        legend = legend
      )
    } else {
      m_img <- Map$addLayer(
        eeObject = ee_get(eeObject, index = py_index)$first(),
        visParams = visParams,
        name = name[index],
        shown = shown,
        opacity = opacity,
        legend = FALSE
      )
    }
    m_img_list[[index]] <- m_img
  }
  Reduce('+', m_img_list)
}


#' Basic base mapview object
#' @noRd
ee_mapview <- function() {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("package leaflet required, please install it first")
  }
  m <- leaflet_default()
  m$x$setView[[1]] <- c(Map$lat, Map$lon)
  m$x$setView[[2]] <- if (is.null(Map$zoom)) 1 else Map$zoom
  m
}

#' Add a mapview object based on a tile_fetcher
#' @noRd
ee_addTile <- function(tile, name, shown, opacity) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("package leaflet required, please install it first")
  }
  m <- ee_mapview()
  m <- m %>%
    leaflet::addTiles(
      urlTemplate = tile,
      layerId = name,
      group = name,
      options = leaflet::tileOptions(opacity = opacity)
    ) %>%
    ee_mapViewLayersControl(names = name) %>%
    leaflet::hideGroup(if (!shown) name else NULL)

  # map parameters
  m$rgee$tokens <- tile
  m$rgee$name <- name
  m$rgee$opacity <- opacity
  m$rgee$shown <- shown

  # legend parameters
  m$rgee$min <- NA
  m$rgee$max <- NA
  m$rgee$palette <-  list(NA)
  m$rgee$legend <-  FALSE
  m
}

#' Add legend to EarthEngineMap objects
#' @noRd
ee_add_legend <- function(m, eeObject, visParams, name) {
  ee_obj_class <- class(eeObject)
  type <- ee_obj_class[ee_obj_class %in%  ee_get_spatial_objects("All")]
  if (type == "ee.image.Image") {
    # add legend only to one-band images
    if (is.null(visParams$bands) | length(visParams$bands) == 1) {
      if (is.null(visParams$max) | is.null(visParams$min)) {
        eeimage_type <- eeObject$bandTypes()$getInfo()
        eeimage_type_min <- eeimage_type[[1]]$min
        # Added to deal with PixelType Images
        if (is.null(eeimage_type_min)) {
          eeimage_type_min <- 0
        }
        eeimage_type_max <- eeimage_type[[1]]$max
        if (is.null(eeimage_type_max)) {
          eeimage_type_max <- 1
        }
      }
      if (is.null(visParams$max)) {
        visParams$max <- eeimage_type_max
      }
      if (is.null(visParams$min)) {
        visParams$min <- eeimage_type_min
      }
      if (is.null(visParams$palette)) {
        visParams$palette <- c("000000", "FFFFFF")
      }
      visParams$palette <- sprintf("#%s", gsub("#", "",visParams$palette))
      pal <- leaflet::colorNumeric(visParams$palette, c(visParams$min, visParams$max))
      m <- m %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = c(visParams$min, visParams$max),
          opacity = 1,
          title = name
        )

      # Extra parameters to EarthEngineMap objects that inherit from
      # one single-band ee$Image and active legend = TRUE
      m$rgee$min <- visParams$min
      m$rgee$max <- visParams$max
      m$rgee$palette <-  list(pal)
      m$rgee$legend <-  TRUE
      m
    } else {
      m
    }
  } else {
    m
  }
}

#' Get the tile_fetcher to display into ee_map
#' @noRd
get_ee_image_url <- function(image) {
  map_id <- ee$data$getMapId(list(image = image))
  url <- map_id[["tile_fetcher"]]$url_format
  url
}


#' Return R classes for Earth Engine Spatial Objects
#' @noRd
ee_get_spatial_objects <- function(type = "All") {
  if (type == "Table") {
    ee_spatial_object <- c(
      "ee.geometry.Geometry",
      "ee.feature.Feature",
      "ee.featurecollection.FeatureCollection"
    )
  }
  if (type == "i+ic") {
    ee_spatial_object <- c(
      "ee.image.Image", "ee.imagecollection.ImageCollection")
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
  if (type == "justfeature") {
    ee_spatial_object <- c(
      "ee.feature.Feature",
      "ee.featurecollection.FeatureCollection"
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
#' Adapted from Python to R
#' https://github.com/fitoprincipe/ipygee/
#' https://stackoverflow.com/questions/6048975/
#' @noRd
ee_getZoom <- function(eeObject, maxError = ee$ErrorMargin(1)) {
  bounds <- ee_get_boundary(eeObject, maxError)

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
#' @noRd
ee_get_boundary <- function(eeObject, maxError) {
  if (any(class(eeObject) %in% "ee.geometry.Geometry")) {
    eeObject <- ee$Feature(eeObject)
  }
  eeObject$geometry()$bounds(maxError)$getInfo() %>%
    '[['('coordinates') %>%
    ee_utils_py_to_r() %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_bbox()
}


#' Get system id from an earth engine spatial object
#' @noRd
ee_get_system_id <- function(eeObject) {
  if (any(class(eeObject) %in% "ee.image.Image")) {
    eeObject %>%
      ee$Image$get("system:id") %>%
      ee$ComputedObject$getInfo() %>%
      basename()
  } else if (any(class(eeObject) %in% "ee.feature.Feature")) {
    eeObject %>%
      ee$Feature$get("system:id") %>%
      ee$ComputedObject$getInfo() %>%
      basename()
  } else if (any(class(eeObject) %in% "ee.featurecollection.FeatureCollection")) {
    eeObject %>%
      ee$FeatureCollection$get("system:id") %>%
      ee$ComputedObject$getInfo() %>%
      basename()
  } else if (any(class(eeObject) %in% "ee.imagecollection.ImageCollection")) {
    eeObject %>%
      ee$ImageCollection$get("system:id") %>%
      ee$ComputedObject$getInfo() %>%
      basename()
  } else {
    stop("Impossible to get system:id")
  }
}


#' Create a default leaflet
#' @noRd
leaflet_default <- function (default_maps = NULL) {
  if (is.null(default_maps)) {
    default_maps <- c(
      "CartoDB.Positron", "OpenStreetMap",
      "CartoDB.DarkMatter", "Esri.WorldImagery",
      "OpenTopoMap"
    )
  }
  m <- initBaseMaps(default_maps)
  m <- leaflet::setView(map = m, -76.942478, -12.172116, zoom = 18)
  m <- leaflet::addLayersControl(
    map = m,
    baseGroups = default_maps,
    position = "topleft"
  )
  m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- leafem::addMouseCoordinates(m)
  m <- leafem::addCopyExtent(m)
  class(m) <- append(class(m),"EarthEngineMap")
  m
}

#' Create a default leaflet with initBaseMaps
#' @noRd
initBaseMaps <- function (map.types, canvas = FALSE, viewer.suppress = FALSE) {
  lid <- seq_along(map.types)
  m <- leaflet::leaflet(
    height = NULL,
    width = NULL,
    options = leaflet::leafletOptions(
      minZoom = 1, maxZoom = 52,
      bounceAtZoomLimits = FALSE,
      maxBounds = list(list(c(-90,-370)), list(c(90, 370))),
      preferCanvas = canvas),
    sizingPolicy = leaflet::leafletSizingPolicy(
      viewer.suppress = viewer.suppress,
      browser.external = viewer.suppress))
  # add Tiles
  m <- leaflet::addProviderTiles(
    map = m,
    provider = map.types[1],
    layerId = map.types[1],
    group = map.types[1],
    options = leaflet::providerTileOptions(pane = "tilePane")
  )
  for (i in 2:length(map.types)) {
    m <- leaflet::addProviderTiles(
      map = m,
      provider = map.types[i],
      layerId = map.types[i],
      group = map.types[i],
      options = leaflet::providerTileOptions(pane = "tilePane"))
  }
  return(m)
}


# Create an Map env and set methods
Map <- Map()
ee_set_methods()
