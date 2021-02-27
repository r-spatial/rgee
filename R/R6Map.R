#' R6 object to display Earth Engine (EE) spatial objects
#'
#' @description Create interactive visualizations of spatial EE objects
#' (ee$Geometry, ee$Image, ee$Feature, and ee$FeatureCollection)
#' using \code{leaflet}.
#'
#' @details
#' `Map` use the Earth Engine method
#' \href{https://developers.google.com/earth-engine/api_docs#ee.data.getmapid/}{
#' getMapId} to fetch and return an ID dictionary being used to create
#' layers in a \code{leaflet} object. Users can specify visualization
#' parameters to Map$addLayer by using the visParams argument. Each Earth
#' Engine spatial object has a specific format. For
#' \code{ee$Image}, the
#' \href{https://developers.google.com/earth-engine/guides/image_visualization}{
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
#' @name Map
#' @aliases Map
#' @noRd
R6Map <- R6::R6Class(
  classname = "EarthEngineMap",
  public = list(
    #' @field lon The longitude of the center, in degrees.
    lon = NULL,

    #' @field lat The latitude of the center, in degrees.
    lat = NULL,

    #' @field zoom The zoom level, from 1 to 24.
    zoom = NULL,

    #' @description Ignore.
    #' @param lon The longitude of the center, in degrees. By default -76.942478.
    #' @param lat The latitude of the center, in degrees. By default -12.172116.
    #' @param zoom The zoom level, from 1 to 24. By default 18.
    #' @return A new `Map` object.
    initialize = function(lon = -76.942478, lat = -12.172116 , zoom = 18) {
      self$lon <- lon
      self$lat <- lat
      self$zoom <- zoom
    },

    #' @description
    #' Display a default map.
    #' @examples
    #' library(rgee)
    #' Map
    #' @return A `EarthEngineMap` object.
    print = function() {
      m <- leaflet_default(self$lon, self$lat, self$zoom)
      print(m)
    },

    #' @description
    #' Centers the map view at the given coordinates with the given zoom level. If
    #' no zoom level is provided, it uses 10 by default.
    #' @param lon The longitude of the center, in degrees. By default -76.942478.
    #' @param lat The latitude of the center, in degrees. By default -12.172116.
    #' @param zoom The zoom level, from 1 to 24. By default 18.
    #' @return No return value, called to set initial coordinates and zoom.
    #' @examples
    #' library(rgee)
    #' Map$setCenter(lon = -76, lat = 0, zoom = 6)
    #' Map
    setCenter = function(lon = 0, lat = 0, zoom = 10) {
      private$set_lat(lat)
      private$set_lon(lon)
      private$set_zoom(zoom)
    },

    #' @description
    #' Sets the zoom level of the map.
    #' @param zoom The zoom level, from 1 to 24. By default 10.
    #' @return No return value, called to set zoom.
    #' @examples
    #' library(rgee)
    #' Map$setZoom(zoom = 3)
    #' Map
    setZoom = function(zoom = 10) {
      private$set_zoom(zoom)
    },

    #' @description
    #' Centers the map view on a given object. If no zoom level is provided, it
    #' will be predicted according to the bounds of the Earth Engine object
    #' specified.
    #' @param eeObject Earth Engine spatial object.
    #' @param zoom The zoom level, from 1 to 24. By default NULL.
    #' @param maxError Max error when input image must be reprojected to an
    #' explicitly requested result projection or geodesic state.
    #' @return No return value, called to set zoom.
    #' @examples
    #' \dontrun{
    #' library(rgee)
    #' ee_Initialize()
    #' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
    #' Map$centerObject(image)
    #' }
    centerObject = function(eeObject,
                            zoom = NULL,
                            maxError = ee$ErrorMargin(1)) {
      viewer_params <- private$get_center(eeObject, zoom, maxError)
      private$set_lat(viewer_params$lat)
      private$set_lon(viewer_params$lon)
      private$set_zoom(viewer_params$zoom)
    },

    #' @description
    #'
    #' Adds a given Eath Engine spatial object to the map as a layer
    #'
    #' @param eeObject The Earth Engine spatial object to display in the interactive map.
    #' @param visParams List of parameters for visualization. See details.
    #' @param name The name of layers.
    #' @param shown A flag indicating whether layers should be on by default.
    #' @param opacity The layer's opacity represented as a number between 0 and 1. Defaults to 1.
    #' @param legend Should a legend be plotted?. Only the legend of the first image is displayed.
    #'
    #' @return A `EarthEngineMap` object.
    #'
    #' @examples
    #' \dontrun{
    #' library(rgee)
    #' ee_Initialize()
    #' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
    #' Map$centerObject(image)
    #' Map$addLayer(image)
    #' }
    addLayer = function(eeObject,
                        visParams = NULL,
                        name = NULL,
                        shown = TRUE,
                        opacity = 1,
                        legend = FALSE) {
      # check packages
      ee_check_packages("Map$addLayer", c("jsonlite", "leaflet", "leafem"))
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
    },

    #' @description
    #'
    #' Adds a given ee$ImageCollection to the map as multiple layers.
    #'
    #' @param eeObject ee$ImageCollection to display in the interactive map.
    #' @param visParams List of parameters for visualization. See details.
    #' @param nmax Numeric. The maximum number of images to display. By default 5.
    #' @param name The name of layers.
    #' @param shown A flag indicating whether layers should be on by default.
    #' @param opacity The layer's opacity represented as a number between 0 and 1. Defaults to 1.
    #' @param legend Should a legend be plotted?. Only the legend of the first image is displayed.
    #'
    #' @return A `EarthEngineMap` object.
    #' @examples
    #' \dontrun{
    #' library(rgee)
    #'
    #' ee_Initialize()
    #'
    #' nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
    #'   st_transform(4326) %>%
    #'   sf_as_ee()
    #'
    #' ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
    #'   filterDate("2016-01-01", "2016-01-31")$
    #'   filterBounds(nc) %>%
    #'   ee_get(0:4)
    #' Map$centerObject(nc$geometry())
    #'
    #' m5 <- Map$addLayers(ee_s2, legend = TRUE)
    #' m5
    #'
    #' # digging up the metadata
    #' m5$rgee$tokens
    #' }
    addLayers = function(eeObject,
                         visParams = NULL,
                         nmax = 5,
                         name = NULL,
                         shown = TRUE,
                         opacity = 1,
                         legend = FALSE) {
      # check packages
      ee_check_packages("Map$addLayers", c("jsonlite", "leaflet"))

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
  ),
  private = list(
    #' Setter method (latitude)
    set_lat = function(val) {
      self$lat <- val
    },
    #' Setter method (longitude)
    set_lon = function(val) {
      self$lon <- val
    },
    #' Setter method (zoom)
    set_zoom = function(val) {
      self$zoom <- val
    },
    #' Predict the latitude, longitude and zoom of a specific Earth Engine Object
    get_center = function(eeObject, zoom, maxError) {
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

      # Set new initial view
      list(lon = center[1], lat = center[2], zoom = zoom)
    }
  )
)


#' Module to display Earth Engine (EE) spatial objects
#'
#' Create interactive visualizations of spatial EE objects
#' (ee$FeatureCollection, ee$ImageCollection, ee$Geometry, ee$Feature, and
#' ee$Image.) using \code{leaflet} in the backend.
#' @format An object of class environment with the
#' following functions:
#' \itemize{
#'   \item  \strong{addLayer(eeObject, visParams, name = NULL, shown = TRUE,
#'   opacity = 1, legend = FALSE)}: Adds a given EE object to the map as a layer. \cr
#'   \itemize{
#'     \item \strong{eeObject:} The object to add to the interactive map.\cr
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
#'     \item \strong{eeObject:} The ee$ImageCollection to add to the interactive map.\cr
#'     \item \strong{visParams:} List of parameters for visualization.
#'     See details.\cr
#'     \item \strong{name:} The name of layers.\cr
#'     \item \strong{shown:} A flag indicating whether
#'     layers should be on by default. \cr
#'     \item \strong{opacity:} The layer's opacity represented as a number
#'      between 0 and 1. Defaults to 1. \cr
#'      \item \strong{nmax:} Numeric. The maximum number of images to display.
#'      By default 5. \cr
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
#' \href{https://developers.google.com/earth-engine/api_docs#ee.data.getmapid/}{
#' getMapId} to fetch and return an ID dictionary being used to create
#' layers in a \code{leaflet} object. Users can specify visualization
#' parameters to Map$addLayer by using the visParams argument. Each Earth
#' Engine spatial object has a specific format. For
#' \code{ee$Image}, the
#' \href{https://developers.google.com/earth-engine/guides/image_visualization}{
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
#' # Case 5: ImageCollection
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
#' # Case 6: Map comparison
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
#' # Case 7: digging up the metadata
#' m6$rgee$tokens
#' m5$rgee$tokens
#' }
#' @export
Map <- R6Map$new()
