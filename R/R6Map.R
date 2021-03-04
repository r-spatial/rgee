#' R6 class to display Earth Engine (EE) spatial objects
#'
#' @description Create interactive visualizations of spatial EE objects
#' (ee$Geometry, ee$Image, ee$Feature, and ee$FeatureCollection)
#' using \code{leaflet}.
#'
#' @details
#' `R6Map` uses the Earth Engine method
#' \href{https://developers.google.com/earth-engine/api_docs#ee.data.getmapid/}{
#' getMapId} to fetch and return an ID dictionary used to create
#' layers in a \code{leaflet} object. Users can specify visualization
#' parameters to Map$addLayer by using the visParams argument. Each Earth
#' Engine spatial object has a specific format. For
#' \code{ee$Image}, the
#' \href{https://developers.google.com/earth-engine/guides/image_visualization}{
#' parameters} available are:
#'
#' \tabular{lll}{
#' \strong{Parameter}\tab \strong{Description}  \tab \strong{Type}\cr
#' \strong{bands}    \tab  Comma-delimited list of three band (RGB) \tab  list \cr
#' \strong{min}      \tab  Value(s) to map to 0 \tab  number or list of three
#' numbers, one for each band \cr
#' \strong{max}      \tab  Value(s) to map to 1 \tab  number or list of three
#' numbers, one for each band \cr
#' \strong{gain}     \tab  Value(s) by which to multiply each pixel value \tab
#' number or list of three numbers, one for each band \cr
#' \strong{bias}     \tab  Value(s) to add to each Digital Number
#' value \tab number or list of three numbers, one for each band \cr
#' \strong{gamma}    \tab  Gamma correction factor(s) \tab  number or list of
#' three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings
#' (single-band only) \tab  comma-separated list of hex strings \cr
#' \strong{opacity}   \tab  The opacity of the layer (from 0 to 1)  \tab  number \cr
#' }
#'
#' If you add an \code{ee$Image} to Map$addLayer without any additional
#' parameters. By default it assigns the first three bands to red,
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
#'
#' @returns Object of class \code{leaflet} and \code{EarthEngineMap}, with the
#' following extra parameters: tokens, name, opacity, shown, min, max, palette,
#' position, and legend. Use the $ method to retrieve the data (e.g., m$rgee$min).
#'
#' @export
R6Map <- R6::R6Class(
  classname = "EarthEngineMap",
  public = list(
    #' @field lon The longitude of the center, in degrees.
    lon = NULL,

    #' @field lat The latitude of the center, in degrees.
    lat = NULL,

    #' @field zoom The zoom level, from 1 to 24.
    zoom = NULL,

    #' @field save_maps Should `R6Map` save the previous maps?. If TRUE, Map
    #' will work in an OOP style. Otherwise it will be a functional programming
    #' style.
    save_maps = NULL,

    #' @field previous_map_left Container on maps in the left side.
    previous_map_left = NULL,

    #' @field previous_map_right Container on maps in the right side.
    previous_map_right = NULL,

    #' @description Constructor of R6Map.
    #' @param lon The longitude of the center, in degrees. By default -76.942478.
    #' @param lat The latitude of the center, in degrees. By default -12.172116.
    #' @param zoom The zoom level, from 1 to 24. By default 18.
    #' @param save_maps Should `R6Map` save previous maps?.
    #' @return A new `EarthEngineMap` object.
    initialize = function(lon = -76.942478, lat = -12.172116 , zoom = 18, save_maps = TRUE) {
      self$lon <- lon
      self$lat <- lat
      self$zoom <- zoom
      self$save_maps <- save_maps
      self$previous_map_left = private$ee_mapview()
      self$previous_map_right = private$ee_mapview()
    },

    #' @description Reset to initial arguments.
    #' @param lon The longitude of the center, in degrees. By default -76.942478.
    #' @param lat The latitude of the center, in degrees. By default -12.172116.
    #' @param zoom The zoom level, from 1 to 24. By default 18.
    #' @param save_maps Should `R6Map` save previous maps?.
    #' @return A new `EarthEngineMap` object.
    #' @examples
    #' library(rgee)
    #' ee_Initialize()
    #'
    #' # Load an Image
    #' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
    #'
    #' # Create
    #' Map <- R6Map$new()
    #' Map$centerObject(image)
    #'
    #' # Simple display: Map just will
    #' Map$addLayer(
    #'   eeObject = image,
    #'   visParams = list(min=0, max = 10000, bands = c("B4", "B3", "B2")),
    #'   name = "l8_01"
    #' )
    #' Map # display map
    #'
    #' Map$reset() # Reset arguments
    #' Map
    reset = function(lon = -76.942478, lat = -12.172116 , zoom = 18, save_maps = TRUE) {
      self$lon <- lon
      self$lat <- lat
      self$zoom <- zoom
      self$save_maps <- save_maps
      self$previous_map_left = private$ee_mapview()
      self$previous_map_right = private$ee_mapview()
    },

    #' @description
    #' Display a \code{EarthEngineMap} object.
    #' @return An `EarthEngineMap` object.
    print = function() {
      m1 <- private$get_previous_map_right()
      m2 <- private$get_previous_map_left()
      if (is.null(m1$rgee$position) & is.null(m2$rgee$position)) {
        m3 <- m1
      } else {
        m3 <- m2 | m1
      }
      print(m3)
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
    #'
    #' ee_Initialize()
    #'
    #' Map <- R6Map$new()
    #' Map$setCenter(lon = -76, lat = 0, zoom = 5)
    #' Map
    #'
    #' # Map$lat
    #' # Map$lon
    #' # Map$zoom
    setCenter = function(lon = 0, lat = 0, zoom = 10) {
      private$upgrade_center_right(lon, lat, zoom)
      private$upgrade_center_left(lon, lat, zoom)
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
    #'
    #' ee_Initialize()
    #'
    #' Map <- R6Map$new()
    #' Map$setZoom(zoom = 4)
    #' Map
    #'
    #' # Map$lat
    #' # Map$lon
    #' # Map$zoom
    setZoom = function(zoom = 10) {
      private$upgrade_center_right(self$lon, self$lat, zoom)
      private$upgrade_center_left(self$lon, self$lat, zoom)
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
    #' library(rgee)
    #'
    #' ee_Initialize()
    #'
    #' Map <- R6Map$new()
    #' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
    #' Map$centerObject(image)
    #' Map
    centerObject = function(eeObject,
                            zoom = NULL,
                            maxError = ee$ErrorMargin(1)) {
      viewer_params <- private$get_center(eeObject, zoom, maxError)
      self$setCenter(viewer_params$lon, viewer_params$lat, viewer_params$zoom)
    },

    #' @description
    #'
    #' Adds a given Eath Engine spatial object to the map as a layer
    #'
    #' @param eeObject The Earth Engine spatial object to display in the interactive map.
    #' @param visParams List of parameters for visualization. See details.
    #' @param name The name of layers.
    #' @param shown A flag indicating whether layers should be on by default.
    #' @param opacity The layer's opacity is represented as a number between 0 and 1. Defaults to 1.
    #' @param legend Should a legend be plotted?. Only the legend of the first image is displayed.
    #' @param position Character. Activate panel creation. If "left" the map will be displayed in
    #' the left panel. Otherwise, if it is "right" the map will be displayed in the right panel.
    #' By default NULL (No panel will be created).
    #'
    #' @return An `EarthEngineMap` object.
    #'
    #' @examples
    #'
    #' library(rgee)
    #' ee_Initialize()
    #'
    #' # Load an Image
    #' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
    #'
    #' # Create
    #' Map <- R6Map$new()
    #' Map$centerObject(image)
    #'
    #' # Simple display: Map just will
    #' Map$addLayer(
    #'   eeObject = image,
    #'   visParams = list(min=0, max = 10000, bands = c("B4", "B3", "B2")),
    #'   name = "l8_01"
    #' )
    #'
    #' Map$addLayer(
    #'   eeObject = image,
    #'   visParams = list(min=0, max = 20000, bands = c("B4", "B3", "B2")),
    #'   name = "l8_02"
    #' )
    #'
    #' # Simple display: Map just will (if the position is not specified it will
    #' # be saved on the right side)
    #' Map$reset() # Reset Map to the initial arguments.
    #' Map$centerObject(image)
    #' Map$addLayer(
    #'   eeObject = image,
    #'   visParams = list(min=0, max=10000, bands = c("B4", "B3", "B2")),
    #'   name = "l8_left",
    #'   position = "left"
    #' )
    #'
    #' Map$addLayer(
    #'   eeObject = image,
    #'   visParams = list(min=0, max=20000, bands = c("B4", "B3", "B2")),
    #'   name = "l8_right"
    #' )
    #'
    addLayer = function(eeObject,
                        visParams = NULL,
                        name = NULL,
                        shown = TRUE,
                        opacity = 1,
                        position = NULL,
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


        # If vizparams is NULL
        width <- 2
        if (!is.null(visParams[["width"]])) {
          width <- visParams[["width"]]
        }

        color <- "000000"
        if (!is.null(visParams[["color"]])) {
          color <- visParams[["color"]]
        }

        # Convert features to a images
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

      # If name is null try to obtain from image metadata if not untitled_
      # would be the name.
      if (is.null(name)) {
        name <- tryCatch(
          expr = ee_get_system_id(eeObject),
          error = function(e) basename(tempfile(pattern = "untitled_"))
        )
        if (is.null(name)) name <- basename(tempfile(pattern = "untitled_"))
      }

      # Get token
      tile <- get_ee_image_url(image)

      # Using the previous token create a map using leaflet package
      map <- private$ee_addTile(
        tile = tile,
        name = name,
        visParams = visParams,
        shown = shown,
        opacity = opacity,
        position = position
      )

      if (legend) {
        map <- ee_add_legend(map, eeObject, visParams, name)
      }

      if (isTRUE(self$save_maps)) {
        # Save the previous map in previous_map_left or previous_map_right
        # according to posisa tion argument.
        private$save_map(map, position = position)
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
    #' @param opacity The layer's opacity is represented as a number between 0 and 1. Defaults to 1.
    #' @param legend Should a legend be plotted?. Only the legend of the first image is displayed.
    #' @param position Character. Activate panel creation. If "left" the map will be displayed in
    #' the left panel. Otherwise, if it is "right" the map will be displayed in the right panel.
    #' By default NULL (No panel will be created).
    #'
    #' @return A `EarthEngineMap` object.
    #' @examples
    #' library(sf)
    #' library(rgee)
    #'
    #' ee_Initialize()
    #'
    #' Map <- R6Map$new()
    #'
    #' nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
    #'   st_transform(4326) %>%
    #'   sf_as_ee()
    #'
    #' ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
    #'   filterDate("2016-01-01", "2016-01-31")$
    #'   filterBounds(nc) %>%
    #'   ee_get(0:2)
    #'
    #' Map$centerObject(nc$geometry())
    #' Map$addLayers(eeObject = ee_s2, legend = TRUE, position = "right")
    #' Map$reset()
    #'
    #' # digging up the metadata
    #' Map$previous_map_right$rgee$tokens
    addLayers = function(eeObject,
                         visParams = NULL,
                         nmax = 5,
                         name = NULL,
                         shown = TRUE,
                         position = NULL,
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

      m_img_list <- list()
      for (index in seq_len(eeObject_size)) {
        py_index <- index - 1
        if (index == 1) {
          m_img <- self$addLayer(
            eeObject = ee_get(eeObject, index = py_index)$first(),
            visParams = visParams,
            name = name[index],
            shown = shown,
            opacity = opacity,
            position = position,
            legend = legend
          )
        } else {
          m_img <- self$addLayer(
            eeObject = ee_get(eeObject, index = py_index)$first(),
            visParams = visParams,
            name = name[index],
            shown = shown,
            opacity = opacity,
            position = position,
            legend = FALSE
          )
        }
        m_img_list[[index]] <- m_img
      }

      if (isFALSE(self$save_maps)) {
        Reduce('+', m_img_list)
      }
    }
  ),

  private = list(
    get_previous_map_right = function() {
      self$previous_map_right
    },
    set_previous_map_right = function(val) {
        self$previous_map_right <- val
    },
    get_previous_map_left = function() {
      self$previous_map_left
    },
    set_previous_map_left = function(val) {
      self$previous_map_left <- val
    },
    set_lat = function(val) {
      self$lat <- val
    },
    set_lon = function(val) {
      self$lon <- val
    },
    set_zoom = function(val) {
      self$zoom <- val
    },
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
    },
    ee_mapview = function() {
      # check packages
      private$ee_check_packages("ee_mapview", "leaflet")
      m <- private$leaflet_default()
      m$x$setView[[1]] <- c(self$lat, self$lon)
      m$x$setView[[2]] <- if (is.null(self$zoom)) 1 else self$zoom

      # EarthEngine Map parameters
      # m$rgee$tokens <- NA
      # m$rgee$name <- NA
      # m$rgee$opacity <- NA
      # m$rgee$shown <- NA
      # m$rgee$position <- NA
      # # legend parameters
      # m$rgee$min <- NA
      # m$rgee$max <- NA
      # m$rgee$palette <-  list(NA)
      # m$rgee$legend <-  NA
      m
    },
    ee_addTile = function(tile, name, visParams, shown, opacity, position) {
      # check packages
      ee_check_packages("Map$addLayer", c("leaflet"))
      m <- private$ee_mapview() %>%
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
      m$rgee$position <- position

      # legend parameters
      m$rgee$min <- if (is.null(visParams$min)) NA else visParams$min
      m$rgee$max <- if (is.null(visParams$max)) NA else visParams$max
      m$rgee$palette <-  if (is.null(visParams$palette)) list(NA) else list(visParams$palette)
      m$rgee$legend <-  FALSE
      m
    },
    save_map = function(map, position = NULL) {
      if (is.null(position)) {
        private$set_previous_map_right(self$previous_map_right + map)
      } else {
        if (tolower(position) == "right") {
          private$set_previous_map_right(self$previous_map_right + map)
        } else if (tolower(position) == "left") {
          private$set_previous_map_left(self$previous_map_left + map)
        } else {
          stop("position should be a character either 'right' or 'left'")
        }
      }
    },
    leaflet_default = function(lon = -76.942478, lat = -12.172116, zoom  = 18, default_maps = NULL) {
      if (is.null(default_maps)) {
        default_maps <- c(
          "CartoDB.Positron", "OpenStreetMap",
          "CartoDB.DarkMatter", "Esri.WorldImagery",
          "OpenTopoMap"
        )
      }
      m <- private$initBaseMaps(default_maps)
      m <- leaflet::setView(map = m, lon, lat, zoom)
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
    },
    initBaseMaps = function (map.types, canvas = FALSE, viewer.suppress = FALSE) {
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
    },
    upgrade_center_right = function(lon = 0, lat = 0, zoom = 10) {
      self$previous_map_right <- self$previous_map_right %>%
        leaflet::setView(lon, lat, zoom = zoom)
    },
    upgrade_center_left = function(lon = 0, lat = 0, zoom = 10) {
      self$previous_map_left <- self$previous_map_left %>%
        leaflet::setView(lon, lat, zoom = zoom)
    },
    ee_check_packages = function(fn_name, packages) {
      pkg_exists <- rep(NA, length(packages))
      counter <- 0
      for(package in packages) {
        counter <- counter + 1
        pkg_exists[counter] <- requireNamespace(package, quietly = TRUE)
      }

      if (!all(pkg_exists)) {
        to_install <- packages[!pkg_exists]
        to_install_len <- length(to_install)
        error_msg <- sprintf(
          "%s required the %s: %s. Please install %s first.",
          bold(fn_name),
          if (to_install_len == 1) "package" else "packages",
          paste0(bold(to_install), collapse = ", "),
          if (to_install_len == 1) "it" else "them"
        )
        stop(error_msg)
      }
    }
  )
)


#' R6 object (Map) to display Earth Engine (EE) spatial objects
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
#'     \item \strong{opacity:} The layer's opacity is represented as a number
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
#'     \item \strong{opacity:} The layer's opacity is represented as a number
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
#' \strong{bands}    \tab  Comma-delimited list of three band (RGB) \tab  list \cr
#' \strong{min}      \tab  Value(s) to map to 0 \tab  number or list of three
#' numbers, one for each band \cr
#' \strong{max}      \tab  Value(s) to map to 1 \tab  number or list of three
#' numbers, one for each band \cr
#' \strong{gain}     \tab  Value(s) by which to multiply each pixel value \tab
#' number or list of three numbers, one for each band \cr
#' \strong{bias}     \tab  Value(s) to add to each Digital Number
#' value \tab number or list of three numbers, one for each band \cr
#' \strong{gamma}    \tab  Gamma correction factor(s) \tab  number or list of
#' three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings
#' (single-band only) \tab  comma-separated list of hex strings \cr
#' \strong{opacity}   \tab  The opacity of the layer (from 0 to 1)  \tab  number \cr
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
#' @export
Map <- R6Map$new(save_maps = FALSE)
