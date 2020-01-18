#' Adds a given Earth Engine (EE) spatial object to mapview as a layer.
#'
#' Create interactive visualizations of spatial EE objects
#' (Geometry, Image, Feature, FeatureCollection or ImageCollection)
#' through \href{mapview}{mapview}.
#'
#' @param eeobject An EE spatial object.
#' @param vizparams A list that contains the visualization parameters. See details.
#' @param center The longitude and latitude of the map center. If it is not defined,
#' ee_map will try to estimate the centroid of the spatial EE object.
#' @param  zoom_start zoom level.
#' @param objname character vector. Name of the map, or maps in case that the EE object
#' be an ImageCollection.
#' @param quiet logical; suppress info messages.
#' @param ... Ignored.
#' @details
#' `ee_map` takes advantage of the ee$Image()$getMapId python function for fetch and return
#' a mapid and token that is suitable for use in a \href{mapview}{mapview}. To achieve desirable
#' visualization effects, it will depend on the type of spatial EE object . For neither Image or
#' ImageCollection, you can provide visualization parameters to ee_map by the parameter vizparams.
#' The \href{https://developers.google.com/earth-engine/image_visualization}{parameters} available are:
#'
#' \tabular{lll}{
#' \strong{Parameter}\tab \strong{Description}\tab\strong{Type}\cr
#' \strong{bands}     \tab  Comma-delimited list of three band names to be mapped to RGB                \tab  list                                               \cr
#' \strong{min}       \tab  Value(s) to map to 0                                                        \tab  number or list of three numbers, one for each band \cr
#' \strong{max}       \tab  Value(s) to map to 1                                                      \tab  number or list of three numbers, one for each band \cr
#' \strong{gain}      \tab  Value(s) by which to multiply each pixel value                              \tab  number or list of three numbers, one for each band \cr
#' \strong{bias}      \tab  Value(s) to add to each Digital Number (DN) value                           \tab  number or list of three numbers, one for each band \cr
#' \strong{gamma}     \tab  Gamma correction factor(s)                                                  \tab  number or list of three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings (single-band images only)                   \tab  comma-separated list of hex strings                \cr
#' \strong{opacity}   \tab  The opacity of the layer (0.0 is fully transparent and 1.0 is fully opaque) \tab  number                                             \cr
#' }
#'
#' If you add an Image or ImageCollection to the map without any additional parameters,
#' by default `ee_map` assigns the first three bands to red, green and blue, respectively.
#' The default stretch is based on the min-max range.  For Geometry, Feature or
#' FeatureCollection. The available vizparams are:
#' \itemize{
#'  \item \strong{color}: A hex string in the format RRGGBB specifying the color to use for drawing the features.
#'  By default 000000.
#'  \item \strong{pointRadius}: The radius of the point markers. By default 3.
#'  \item \strong{strokeWidth}: The width of lines and polygon borders. By default 3.
#' }
#' @examples
#'
#' library(rgee)
#' ee_reattach() # reattach ee as a reserve word
#' ee_Initialize()
#'
#' # Case: Geometry*
#' geom <- ee$Geometry$Point(list(-73.53522, -15.75453))
#' m1 <- ee_map(
#'   eeobject = geom,
#'   vizparams = list(pointRadius = 10, color = "FF0000"),
#'   objname = "Geometry-Arequipa"
#' )
#' m1
#'
#' # Case: Feature
#' eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")$first()
#' m2 <- ee_map(eeobject = ee$Feature(eeobject_fc), objname = "Feature-Arequipa")
#' m2 + m1
#'
#' # Case: FeatureCollection
#' eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")
#' m3 <- ee_map(eeobject = eeobject_fc, objname = "FeatureCollection")
#' m3 + m2 + m1
#'
#' # Case: Image
#' image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
#' m4 <- ee_map(
#'   eeobject = image,
#'   vizparams = list(
#'     bands = c("B4", "B3", "B2"),
#'     max = 10000
#'   ),
#'   objname = "SF",
#'   zoom_start = "8"
#' )
#' m4
#'
#' # Case: ImageCollection
#' collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
#'   filter(ee$Filter()$eq("WRS_PATH", 44))$
#'   filter(ee$Filter()$eq("WRS_ROW", 34))$
#'   filterDate("2014-01-01", "2015-01-01")$
#'   sort("CLOUD_COVER")
#'
#' m5 <- ee_map(
#'   eeobject = collection,
#'   vizparams = list(bands = c("B4", "B3", "B2"), max = 1),
#'   objname = c("Scene_2019", "Scene_2016", "Scene_2011"),
#'   max_nimage = 3,
#'   zoom_start = 10
#' )
#' m5
#'
#' @importFrom mapview mapview
#' @importFrom leaflet addWMSTiles setView
#' @importFrom sf st_polygon st_centroid st_coordinates
#' @export
#'
ee_map <- function(eeobject, ...) {
  UseMethod("ee_map")
}


#' @name ee_map
#' @export
ee_map.default <- function(eeobject, ...) {
  mapview()
}

#' @name ee_map
#' @export
ee_map.ee.geometry.Geometry <- function(eeobject,
                                        vizparams,
                                        center,
                                        zoom_start = 8,
                                        objname = "map",
                                        quiet = FALSE,
                                        ...) {
  oauth_func_path <- system.file("python/ee_map.py", package = "rgee")
  map_py <- ee_source_python(oauth_func_path)

  if (missing(vizparams)) {
    vizparams <- ee_geom_vizparams()
    vzn <- names(vizparams)
    vzv <- as.character(vizparams)
    if (!quiet) {
      cat(
        " vizparams is missing, the following params are assigned. \n",
        sprintf(
          "vizparams: {%s:'%s', %s:%s, %s:%s}\n", vzn[1], vzv[1],
          vzn[2], vzv[2], vzn[3], vzv[3]
        )
      )
    }
  }

  if (missing(center)) {
    center <- ee_py_to_r(eeobject$centroid()$getInfo()$coordinates)
    if (!quiet) {
      cat(
        " center is missing, the centroid of this EE Geometry is used: \n",
        "center: ", paste(center, collapse = " "), "\n"
      )
    }
  }

  ee_match_geom_geoviz(names(vizparams))
  vizparams <- ee_geom_exist_color(vizparams)

  tile <- ee_py_to_r(map_py$ee_map_py(eeobject, vizparams))
  create_beauty_basemap(eeobject, tile, center, objname, zoom_start)
}

#' @name ee_map
#' @export
ee_map.ee.feature.Feature <- function(eeobject,
                                      vizparams,
                                      center,
                                      zoom_start = 8,
                                      objname = "map",
                                      quiet = FALSE,
                                      ...) {
  oauth_func_path <- system.file("python/ee_map.py", package = "rgee")
  map_py <- ee_source_python(oauth_func_path)

  if (missing(vizparams)) {
    vizparams <- ee_geom_vizparams()
    vzn <- names(vizparams)
    vzv <- as.character(vizparams)
    if (!quiet) {
      cat(
        " vizparams is missing, the following params are assigned. \n",
        sprintf(
          "vizparams: {%s:'%s', %s:%s, %s:%s}\n", vzn[1], vzv[1],
          vzn[2], vzv[2], vzn[3], vzv[3]
        )
      )
    }
  }
  if (missing(center)) {
    center <- ee_py_to_r(eeobject$geometry()$centroid()$getInfo()$coordinates)
    if (!quiet) {
      cat(
        " center is missing, the centroid of this EE Feature is used: \n",
        "center: ", paste(center, collapse = " "), "\n"
      )
    }
  }

  ee_match_geom_geoviz(names(vizparams))
  vizparams <- ee_geom_exist_color(vizparams)

  tile <- py_to_r(map_py$ee_map_py(eeobject, vizparams))
  create_beauty_basemap(eeobject, tile, center, objname, zoom_start)
}

#' @name ee_map
#' @export
ee_map.ee.featurecollection.FeatureCollection <- function(eeobject,
                                                          vizparams,
                                                          center,
                                                          zoom_start = 8,
                                                          objname = "map",
                                                          quiet = FALSE,
                                                          ...) {
  oauth_func_path <- system.file("python/ee_map.py", package = "rgee")
  map_py <- ee_source_python(oauth_func_path)

  if (missing(vizparams)) {
    vizparams <- ee_geom_vizparams()
    vzn <- names(vizparams)
    vzv <- as.character(vizparams)

    if (!quiet) {
      cat(
        " vizparams is missing, the following params are assigned. \n",
        sprintf(
          "vizparams: {%s:'%s', %s:%s, %s:%s}\n", vzn[1], vzv[1],
          vzn[2], vzv[2], vzn[3], vzv[3]
        )
      )
    }
  }

  if (missing(center)) {
    center <- ee_py_to_r(eeobject$geometry()$centroid()$getInfo()$coordinates)
    if (!quiet) {
      cat(
        " center is missing, the centroid of this EE FeatureCollection is used: \n",
        "center: ", paste(center, collapse = " "), "\n"
      )
    }
  }

  ee_match_geom_geoviz(names(vizparams))
  vizparams <- ee_geom_exist_color(vizparams)

  tile <- ee_py_to_r(map_py$ee_map_py(eeobject, vizparams))
  create_beauty_basemap(eeobject, tile, center, objname, zoom_start)
}

#' @name ee_map
#' @export
ee_map.ee.image.Image <- function(eeobject,
                                  vizparams,
                                  center,
                                  zoom_start = 8,
                                  objname = "map",
                                  quiet = FALSE,
                                  ...) {
  oauth_func_path <- system.file("python/ee_map.py", package = "rgee")
  map_py <- ee_source_python(oauth_func_path)

  if (missing(vizparams)) {
    vizparams <- ee_img_vizparams(eeobject)
    vzn <- names(vizparams)
    vzv <- as.character(vizparams)
    if (!quiet) {
      cat(
        " vizparams is missing, the following params are assigned. \n",
        sprintf(
          "vizparams: {%s:'%s', %s:%s, %s:%s}\n", vzn[1], vzv[1],
          vzn[2], vzv[2], vzn[3], vzv[3]
        )
      )
    }
  }

  if (missing(center)) {
    center <- eeobject$
      geometry()$
      getInfo()$coordinates[[1]] %>%
      unlist() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      list() %>%
      st_polygon() %>%
      st_centroid() %>%
      st_coordinates() %>%
      ee_py_to_r
    if (!quiet) {
      cat(
        " center is missing, the centroid of this EE Image object is used: \n",
        "center: ", paste(center, collapse = " "), "\n"
      )
    }
  }

  ee_match_img_geoviz(names(vizparams))

  tile <- ee_py_to_r(map_py$ee_map_py(eeobject, vizparams))
  create_beauty_basemap(eeobject, tile, center, objname, zoom_start)
}

#' @name ee_map
#' @param max_nimage Max number of Image to display.
#' @export
ee_map.ee.imagecollection.ImageCollection <- function(eeobject,
                                                      vizparams,
                                                      center,
                                                      zoom_start = 8,
                                                      objname = "map",
                                                      max_nimage = 10,
                                                      quiet = FALSE,
                                                      ...) {
  oauth_func_path <- system.file("python/ee_map.py", package = "rgee")
  map_py <- ee_source_python(oauth_func_path)

  if (missing(vizparams)) {
    vizparams <- ee_img_vizparams(ee$Image(eeobject$first()))
    vzn <- names(vizparams)
    vzv <- as.character(vizparams)
    if (!quiet) {
      cat(
        " vizparams is missing, the following params are assigned. \n",
        sprintf(
          "vizparams: {%s:'%s', %s:%s, %s:%s}\n", vzn[1], vzv[1],
          vzn[2], vzv[2], vzn[3], vzv[3]
        )
      )
    }
  }
  if (missing(center)) {
    center <- eeobject$
      first()$
      geometry()$
      getInfo()$coordinates[[1]] %>%
      unlist() %>%
      matrix(ncol = 2, byrow = TRUE) %>%
      list() %>%
      st_polygon() %>%
      st_centroid() %>%
      st_coordinates() %>%
      ee_py_to_r
    if (!quiet) {
      cat(
        " center is missing, the centroid of this EE ImageCollection is used: \n",
        "center: ", paste(center, collapse = " "), "\n"
      )
    }
  }
  ee_match_img_geoviz(names(vizparams))

  ee_size <- eeobject$size()$getInfo()
  if (ee_size > max_nimage) ee_size <- max_nimage
  if (length(objname) == 1L) objname <- sprintf("%s_%02d", objname, 1:ee_size)
  if (length(objname) != ee_size) stop("The length of ee$ImageCollection and 'objname' are different")

  eeobject_list <- eeobject$toList(ee_size) # collection to list

  m <- mapview()
  tokens <- rep(NA, ee_size)
  for (x in 1:ee_size) {
    eeobj <- ee$Image(eeobject_list$get(x - 1)) # index init - R(0) vs Python(1)
    tile <- py_to_r(map_py$ee_map_py(eeobj, vizparams))
    tokens[x] <- tile
    m@map <- m@map %>%
      addWMSTiles(group = objname[x], baseUrl = tile, layers = "0") %>%
      setView(center[1], center[2], zoom = zoom_start) %>%
      ee_mapViewLayersControl(names = c(objname[x]))
  }

  m@object$tokens <- tokens
  m@object$names <- objname
  m@object$eeobject <- eeobject$name()
  m
}

#' Default Geometry vizparams
#' @noRd
ee_geom_vizparams <- function() {
  list(color = "000000", strokeWidth = 3, pointRadius = 3)
}

#' Making sure the vizparams names on Images are correct!
#' @param x numeric vector; list names.
#' @noRd
ee_match_img_geoviz <- function(x) {
  band_names <- c("bands", "min", "max", "gain", "bias", "gamma", "palette", "opacity", "format")
  if (!all(x %in% band_names)) {
    stop("vizparams has not been setting correctly")
  }
}

#' Making sure the vizparams names on Vectors are correct!
#' @param x numeric vector; list names.
#' @noRd
ee_match_geom_geoviz <- function(x) {
  band_names <- c("color", "pointRadius", "strokeWidth")
  if (!all(x %in% band_names)) {
    stop("vizparams has not been setting correctly")
  }
}

#' Set colour whether it not exist.
#' @param x list; visualization parameters
#' @noRd
ee_geom_exist_color <- function(x) {
  if (!any(names(x) %in% "color")) {
    x$color <- "000000"
  }
  x
}

#' Default Image parameters
#' @param x EEobject
#' @noRd
ee_img_vizparams <- function(x) {
  precision <- x$getInfo()$bands[[1]]$data_type$precision
  if (precision == "float") minmax <- c(0, 1) else minmax <- c(0, 255)
  bands <- x$bandNames()$getInfo()
  if (length(bands) > 3) bands <- bands[3:1]
  list(bands = bands, min = minmax[1], max = minmax[2])
}


if (!isGeneric("+")) {
  setGeneric("+", function(x, y, ...)
    standardGeneric("+"))
}
#' mapview + mapview; adds data from the second map to the first
#'
#' @author Adapted from \href{https://github.com/r-spatial/mapview/blob/95050618a4eab75c73fea6e50a6aa31dcd152f14/R/plus.R}{tim-salabim code}.
#' @param e1 a mapview map to which e2 should be added.
#' @param e2 a mapview map from which the objects should be added to e1.
#' @examples
#' \dontrun{
#' eeobject <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")
#' center <- eeobject$geometry()$centroid()$getInfo()$coordinates
#' vizparams <- list(color = "FF0000", strokeWidth = 5)
#' m1 <- ee_map(eeobject, vizparams, center, objname = "Arequipa-landuse")
#'
#' collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
#'   filter(ee$Filter()$eq("WRS_PATH", 44))$
#'   filter(ee$Filter()$eq("WRS_ROW", 34))$
#'   filterDate("2014-01-01", "2015-01-01")$
#'   sort("CLOUD_COVER")
#' eeobject <- collection$median()
#' vizparams <- list(bands = c("B4", "B3", "B2"), max = 0.3)
#' center <- c(-122.3578, 37.7726)
#' m2 <- ee_map(eeobject, vizparams, center, objname = "SF")
#' m1 + m2
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
    for (x in 1:length(e2_name)) {
      e1@map <- e1@map %>%
        addWMSTiles(group = e2_name[x], baseUrl = e2_token[x], layers = "0") %>%
        ee_mapViewLayersControl(names = e2_name[x])
    }
    return(e1)
  }
)


#' Create a mapview based on an EE token
#' @noRd
create_beauty_basemap <- function(eeobject, tile, center, objname, zoom_start) {
  m <- mapview()
  m@map <- m@map %>%
    addTiles(urlTemplate = tile, group = objname) %>%
    setView(center[1], center[2], zoom = zoom_start) %>%
    ee_mapViewLayersControl(names = c(objname))

  m@object$tokens <- tile
  m@object$names <- objname
  m@object$eeobject <- eeobject$name()
  m
}
