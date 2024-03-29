#' Estimates the zoom level for a given bounds
#' Adapted from Python to R
#' https://github.com/fitoprincipe/ipygee/
#' https://stackoverflow.com/questions/6048975/
#' @noRd
ee_getZoom <- function(eeObject, maxError = ee$ErrorMargin(1)) {
  # added for support COG
  if (inherits(eeObject, "list")) {
    bounds <- unlist(eeObject$bounds)
    names(bounds) <- c("xmin", "ymin", "xmax", "ymax")
  } else {
    bounds <- ee_get_boundary(eeObject, maxError)
  }

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

#' Get the tile_fetcher to display into ee_map
#' @noRd
get_ee_image_url <- function(image) {
  map_id <- ee$data$getMapId(list(image = image))
  url <- map_id[["tile_fetcher"]]$url_format
  url
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


#' leaflet::addTiles in a batch way
#' @noRd
addTiles_batch <- function(map_default, e2_token, e2_name, e2_opacity, position = "right") {
  for (index in seq_along(e2_token)) {
    map_default <- map_default %>% leaflet::addTiles(
      urlTemplate = e2_token[index],
      layerId = e2_name[index],
      group = e2_name[index],
      options = c(
        leaflet::pathOptions(pane = position),
        leaflet::tileOptions(opacity = e2_opacity[index])
      )
    ) %>%
      ee_mapViewLayersControl(names = e2_name[index])
  }
  map_default
}


#' Create a default map
#' @noRd
leaflet_default <- function(lon = -76.942478, lat = -12.172116, zoom  = 18, default_maps = NULL) {
  if (is.null(default_maps)) {
    default_maps <- c(
      "CartoDB.Positron", "OpenStreetMap",
      "CartoDB.DarkMatter", "Esri.WorldImagery",
      "OpenTopoMap"
    )
  }
  m <- initBaseMaps(default_maps)
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


#' Get metadata from COG
#' @noRd
ee_get_metadata <- function(resource, titiler_server = "https://api.cogeo.xyz/") {
  titiler_server_service <- sprintf("%s/%s", titiler_server, "cog/metadata")
  response <- httr::GET(
    url = titiler_server_service,
    config = httr::accept_json(),
    query = c(list("url" = resource))
  )
  httr::content(response, type="application/json")
}
