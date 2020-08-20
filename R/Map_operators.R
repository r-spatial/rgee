#' EarthEngineMap + EarthEngineMap; adds data from the second map to the first
#'
#' @author tim-salabim. Adapted from mapview code.
#' @param e1 a EarthEngineMap map to which e2 should be added.
#' @param e2 a EarthEngineMap map from which the objects should be added to e1.
#'
setMethod(
  "+",
  signature(
    e1 = "EarthEngineMap",
    e2 = "EarthEngineMap"
  ),
  function(e1, e2) {
    e2_token <- e2@object$tokens
    e2_name <- e2@object$name
    e2_opacity <- e2@object$opacity
    e2_shown <- e2@object$shown
    e2_min <- e2@object$min
    e2_max <- e2@object$max
    e2_pal <- e2@object$palette
    e2_legend <- e2@object$legend

    for (x in seq_len(length(e2_name))) {
      e1@map <- e1@map %>%
        leaflet::addTiles(
          urlTemplate = e2_token[x],
          layerId = e2_name[x],
          group = e2_name[x],
          options = leaflet::tileOptions(opacity = e2_opacity[x])
        ) %>%
        ee_mapViewLayersControl(names = e2_name[x]) %>%
        leaflet::hideGroup(if (!e2_shown[x]) e2_name[x] else NULL)
    }

    if (isTRUE(e2_legend)) {
      e1@map <- e1@map %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = e2_pal,
          values = c(e2_min, e2_max),
          opacity = 1,
          title = e2_name
        )
    }
    out_obj <- append(e1@object, e2@object)
    out_obj <- out_obj[lengths(out_obj) != 0]
    methods::new('EarthEngineMap', object = out_obj, map = e1@map)
  }
)


#' EarthEngineMap + ANY; adds data from the second map to the first
#'
#' @author tim-salabim Adapted from mapview code.
#' @param e1 a EarthEngineMap map to which e2 should be added.
#' @param e2 a EarthEngineMap map from which the objects should be added to e1.
#'
setMethod(
  "+",
  signature(
    e1 = "EarthEngineMap",
    e2 = "mapview"
  ),
  function(e1, e2) {
    mapview_e1 <- ee_as_mapview(e1)
    idx <- ee_getCallEntryFromMap(e2@map, "addProviderTiles")
    if (length(idx) > 0) {
      e2@map$x$calls[idx] = NULL
    }
    idx = ee_getCallEntryFromMap(e2@map, "addLayersControl")
    if (length(idx) > 0) {
      e2@map$x$calls[idx][[1]]$args[[1]] = character(0)
    }
    m <- ee_appendMapCallEntries_lf(map1 = mapview_e1@map, map2 = e2@map)

    out_obj <- append(e1@object, e2@object)
    out_obj <- out_obj[lengths(out_obj) != 0]
    methods::new('EarthEngineMap', object = out_obj, map = m)
  }
)



#' ANY + EarthEngineMap; adds data from the second map to the first
#'
#' @author tim-salabim Adapted from mapview code.
#' @param e1 a EarthEngineMap map to which e2 should be added.
#' @param e2 a EarthEngineMap map from which the objects should be added to e1.
#'
setMethod(
  "+",
  signature(
    e1 = "mapview",
    e2 = "EarthEngineMap"
  ),
  function(e1, e2) {
    mapview_e2 <- ee_as_mapview(e2)
    idx <- ee_getCallEntryFromMap(e2@map, "addProviderTiles")
    if (length(idx) > 0) {
      e2@map$x$calls[idx] = NULL
    }
    idx = ee_getCallEntryFromMap(e2@map, "addLayersControl")
    if (length(idx) > 0) {
      e2@map$x$calls[idx][[1]]$args[[1]] = character(0)
    }
    m <- ee_appendMapCallEntries_lf(map1 = e1@map, map2 = mapview_e2@map)
    out_obj <- append(e1@object, e2@object)
    out_obj <- out_obj[lengths(out_obj) != 0]
    methods::new('EarthEngineMap', object = out_obj, map = m)
  }
)


#' EarthEngineMap | EarthEngineMap provides a slider in the middle to compare two maps.
#'
#' @param e1 a EarthEngineMap.
#' @param e2 a EarthEngineMap.
#' @noRd
if ( !isGeneric('|') ) {
  setGeneric('|', function(x, y, ...)
    standardGeneric('|'))
}

#' Comparison operator to  EarthEngineMap | EarthEngineMap
#' @author tim-salabim. Adapted from mapview code.
#' @noRd
setMethod(
  "|",
  signature(
    e1 = "EarthEngineMap",
    e2 = "EarthEngineMap"
  ), function(e1, e2) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      stop("package leaflet required, please install it first")
    }
    if (!requireNamespace("leaflet.extras2", quietly = TRUE)) {
      stop("package leaflet.extras2 required, please install it first")
    }

    # e2 properties
    e1_name <- e1@object$name
    e2_name <- e2@object$name
    e2_token <- e2@object$tokens
    e2_opacity <- e2@object$opacity
    e2_shown <- e2@object$shown
    e2_min <- e2@object$min
    e2_max <- e2@object$max
    e2_pal <- e2@object$palette
    e2_legend <- e2@object$legend

    # Create map with addSidebyside
    e1@map <- e1@map %>%
      leaflet::addMapPane("right", zIndex = 0) %>%
      leaflet::addMapPane("left", zIndex = 0) %>%
      add_basemaps(pane = "right") %>%
      leaflet::addTiles(
        urlTemplate = e2_token,
        layerId = e2_name,
        group = e2_name,
        options = c(
          leaflet::pathOptions(pane = "right"),
          leaflet::tileOptions(opacity = e2_opacity)
        )
      ) %>%
      ee_mapViewLayersControl(names = e1_name) %>%
      ee_mapViewLayersControl(names = e2_name) %>%
      leaflet.extras2::addSidebyside(layerId = "e3", leftId = e1_name,
                                     rightId = e2_name)

    if (isTRUE(e2_legend)) {
      e1@map <- e1@map %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = e2_pal,
          values = c(e2_min, e2_max),
          opacity = 1,
          title = e2_name
        )
    }
    out_obj <- append(e1@object, e2@object)
    out_obj <- out_obj[lengths(out_obj) != 0]
    methods::new('EarthEngineMap', object = out_obj, map = e1)
  }
)


#' Comparison operator to EarthEngineMap | mapview
#' @author tim-salabim. Adapted from mapview code.
#' @noRd
setMethod(
  "|",
  signature(
    e1 = "EarthEngineMap",
    e2 = "mapview"
  ), function(e1, e2) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      stop("package leaflet required, please install it first")
    }
    if (!requireNamespace("leaflet.extras2", quietly = TRUE)) {
      stop("package leaflet.extras2 required, please install it first")
    }
    e1_name <- e1@object$name
    e2_name <- ee_getLayerNamesFromMap(e2@map)
    mapview_e1 <- ee_as_mapview(e1)
    idx <- ee_getCallEntryFromMap(e2@map, "addProviderTiles")
    if (length(idx) > 0) {
      e2@map$x$calls[idx] = NULL
    }
    idx = ee_getCallEntryFromMap(e2@map, "addLayersControl")
    if (length(idx) > 0) {
      e2@map$x$calls[idx][[1]]$args[[1]] = character(0)
    }
    m <- ee_appendMapCallEntries_lf(map1 = mapview_e1@map, map2 = e2@map)
    m <- m %>%
      leaflet::addMapPane("right", zIndex = 0) %>%
      leaflet::addMapPane("left", zIndex = 0) %>%
      add_basemaps(pane = "right") %>%
      ee_mapViewLayersControl(names = e1_name) %>%
      ee_mapViewLayersControl(names = e2_name) %>%
      leaflet.extras2::addSidebyside(layerId = "e3", leftId = e1_name,
                                     rightId = e2_name)
    out_obj <- append(e1@object, e2@object)
    out_obj <- out_obj[lengths(out_obj) != 0]
    methods::new('EarthEngineMap', object = out_obj, map = m)
  }
)

#' Comparison operator to mapview | EarthEngineMap
#' @author tim-salabim. Adapted from mapview code.
#' @noRd
setMethod(
  "|",
  signature(
    e1 = "mapview",
    e2 = "EarthEngineMap"
  ), function(e1, e2) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      stop("package leaflet required, please install it first")
    }
    if (!requireNamespace("leaflet.extras2", quietly = TRUE)) {
      stop("package leaflet.extras2 required, please install it first")
    }
    e1_name <- ee_getLayerNamesFromMap(e1@map)
    e2_name <- e2@object$name
    mapview_e1 <- ee_as_mapview(e1)
    idx <- ee_getCallEntryFromMap(e2@map, "addProviderTiles")
    if (length(idx) > 0) {
      e2@map$x$calls[idx] = NULL
    }
    idx = ee_getCallEntryFromMap(e2@map, "addLayersControl")
    if (length(idx) > 0) {
      e2@map$x$calls[idx][[1]]$args[[1]] = character(0)
    }
    m <- ee_appendMapCallEntries_lf(map1 = mapview_e1@map, map2 = e2@map)
    m <- m %>%
      leaflet::addMapPane("right", zIndex = 0) %>%
      leaflet::addMapPane("left", zIndex = 0) %>%
      add_basemaps(pane = "right") %>%
      ee_mapViewLayersControl(names = e1_name) %>%
      ee_mapViewLayersControl(names = e2_name) %>%
      leaflet.extras2::addSidebyside(layerId = "e3", leftId = e1_name,
                                     rightId = e2_name)
    out_obj <- append(e1@object, e2@object)
    out_obj <- out_obj[lengths(out_obj) != 0]
    methods::new('EarthEngineMap', object = out_obj, map = m)
  }
)
