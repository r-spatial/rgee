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
    e1_name <- e1@object$name
    e2_name <- e2@object$name
    e2_token <- e2@object$tokens
    e2_opacity <- e2@object$opacity
    e2_shown <- e2@object$shown
    e2_min <- e2@object$min
    e2_max <- e2@object$max
    e2_pal <- e2@object$palette
    e2_legend <- e2@object$legend

    if (e1_name == e2_name) {
      e2_name <- paste0(e1_name,"_duplicated")
      message_01 <-  c(
        "Both maps have the same name argument. The name of the second map was changed to:",
        sprintf("m1 <- Map$addLayer(..., name = \"%s\")", e1_name),
        sprintf("m2 <- Map$addLayer(..., name = \"%s\")", e2_name),
        "m1 + m2"
      )
      # message(paste0(message_01,collapse = "\n"))
    }

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
    e1_name <- e1@object$name
    e2_name <- ee_getLayerNamesFromMap(e2@map)
    if (e1_name == e2_name) {
      e2_name <- paste0(e1_name,"_duplicated")
      message_01 <-  c(
        "Both maps have the same name argument. The name of the second map was changed to:",
        sprintf("m1 <- Map$addLayer(..., name = \"%s\")", e1_name),
        sprintf("m2 <- Map$addLayer(..., name = \"%s\")", e2_name),
        "m1 + m2"
      )
      # message(paste0(message_01, collapse = "\n"))
    }

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
    e1_name <- ee_getLayerNamesFromMap(e1@map)
    e2_name <- e2@object$name

    if (e1_name == e2_name) {
      e2_name <- paste0(e1_name,"_duplicated")
      message_01 <-  c(
        "Both maps have the same name argument. The name of the second map was changed to:",
        sprintf("m1 <- Map$addLayer(..., name = \"%s\")", e1_name),
        sprintf("m2 <- Map$addLayer(..., name = \"%s\")", e2_name),
        "m1 + m2"
      )
      # message(paste0(message_01,collapse = "\n"))
    }

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


if ( !isGeneric('|') ) {
  setGeneric('|', function(x, y, ...)
    standardGeneric('|'))
}

#' EarthEngineMap | EarthEngineMap provides a slider in the middle to compare two maps.
#' @author tim-salabim. Adapted from mapview code.
#' @param e1 a leaflet or mapview map, or NULL.
#' @param e2 a leaflet or mapview map, or NULL.
#' @name slider
#' @aliases |,EarthEngineMap,EarthEngineMap-method
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

    # e1 properties
    e1_name <- e1@object$name
    e1_token <- e1@object$tokens
    e1_opacity <- e1@object$opacity
    e1_shown <- e1@object$shown
    e1_min <- e1@object$min
    e1_max <- e1@object$max
    e1_pal <- e1@object$palette
    e1_legend <- e1@object$legend

    # e2 properties
    e2_name <- e2@object$name
    e2_token <- e2@object$tokens
    e2_opacity <- e2@object$opacity
    e2_shown <- e2@object$shown
    e2_min <- e2@object$min
    e2_max <- e2@object$max
    e2_pal <- e2@object$palette
    e2_legend <- e2@object$legend

    if (e1_name == e2_name) {
      e2_name <- paste0(e2_name,"_duplicated")
      message_01 <-  c(
        "Both map have the same name argument. The name of the second map was changed to:",
        sprintf("m1 <- Map$addLayer(..., name = \"%s\")", e1_name),
        sprintf("m2 <- Map$addLayer(..., name = \"%s\")", e2_name),
        "m1 | m2"
      )
      # message(paste0(message_01,collapse = "\n"))
    }

    # Create map with addSidebyside
    m <- mapview::mapview()@map %>%
      leaflet::setView(Map$lon, Map$lat, zoom = Map$zoom) %>%
      leaflet::addMapPane("right", zIndex = 402) %>%
      leaflet::addMapPane("left", zIndex = 403) %>%
      leaflet::addTiles(
        urlTemplate = e2_token,
        layerId = e2_name,
        group = e2_name,
        options = c(
          leaflet::pathOptions(pane = "right"),
          leaflet::tileOptions(opacity = e2_opacity)
        )
      ) %>%
      leaflet::addTiles(
        urlTemplate = e1_token,
        layerId = e1_name,
        group = e1_name,
        options = c(
          leaflet::pathOptions(pane = "left"),
          leaflet::tileOptions(opacity = e1_opacity)
        )
      ) %>%
      ee_mapViewLayersControl(names = e1_name) %>%
      ee_mapViewLayersControl(names = e2_name) %>%
      leaflet.extras2::addSidebyside(layerId = "e3", leftId = e1_name,
                                     rightId = e2_name)
    if (isTRUE(e1_legend)) {
      m <- m %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = e1_pal,
          values = c(e1_min, e1_max),
          opacity = 1,
          title = e1_name
        )
    }
    if (isTRUE(e2_legend)) {
      m <- m %>%
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
    methods::new('EarthEngineMap', object = out_obj, map = m)
  }
)

#' Comparison operator to EarthEngineMap | mapview
#' @author tim-salabim. Adapted from mapview code.
#' @param e1 a EarthEngineMap.
#' @param e2 a leaflet or mapview map, or NULL.
#' @name slider
#' @aliases |,EarthEngineMap,mapview-method
# setMethod(
#   "|",
#   signature(
#     e1 = "EarthEngineMap",
#     e2 = "mapview"
#   ), function(e1, e2) {
#     if (!requireNamespace("leaflet", quietly = TRUE)) {
#       stop("package leaflet required, please install it first")
#     }
#     if (!requireNamespace("leaflet.extras2", quietly = TRUE)) {
#       stop("package leaflet.extras2 required, please install it first")
#     }
#     e1_name <- e1@object$name
#     e2_name <- ee_getLayerNamesFromMap(e2@map)
#
#     if (e1_name == e2_name) {
#       message_01 <-  c(
#         "Both map have the same name argument, run to fix:",
#         "m1 <- Map$addLayer(..., name = \"map_01\")",
#         "m2 <- mapview(..., layer.name = \"map_02\")",
#         "m1 | m2"
#       )
#       stop(paste0(message_01,collapse = "\n"))
#     }
#
#     mapview_e1 <- ee_as_mapview(e1)
#     idx <- ee_getCallEntryFromMap(e2@map, "addProviderTiles")
#     if (length(idx) > 0) {
#       e2@map$x$calls[idx] = NULL
#     }
#     idx = ee_getCallEntryFromMap(e2@map, "addLayersControl")
#     if (length(idx) > 0) {
#       e2@map$x$calls[idx][[1]]$args[[1]] = character(0)
#     }
#     m <- ee_appendMapCallEntries_lf(map1 = mapview_e1@map, map2 = e2@map)
#     m <- m %>%
#       leaflet::addMapPane("right", zIndex = 0) %>%
#       leaflet::addMapPane("left", zIndex = 0) %>%
#       add_basemaps(pane = "right") %>%
#       ee_mapViewLayersControl(names = e1_name) %>%
#       ee_mapViewLayersControl(names = e2_name) %>%
#       leaflet.extras2::addSidebyside(layerId = "e3", leftId = e1_name,
#                                      rightId = e2_name)
#     out_obj <- append(e1@object, e2@object)
#     out_obj <- out_obj[lengths(out_obj) != 0]
#     methods::new('EarthEngineMap', object = out_obj, map = m)
#   }
# )

#' Comparison operator to mapview | EarthEngineMap
#' @author tim-salabim. Adapted from mapview code.
#' @param e1 a leaflet or mapview map, or NULL.
#' @param e2 a EarthEngineMap.
#' @name slider
#' @aliases |,mapview,EarthEngineMap-method
#'
# setMethod(
#   "|",
#   signature(
#     e1 = "mapview",
#     e2 = "EarthEngineMap"
#   ), function(e1, e2) {
#     if (!requireNamespace("leaflet", quietly = TRUE)) {
#       stop("package leaflet required, please install it first")
#     }
#     if (!requireNamespace("leaflet.extras2", quietly = TRUE)) {
#       stop("package leaflet.extras2 required, please install it first")
#     }
#     e1_name <- ee_getLayerNamesFromMap(e1@map)
#     e2_name <- e2@object$name
#
#     if (e1_name == e2_name) {
#       message_01 <-  c(
#         "Both map have the same name argument, run to fix:",
#         "m1 <- mapview(..., name = \"map_01\")",
#         "m2 <- Map$addLayer(..., layer.name = \"map_02\")",
#         "m1 | m2"
#       )
#       stop(paste0(message_01,collapse = "\n"))
#     }
#
#     mapview_e1 <- ee_as_mapview(e1)
#     idx <- ee_getCallEntryFromMap(e2@map, "addProviderTiles")
#     if (length(idx) > 0) {
#       e2@map$x$calls[idx] = NULL
#     }
#     idx = ee_getCallEntryFromMap(e2@map, "addLayersControl")
#     if (length(idx) > 0) {
#       e2@map$x$calls[idx][[1]]$args[[1]] = character(0)
#     }
#     m <- ee_appendMapCallEntries_lf(map1 = mapview_e1@map, map2 = e2@map)
#     m <- m %>%
#       leaflet::addMapPane("right", zIndex = 0) %>%
#       leaflet::addMapPane("left", zIndex = 0) %>%
#       add_basemaps(pane = "right") %>%
#       ee_mapViewLayersControl(names = e1_name) %>%
#       ee_mapViewLayersControl(names = e2_name) %>%
#       leaflet.extras2::addSidebyside(layerId = "e3", leftId = e1_name,
#                                      rightId = e2_name)
#     out_obj <- append(e1@object, e2@object)
#     out_obj <- out_obj[lengths(out_obj) != 0]
#     methods::new('EarthEngineMap', object = out_obj, map = m)
#   }
# )
