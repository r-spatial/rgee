#' EarthEngineMap + EarthEngineMap; adds data from the second map to the first
#'
#' @author tim-salabim Adapted from mapview code.
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
    e2_name <- e2@object$names
    e2_opacity <- e2@object$opacity
    e2_shown <- e2@object$shown

    for (x in seq_len(length(e2_name))) {
      e1@map <- e1@map %>%
        leaflet::addTiles(
          urlTemplate = e2_token[x],
          group = e2_name[x],
          options = leaflet::tileOptions(opacity = e2_opacity[x])
        ) %>%
        ee_mapViewLayersControl(names = e2_name[x]) %>%
        leaflet::hideGroup(if (!e2_shown[x]) e2_name[x] else NULL)
    }
    return(e1)
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

