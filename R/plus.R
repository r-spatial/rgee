#' EarthEngineMap + EarthEngineMap; adds data from the second map to the first
#'
#' @author Adapted from
#' \href{https://github.com/r-spatial/mapview/blob/develop/R/plus.R}{
#' tim-salabim} code.
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
        rgee:::ee_mapViewLayersControl(names = e2_name[x]) %>%
        leaflet::hideGroup(if (!e2_shown[x]) e2_name[x] else NULL)
    }
    return(e1)
  }
)
