#' Convenience functions for working with spatial objects and leaflet maps
#' @author \href{https://github.com/tim-salabim}{Tim Salabim}
#' @noRd
ee_getCallMethods = function(map) {
  sapply(map$x$calls, "[[", "method")
}

#' @author \href{https://github.com/tim-salabim}{Tim Salabim}
#' @noRd
ee_getLayerControlEntriesFromMap <- function(map) {
  grep("addLayersControl", ee_getCallMethods(map), fixed = TRUE, useBytes = TRUE)
}

#' @author \href{https://github.com/tim-salabim}{tim-salabim}
#' @noRd
ee_getCallEntryFromMap <- function(map, call) {
  grep(call, ee_getCallMethods(map), fixed = TRUE, useBytes = TRUE)
}

# Add leaflet control button to map ---------------------------------------
#' @author \href{https://github.com/tim-salabim}{Tim Salabim}
#' @importFrom leaflet addLayersControl
#' @importFrom mapview mapviewGetOption
#' @noRd
ee_mapViewLayersControl <- function (map, map.types, names, native.crs = FALSE)
{
  ind = ee_getCallEntryFromMap(map, call = "addLayersControl")
  if (!length(ind)) {
    bgm <- map.types
  }
  else {
    bgm <- map$x$calls[[ind[1]]]$args[[1]]
  }
  if (!native.crs) {
    m <- addLayersControl(map = map, position = mapviewGetOption("layers.control.pos"),
                          baseGroups = bgm, overlayGroups = c(ee_getLayerNamesFromMap(map),
                                                                       names))
  }
  else {
    m <- addLayersControl(map = map, position = mapviewGetOption("layers.control.pos"),
                          overlayGroups = c(ee_getLayerNamesFromMap(map), names))
  }
  return(m)
}


#' Get layer names of leaflet map ------------------------------------------
#' @author \href{https://github.com/tim-salabim}{Tim Salabim}
#' @importFrom leaflet addLayersControl
#' @noRd
ee_getLayerNamesFromMap <- function(map) {

  len <- ee_getLayerControlEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[2]] else NULL

}
