#' Convenience functions for working with spatial objects and leaflet maps
#' @author \href{https://github.com/tim-salabim/}{Tim Appelhans}
#' @noRd
ee_getCallMethods <- function(map) {
  vapply(map$x$calls, "[[", "method", FUN.VALUE = "")
}

#' @author \href{https://github.com/tim-salabim/}{Tim Appelhans}
#' @noRd
ee_getLayerControlEntriesFromMap <- function(map) {
  grep(pattern = "addLayersControl", x = ee_getCallMethods(map), fixed = TRUE,
       useBytes = TRUE)
}

#' @author \href{https://github.com/tim-salabim/}{Tim Appelhans}
#' @noRd
ee_getCallEntryFromMap <- function(map, call) {
  grep(call, ee_getCallMethods(map), fixed = TRUE, useBytes = TRUE)
}

# Add leaflet control button to map ---------------------------------------
#' @author \href{https://github.com/tim-salabim/}{Tim Appelhans}
#' @noRd
ee_mapViewLayersControl <- function(map, map.types, names, native.crs = FALSE) {
  ind <- ee_getCallEntryFromMap(map, call = "addLayersControl")

  # Add Layer Control
  bgm <- map$x$calls[[ind[1]]]$args[[1]]

  if (!native.crs) {
    m <- leaflet::addLayersControl(
      map = map, position = "topleft",
      baseGroups = bgm, overlayGroups = c(
        ee_getLayerNamesFromMap(map),
        names
      )
    )
  }
  else {
    m <- leaflet::addLayersControl(
      map = map, position = "topleft",
      overlayGroups = c(ee_getLayerNamesFromMap(map), names)
    )
  }
  return(m)
}


#' Get layer names of leaflet map ------------------------------------------
#' @author \href{https://github.com/tim-salabim/}{Tim Appelhans}
#' @noRd
ee_getLayerNamesFromMap <- function(map) {
  len <- ee_getLayerControlEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[2]] else NULL
}



# Add leaflet control button to map ---------------------------------------
#' @author \href{https://github.com/tim-salabim/}{Tim Appelhans}
#' @noRd
ee_getLayerControlEntriesFromMap <- function(map) {
  grep(
    pattern = "addLayersControl",
    x = ee_getCallMethods(map),
    fixed = TRUE,
    useBytes = TRUE
  )
}

