#' Convenience functions for working with spatial objects and leaflet maps
#' @author \href{https://github.com/tim-salabim}{Tim Appelhans}
#' @noRd
ee_getCallMethods <- function(map) {
  vapply(map$x$calls, "[[", "method", FUN.VALUE = "")
}

#' @author \href{https://github.com/tim-salabim}{Tim Appelhans}
#' @noRd
ee_getLayerControlEntriesFromMap <- function(map) {
  grep(pattern = "addLayersControl", x = ee_getCallMethods(map), fixed = TRUE,
       useBytes = TRUE)
}

#' @author \href{https://github.com/tim-salabim}{Tim Appelhans}
#' @noRd
ee_getCallEntryFromMap <- function(map, call) {
  grep(call, ee_getCallMethods(map), fixed = TRUE, useBytes = TRUE)
}

# Add leaflet control button to map ---------------------------------------
#' @author \href{https://github.com/tim-salabim}{Tim Appelhans}
#' @noRd
ee_mapViewLayersControl <- function(map, map.types, names, native.crs = FALSE) {
  ind <- ee_getCallEntryFromMap(map, call = "addLayersControl")
  if (!length(ind)) {
    bgm <- map.types
  }
  else {
    bgm <- map$x$calls[[ind[1]]]$args[[1]]
  }
  if (!native.crs) {
    m <- leaflet::addLayersControl(
      map = map, position = mapview::mapviewGetOption("layers.control.pos"),
      baseGroups = bgm, overlayGroups = c(
        ee_getLayerNamesFromMap(map),
        names
      )
    )
  }
  else {
    m <- leaflet::addLayersControl(
      map = map, position = mapview::mapviewGetOption("layers.control.pos"),
      overlayGroups = c(ee_getLayerNamesFromMap(map), names)
    )
  }
  return(m)
}


#' Get layer names of leaflet map ------------------------------------------
#' @author \href{https://github.com/tim-salabim}{Tim Appelhans}
#' @noRd
ee_getLayerNamesFromMap <- function(map) {
  len <- ee_getLayerControlEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[2]] else NULL
}



# Add leaflet control button to map ---------------------------------------
#' @author \href{https://github.com/tim-salabim}{Tim Appelhans}
#' @noRd
ee_getLayerControlEntriesFromMap <- function(map) {
  grep(
    pattern = "addLayersControl",
    x = ee_getCallMethods(map),
    fixed = TRUE,
    useBytes = TRUE
  )
}

# Add leaflet control button to map ---------------------------------------
#' @author \href{https://github.com/tim-salabim}{Tim Appelhans}
#' @noRd
ee_appendMapCallEntries_lf <- function(map1, map2) {
  ## calls
  m1_calls = map1$x$calls
  m2_calls = map2$x$calls

  ## dependencies
  m1_deps = map1$dependencies
  m2_deps = map2$dependencies

  mp_deps = append(m1_deps, m2_deps)
  mp_deps = mp_deps[!duplicated(mp_deps)]

  ## base map controls
  ctrls1 <- ee_getLayerControlEntriesFromMap(map1)
  ctrls2 <- ee_getLayerControlEntriesFromMap(map2)
  bmaps1 <- m1_calls[[ctrls1[1]]]$args[[1]]
  bmaps2 <- m2_calls[[ctrls2[1]]]$args[[1]]
  bmaps <- c(bmaps1, bmaps2)[!duplicated(c(bmaps1, bmaps2))]

  ## layer controls
  len1 <- ctrls1[length(ctrls1)]
  lyrs1 = if (length(len1) != 0) m1_calls[[len1]]$args[[2]] else NULL
  len2 <- ctrls2[length(ctrls2)]
  lyrs2 = if (length(len2) != 0) m2_calls[[len2]]$args[[2]] else NULL
  # lyrs1 <- getLayerNamesFromMap(map1)
  # lyrs2 <- getLayerNamesFromMap(map2)
  lyrs <- c(lyrs1, lyrs2)
  # dup <- duplicated(lyrs)
  # lyrs[dup] <- sapply(seq(lyrs[dup]),
  # function(i) paste0(lyrs[dup][[i]], ".", as.character(i + 1)))

  ## merge
  mpcalls <- append(m1_calls, m2_calls)
  mpcalls <- mpcalls[!duplicated(mpcalls)]
  mpcalls[[ctrls1[1]]]$args[[1]] <- bmaps
  mpcalls[[ctrls1[1]]]$args[[2]] <- lyrs

  # ind <- which(sapply(mpcalls, function(i) {
  #   i$method == "addLayersControl"
  # }))

  ind <-  grep(
    pattern = "addLayersControl",
    x = sapply(mpcalls, "[[", "method"),
    fixed = TRUE,
    useBytes = TRUE
  )

  #   ind <- seq_along(mpcalls)[sapply(mpcalls,
  #                                    FUN = function(X) {
  #                                      "addLayersControl" %in% X
  #                                      })]
  ind1 <- ind[1]
  ind2 <- ind[length(ind)]
  try({
    mpcalls[[ind2]] <- mpcalls[[ind1]]
    mpcalls[[ind1]] <- NULL
  }, silent = TRUE)
  map1$x$calls <- mpcalls

  map1$x$calls <- mpcalls
  map1$dependencies = mp_deps
  return(map1)
}

# Add basemap to leaflet ---------------------------------------
#' @noRd
add_basemaps <- function(map, pane = "right") {
  bgm <- c("CartoDB.Positron", "CartoDB.DarkMatter", "OpenStreetMap",
           "Esri.WorldImagery", "OpenTopoMap")
  map %>%
    addProviderTiles(bgm[1], group = bgm[1],
                     options = pathOptions(pane = pane)) %>%
    addProviderTiles(bgm[2], group = bgm[2],
                     options = pathOptions(pane = pane)) %>%
    addProviderTiles(bgm[3], group = bgm[3],
                     options = pathOptions(pane = pane)) %>%
    addProviderTiles(bgm[4], group = bgm[4],
                     options = pathOptions(pane = pane)) %>%
    addProviderTiles(bgm[5], group = bgm[5],
                     options = pathOptions(pane = pane)) %>%
    leaflet::addLayersControl(
      baseGroups = bgm
    )
}
