#' EarthEngineMap + EarthEngineMap; adds data from the second map to the first
#'
#' @author tim-salabim. Adapted from mapview code.
#' @param e1 a EarthEngineMap map to which e2 should be added.
#' @param e2 a EarthEngineMap map from which the objects should be added to e1.
#' @name null-default
#' @export
'+.EarthEngineMap' <- function(e1, e2) {
  if (!any(class(e2) %in% "EarthEngineMap")) {
    stop("right map is not an EarthEngineMap object")
  }

  # e1 metadata
  e1_max <- e1$rgee$max
  e1_min <- e1$rgee$min
  e1_name <- e1$rgee$name
  e1_pal <- e1$rgee$palette
  e1_legend <- e1$rgee$legend
  e1_shown <- e1$rgee$shown
  e1_token <- e1$rgee$tokens
  e1_opacity <- e1$rgee$opacity

  # e2 metadata
  e2_max <- e2$rgee$max
  e2_min <- e2$rgee$min
  e2_name <- e2$rgee$name
  e2_pal <- e2$rgee$palette
  e2_shown <- e2$rgee$shown
  e2_token <- e2$rgee$tokens
  e2_opacity <- e2$rgee$opacity
  e2_legend <- e2$rgee$legend

  # If e1 and e2 have the same name add to $rgee$name the suffix _duplicated
  if (any(e1_name %in% e2_name)) {
    warning(
      paste0(
        "EarthEngineMap objects with the same name ...",
        " Adding a random sufix to the second map."
      )
    )
    # Index of the duplicated name.
    positions_in_e1 <- which(e1_name %in% e2_name)
    e2_name <- basename(
      tempfile(
        paste0(
          e2_name[positions_in_e1], "_"
        )
      )
    )
  }

  # Add all the tokens in the same leaflet map
  for (x in seq_len(length(e2_name))) {
    e1 <- e1 %>%
      leaflet::addTiles(
        urlTemplate = e2_token[x],
        layerId = e2_name[x],
        group = e2_name[x],
        options = leaflet::tileOptions(opacity = e2_opacity[x])
      ) %>%
      ee_mapViewLayersControl(names = e2_name[x]) %>%
      leaflet::hideGroup(if (!e2_shown[x]) e2_name[x] else NULL)
  }

  # Add the legend of e2
  if (isTRUE(e2_legend)) {
    e1 <- e1 %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = e2_pal[[1]],
        values = c(e2_min, e2_max),
        opacity = 1,
        title = e2_name
      )
  }

  # Save metadata
  e1$rgee$tokens <- c(e1_token, e2_token)
  e1$rgee$name <- c(e1_name, e2_name)
  e1$rgee$opacity <- c(e1_opacity, e2_opacity)
  e1$rgee$shown <- c(e1_shown, e2_shown)

  e1$rgee$min <- c(e1_min, e2_min)
  e1$rgee$max <- c(e1_max, e2_max)
  e1$rgee$palette <-  do.call(list, unlist(list(e1_pal, e2_pal), recursive=FALSE))
  e1$rgee$legend <- c(e1_legend, e2_legend)
  e1
}

#' EarthEngineMap | EarthEngineMap provides a slider in the middle to compare two maps.
#'
#' @author tim-salabim. Adapted from mapview code.
#' @param e1 an EarthEngineMap object.
#' @param e2 an EarthEngineMap object.
#' @name null-default
#' @aliases |, EarthEngineMap, EarthEngineMap-method
#' @export
'|.EarthEngineMap' <- function(e1, e2) {
  #check packages
  ee_check_packages("| operator", c("leaflet", "leaflet.extras2"))

  if (!any(class(e2) %in% "EarthEngineMap")) {
    stop("right map is not an EarthEngineMap object")
  }

  # e1 metadata
  e1_max <- e1$rgee$max
  e1_min <- e1$rgee$min
  e1_name <- e1$rgee$name
  e1_pal <- e1$rgee$palette
  e1_legend <- e1$rgee$legend
  e1_shown <- e1$rgee$shown
  e1_token <- e1$rgee$tokens
  e1_opacity <- e1$rgee$opacity
  e1_position <-  e1$rgee$position

  # e2 metadata
  e2_max <- e2$rgee$max
  e2_min <- e2$rgee$min
  e2_name <- e2$rgee$name
  e2_pal <- e2$rgee$palette
  e2_shown <- e2$rgee$shown
  e2_token <- e2$rgee$tokens
  e2_opacity <- e2$rgee$opacity
  e2_legend <- e2$rgee$legend
  e2_position <-  e2$rgee$position
  # If e1 and e2 have the same name add to $rgee$name the suffix _duplicated
  if (any(e1_name %in% e2_name)) {
    warning(
      paste0(
        "EarthEngineMap objects with the same name ...",
        " Adding a random sufix to the second map."
      )
    )
    # Index of the duplicated name.
    positions_in_e1 <- which(e1_name %in% e2_name)
    e2_name[positions_in_e1] <- basename(
      tempfile(
        paste0(
          e2_name[positions_in_e1], "_"
        )
      )
    )
  }

  # Create map with addSidebyside
  m <- leaflet_default() %>%
    leaflet::setView(self$lon, self$lat, zoom = self$zoom) %>%
    leaflet::addMapPane("right", zIndex = 402) %>%
    leaflet::addMapPane("left", zIndex = 403) %>%
    addTiles_batch(e2_token, e2_name, e2_opacity, position = "right") %>%
    addTiles_batch(e1_token, e1_name, e1_opacity, position = "left") %>%
    leaflet.extras2::addSidebyside(
      layerId = "e3",
      leftId = e2_name[1],
      rightId = e1_name[2])
  m
  # Add the legend of e2
  if (isTRUE(e2_legend)) {
    e1 <- e1 %>%
      leaflet::addLegend(
        position = "bottomright",
        pal = e2_pal[[1]],
        values = c(e2_min, e2_max),
        opacity = 1,
        title = e2_name
      )
  }

  # Save metadata
  m$rgee$tokens <- c(e1_token, e2_token)
  m$rgee$name <- c(e1_name, e2_name)
  m$rgee$opacity <- c(e1_opacity, e2_opacity)
  m$rgee$shown <- c(e1_shown, e2_shown)
  m$rgee$position <- c(e1_position, e2_position)

  m$rgee$min <- c(e1_min, e2_min)
  m$rgee$max <- c(e1_max, e2_max)

  m$rgee$palette <- do.call(list, unlist(list(e1_pal, e2_pal), recursive=FALSE))
  m$rgee$legend <- c(e1_legend, e2_legend)

  if (e2_legend[1]) {
    e2_min <- e2_min[1]
    e2_max <- e2_max[1]
    e2_pal <- e2_pal[[1]]
    e2_name <- e2_name[1]
    m <- m %>% leaflet::addLegend(
      position = "bottomright",
      pal = e2_pal,
      values = c(e2_min, e2_max),
      opacity = 1,
      title = e2_name
    )
  }

  if (e1_legend[1]) {
    e1_min <- e1_min[1]
    e1_max <- e1_max[1]
    e1_pal <- e1_pal[[1]]
    e1_name <- e1_name[1]
    m <- m %>% leaflet::addLegend(
      position = "bottomleft",
      pal = e1_pal,
      values = c(e1_min, e1_max),
      opacity = 1,
      title = e1_name
    )
  }
  m
}


#' Create map with addSidebyside
#' @noRd
create_sidebyside_map <- function(map, e1_token, e1_name, e1_opacity,
                                  e2_token, e2_name, e2_opacity) {
  if (is.na(e2_token)) {
    m <- map %>%
      leaflet::setView(self$lon, self$lat, zoom = self$zoom) %>%
      leaflet::addMapPane("right", zIndex = 402) %>%
      leaflet::addMapPane("left", zIndex = 403) %>%
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
      leaflet.extras2::addSidebyside(
        layerId = "e3",
        leftId = e1_name,
        rightId = e2_name)
  } else if (is.na(e1_token)) {
    m <- map %>%
      leaflet::setView(self$lon, self$lat, zoom = self$zoom) %>%
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
      ee_mapViewLayersControl(names = e2_name) %>%
      leaflet.extras2::addSidebyside(
        layerId = "e3",
        leftId = e1_name,
        rightId = e2_name)
  } else {
    m <- map %>%
      leaflet::setView(self$lon, self$lat, zoom = self$zoom) %>%
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
      leaflet.extras2::addSidebyside(
        layerId = "e3",
        leftId = e1_name,
        rightId = e2_name)
  }
  m
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
