#' EarthEngineMap + EarthEngineMap; adds data from the second map to the first
#'
#' @author tim-salabim. Adapted from mapview code.
#' @param e1 a EarthEngineMap map to which e2 should be added.
#' @param e2 a EarthEngineMap map from which the objects should be added to e1.
#' @name map-operator
#' @export
'+.EarthEngineMap' <- function(e1, e2) {
  if (inherits(e2, "EarthEngineMap")) {
    # e1 metadata
    e1_token <- e1$rgee$tokens
    e1_name <- e1$rgee$name
    e1_opacity <- e1$rgee$opacity
    e1_shown <- e1$rgee$shown
    e1_position <- e1$rgee$position
    e1_legend_params <- e1$rgee$legend_params

    # e2 metadata
    e2_token <- e2$rgee$tokens
    e2_name <- e2$rgee$name
    e2_opacity <- e2$rgee$opacity
    e2_shown <- e2$rgee$shown
    e2_position <- e2$rgee$position
    e2_legend_params <- e2$rgee$legend_params

    # If e1 and e2 have the same name add to $rgee$name the suffix _duplicated
    if (any(e1_name %in% e2_name)) {
      # Index of the duplicated name.
      positions_in_e2 <- which(e2_name %in% e1_name)
      e2_name <- basename(
        tempfile(
          paste0(
            e2_name[positions_in_e2], "_"
          )
        )
      )
    }

    # Add all the tokens in the same leaflet map
    for (x in seq_len(length(e2_name))) {
      if (!is.null(e2_token)) {
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
    }

    # Save metadata
    e1$rgee$tokens <- c(e1_token, e2_token)
    e1$rgee$name <- c(e1_name, e2_name)
    e1$rgee$opacity <- c(e1_opacity, e2_opacity)
    e1$rgee$shown <- c(e1_shown, e2_shown)
    e1$rgee$position <- c(e1_position, e2_position)
    e1$rgee$legend_params <- c(e1_legend_params, e2_legend_params)
    e1
  } else if (is.list(e2)) {
    leaflet_legend_args <- e2 %>% append(list(map = e1))
    e1 <- do.call(leaflet::addLegend, leaflet_legend_args)
    # If it is the first time attaching a legend to the map
    if (is.null(e1$rgee$legend_params)) {
      e1$rgee$legend_params <- list(e2)
    } else {
      # It is necessary to keep the x$rgee$legend_params after
      # (m1 | m2) + m3
      e1$rgee$legend_params <- c(e1$rgee$legend_params, list(e2))
    }
    e1
  } else {
    stop("right map is neither EarthEngineMap nor EarthEngineLegend object")
  }
}

#' EarthEngineMap | EarthEngineMap provides a slider in the middle to compare two maps.
#'
#' @param e1 an EarthEngineMap object.
#' @param e2 an EarthEngineMap object.
#' @name map-operator
#' @aliases |, EarthEngineMap, EarthEngineMap-method
#' @export
'|.EarthEngineMap' <- function(e1, e2) {
  #check packages
  ee_check_packages("| operator", c("leaflet", "leaflet.extras2"))

  if (!inherits(e2, "EarthEngineMap")) {
    stop("right map is not an EarthEngineMap object")
  }

  # e1 metadata
  e1_token <- e1$rgee$tokens
  e1_name <- e1$rgee$name
  e1_opacity <- e1$rgee$opacity
  e1_shown <- e1$rgee$shown
  e1_position <-  e1$rgee$position
  e1_legend_params <- e1$rgee$legend_params

  # e2 metadata
  e2_token <- e2$rgee$tokens
  e2_name <- e2$rgee$name
  e2_opacity <- e2$rgee$opacity
  e2_shown <- e2$rgee$shown
  e2_position <- e2$rgee$position
  e2_legend_params <- e2$rgee$legend_params


  # If e1 and e2 have the same name add to $rgee$name the suffix _duplicated
  if (any(e1_name %in% e2_name)) {
    # Index of the duplicated name.
    positions_in_e2 <- which(e2_name %in% e1_name)
    e2_name[positions_in_e2] <- basename(
      tempfile(
        paste0(
          e2_name[positions_in_e2], "_"
        )
      )
    )
  }

  # Create map with addSidebyside
  lon <- e1$x$setView[[1]][2]
  lat <- e1$x$setView[[1]][1]
  zoom <- e1$x$setView[[2]]

  m <- leaflet_default() %>%
    leaflet::setView(lon, lat, zoom = zoom) %>%
    leaflet::addMapPane("right", zIndex = 402) %>%
    leaflet::addMapPane("left", zIndex = 403) %>%
    addTiles_batch(e1_token, e1_name, e1_opacity, position = "left") %>%
    addTiles_batch(e2_token, e2_name, e2_opacity, position = "right") %>%
    leaflet.extras2::addSidebyside(
      layerId = "e3",
      leftId = e1_name[1],
      rightId = e2_name[1])

  # Save metadata
  m$rgee$tokens <- c(e1_token, e2_token)
  m$rgee$name <- c(e1_name, e2_name)
  m$rgee$opacity <- c(e1_opacity, e2_opacity)
  m$rgee$shown <- c(e1_shown, e2_shown)
  m$rgee$position <- c(e1_position, e2_position)
  m$rgee$legend_params <- c(e1_legend_params, e2_legend_params)


  # Add previous leaflet legend to the map
  if (!is.null(m$rgee$legend_params)) {
    for(legend_params in m$rgee$legend_params) {
      leaflet_legend_args <- legend_params %>% append(list(map = m))
      m <- do.call(leaflet::addLegend, leaflet_legend_args)
    }
  }
  m
}

