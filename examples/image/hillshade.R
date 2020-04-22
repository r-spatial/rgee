#' Compute hillshade from elevation.
library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

Map$setCenter(-121.767, 46.852, 11)

ee_get_radians <- function(img) {
  img$toFloat()$multiply(base::pi)$divide(180)
}

#' Compute hillshade for the given illumination az, el.
ee_get_hillshade <- function(az, ze, slope, aspect) {
  azimuth <- ee_get_radians(ee$Image(az))
  zenith <- ee_get_radians(ee$Image(ze))
  azimuth %>%
    ee$Image$subtract(aspect) %>%
    ee$Image$cos() %>%
    ee$Image$multiply(slope$sin()) %>%
    ee$Image$multiply(zenith$sin()) %>%
    ee$Image$add(zenith$cos()$multiply(slope$cos()))
}

# From SRTMv4 90 calculate slope and aspect
terrain <- ee$Algorithms$Terrain(ee$Image("srtm90_v4"))
slope_img <- ee_get_radians(terrain$select("slope"))
aspect_img <- ee_get_radians(terrain$select("aspect"))

# Add 1 hillshade at az=0, el=60.
srtm_hillshade <- ee_get_hillshade(0, 60, slope_img, aspect_img)

Map$setCenter(0,0,1)
Map$addLayer(srtm_hillshade, name = "Hillshade")
