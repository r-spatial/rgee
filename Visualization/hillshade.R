library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

Radians <- function(img) {
  img$toFloat()$multiply(base::pi)$divide(180)
}

# Compute hillshade for the given illumination az, el.
Hillshade <- function(az, ze, slope, aspect) {
  azimuth <- Radians(ee$Image(az))
  zenith <- Radians(ee$Image(ze))
  azimuth$subtract(aspect)$cos()$
    multiply(slope$sin())$
    multiply(zenith$sin())$
    add(zenith$cos()$multiply(slope$cos()))
}

terrain <- ee$Algorithms$Terrain(ee$Image("srtm90_v4"))
slope_img <- Radians(terrain$select("slope"))
aspect_img <- Radians(terrain$select("aspect"))

Map$addLayer(Hillshade(0, 60, slope_img, aspect_img))
