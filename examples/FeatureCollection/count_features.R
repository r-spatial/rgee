#' Count features example.
#' Count Panoramio photos near SF that mention bridges.

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

Map$setCenter(-122.39, 37.7857, 12)

photos_near_sf <- ee$FeatureCollection(
  "GOOGLE/EE/DEMOS/sf-photo-locations"
)

bridge_photos <- photos_near_sf$filter(
  ee$Filter()$Or(
    ee$Filter$stringContains("title", "Bridge"),
    ee$Filter$stringContains("title", "bridge")
  )
)

Map$addLayer(photos_near_sf, list(color = "0040b0"), "Photos near SF") +
  Map$addLayer(bridge_photos, list(color = "e02070"), "Bridge photos")

sprintf(
  "There are %d bridge photos around SF.",
  bridge_photos$size()$getInfo()
)
