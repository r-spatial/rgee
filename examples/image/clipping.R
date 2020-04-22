library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("LANDSAT/LC8_L1T_TOA/LC80440342014077LGN00")
roi <- ee$Geometry$Point(list(-122.4481, 37.7599))$buffer(20000)
clipped <- image$clip(roi)

# print(image$getInfo())
Map$setCenter(-122.464, 37.7630, 10)
vis <- list(
  bands = c("B5", "B4", "B3"),
  min = 0,
  max = 0.5,
  gamma = c(0.95, 1.1, 1)
)
Map$addLayer(image, vis, "Full Image", FALSE) +
Map$addLayer(clipped, vis, "Clipped Image")

