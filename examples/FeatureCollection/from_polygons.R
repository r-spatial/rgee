# Create and render a feature collection from polygons.
library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fc <- ee$FeatureCollection(list(
  ee$Feature(
    ee$Geometry$Polygon(
      list(
        c(-109.05, 41),
        c(-109.05, 37),
        c(-102.05, 37),
        c(-102.05, 41)
      )
    ),
    list(name = "Colorado", fill = 1)
  ),
  ee$Feature(
    ee$Geometry$Polygon(
      list(
        c(-114.05, 37.0),
        c(-109.05, 37.0),
        c(-109.05, 41.0),
        c(-111.05, 41.0),
        c(-111.05, 42.0),
        c(-114.05, 42.0)
      )
    ),
    list(name = "Utah", fill = 2)
  )
))

# Fill, then outline the polygons into a blank image.
image1 <- ee$Image(0)$mask(0)$toByte()
image2 <- image1$paint(fc, "fill") # Get color from property named 'fill'
image3 <- image2$paint(fc, 3, 5) # Outline using color 3, width 5.

Map$setCenter(-107, 41, 4)
Map$addLayer(
  eeObject = image3,
  visParams = list(
    palette = c("000000", "FF0000", "00FF00", "0000FF"),
    max = 3,
    opacity = 0.5
  ),
  name = "Colorado & Utah"
)
