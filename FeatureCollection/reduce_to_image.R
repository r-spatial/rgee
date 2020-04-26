library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Example of FeatureCollection$reduceToImage()

# Define a feature collection with a value we want to average$
fc <- ee$FeatureCollection(
  list(
    ee$Feature(
      ee$Geometry$Rectangle(
        -122.4550, 37.8035,
        -122.4781, 37.7935
      ),
      list(value = 0)
    ),
    ee$Feature(
      ee$Geometry$Polygon(
        list(
          c(-122.4427, 37.8027),
          c(-122.4587, 37.7987),
          c(-122.4440, 37.7934)
        )
      ),
      list(value = 1)
    )
  )
)

# Reduce the collection to an image, where each pixel
# is the mean of the 'value' property in all features
# intersecting that pixel.
image_reduced <- fc$reduceToImage(list("value"), "mean")

Map$setCenter(-122.4561, 37.7983, 14)
Map$addLayer(
  eeObject = image_reduced,
  visParams = list(
    min = 0,
    max = 1,
    palette = c("008800", "00FF00")
  ),
  name = "Image"
)
