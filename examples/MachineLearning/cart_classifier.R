library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Input imagery is a cloud-free Landsat 8 composite.
l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1")

image <- ee$Algorithms$Landsat$simpleComposite(
  collection = l8$filterDate("2018-01-01", "2018-12-31"),
  asFloat = TRUE
)

# Use these bands for prediction.
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11")

# Load training points. The numeric property 'class' stores known labels.
points <- ee$FeatureCollection("GOOGLE/EE/DEMOS/demo_landcover_labels")

# This property of the table stores the land cover labels.
label <- "landcover"

# Overlay the points on the imagery to get training.
training <- image$select(bands)$sampleRegions(
  collection = points,
  properties = list(label),
  scale = 30
)

# Train a CART classifier with default parameters.
trained <- ee$Classifier$cart()$train(training, label, bands)

# Classify the image with the same bands used for training.
classified <- image$select(bands)$classify(trained)

# Viz parameters.
viz_img <- list(bands = c("B4", "B3", "B2"), max = 0.4)
viz_class <- list(palette = c("red", "green", "blue"), min = 0, max = 2)

# Display the inputs and the results.
Map$centerObject(points)
Map$addLayer(image, viz_img, name = "image") +
Map$addLayer(classified, viz_class, name = "classification")
