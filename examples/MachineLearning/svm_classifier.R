library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Input imagery is a cloud-free Landsat 8 composite.
l8 = ee$ImageCollection("LANDSAT/LC08/C01/T1")

image = ee$Algorithms$Landsat$simpleComposite(
  collection = l8$filterDate("2018-01-01", "2018-12-31"),
  asFloat = TRUE
)

# Use these bands for prediction.
bands = c("B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11")

# Manually created polygons.
forest1 = ee$Geometry$Rectangle(-63.0187, -9.3958, -62.9793, -9.3443)
forest2 = ee$Geometry$Rectangle(-62.8145, -9.206, -62.7688, -9.1735)
nonForest1 = ee$Geometry$Rectangle(-62.8161, -9.5001, -62.7921, -9.4486)
nonForest2 = ee$Geometry$Rectangle(-62.6788, -9.044, -62.6459, -8.9986)

# Make a FeatureCollection from the hand-made geometries.
polygons = ee$FeatureCollection(c(
  ee$Feature(nonForest1, list(class = 0)),
  ee$Feature(nonForest2, list(class = 0)),
  ee$Feature(forest1, list(class = 1)),
  ee$Feature(forest2, list(class = 1))
))

# Get the values for all pixels in each polygon in the training.
training = image$sampleRegions(
  # Get the sample from the polygons FeatureCollection.
  collection = polygons,
  # Keep this list of properties from the polygons.
  properties = list("class"),
  # Set the scale to get Landsat pixels in the polygons.
  scale = 30
)

# Create an SVM classifier with custom parameters.
classifier = ee$Classifier$svm(
  kernelType = "RBF",
  gamma = 0.5,
  cost = 10
)

# Train the classifier.
trained = classifier$train(training, "class", bands)

# Classify the image.
classified = image$classify(trained)

# Display the classification result and the input image.
geoviz_image = list(bands = c("B4", "B3", "B2"), max = 0.5, gamma = 2)
geoviz_class = list(min = 0, max = 1, palette = c("red", "green"))

Map$setCenter(-62.836, -9.2399, 9)
Map$addLayer(
  eeObject = image,
  visParams = geoviz_image,
  name = "image"
) +
Map$addLayer(
  eeObject = classified,
  visParams = geoviz_class,
  name = "deforestation"
) +
Map$addLayer(
  eeObject = polygons,
  name = "training polygons"
)
