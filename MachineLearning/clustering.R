library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a pre-computed Landsat composite for input.
input <- ee$Image("LANDSAT/LE7_TOA_1YEAR/2001")

# Define a region in which to generate a sample of the input.
region <- ee$Geometry$Rectangle(29.7, 30, 32.5, 31.7)

# Display the sample region.
Map$setCenter(31.5, 31.0, 7)
Map$addLayer(
  eeObject = ee$Image()$paint(region, 0, 2),
  name = "region"
)

# Make the training dataset.
training <- input$sample(
  region = region,
  scale = 30,
  numPixels = 5000
)

# Instantiate the clusterer and train it.
clusterer <- ee$Clusterer$wekaKMeans(15)$train(training)

# Cluster the input using the trained clusterer.
result <- input$cluster(clusterer)

# Display the clusters with random colors.
Map$centerObject(region)
Map$addLayer(
  eeObject = result$randomVisualizer(),
  name = "clusters"
)
