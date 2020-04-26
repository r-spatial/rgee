library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load datasets
HUC10 <- ee$FeatureCollection("USGS/WBD/2017/HUC10")
HUC08 <- ee$FeatureCollection("USGS/WBD/2017/HUC08")
roi <- HUC08$filter(ee$Filter$eq("name", "Pipestem"))

# Select polygons intersecting the roi
roi2 <- HUC10$filter(
  ee$Filter$contains(leftValue = roi$geometry(), rightField = ".geo")
)
roi3 <- roi2$randomColumn("random")

cat("Random value: ", roi3$first()$get("random")$getInfo())
ee_help(ee$Filter$contains)
