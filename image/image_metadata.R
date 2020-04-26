library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load an Landsat8 Image - San Francisco, California.
image <- ee$Image("LANDSAT/LC8_L1T/LC80440342014077LGN00")

# Get Band names of an ee$Image
bandNames <- image$bandNames()
ee_help(bandNames)
cat("Band names: ", paste(bandNames$getInfo(), collapse = " "))

# Get Band names of an ee$Image
b1proj <- image$select("B1")$projection()$getInfo()
cat("Band 1 projection: ")
cat("type: ", b1proj$type)
cat("crs: ", b1proj$crs)
cat("geotransform: ", paste0(b1proj$transform, " "))

b1scale <- image$select("B1")$projection()$nominalScale()
cat("Band 1 scale: ", b1scale$getInfo())

b8scale <- image$select("B8")$projection()$nominalScale()
cat("Band 8 scale: ", b8scale$getInfo())

properties <- image$propertyNames()$getInfo()
cat("Metadata properties: \n-", paste0(properties, collapse = "\n- "))

cloudiness <- image$get("CLOUD_COVER")$getInfo()
cat("CLOUD_COVER: ", cloudiness)

iso_date <- eedate_to_rdate(image$get("system:time_start"))
iso_timestamp <- eedate_to_rdate(
  ee_date = image$get("system:time_start"),
  js = TRUE
)
cat("ISO Date: ", as.character(iso_date))
cat("Timestamp : ", format(iso_timestamp, scientific = FALSE))
