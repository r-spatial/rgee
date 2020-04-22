library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("LANDSAT/LC8_L1T/LC80440342014077LGN00")$select(0)
img_proj <- image$projection()$getInfo()

cat(img_proj$type)
cat("CRS:", img_proj$crs)
cat("crs_transform:", img_proj$transform)
cat("Scale in meters:", image$projection()$nominalScale()$getInfo())
