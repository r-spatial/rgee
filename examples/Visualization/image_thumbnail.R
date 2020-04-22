library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Fetch a digital elevation model.
image <- ee$Image("CGIAR/SRTM90_V4")

# Request a default thumbnail of the DEM with defined linear stretch.
# Set masked pixels (ocean) to 1000 so they map as gray.
thumbnail1 <- image$unmask(1000)$getThumbURL(
  list(
    min = 0,
    max = 3000,
    dimensions = 500,
    region = ee$Geometry$Rectangle(c(-84.6, -55.9, -32.9, 15.7))
  )
)
cat("Thumbnail:", thumbnail1)
browseURL(thumbnail1)
