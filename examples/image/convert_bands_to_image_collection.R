library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
cat("Number of bands:", image$bandNames()$size()$getInfo())

imageCollection <- ee$ImageCollection(
  image %>%
    ee$Image$bandNames() %>%
    ee$List$map(
      ee_pyfunc(function(b) image$select(ee$String(b)))
    )
)

cat("ImageCollection size: ", imageCollection$size()$getInfo())
