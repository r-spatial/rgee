library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

images <- ee$ImageCollection("MODIS/MCD43A4")$
  filterDate("2017-01-01", "2017-01-31")$
  select("Nadir_Reflectance_Band1")

# unmask to ensure we have the same number of values everywhere
images <- images$map(function(i) i$unmask(-1))

# convert to array
array <- images$toArray()

# convert to an image
bandNames <- images$aggregate_array("system:index")
image <- array$arrayProject(list(0))$arrayFlatten(list(bandNames))

print(image$getInfo())

bandNames <- image$bandNames()
print(bandNames$getInfo())
