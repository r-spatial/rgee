library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# traditional R character
print("Hello world!")

# Earth Engine Python Style
ee$String("Hello World from Earth Engine!")$getInfo()
ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")$getInfo()

# Earth Engine Pipes Style
ee$String("Hello World from Earth Engine!") %>%
  ee$String$getInfo()

ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318") %>%
  ee$Image$getInfo()
