library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 raw image.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")

# Define a RasterSymbolizer element with '_enhance_' for a placeholder.
template_sld <- paste0(
  "<RasterSymbolizer>",
  "<ContrastEnhancement><_enhance_/></ContrastEnhancement>",
  "<ChannelSelection>",
  "<RedChannel>",
  "<SourceChannelName>B5</SourceChannelName>",
  "</RedChannel>",
  "<GreenChannel>",
  "<SourceChannelName>B4</SourceChannelName>",
  "</GreenChannel>",
  "<BlueChannel>",
  "<SourceChannelName>B3</SourceChannelName>",
  "</BlueChannel>",
  "</ChannelSelection>",
  "</RasterSymbolizer>"
)

# Get SLDs with different enhancements.

equalize_sld <- gsub("\\_enhance\\_", "Histogram", template_sld)
normalize_sld <- gsub("\\_enhance\\_", "Normalize", template_sld)

# Display the results.
viz <- list(bands = c("B5", "B4", "B3"), min = 0, max = 15000)

Map$centerObject(image, 10)
Map$addLayer(image, name = "Linear") +
Map$addLayer(image$sldStyle(equalize_sld), name = "Equalized") +
Map$addLayer(image$sldStyle(normalize_sld), name = "Normalized")
