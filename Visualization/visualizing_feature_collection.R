library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a FeatureCollection from a table dataset: 'RESOLVE' ecoregions.
ecoregions <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")

# Display as default and with a custom color.
Map$addLayer(ecoregions, {}, "default display") +
Map$addLayer(ecoregions, list(color = "FF0000"), "colored") +
Map$addLayer(ecoregions$draw(color = "006600", strokeWidth = 5), {}, "drawn")

# Create an empty image into which to paint the features, cast to byte.
empty <- ee$Image()$byte()

# Paint all the polygon edges with the same number and 'width', display.
outline <- empty$paint(
  featureCollection = ecoregions,
  color = 1,
  width = 3
)
Map$addLayer(outline, list(palette = "FF0000"), "edges")

# Paint the edges with different colors, display.
outlines <- empty$paint(
  featureCollection = ecoregions,
  color = "BIOME_NUM",
  width = 4
)
palette <- c("FF0000", "00FF00", "0000FF")
Map$addLayer(
  outlines,
  list(palette = palette, max = 14),
  "different color edges"
)

# Paint the edges with different colors and 'width's.
outlines <- empty$paint(
  featureCollection = ecoregions,
  color = "BIOME_NUM",
  width = "NNH"
)
Map$addLayer(
  outlines,
  list(palette = palette, max = 14),
  "different color, width edges"
)

# Paint the interior of the polygons with different colors.
fills <- empty$paint(
  featureCollection = ecoregions,
  color = "BIOME_NUM"
)
Map$addLayer(fills, list(palette = palette, max = 14), "colored fills")

# Paint both the fill and the edges.
filledOutlines <- empty$paint(ecoregions, "BIOME_NUM")$paint(ecoregions, 0, 2)
Map$addLayer(
  filledOutlines,
  list(palette = c("000000", palette), max = 14),
  "edges and fills"
)
