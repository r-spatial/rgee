library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Create a constant Image with a pixel value of 1
image1 <- ee$Image(1)
print(image1, type = "json")
print(image1, type = "simply")
print(image1, type = "ee_print")
# Map$addLayer(image1)

# Create a constant Image with a pixel value of 1
image2 <- ee$Image(2)

# Concatenate Images
image3 <- ee$Image$cat(c(image1, image2))
ee_print(image3, clean = TRUE)

# Change print option by: "simply","json","ee_print"
options(rgee.print.option = "simply")
multiband <- ee$Image(c(1, 2, 3))
print(multiband)

# Rename images using ee$Image$select
renamed <- multiband$select(
  opt_selectors = c("constant", "constant_1", "constant_2"),
  opt_names = c("band1", "band2", "band3")
)
ee_print(renamed)
