library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Create arbitrary constant images.
constant1 <- ee$Image(1)
constant2 <- ee$Image(2)

# Create a collection by giving a list to the constructor.
collectionFromConstructor <- ee$ImageCollection(list(constant1, constant2))
collectionFromConstructor$getInfo()

# Create a collection with fromImages().
collectionFromImages <- ee$ImageCollection$fromImages(
  list(
    ee$Image(3),
    ee$Image(4)
  )
)

collectionFromImages$getInfo()

# Merge two collections.
mergedCollection <- collectionFromConstructor$merge(collectionFromImages)
mergedCollection$getInfo()
