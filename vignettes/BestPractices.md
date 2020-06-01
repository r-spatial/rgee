**Adapted from [Google Earth Engine
Documentation](https://developers.google.com/earth-engine/best_practices).**

This doc describes coding practices that are intended to maximize the
chance of success for complex or expensive Earth Engine computations.

Avoid mixing client functions and objects with server functions and objects
---------------------------------------------------------------------------

Earth Engine server objects are objects with constructors that start
with `ee` (e.g. ee\$Image, ee\$Reducer) and any methods on such objects
are server functions. Any object not constructed in this manner is a
client object. Client objects may come from the R Earth Engine client
(e.g. Map) or the R language (e.g. date, data.frame, c(), list()).

To avoid unintended behavior, do not mix client and server functions in
your script as discussed
[here](https://developers.google.com/earth-engine/debugging#avoid-mixing)
and
[here](https://developers.google.com/earth-engine/debugging#browser-lock)
and
[here](https://developers.google.com/earth-engine/debugging#mapped-functions).
See [this
page](https://developers.google.com/earth-engine/client_server) and/or
[this
tutorial](https://developers.google.com/earth-engine/tutorial_js_01) for
in-depth explanation of client vs. server in Earth Engine. The following
example illustrates the dangers of mixing client and server
functionality:

<img src="thumb_down.png" width=25px>   **Error** — This code doesn’t
work!

``` r
# Won't work.
for (i in seq_len(table$size())) {
  print('No!') 
}
```

Can you spot the error? Note that `table$size()` is a server method on a
server object and can not be used with client-side functionality such as
the `seq_len` function.

A situation in which you may want to use for-loops is with to display
results with `Map`, since the Map object and methods are client-side.

<img src="thumb_up.png" width=25px>   **Good** — Use client functions
for display Earth Engine spatial objects.

``` r
l8_ts <- sprintf(
  "LANDSAT/LC08/C01/T1/LC08_044034_%s",
  c("20140318", "20140403","20140419","20140505")
)

display_l8ts <- list()
for (l8 in l8_ts) {
  ee_l8 <- ee$Image(l8)
  display_l8ts[[l8]] <- Map$addLayer(ee_l8)
}

Map$centerObject(ee_l8)
Reduce('+', display_l8ts)
```

Conversely, `map()` is a server function and client functionality won’t
work inside the function passed to map(). For example:

<img src="thumb_down.png" width=25px>   **Error** — This code doesn’t
work!

``` r
table <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017')

# Error:
foobar <- table$map(function(f) {
  print(f); # Can't use a client function here.
  # Can't Export, either.
})
```

<img src="thumb_up.png" width=25px>   **Good** — Use `map()` `set()`.

``` r
table <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017')

# Do something to every element of a collection.
withMoreProperties = table$map(function(f) {
  # Set a property.
  f$set("area_sq_meters", f$area())
})
print(withMoreProperties$first()$get("area_sq_meters")$getInfo())
```

You can also `filter()` the collection based on computed or existing
properties and `print()` the result. Note that you can not print a
collection with more 5000 elements. If you get the “Collection query
aborted after accumulating over 5000 elements” error, `filter()` or
`limit()` the collection before printing.

Avoid converting to list unnecessarily
--------------------------------------

Collections in Earth Engine are processed using optimizations that are
broken by converting the collection to a `List` or `Array` type. Unless
you need random access to collection elements (i.e. you need to get the
i’th element of a collection), use filters on the collection to access
individual collection elements. The following example illustrates the
difference between type conversion (not recommended) and filtering
(recommended) to access an element in a collection:

<img src="thumb_down.png" width=25px>   **Bad** — Don’t convert to list
unnecessarily!

``` r
table <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017');

# Do NOT do this!!
list <- table$toList(table$size())
print(list$get(13)$getInfo()) # User memory limit exceeded.
```

Note that you can easily trigger errors by converting a collection to a
list unnecessarily. The safer way is to use `filter()`:

<img src="thumb_up.png" width=25px>   **Good** — Use `filter()`.

``` r
print(table$filter(ee$Filter$eq('country_na', 'Niger'))$first()$getInfo())
```

Note that you should [use filters as early as possible in your
analysis](https://developers.google.com/earth-engine/best_practices#filter-and-select-first).

Avoid **ee.Algorithms.If()**
----------------------------

Do not use `ee.Algorithms.If()` to implement branching logic, especially
in a mapped function. As the following example illustrates,
`ee.Algorithms.If()` can be memory intensive and is not recommended:

<img src="thumb_down.png" width=25px>   **Bad** — Don’t use `If()`:

``` r
table <- ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017')

# Do NOT do this!
veryBad = table$map(function(f) {
  ee$Algorithms$If(
    condition = ee$String(f$get('country_na'))$compareTo('Chad')$gt(0),
    trueCase = f,      # Do something.
    falseCase = NULL   # Do something else.
  )
}, TRUE)
print(veryBad$getInfo()) # User memory limit exceeded.

# If() may evaluate both the true and false cases.
```

Note that the second argument to `map()` is `TRUE`. This means that the
mapped function may return nulls and they will be dropped in the
resultant collection. That can be useful (without `If()`), but here the
easiest solution is to use a filter:

<img src="thumb_up.png" width=25px>   **Good** — Use `filter()`.

``` r
print(table$filter(ee$Filter$eq('country_na', 'Chad')))
```

As shown in [this
tutorial](https://developers.google.com/earth-engine/tutorial_js_03#ifelse-conditions),
a functional programming approach using filters is the correct way to
apply one logic to some elements of a collection and another logic to
the other elements of the collection.

Avoid **reproject()**
---------------------

Don’t use `reproject` unless absolutely necessary. One reason you might
want to use reproject() is to force `Map` display computations to happen
at a specific scale so you can examine the results at your desired scale
of analysis. In the next example, patches of hot pixels are computed and
the count of pixels in each patch is computed. Run the example and click
on one of the patches. Note that the count of pixels differs between the
reprojected data the data that has not been reprojected.

``` r
l8sr <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")
sf <- ee$Geometry$Point(c(-122.405, 37.786))
Map$centerObject(sf, 13)

# A reason to reproject - counting pixels and exploring interactively.
image <- l8sr$filterBounds(sf)$
  filterDate("2019-06-01", "2019-12-31")$
  first()
Map$addLayer(image, list(bands = "B10", min = 2800, max = 3100), "image")

hotspots <- image$select("B10")$
  gt(3100)$
  selfMask()$
  rename("hotspots")

objectSize <- hotspots$connectedPixelCount(256)
# Beware of reproject!  Don't zoom out on reprojected data.
reprojected <- objectSize$reproject(hotspots$projection())
Map$addLayer(objectSize, list(min = 1, max = 256), "Size No Reproject", FALSE) +
Map$addLayer(reprojected, list(min = 1, max = 256), "Size Reproject", FALSE)
```

The reason for the discrepancy is because the [scale of
analysis](https://developers.google.com/earth-engine/scale#scale-of-analysis)
is set by the Code Editor zoom level. By calling `reproject()` you set
the scale of the computation instead of the Map display. Use
`reproject()` with extreme caution for reasons described in [this
doc](https://developers.google.com/earth-engine/projections#reprojecting).

Filter and **select()** first
-----------------------------

In general, filter input collections by time, location and/or metadata
prior to doing anything else with the collection. Apply more selective
filters before less selective filters. Spatial and/or temporal filters
are often more selective. For example, note that `select()` and
`filter()` are applied before `map()`:

``` r
images <- ee$ImageCollection("COPERNICUS/S2_SR")
sf <- ee$Geometry$Point(c(-122.463, 37.768))

# Expensive function to reduce the neighborhood of an image.
reduceFunction <- function(image) {
  image$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$square(4)
  )
}

bands <- list("B4", "B3", "B2")
# Select and filter first!
reasonableComputation <- images$select(bands)$
  filterBounds(sf)$
  filterDate("2018-01-01", "2019-02-01")$
  filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 1))$
  aside(ee_print)$ # Useful for debugging.
  map(reduceFunction)$
  reduce('mean')$
  rename(bands)

viz <- list(bands = bands, min = 0, max = 10000)
Map$addLayer(reasonableComputation, viz, "resonableComputation")
```

Use **updateMask()** instead of **mask()**
------------------------------------------

The difference between `updateMask()` and `mask()` is that the former
does a logical `and()` of the argument (the new mask) and the existing
image mask whereas `mask()` simply replaces the image mask with the
argument. The danger of the latter is that you can unmask pixels
unintentionally. In this example, the goal is to mask pixels less than
or equal to 300 meters elevation. As you can see (zoom out), using
`mask()` causes a lot of pixels to become unmasked, pixels that don’t
belong in the image of interest:

``` r
l8sr <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")
sf <- ee$Geometry$Point(c(-122.40554461769182, 37.786807309873716))
aw3d30 <- ee$Image("JAXA/ALOS/AW3D30_V1_1")

Map$centerObject(sf, 7)

image <- l8sr$filterBounds(sf)$filterDate("2019-06-01", "2019-12-31")$first()
vis <- list(bands = c("B4", "B3", "B2"), min = 0, max = 3000)

Map$addLayer(image, vis, "image", FALSE)

mask <- aw3d30$select("AVE")$gt(300)
Map$addLayer(mask, {}, 'mask', FALSE)

# NO!  Don't do this!
badMask <- image$mask(mask)
Map$addLayer(badMask, vis, "badMask")

goodMask <- image.updateMask(mask)
Map$addLayer(goodMask, vis, "goodMask", FALSE)
```

Combine reducers
----------------

If you need multiple statistics (e.g. mean and standard deviation) from
a single input (e.g. an image region), it is more efficient to combine
reducers. For example, to get image statistics, combine reducers as
follows:

``` r
image <- ee$Image('COPERNICUS/S2/20150821T111616_20160314T094808_T30UWU')

# Get mean and SD in every band by combining reducers.
stats <- image$reduceRegion(
  reducer = ee$Reducer$mean()$combine(
    reducer2 = ee$Reducer$stdDev(),
    sharedInputs = TRUE
  ),
  geometry = ee$Geometry$Rectangle(c(-2.15, 48.55, -1.83, 48.72)),
  scale = 10,
  bestEffort = TRUE # Use maxPixels if you care about scale.
)

print(stats$getInfo())

# Extract means and SDs to images.
meansImage <- stats$toImage()$select('.*_mean')
sdsImage <- stats$toImage()$select('.*_stdDev')
```

In this example, note that the mean reducer is combined with the
standard deviation reducer and `sharedInputs` is true to enable a single
pass through the input pixels. In the output dictionary, the name of the
reducer is appended to the band name. To get mean and SD images (for
example to normalize the input image), you can turn the values into an
image and use regexes to extract means and SDs individually as
demonstrated in the example.

Use **Export**
--------------

For computations that result in “User memory limit exceeded” or
“Computation timed out” errors in the Code Editor, the same computations
may be able to succeed by using `Export`. This is because the timeouts
are longer and the allowable memory footprint is larger when running in
the batch system (where exports run). (There are other approaches you
may want to try first as detailed in the debugging doc). Continuing the
previous example, suppose that dictionary returned an error. You could
obtain the results by doing something like:

``` r
link <- '86836482971a35a5e735a17e93c23272'
task <- ee$batch$Export$table$toDrive(
  collection = ee$FeatureCollection(ee$Feature(NULL, stats)),
  description = paste0("exported_stats_demo_", link),
  fileFormat = "CSV"
)

# Using rgee I/O
task <- ee_table_to_drive(
  collection = ee$FeatureCollection(ee$Feature(NULL, stats)),
  description = paste0("exported_stats_demo_", link),
  fileFormat = "CSV"
)
task$start()
ee_monitoring(task)

exported_stats <- ee_drive_to_local(task = task,dsn = "exported_stats.csv")
read.csv(exported_stats)
```

Note that the link is embedded into the asset name, for reproducability.
Also note that if you want to export `toAsset`, you will need to supply
a geometry, which can be anything, for example the image centroid, which
is small and cheap to compute. (i.e. don’t use a complex geometry if you
don’t need it).

See the debugging page for examples of using `Export` to resolve
[Computation](https://developers.google.com/earth-engine/debugging#timed-out)
timed out and [Too many concurrent
aggregations](https://developers.google.com/earth-engine/debugging#too-many).
See [this doc](https://developers.google.com/earth-engine/exporting) for
details on exporting in general.

If you don’t need to clip, don’t use clip()
-------------------------------------------

Using `clip()` unnecessarily will increase computation time. Avoid
`clip()` unless it’s necessary to your analysis. If you’re not sure,
don’t clip. An example of a bad use of clip:

<img src="thumb_down.png" width=25px>   **Bad** — Don’t clip inputs
unnecessarily!

``` r
table <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017')
l8sr <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')

chad <- table$filter(ee$Filter$eq('country_na', 'Chad'))$first()

# Do NOT clip unless you need to.
unnecessaryClip <- l8sr$
  select('B4')$                           # Good.
  filterBounds(chad$geometry())$          # Good.
  filterDate('2019-01-01', '2019-12-31')$ # Good.
  map(function(image) {
    image$clip(chad$geometry())   # NO! Bad! Not necessary.
  })$
  median()$
  reduceRegion(
    reducer = ee$Reducer$mean(),
    geometry = chad$geometry(),
    scale = 30,
    maxPixels = 1e10
  )
print(unnecessaryClip$getInfo())
```

Clipping the input images can be skipped entirely, because the region is
specified in the `reduceRegion()` call:

<img src="thumb_up.png" width=25px>   **Good** — Specify the region on
the output.

``` r
noClipNeeded <- l8sr$
  select('B4')$                          # Good.
  filterBounds(chad$geometry())$          # Good.
  filterDate('2019-01-01', '2019-12-31')$ # Good.
  median()$
  reduceRegion(
    reducer = ee$Reducer$mean(),
    geometry = chad$geometry(), # Geometry is specified here.
    scale = 30,
    maxPixels = 1e10
  )
print(noClipNeeded$getInfo())
```

If this computation times out, `Export` it as in [this
example](https://developers.google.com/earth-engine/best_practices#use-export).

If you need to clip with a complex collection, use **clipToCollection()**
-------------------------------------------------------------------------

If you really need to clip something, and the geometries you want to use
for clipping are in a collection, use `clipToCollection()`:

``` r
ecoregions <- ee$FeatureCollection('RESOLVE/ECOREGIONS/2017')
image <- ee$Image('JAXA/ALOS/AW3D30_V1_1')

complexCollection <- ecoregions$
  filter(
    ee$Filter$eq(
      'BIOME_NAME',
      'Tropical & Subtropical Moist Broadleaf Forests'
    )
  )

Map$addLayer(complexCollection, {}, 'complexCollection')

clippedTheRightWay <- image$select('AVE')$
  clipToCollection(complexCollection)

Map$addLayer(clippedTheRightWay, {}, 'clippedTheRightWay', FALSE)
```

Do NOT use `featureCollection.geometry()` or `featureCollection.union()`
on large and/or complex collections, which can be more memory intensive.

Don’t use a complex collection as the region for a reducer
----------------------------------------------------------

If you need to do a spatial reduction such that the reducer pools inputs
from multiple regions in a `FeatureCollection`, don’t supply
`featureCollection.geometry()` as the `geometry` input to the reducer.
Instead, use `clipToCollection()` and a region large enough to encompass
the bounds of the collection. For example:

``` r
ecoregions <- ee$FeatureCollection('RESOLVE/ECOREGIONS/2017')
image <- ee$Image('JAXA/ALOS/AW3D30_V1_1')
complexCollection <- ecoregions$filter(
  ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Moist Broadleaf Forests')
)

clippedTheRightWay <- image$select('AVE')$clipToCollection(complexCollection)
Map$addLayer(clippedTheRightWay, {}, 'clippedTheRightWay')

reduction <- clippedTheRightWay$reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = ee$Geometry$Rectangle(
    coords = c(-179.9, -50, 179.9, 50),  # Almost global.
    geodesic = FALSE
  ),
  scale = 30,
  maxPixels = 1e12
)

print(reduction$getInfo()) # If this times out, export it.
```

Use a non-zero **errorMargin**
------------------------------

For possibly expensive geometry operations, use the largest error margin
possible given the required precision of the computation. The error
margin specifies the maximum allowable error (in meters) permitted
during operations on geometries (e.g. during reprojection). Specifying a
small error margin can result in the need to densify geometries (with
coordinates), which can be memory intensive. It’s good practice to
specify as large an error margin as possible for your computation:

``` r
ecoregions <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")
complexCollection <- ecoregions$limit(10)

Map$centerObject(complexCollection)
Map$addLayer(complexCollection)

expensiveOps <- complexCollection$map(function(f) {
  f$buffer(10000, 200)$bounds(200)
})

Map$addLayer(expensiveOps, {}, 'expensiveOps')
```

Don’t use a ridiculously small scale with **reduceToVectors()**
---------------------------------------------------------------

If you want to convert a raster to a vector, use an appropriate scale.
Specifying a very small scale can substantially increase computation
cost. Set scale as high as possible give the required precision. For
example, to get polygons representing global land masses:

``` r
etopo <- ee$Image('NOAA/NGDC/ETOPO1')

# Approximate land boundary.
bounds <- etopo$select(0)$gt(-100)

# Non-geodesic polygon.
almostGlobal <- ee$Geometry$Polygon(
  coords = list(
    c(-180, -80),
    c(180, -80),
    c(180, 80),
    c(-180, 80),
    c(-180, -80)
  ),
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$addLayer(almostGlobal, {}, "almostGlobal")

vectors <- bounds$selfMask()$reduceToVectors(
  reducer = ee$Reducer$countEvery(),
  geometry = almostGlobal,
  # Set the scale to the maximum possible given
  # the required precision of the computation.
  scale = 50000
)

Map$addLayer(vectors, {}, "vectors")
```

In the previous example, note the use of a non-geodesic polygon for use
in global reductions.

Don’t use **reduceToVectors()** with **reduceRegions()**
--------------------------------------------------------

Don’t use a `FeatureCollection` returned by `reduceToVectors()` as an
input to `reduceRegions()`. Instead, add the bands you want to reduce
before calling `reduceToVectors()`:

``` r
etopo <- ee$Image('NOAA/NGDC/ETOPO1')
mod11a1 <- ee$ImageCollection('MODIS/006/MOD11A1')

# Approximate land boundary.
bounds <- etopo$select(0)$gt(-100)

# Non-geodesic polygon.
almostGlobal <- ee$Geometry$Polygon(
  coords = list(c(-180, -80), c(180, -80), c(180, 80), c(-180, 80), c(-180, -80)),
  proj = "EPSG:4326",
  geodesic = FALSE
)

lst <- mod11a1$first()$select(0)
means <- bounds$selfMask()$addBands(lst)$reduceToVectors(
  reducer = ee$Reducer$mean(),
  geometry = almostGlobal,
  scale = 1000,
  maxPixels = 1e10
)
print(means$limit(10)$getInfo())
```

Note that other ways of reducing pixels of one image within zones of
another include
[reduceConnectedCommponents()](https://developers.google.com/earth-engine/api_docs#ee.image.reduceconnectedcomponents)
and/or [grouping
reducers](https://developers.google.com/earth-engine/api_docs#ee.image.reduceconnectedcomponents).

Use **fastDistanceTransform()** for neighborhood operations
-----------------------------------------------------------

For some convolution operations, `fastDistanceTransform()` may be more
efficient than `reduceNeighborhood()` or `convolve()`. For example, to
do erosion and/or dilation of binary inputs:

``` r
aw3d30 <- ee$Image("JAXA/ALOS/AW3D30_V1_1")

# Make a simple binary layer from a threshold on elevation.
mask <- aw3d30$select("AVE")$gt(300)
Map$setCenter(-122.0703, 37.3872, 11)
Map$addLayer(mask, {}, "mask")

# Distance in pixel units.
distance <- mask$fastDistanceTransform()$sqrt()
# Threshold on distance (three pixels) for a dilation.
dilation <- distance$lt(3)
Map$addLayer(dilation, {}, "dilation")

# Do the reverse for an erosion.
notDistance <- mask$Not()$fastDistanceTransform()$sqrt()
erosion <- notDistance$gt(3)
Map$addLayer(erosion, {}, 'erosion')
```

Use the optimizations in **reduceNeighborhood()**
-------------------------------------------------

If you need to perform a convolution and can’t use
`fastDistanceTransform()`, use the optimizations in
`reduceNeighborhood()`.

``` r
l8raw <- ee$ImageCollection('LANDSAT/LC08/C01/T1_RT')
composite <- ee$Algorithms$Landsat$simpleComposite(l8raw)

bands <- c('B4', 'B3', 'B2')

optimizedConvolution <- composite$select(bands)$reduceNeighborhood(
  reducer = ee$Reducer$mean(),
  kernel = ee$Kernel$square(3),
  optimization = "boxcar" # Suitable optimization for mean.
)$rename(bands)

viz <- list(bands = bands, min = 0, max = 72)
Map$setCenter(-122.0703, 37.3872, 11)
Map$addLayer(composite, viz, "composite") +
Map$addLayer(optimizedConvolution, viz, "optimizedConvolution")
```

Don’t sample more data than you need
------------------------------------

Resist the urge to increase your training dataset size unnecessarily.
Although increasing the amount of training data is an effective machine
learning strategy in some circumstances, it can also increase
computational cost with no corresponding increase in accuracy. (For an
understanding of when to increase training dataset size, see [this
reference](https://www.deeplearning.ai/machine-learning-yearning/)). The
following example demonstrates how requesting too much training data can
result in the dreaded “Computed value is too large” error:

<img src="thumb_down.png" width=25px>   **Bad** — Don’t sample too much
data!

``` r
l8raw <- ee$ImageCollection('LANDSAT/LC08/C01/T1_RT')
composite <- ee$Algorithms$Landsat$simpleComposite(l8raw)
labels <- ee$FeatureCollection('projects/google/demo_landcover_labels')

# No!  Not necessary.  Don't do this:
labels <- labels$map(function(f){f$buffer(100000, 1000)})

bands <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7')

training <- composite$select(bands)$sampleRegions(
  collection = labels,
  properties = list("landcover"),
  scale = 30
)

classifier <- ee$Classifier$smileCart()$train(
  features = training,
  classProperty = "landcover",
  inputProperties = bands
)

print(classifier$explain()) # Computed value is too large
```

The better approach is to start with a moderate amount of data and tune
the hyperparameters of the classifier to determine if you can achieve
your desired accuracy:

<img src="thumb_up.png" width=25px>   **Good** — Tune hyperparameters.

``` r
l8raw <- ee$ImageCollection("LANDSAT/LC08/C01/T1_RT")
composite <- ee$Algorithms$Landsat$simpleComposite(l8raw)
labels <- ee$FeatureCollection("projects/google/demo_landcover_labels")

# Increase the data a little bit, possibly introducing noise.
labels <- labels$map(function(f) {f$buffer(100, 10)})

bands <- c('B2', 'B3', 'B4', 'B5', 'B6', 'B7')

data <- composite$select(bands)$sampleRegions(
  collection = labels,
  properties = list("landcover"),
  scale = 30
)

# Add a column of uniform random numbers called 'random'.
data <- data$randomColumn()

# Partition into training and testing.
training <- data$filter(ee$Filter$lt("random", 0.5))
testing <- data$filter(ee$Filter$gte("random", 0.5))

# Tune the minLeafPopulation parameter.
minLeafPops <- ee$List$sequence(1, 10)

accuracies <- minLeafPops$map(
  ee_utils_pyfunc(
    function(p) {
      classifier <- ee$Classifier$smileCart(minLeafPopulation = p)$
        train(
          features = training,
          classProperty = "landcover",
          inputProperties = bands
        )
      
      testing$
        classify(classifier)$
        errorMatrix("landcover", "classification")$
        accuracy()
    }
  )
)

minLeafPopulation_array <- accuracies$getInfo()
plot(
  x = minLeafPopulation_array,
  type = "b", 
  col = "blue",
  lwd = 2,
  ylab = "accuracy",
  xlim = c(0,10),
  xlab = "value",
  main = "Hyperparameter tunning (minLeafPopulation)"
)
```

In this example, the classiifer is already very accurate, so there’s not
much tuning to do. You might want to choose the smallest tree possible
(i.e. largest `minLeafPopulation`) that still has the required accuracy.

**Export** intermediate results
-------------------------------

Suppose your objective is to take samples from a relatively complex
computed image. It is often more efficient to `Export` the image
`toAsset()`, load the exported image, then sample. For example:

``` r
image <- ee$Image('UMD/hansen/global_forest_change_2018_v1_6')
geometry <- ee$Geometry$Polygon(
  coords = list(
    c(-76.64069800085349, 5.511777325802095),
    c(-76.64069800085349, -20.483938229362376),
    c(-35.15632300085349, -20.483938229362376),
    c(-35.15632300085349, 5.511777325802095)
  ),
  proj =  "EPSG:4326",
  geodesic =  FALSE
)

testRegion <- ee$Geometry$Polygon(
  coords = list(
    c(-48.86726050085349, -3.0475996402515717),
    c(-48.86726050085349, -3.9248707849303295),
    c(-47.46101050085349, -3.9248707849303295),
    c(-47.46101050085349, -3.0475996402515717)
  ),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# Forest loss in 2016, to stratify a sample.
loss <- image$select("lossyear")
loss16 <- loss$eq(16)$rename("loss16")

# Cloud masking function.
maskL8sr <- function(image) {
  cloudShadowBitMask <- bitwShiftL(1, 3)
  cloudsBitMask <- bitwShiftL(1, 5)
  qa <- image$select('pixel_qa')
  mask <- qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$
    And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
  
  image$updateMask(mask)$
    divide(10000)$
    select("B[0-9]*")$
    copyProperties(image, list("system:time_start"))
}

collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$map(maskL8sr)

# Create two annual cloud-free composites.
composite1 <- collection$filterDate('2015-01-01', '2015-12-31')$median()
composite2 <- collection$filterDate('2017-01-01', '2017-12-31')$median()

# We want a strtatified sample of this stack.
stack <- composite1$addBands(composite2)$float() # Export the smallest size possible.

# Export the image.  This block is commented because the export is complete.
# link <- "0b8023b0af6c1b0ac7b5be649b54db06"
# desc <- paste0(ee_get_assethome(), "/Logistic_regression_stack_", link)
# 
# #ee_image_info(stack)
# task <- ee_image_to_asset(
#   image = stack,
#   description = link,
#   assetId = desc,
#   region = geometry,
#   scale = 100,
#   maxPixels = 1e10
# )

  
# Load the exported image.
exportedStack <- ee$Image(
  "projects/google/Logistic_regression_stack_0b8023b0af6c1b0ac7b5be649b54db06"
)

# Take a very small sample first, to debug.
testSample <- exportedStack$addBands(loss16)$stratifiedSample(
  numPoints = 1,
  classBand = "loss16",
  region = testRegion,
  scale = 30,
  geometries = TRUE
)

print(testSample$getInfo()) # Check this in the console.

# Take a large sample.
sample <- exportedStack$addBands(loss16)$stratifiedSample(
  numPoints = 10000,
  classBand = "loss16",
  region = geometry,
  scale = 30
)

# Export the large sample...
```

In this example, note that the imagery is exported as float. Don’t
export at double precision unless absolutely necessary.

Once the export is completed, reload the asset and proceed with sampling
from it. Note that a very small sample over a very small test area is
run first, for debugging. When that is shown to succeed, take a larger
sample and export it. Such large samples typically need to be exported.
Do not expect such samples to be available interactively (for example
through `print()`) or useable (for example as input to a classifier)
without exporting them first.

Join vs. map-filter
-------------------

Suppose you want to join collections based on time, location or some
metadata property. Generally, this is most efficiently accomplished with
a join. The following example does a spatio-temporal join between the
Landsat 8 and Sentinel-2 collections:

``` r
s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterBounds(ee$Geometry$Point(c(-2.0205, 48.647)))

l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")

joined <- ee$Join$saveAll("landsat")$apply(
  primary = s2,
  secondary = l8,
  condition = ee$Filter$And(
    ee$Filter$maxDifference(
      difference = 1000 * 60 * 60 * 24, # One day in milliseconds
      leftField = "system:time_start",
      rightField = "system:time_start"
    ),
    ee$Filter$intersects(
      leftField = ".geo",
      rightField = ".geo"
    )
  )
)

print(joined$first()$getInfo())
```

Although you should try a join first (`Export` if needed), occasionally
a `filter()` within a `map()` can also be effective, particularly for
very large collections.

``` r
s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterBounds(ee$Geometry$Point(c(-2.0205, 48.647)))

l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")

mappedFilter <- s2$map(function(image) {
  date <- image$date()
  landsat <- l8$
    filterBounds(image$geometry())$
    filterDate(date$advance(-1, "day"), date$advance(1, "day"))
    # Return the input image with matching scenes in a property.
  image$set(
    list(
      landsat = landsat,
      size = landsat$size()
    )
  )
})$filter(ee$Filter$gt("size", 0))

print(mappedFilter$first()$getInfo())
```

**reduceRegion() vs. reduceRegions()** vs. for-loop
---------------------------------------------------

Calling `reduceRegions()` with a very large or complex
`FeatureCollection` as input may result in the dreaded “Computed value
is too large” error. One potential solution is to map `reduceRegion()`
over the `FeatureCollection` instead. Another potential solution is to
use a (gasp) for-loop. Although this is strongly discouraged in Earth
Engine as described [here](), [here]() and [here](), `reduceRegion()`
can be implemented in a for-loop to perform large reductions.

Suppose your objective is to obtain the mean of pixels (or any
statistic) in each feature in a `FeatureCollection` for each image in an
`ImageCollection`. The following example compares the three approaches
previously described:

``` r
# Table of countries.
countriesTable <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017")

# Time series of images.
mod13a1 <- ee$ImageCollection("MODIS/006/MOD13A1")

# MODIS vegetation indices (always use the most recent version).
band <- "NDVI"
imagery <- mod13a1$select(band)

# Option 1: reduceRegions()
testTable <- countriesTable$limit(1) # Do this outside map()s and loops.

data <- imagery$map(function(image) {
  image$reduceRegions(
    collection = testTable,
    reducer = ee$Reducer$mean(),
    scale = 500
  )$map(function(f) {
    f$set(
      list(
        time = image$date()$millis(),
        date = image$date()$format()
      )
    )
  })
})$flatten()

print(data$first()$getInfo())

# Option 2: mapped reduceRegion()
data <- countriesTable$map(function(feature) {
  imagery$map(
    function(image) {
      ee$Feature(
        feature$geometry()$centroid(100),
        image$reduceRegion(
          reducer = ee$Reducer$mean(),
          geometry = feature$geometry(),
          scale = 500
        )
      )$set(
        list(
          time = image$date()$millis(),
          date = image$date()$format()
        )
      )$copyProperties(feature)
    }
  )
})$flatten()

print(data$first()$getInfo())

# Option 3: for-loop (WATCH OUT!)
size <- countriesTable$size()
print(size$getInfo()) # 312

countriesList <- countriesTable$toList(1) # Adjust size.
data <- ee$FeatureCollection(list()) # Empty table.

for (j in (seq_len(countriesList$length()$getInfo()) - 1)) {
  feature <- ee$Feature(countriesList$get(j))
  # Convert ImageCollection > FeatureCollection
  fc <- ee$FeatureCollection(
    imagery$map(
      function(image) {
        ee$Feature(
          feature$geometry()$centroid(100),
          image$reduceRegion(
            reducer = ee$Reducer$mean(),
            geometry = feature$geometry(),
            scale = 500
          )
        )$set(
          list(
            time = image$date()$millis(),
            date = image$date()$format()
          )
        )$copyProperties(feature)
      }
    )
  )
  data <- data$merge(fc)
}
print(data$first()$getInfo())
```

Note that the `first()` thing from each collection is printed, for
debugging purposes. You should not expect that the complete result will
be available interactively: you’ll need to `Export`. Also note that
for-loops should be used with extreme caution and only as a last resort.
Finally, the for-loop requires manually obtaining the size of the input
collection and hardcoding that in the appropriate locations. If any of
that sounds unclear to you, don’t use a for-loop.

Use forward differencing for neighbors in time
----------------------------------------------

Suppose you have a temporally sorted `ImageCollection` (i.e. a time
series) and you want to compare each image to the previous (or next)
image. Rather than use `iterate()` for this purpose, it may be more
efficient to use an array-based forward differencing. The following
example uses this method to de-duplicate the Sentinel-2 collection,
where duplicates are defined as images with the same day of year:

``` r
sentinel2 <- ee$ImageCollection("COPERNICUS/S2")
sf <- ee$Geometry$Point(c(-122.47555371521855, 37.76884708376152))
s2 <- sentinel2$
  filterBounds(sf)$
  filterDate("2018-01-01", "2019-12-31")

withDoys <- s2$map(function(image) {
  ndvi <- image$normalizedDifference(c("B4", "B8"))$rename("ndvi")
  date <- image$date()
  doy <- date$getRelative("day", "year")
  time <- image$metadata("system:time_start")
  doyImage <- ee$Image(doy)$
    rename("doy")$
    int()
  
  ndvi$
    addBands(doyImage)$
    addBands(time)$
    clip(image$geometry()) # Appropriate use of clip.
})

array <- withDoys$toArray()
timeAxis <- 0
bandAxis <- 1

dedup <- function(array) {
  time <- array$arraySlice(bandAxis, -1)
  sorted <- array$arraySort(time)
  doy <- sorted$arraySlice(bandAxis, -2, -1)
  left <- doy$arraySlice(timeAxis, 1)
  right <- doy$arraySlice(timeAxis, 0, -1)
  mask <- ee$Image(ee$Array(list(list(1))))$
    arrayCat(left$neq(right), timeAxis)
  array$arrayMask(mask)
}

deduped <- dedup(array)

# Inspect these outputs to confirm that duplicates have been removed.
print(array$reduceRegion("first", sf, 10)$getInfo())
print(deduped$reduceRegion("first", sf, 10)$getInfo())
```
