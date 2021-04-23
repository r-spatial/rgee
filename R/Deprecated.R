#' Earth Engine arithmetic, logic and compare generic functions
#'
#' Arithmetic, logic and compare operators for computation with \code{ee$Image}
#' objects and numeric values.
#'
#' \itemize{
#'   \item \strong{Arith}: +, -, *, /, ^, %%, %/%, %>>% and %>>%.
#'   \item \strong{Logic}: !, &, |.
#'   \item \strong{Comparison}: ==, !=, >, <, <=, >=
#' }
#'
#' @param e1 Numeric or ee$Image.
#' @param e2 Numeric or ee$Image.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' # Sum Operator
#' ee1 <- ee$Image(1)
#' ee2 <- ee$Image(2)
#' ee3 <- ee1 + ee2
#' ee_extract(ee3, ee$Geometry$Point(0, 0))
#'
#' v1 <- 1
#' v2 <- 2
#' v3 <- v1 + v2
#' v3
#'
#' # Multiple Operators
#' ee4 <- ee1 / 10
#' ee5 <- ee4 * (ee2 - 1 + ee1^2 / ee2)
#' ee_extract(ee5, ee$Geometry$Point(0, 0))
#'
#' v4 <- v1 / 10
#' v5 <- v4 * (v2 - 1 + v1^2 / v2)
#' v5
#'
#' # multi-layer object mutiplication, no recycling
#' ee6 <- ee1 + c(1, 5, 10)
#' ee_extract(ee6, ee$Geometry$Point(0, 0))
#'
#' v6 <- v1 + c(1, 5, 10)
#' v6
#' }
#' @name Ops-methods
#' @export
Ops.ee.image.Image <- function(e1, e2) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
            "Ops.ee.image.Image will be deprecated in rgee v.1.1.0.",
            "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
            "Deeply sorry for the inconveniences."
    )
  )
  # Convert logical values to numeric.
  if (!missing(e2)) {
    if (is.logical(e2)) {
      e2 <- as.numeric(e2)
    }
  }
  if (is.logical(e1)) {
    e1 <- as.numeric(e1)
  }

  if (.Generic == "+") {
    if (missing(e2)) {
      e1
    } else {
      ee$Image(e1)$add(ee$Image(e2))
    }
  } else if(.Generic == "-") {
    if (missing(e2)) {
      e1$multiply(-1L)
    } else {
      ee$Image(e1)$subtract(ee$Image(e2))
    }
  } else if(.Generic == "*") {
    ee$Image(e1)$multiply(ee$Image(e2))
  } else if(.Generic == "^") {
    ee$Image(e1)$pow(ee$Image(e2))
  } else if(.Generic == "%%") {
    ee$Image(e1)$mod(ee$Image(e2))
  } else if(.Generic == "%/%") {
    ee$Image(e1)$divide(ee$Image(e2))$toInt64()
  } else if(.Generic == "/") {
    ee$Image(e1)$divide(ee$Image(e2))
  } else if (.Generic == "!") {
    if (missing(e2)) {
      ee$Image(e1)$Not()
    } else {
      stop("Unexpected use of !")
    }
  } else if(.Generic == "&") {
    ee$Image(e1)$And(ee$Image(e2))
  } else if(.Generic == "|") {
    ee$Image(e1)$Or(ee$Image(e2))
  } else if(.Generic == "==") {
    ee$Image(e1)$eq(ee$Image(e2))
  } else if(.Generic == "!=") {
    ee$Image(e1)$neq(ee$Image(e2))
  } else if(.Generic == "<") {
    ee$Image(e1)$lt(ee$Image(e2))
  } else if(.Generic == "<=") {
    ee$Image(e1)$lte(ee$Image(e2))
  } else if(.Generic == ">") {
    ee$Image(e1)$gt(ee$Image(e2))
  }  else if(.Generic == ">=") {
    ee$Image(e1)$gte(ee$Image(e2))
  }
}


#' Mathematical functions
#'
#' @param x ee$Image
#' @param ... Ignored
#'
#' Generic mathematical functions that can be used with an \code{ee$Image}
#' object as argument: \code{abs}, \code{sign}, \code{sqrt}, \code{ceiling},
#' \code{cumprod}, \code{cumsum}, \code{log}, \code{log10}, \code{log1p},
#' \code{log2}, \code{acos}, \code{floor}, \code{asin}, \code{atan}, \code{exp},
#' \code{expm1}, \code{cos}, \code{cosh}, \code{sin}, \code{sinh},
#' \code{tan}, and \code{tanh}.
#'
#' @name Math-methods
#' @export
Math.ee.image.Image <- function(x, ...) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "Math.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  if (.Generic == "abs") {
    ee$Image$abs(x)
  } else if(.Generic == "sign") {
    ee$Image$signum(x$float())
  } else if(.Generic == "sqrt") {
    ee$Image$sqrt(x)
  } else if(.Generic == "floor") {
    ee$Image$floor(x)
  } else if(.Generic == "ceiling") {
    ee$Image$ceil(x)
  } else if(.Generic == "round") {
    ee$Image$round(x)
  } else if(.Generic == "log") {
    args <- list(...)
    if (length(args) == 0) {
      ee$Image$log(x) / ee$Image$log(ee$Image$exp(1))
    } else {
      if (is.null(args$base)) {
        stop("Unused argument.")
      }
      ee$Image$log(x) / ee$Image$log(ee$Image(args$base))
    }
  } else if(.Generic == "log10") {
    ee$Image$log10(x)
  } else if(.Generic == "log2") {
    ee$Image$log(x) / ee$Image$log(2)
  } else if(.Generic == "log1p") {
    ee$Image$log(x + 1)
  } else if(.Generic == "exp") {
    ee$Image$exp(x)
  } else if(.Generic == "expm1") {
    ee$Image$exp(x) - 1
  } else if(.Generic == "sin") {
    ee$Image$sin(x)
  } else if(.Generic == "cos") {
    ee$Image$cos(x)
  } else if(.Generic == "tan") {
    ee$Image$tan(x)
  } else if(.Generic == "asin") {
    ee$Image$asin(x)
  } else if(.Generic == "acos") {
    ee$Image$acos(x)
  } else if(.Generic == "atan") {
    ee$Image$atan(x)
  } else if(.Generic == "cosh") {
    ee$Image$cosh(x)
  } else if(.Generic == "sinh") {
    ee$Image$sinh(x)
  } else if(.Generic == "tanh") {
    ee$Image$tanh(x)
  } else if(.Generic == "cumsum") {
    total <- 0
    x_list <- list() # List to save.
    x_bandnames <- x$bandNames()$getInfo() #band names.
    for (index in seq_along(x_bandnames)) {
      total <- total + x[[x_bandnames[[index]]]]
      x_list[[index]] <- total
    }
    ee$ImageCollection(x_list)$toBands()
  } else if(.Generic == "cumprod") {
    total <- 1
    x_list <- list() # List to save.
    x_bandnames <- x$bandNames()$getInfo() #band names.
    for (index in seq_along(x_bandnames)) {
      total <- total * x[[x_bandnames[[index]]]]
      x_list[[index]] <- total
    }
    ee$ImageCollection(x_list)$toBands()
  } else {
    stop(sprintf("rgee does not support %s yet.", .Generic))
  }
}

#' Summary Methods
#'
#' @param ... ee$Image.
#' @param na.rm Ignore.
#' @name Summary-methods
#' @export
Summary.ee.image.Image <- function(..., na.rm = TRUE) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "Summary.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )

  img <- ee$ImageCollection(list(...))$toBands()

  if (.Generic == "max") {
    img$reduce(ee$Reducer$max())
  } else if (.Generic == "min") {
    img$reduce(ee$Reducer$min())
  } else if (.Generic == "range") {
    img$reduce(ee$Reducer$minMax())
  } else if (.Generic == "sum") {
    img$reduce(ee$Reducer$sum())
  } else if (.Generic == "prod") {
    img$reduce(ee$Reducer$product())
  } else {
    stop(sprintf("rgee does not support %s yet.", .Generic))
  }
}

#' @name Summary-methods
#' @export
mean.ee.image.Image <- function(..., na.rm = TRUE) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "mean.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )

  img <- ee$ImageCollection(list(...))$toBands()
  img$reduce(ee$Reducer$mean())
}


#' Return the element at a specified position in an Earth Engine Image or ImageCollection
#'
#' @param ee_c ImageCollection or FeatureCollection.
#' @param index Numeric. Specified position.
#' @return Depending of \code{ee_c} can return either an \code{ee$FeatureCollection}
#' or \code{ee$ImageCollection}.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#'
#' ee_Initialize()
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
#'   st_transform(4326) %>%
#'   sf_as_ee()
#'
#' ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
#'   filterDate("2016-01-01", "2016-01-31")$
#'   filterBounds(nc)
#'
#' ee_s2$size()$getInfo() # 126
#'
#' # Get the first 5 elements
#' ee_get(ee_s2, index = 0:4)$size()$getInfo() # 5
#' }
#' @export
ee_get <- function(ee_c, index = 0) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "ee_get will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  is_consecutive <- all(diff(index) == 1)
  if (any(index < 0)) {
    stop("index must be a positive value")
  }
  if (any(class(ee_c) %in%  c("ee.imagecollection.ImageCollection"))) {
    # Index is a single value?
    if (length(index) == 1) {
      ee_c %>%
        ee$ImageCollection$toList(count = 1, offset = index) %>%
        ee$ImageCollection()
    } else {
      # Index is a n-length vector and consecutive?
      if (length(index) > 1 & is_consecutive) {
        ee_c %>%
          ee$ImageCollection$toList(count = length(index), offset = min(index)) %>%
          ee$ImageCollection()
      } else {
        stop("ee_get only support ascending index order")
      }
    }
  } else  if (any(class(ee_c) %in%  c("ee.featurecollection.FeatureCollection"))) {
    # Index is a single value?
    if (length(index) == 1) {
      ee_c %>%
        ee$FeatureCollection$toList(count = 1, offset = index) %>%
        ee$FeatureCollection()
    } else {
      # Index is a n-length vector and consecutive?
      if (length(index) > 1 & is_consecutive) {
        ee_c %>%
          ee$FeatureCollection$toList(count = length(index), offset = min(index)) %>%
          ee$FeatureCollection()
      } else {
        stop("ee_get only support ascending index order")
      }
    }
  } else {
    stop("ee_get only support objects of class FeatureCollection and ImageCollection.",
         "\nEnter: ", class(ee_c)[1],
         "\nExpected: ee$ImageCollection or ee$FeatureCollection")
  }
}


#' Extract or replace parts of and ee$ImageCollection
#' @param x ee$ImageCollection or ee$Image.
#' @param index Integer. Index specifying elements to extract or replace.
#' @param value ee$ImageCollection or ee$Image to replace in.
#' @name ee_subsetting
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#'
#' ee_Initialize(gcs = TRUE, drive = TRUE)
#'
#' # Define a Image or ImageCollection: Terraclimate
#' terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
#'   ee$ImageCollection$filterDate("2001-01-01", "2002-01-01")
#'
#' # Define temperature Vis parameters
#' maximumTemperatureVis <- list(
#'   min = -300.0,
#'   max = 300.0,
#'   palette = c(
#'     '1a3678', '2955bc', '5699ff', '8dbae9', 'acd1ff', 'caebff', 'e5f9ff',
#'     'fdffb4', 'ffe6a2', 'ffc969', 'ffa12d', 'ff7c1f', 'ca531a', 'ff0000',
#'     'ab0000'
#'   )
#' )
#'
#' Map$setCenter(71.72, 52.48, 2)
#' m1 <- Map$addLayer(terraclimate[[2]][["tmmx"]], maximumTemperatureVis)
#'
#' terraclimate[[2]] <- terraclimate[[2]]*1.4
#' m2 <- Map$addLayer(terraclimate[[2]][["tmmx"]], maximumTemperatureVis)
#' m1 | m2
#' }
#' @export
'[[.ee.imagecollection.ImageCollection' <- function(x, index) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "[[.ee.imagecollection.ImageCollection will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  # 1. Deal with negative and zero index
  if (index < 1) {
    if (index == 0) {
      stop(
        "rgee respect the one-based index. Therefore if you want to obtain the ",
        "first ee$Image you must use 1 rather than 0."
      )
    } else {
      stop("Negative index are not supported.")
    }
  }

  if (length(index) > 1) {
    x %>% ee_get((index) - 1)
  } else {
    x %>% ee_get((index) - 1) %>% ee$ImageCollection$first()
  }
}


#' @name ee_subsetting
#' @export
'[[<-.ee.imagecollection.ImageCollection' <- function(x, index, value) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "[[<-.ee.imagecollection.ImageCollection will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )

  # 1. Deal with negative and zero index
  if (any(index < 1)) {
    if (any(index == 0)) {
      stop(
        "rgee respect the one-based index. Therefore if you want to obtain the ",
        "first ee$Image you must use 1 rather than 0."
      )
    } else {
      stop("Negative index are not supported.")
    }
  }

  if (length(index) == 2) {
    ee_ic_size <- index[2]
    index <- index[1]
  } else {
    # 2. Length of the ImageCollection
    ee_ic_size <- x %>%
      ee$ImageCollection$size() %>%
      ee$Number$getInfo()
  }

  # 3. From ImageCollection to list of images
  ic_list <- lapply(
    (seq_len(ee_ic_size) - 1),
    function(index) x %>% ee_get(index) %>% ee$ImageCollection$first()
  )

  # 4. Convert value from ee.Image, ee.ImageCollection or list of ee.Image to
  #    list of ee.Image.
  if (any(class(value) %in% "ee.image.Image")) {
    list_value <- list(value)
  } else if(any(class(value) %in% "ee.imagecollection.ImageCollection")) {
    # 4.1. Length of the ImageCollection
    ee_value_size <- value %>%
      ee$ImageCollection$size() %>%
      ee$Number$getInfo()

    # 4.2. From ImageCollection to list of images
    value_list <- lapply(
      (seq_len(ee_value_size) - 1),
      function(index) x %>% ee_get(index) %>% ee$ImageCollection$first()
    )
    list_value <- value_list
  } else if(any(class(value) %in% "list")) {
    list_value <- value
  } else {
    stop(
      sprintf(
        "value should be a ee.Image, ee.ImageCollection of a list of ee.Image not a %s.",
        class(value)
      )
    )
  }

  # 5. Do the index and value have the same length?
  if (length(index) != length(list_value)) {
    stop("The value to assign should have the same length that the ee$ImageCollection.")
  }

  # 6. Condition: Index is outside of ic
  if (!any(seq_len(ee_ic_size) %in% seq_along(list_value))) {
    stop("Not a valid subset")
  }

  # 7. Condition: Index is outside of ic
  counter <- 1
  for (list_value_index in seq_along(list_value)) {
    ic_list[[index[counter]]] <- list_value[[list_value_index]]
    counter <- counter + 1
  }
  ee$ImageCollection(ic_list)
}


#' @name ee_subsetting
#' @export
'[[.ee.image.Image' <- function(x, index) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "[[.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )

  # 2. Select just an specific band
  if (is.numeric(index)) {

    # 2.1. Deal with negative and zero index
    if (any(index < 1)) {
      if (any(index == 0)) {
        stop(
          "rgee respect the one-based index. Therefore if you want to obtain the ",
          "first image-band you must use 1 rather than 0."
        )
      } else {
        stop("Negative index are not supported.")
      }
    }

    x$select(index - 1)
  } else if (is.character(index)) {
    x$select(index)
  } else {
    stop(
      sprintf("index must be a numeric or a character not a %s.", class(index))
    )
  }
}


#' @name ee_subsetting
#' @export
'[[<-.ee.image.Image' <- function(x, index, value) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "[[<-.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )

  # 1. Get band names
  if (!any(class(value) %in% "ee.image.Image")) {
    stop(sprintf("value must be a ee.Image not a %s.", class(value)))
  }

  # 2. From multiband Image to single-band ImageCollection
  x_ic <- x %>%
    ee$Image$bandNames() %>%
    ee$List$map(
      ee_utils_pyfunc(
        function(band) x$select(list(band))
      )
    ) %>% ee$ImageCollection()


  if (is.character(index)) {
    # 3. If index is a character obtain band name
    bdn <- ee$Image$bandNames(x)$getInfo()
    if (any(bdn %in% index)) {
      # 3.1. Send to '[['.ImageCollection operator the numerical index and the
      # length of the ImageCollection (to avoid estimate it again!)
      x_ic[[c(which(bdn %in% index), length(bdn))]] <- value
      x_ic$toBands()
    } else {
      stop("Index does not match with any band in the ee$Image.")
    }
  } else {
    # 3. Replace value
    x_ic[[index]] <- value
    x_ic$toBands()
  }
}


#' Names of Earth Engine Images Layers
#'
#' Get or set the names of the layers of an Earth Engine Image object.
#' @param x an EE Image object.
#' @param value a character vector with the same length as x.
#' @name ee_name
#' @export
'names<-.ee.image.Image' <-function(x, value) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "names<-.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  ee$Image$rename(x, value)
}


#' @name ee_name
#' @export
'names.ee.image.Image' <-function(x) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "names.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  x$bandNames()$getInfo()
}



#' Length of an Earth Engine Image Object
#'
#' Get or set the length of an Earth Engine Image.
#' @param x an EE Image Object.
#' @param value a non-negative integer.
#' @details
#'  If a vector is shortened, extra values are discarded and when a vector
#'  is lengthened, it is padded out to its new length with ee$Image(0), with
#'  band name of zzz_rgee_NA_%02d.
#' @name ee_length
#' @export
'length.ee.image.Image' <-function(x) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "length.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  length(x$bandNames()$getInfo())
}



#' @name ee_length
#' @export
'length<-.ee.image.Image' <-function(x, value) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "length<-.ee.image.Image will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )

  ee_x_length <- length(x)
  how_much_add <- value - ee_x_length
  if(how_much_add < 0) {
    x[[seq_len(value)]]
  } else if (how_much_add == 0) {
    x
  } else if (how_much_add > 0) {
    ee_zero_img <- ee$ImageCollection(
      lapply(seq_len(how_much_add), function(z) ee$Image(0))
    )$toBands()
    names(ee_zero_img) <- sprintf("zzz_rgee_NA_%02d", seq_len(how_much_add))
    x$addBands(ee_zero_img)
  }
}


#' Length of an Earth Engine ImageCollection Object
#'
#' Set the length of an Earth Engine Image.
#'
#' @param x an EE ImageCollection Object.
#' @name ee_length_ic
#' @export
'length.ee.imagecollection.ImageCollection' <-function(x) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "length.ee.imagecollection.ImageCollection will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  x$size()$getInfo()
}



# GIF ---------------------------------------------------------------------

#' Create a GIF from an Earth Engine ImageCollection
#'
#' Create an GIF (as a magick-image object) from a EE
#' ImageCollection. Note: Animations can only be created when ImageCollections
#' is composed by RGB or RGBA image. This can be done by mapping
#' a visualization function onto an ImageCollection (e.g. \code{ic$map(function(img) img$visualize(...))})
#' or specifying three bands in parameters argument (See examples). [ee_utils_gif_creator] is a
#' wrapper around \strong{\code{ee$ImageCollection$getVideoThumbURL}}.
#'
#' @author Jeroen Ooms
#'
#' @param ic An ee$ImageCollection.
#' @param parameters List of parameters for visualization and animation. See details.
#' @param quiet Logical. Suppress info message.
#' @param ... parameter(s) passed on to [download.file][utils::download.file]
#' @details
#' The parameters argument is identical to visParams (See \code{rgee::Map$addLayer}),
#' plus, optionally:
#' \itemize{
#'  \item \strong{dimensions}: A number or pair of numbers in format c(WIDTH,HEIGHT).
#'  Max dimensions of the thumbnail to render, in pixels. If only one number is
#'  passed, it is used as the maximum, and the other dimension is computed by
#'  proportional scaling.
#'  \item \strong{crs}: A CRS string specifying the projection of the output.
#'  \item \strong{crs_transform}: The affine transform to use for the output
#'  pixel grid.
#'  \item \strong{scale}: A scale to determine the output pixel grid; ignored if
#'  both crs and crs_transform are specified.
#'  \item \strong{region}: ee$Geometry$Polygon, GeoJSON or c(E,S,W,N). Geospatial
#'  region of the result. By default, the whole image.
#'  \item \strong{format}: String. The output format (only 'gif' is currently
#'  supported).
#'  \item \strong{framesPerSecond}: String. Animation speed.
#' }
#' @return A magick-image object of the specified ImageCollection.
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize()
#'
#' col <- ee$ImageCollection("JRC/GSW1_1/YearlyHistory")$map(function(img) {
#'   year <- img$date()$get("year")
#'   yearImg <- img$gte(2)$multiply(year)
#'   despeckle <- yearImg$connectedPixelCount(15, TRUE)$eq(15)
#'   yearImg$updateMask(despeckle)$selfMask()$set("year", year)
#' })
#'
#' appendReverse <- function(col) col$merge(col$sort('year', FALSE))
#'
#' # -----------------------------------
#' # 1 Basic Animation - Ucayali Peru
#' # -----------------------------------
#'
#' bgColor = "FFFFFF" # Assign white to background pixels.
#' riverColor = "0D0887" # Assign blue to river pixels.
#'
#' ## 1.1 Create the dataset
#' annualCol = col$map(function(img) {
#'   img$unmask(0)$
#'     visualize(min = 0, max = 1, palette = c(bgColor, riverColor))$
#'     set("year", img$get("year"))
#' })
#' basicAnimation <- appendReverse(annualCol)
#'
#'
#' ## 1.2 Set video arguments
#' aoi <- ee$Geometry$Rectangle(-74.327, -10.087, -73.931, -9.327)
#' videoArgs = list(
#'   dimensions = 600, # Max dimension (pixels), min dimension is proportionally scaled.
#'   region = aoi,
#'   framesPerSecond = 10
#' )
#'
#' ## 1.2 Download, display and save the GIF!
#' animation <- ee_utils_gif_creator(basicAnimation, videoArgs, mode = "wb")
#' get_years <- basicAnimation$aggregate_array("year")$getInfo()
#' animation %>%
#'   ee_utils_gif_annotate("Ucayali, Peru") %>%
#'   ee_utils_gif_annotate(get_years, size = 15, location = "+90+40",
#'                         boxcolor = "#FFFFFF") %>%
#'   ee_utils_gif_annotate("created using {magick} + {rgee}",
#'                         size = 15, font = "sans",location = "+70+20") ->
#'   animation_wtxt
#' gc(reset = TRUE)
#' ee_utils_gif_save(animation_wtxt, path = paste0(tempfile(), ".gif"))
#' }
#' @family GIF functions
#' @export
ee_utils_gif_creator <- function(ic, parameters, quiet = FALSE, ...) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "ee_utils_gif_creator will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  # check packages
  ee_check_packages("ee_utils_gif_creator", "magick")

  if (!quiet) {
    message("1. Creating gif ... please wait ....")
  }
  animation_url <- ee$ImageCollection$getVideoThumbURL(ic, parameters)
  temp_gif <- tempfile()
  if (!quiet) {
    message("1. Downloading GIF from: ", animation_url)
  }
  download.file(
    url = animation_url,
    destfile = temp_gif,
    quiet = quiet,
    ...)
  magick::image_read(path = temp_gif)
}


#' Add text to a GIF
#'
#' Add text to a GIF (magick-image object). This function is a wrapper around
#' [image_annotate][magick::image_annotate].
#'
#' @author Jeroen Ooms
#'
#' @param image magick image object returned by [magick::image_read()] or [magick::image_graph()]
#' @param text character vector of length equal to 'image' or length 1
#' @param gravity string with [gravity](https://www.imagemagick.org/Magick++/Enumerations.html#GravityType)
#' value from [gravity_types][magick::gravity_types].
#' @param location geometry string with location relative to `gravity`
#' @param degrees rotates text around center point
#' @param size font-size in pixels
#' @param font string with font family such as `"sans"`, `"mono"`, `"serif"`,
#' `"Times"`, `"Helvetica"`, `"Trebuchet"`, `"Georgia"`, `"Palatino"` or `"Comic Sans"`.
#' @param style value of [style_types][magick::style_types] for example `"italic"`
#' @param weight thickness of the font, 400 is normal and 700 is bold.
#' @param kerning increases or decreases whitespace between letters
#' @param decoration value of [decoration_types][magick::decoration_types] for example `"underline"`
#' @param color a valid [color string](https://www.imagemagick.org/Magick++/Color.html) such as
#' `"navyblue"` or `"#000080"`. Use `"none"` for transparency.
#' @param strokecolor a [color string](https://www.imagemagick.org/Magick++/Color.html)
#' adds a stroke (border around the text)
#' @param boxcolor a [color string](https://www.imagemagick.org/Magick++/Color.html)
#' for background color that annotation text is rendered on.
#'
#' @return A magick-image object
#'
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize()
#'
#' col <- ee$ImageCollection("JRC/GSW1_1/YearlyHistory")$map(function(img) {
#'   year <- img$date()$get("year")
#'   yearImg <- img$gte(2)$multiply(year)
#'   despeckle <- yearImg$connectedPixelCount(15, TRUE)$eq(15)
#'   yearImg$updateMask(despeckle)$selfMask()$set("year", year)
#' })
#'
#' appendReverse <- function(col) col$merge(col$sort('year', FALSE))
#'
#' # -----------------------------------
#' # 1 Basic Animation - Ucayali Peru
#' # -----------------------------------
#'
#' bgColor = "FFFFFF" # Assign white to background pixels.
#' riverColor = "0D0887" # Assign blue to river pixels.
#'
#' ## 1.1 Create the dataset
#' annualCol = col$map(function(img) {
#'   img$unmask(0)$
#'     visualize(min = 0, max = 1, palette = c(bgColor, riverColor))$
#'     set("year", img$get("year"))
#' })
#' basicAnimation <- appendReverse(annualCol)
#'
#'
#' ## 1.2 Set video arguments
#' aoi <- ee$Geometry$Rectangle(-74.327, -10.087, -73.931, -9.327)
#' videoArgs = list(
#'   dimensions = 600, # Max dimension (pixels), min dimension is proportionally scaled.
#'   region = aoi,
#'   framesPerSecond = 10
#' )
#'
#' ## 1.2 Download, display and save the GIF!
#' animation <- ee_utils_gif_creator(basicAnimation, videoArgs, mode = "wb")
#' get_years <- basicAnimation$aggregate_array("year")$getInfo()
#' animation %>%
#'   ee_utils_gif_annotate("Ucayali, Peru") %>%
#'   ee_utils_gif_annotate(get_years, size = 15, location = "+90+40",
#'                         boxcolor = "#FFFFFF") %>%
#'   ee_utils_gif_annotate("created using {magick} + {rgee}",
#'                         size = 15, font = "sans",location = "+70+20") ->
#'   animation_wtxt
#' gc(reset = TRUE)
#' ee_utils_gif_save(animation_wtxt, path = paste0(tempfile(), ".gif"))
#' }
#' @family GIF functions
#' @export
ee_utils_gif_annotate <- function(image,
                                  text,
                                  gravity = "northwest",
                                  location = "+0+0",
                                  degrees = 0,
                                  size = 20,
                                  font = "sans",
                                  style = "normal",
                                  weight = 400,
                                  kerning = 0,
                                  decoration = NULL,
                                  color = NULL,
                                  strokecolor = NULL,
                                  boxcolor = NULL) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "ee_utils_gif_annotate will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )
  # check packages
  ee_check_packages("ee_utils_gif_annotate", "magick")

  if (length(text) == 1) {
    image <- magick::image_annotate(image, text, gravity = gravity,
                                    location = location, degrees = degrees, size = size,
                                    font = font, style = style, weight = weight,
                                    kerning = kerning, decoration = decoration,
                                    color = color, strokecolor = strokecolor,
                                    boxcolor = boxcolor)
  } else if(length(text) == length(image)) {
    image <- magick::image_annotate(image, text, gravity = gravity,
                                    location = location, degrees = degrees, size = size,
                                    font = font, style = style, weight = weight,
                                    kerning = kerning, decoration = decoration,
                                    color = color, strokecolor = strokecolor,
                                    boxcolor = boxcolor)
  } else {
    stop(
      "The text argument has not the same length as the magick-image object",
      "\nActual:",length(text),
      "\nExpected:", length(image)
    )
  }
  image
}


#' Write a GIF
#'
#' Write a magick-image object as a GIF file using the \code{magick} package. This
#' function is a wrapper around [image_write][magick::image_write].
#'
#' @author Jeroen Ooms
#'
#' @param image magick image object returned by [image_read][magick::image_read].
#' @param path path a file, url, or raster object or bitmap array.
#' @param format output format such as `"png"`, `"jpeg"`, `"gif"`, `"rgb"` or `"rgba"`.
#' @param quality number between 0 and 100 for jpeg quality. Defaults to 75.
#' @param depth color depth (either 8 or 16).
#' @param density resolution to render pdf or svg.
#' @param comment text string added to the image metadata for supported formats.
#' @param flatten should the image be flattened before writing? This also replaces
#' transparency with a background color.
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize()
#'
#' col <- ee$ImageCollection("JRC/GSW1_1/YearlyHistory")$map(function(img) {
#'   year <- img$date()$get("year")
#'   yearImg <- img$gte(2)$multiply(year)
#'   despeckle <- yearImg$connectedPixelCount(15, TRUE)$eq(15)
#'   yearImg$updateMask(despeckle)$selfMask()$set("year", year)
#' })
#'
#' appendReverse <- function(col) col$merge(col$sort('year', FALSE))
#'
#' # -----------------------------------
#' # 1 Basic Animation - Ucayali Peru
#' # -----------------------------------
#'
#' bgColor = "FFFFFF" # Assign white to background pixels.
#' riverColor = "0D0887" # Assign blue to river pixels.
#'
#' ## 1.1 Create the dataset
#' annualCol = col$map(function(img) {
#'   img$unmask(0)$
#'     visualize(min = 0, max = 1, palette = c(bgColor, riverColor))$
#'     set("year", img$get("year"))
#' })
#' basicAnimation <- appendReverse(annualCol)
#'
#'
#' ## 1.2 Set video arguments
#' aoi <- ee$Geometry$Rectangle(-74.327, -10.087, -73.931, -9.327)
#' videoArgs = list(
#'   dimensions = 600, # Max dimension (pixels), min dimension is proportionally scaled.
#'   region = aoi,
#'   framesPerSecond = 10
#' )
#'
#' ## 1.2 Download, display and save the GIF!
#' animation <- ee_utils_gif_creator(basicAnimation, videoArgs, mode = "wb")
#' get_years <- basicAnimation$aggregate_array("year")$getInfo()
#' animation %>%
#'   ee_utils_gif_annotate("Ucayali, Peru") %>%
#'   ee_utils_gif_annotate(get_years, size = 15, location = "+90+40",
#'                         boxcolor = "#FFFFFF") %>%
#'   ee_utils_gif_annotate("created using {magick} + {rgee}",
#'                         size = 15, font = "sans",location = "+70+20") ->
#'   animation_wtxt
#' gc(reset = TRUE)
#' ee_utils_gif_save(animation_wtxt, path = paste0(tempfile(), ".gif"))
#' }
#' @family GIF functions
#' @return No return value, called to write a GIF file.
#' @export
ee_utils_gif_save <- function(image,
                              path = NULL,
                              format = NULL,
                              quality = NULL,
                              depth = NULL,
                              density = NULL,
                              comment = NULL,
                              flatten = FALSE) {
  .Deprecated(
    msg = sprintf("%s %s. %s",
                  "ee_utils_gif_save will be deprecated in rgee v.1.1.0.",
                  "Please install rgeeExtra  (https://github.com/r-earthengine/rgeeExtra)",
                  "Deeply sorry for the inconveniences."
    )
  )

  # check packages
  ee_check_packages("ee_utils_gif_save", "magick")
  magick::image_write(image = image, path = path, format = format,
                      quality = quality, depth = depth, density = density,
                      comment = comment, flatten = flatten)
}
