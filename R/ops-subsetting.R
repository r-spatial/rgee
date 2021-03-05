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
#' Map$addLayer(terraclimate[[2]][["tmmx"]], maximumTemperatureVis)
#'
#' terraclimate[[1:2]] <- terraclimate[[1:2]]*1.4
#' Map$addLayer(terraclimate[[2]][["tmmx"]], maximumTemperatureVis)
#' }
#' @export
'[[.ee.imagecollection.ImageCollection' <- function(x, index) {
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

  # 6. Do the index and value have the same length?
  if (length(index) != length(list_value)) {
    stop("The value to assign should have the same length that the ee$ImageCollection.")
  }

  # 5. Condition: Index is outside of ic
  if (!any(seq_len(ee_ic_size) %in% seq_along(list_value))) {
    stop("Not a valid subset")
  }

  # 6. Condition: Index is outside of ic
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
  # 2. Select just an specific band
  if (is.numeric(index)) {

    # 2.1. Deal with negative and zero index
    if (index < 1) {
      if (index == 0) {
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

#' @name ee_subsetting
#' @export
'names<-.ee.image.Image' <-function(x, value) {
  ee$Image$rename(x, value)
}
