#' Print and return metadata about Spatial Earth Engine Objects
#'
#' Print and return metadata about Spatial Earth Engine Objects. By default
#' ee_print will create a table printing all the available metadata.
#'
#' @param eeobject Earth Engine Object. Available for: Geometry, Feature,
#' FeatureCollection, Image or ImageCollection.
#' @param img_index Numeric. Index of the \code{ee$ImageCollection} to fetch.
#' Relevant just for \code{ee$ImageCollection} objects.
#' @param f_index Numeric. Index of the \code{ee$FeatureCollection} to fetch.
#' Relevant just for \code{ee$FeatureCollection} objects.
#' @param img_band Character. Band name of the \code{ee$Image} to fetch.
#' Relevant just for \code{ee$ImageCollection} and \code{ee$Image} objects.
#' @param clean Logical. If TRUE, the cache will be cleaned.
#' @param quiet logical. Suppress info message
#' @param ... ignored
#' @importFrom sf st_crs
#' @importFrom crayon bold blue
#' @importFrom cli rule
#' @importFrom digest digest
#' @examples
#' library(rgee)
#'
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' eeobject <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
#'   filter(ee$Filter()$eq("WRS_PATH", 44))$
#'   filter(ee$Filter()$eq("WRS_ROW", 34))$
#'   filterDate("2014-03-01", "2014-08-01")
#' ee_print(eeobject, max_display = 0)
#' @export
ee_print <- function(eeobject, ...) {
  UseMethod("ee_print")
}

#' @name ee_print
#' @export
ee_print.ee.geometry.Geometry <- function(eeobject, ..., clean = FALSE, quiet = FALSE) {
  # 1. Search if Geometry metadata exist in the /tempdir
  past_eeobject <- NULL
  metadata_file <- sprintf("%s/%s", tempdir(), ee_hash(eeobject))
  if (file.exists(metadata_file) && !clean) {
    suppressWarnings(
      try(load(metadata_file), silent = TRUE)
    )
  }
  if (!identical(past_eeobject, ee_hash(eeobject))) {
    print("hi")
    # 2. Feature metadata
    geometry <- eeobject$getInfo()

    # 3. Geometry metadata
    geom_type <- toupper(geometry$type)
    geom_info <- eeobject$projection()$getInfo()
    geom_epsg <- as.numeric(gsub("EPSG:", "", geom_info$crs))
    geom_proj4string <- st_crs(geom_epsg)$proj4string
    geom_geotransform <- paste0(geom_info$transform, collapse = " ")

    ee_metadata <- list(
      name = "Geometry",
      geom_type = geom_type,
      geom_epsg = geom_epsg,
      geom_geotransform = geom_geotransform,
      geom_proj4string = geom_proj4string
    )

    # 4. Save ee_metadata in /tmpdir
    past_eeobject <- ee_hash(eeobject)
    suppressWarnings(
      try(save(ee_metadata, past_eeobject, file = metadata_file), silent = TRUE)
    )
  }
  if (isFALSE(quiet)) {
    # 7. Display Results
    cat(rule(right = bold(paste0("Earth Engine Geometry"))))
    cat(blue$bold("\nGeometry Metadata:"))
    cat("\n - EPSG (SRID)                :", ee_metadata$geom_epsg)
    cat("\n - proj4string                :", ee_metadata$geom_proj4string)
    cat("\n - Geotransform               :", ee_metadata$geom_geotransform)
    cat("\n - WKT                        :", ee_metadata$geom_type)
    cat("\n", rule())
  }
  invisible(ee_metadata)
}


#' @name ee_print
#' @export
ee_print.ee.feature.Feature <- function(eeobject, ...,  clean = FALSE, quiet = FALSE) {
  # 1. Search if FeatureCollection metadata exist in the /tempdir
  past_eeobject <- NULL
  metadata_file <- sprintf("%s/%s", tempdir(), ee_hash(eeobject))

  if (file.exists(metadata_file) && !clean) {
    suppressWarnings(
      try(load(metadata_file), silent = TRUE)
    )
  }
  if (!identical(past_eeobject, ee_hash(eeobject))) {
    # 2. Feature metadata
    feature_info <- eeobject$getInfo()
    f_properties_names <- names(feature_info$properties)
    f_properties_length <- length(names(feature_info$properties))

    # 3. Geometry metadata
    geom_type <- toupper(feature_info$geometry$type)
    geom_info <- eeobject$geometry()$projection()$getInfo()
    geom_epsg <- as.numeric(gsub("EPSG:", "", geom_info$crs))
    geom_proj4string <- st_crs(geom_epsg)$proj4string
    geom_geotransform <- paste0(geom_info$transform, collapse = " ")

    ee_metadata <- list(
      name = "Feature",
      f_properties_names = f_properties_names,
      f_properties_length = f_properties_length,
      geom_type = geom_type,
      geom_epsg = geom_epsg,
      geom_geotransform = geom_geotransform,
      geom_proj4string = geom_proj4string
    )

    # 4. Save ee_metadata in /tmpdir
    past_eeobject <- ee_hash(eeobject)
    suppressWarnings(
      try(save(ee_metadata, past_eeobject, file = metadata_file), silent = TRUE)
    )
  }
  if (isFALSE(quiet)) {
    # 7. Display Results
    cat(rule(right = bold(paste0("Earth Engine Feature"))))
    cat(blue$bold("\nFeature Metadata:"))
    cat("\n - Number of Properties       :", ee_metadata$f_properties_length)
    cat(blue$bold("\nGeometry Metadata:"))
    cat("\n - EPSG (SRID)                :", ee_metadata$geom_epsg)
    cat("\n - proj4string                :", ee_metadata$geom_proj4string)
    cat("\n - Geotransform               :", ee_metadata$geom_geotransform)
    cat("\n - WKT                        :", ee_metadata$geom_type)
    cat("\n", rule())
  }
  invisible(ee_metadata)
}

#' @name ee_print
#' @export
ee_print.ee.featurecollection.FeatureCollection <- function(eeobject, ..., f_index = 0, clean = FALSE, quiet = FALSE) {
  # 1. Search if FeatureCollection metadata exist in the /tempdir
  past_eeobject <- NULL
  metadata_file <- sprintf("%s/%s", tempdir(), ee_hash(eeobject, f_index))

  if (file.exists(metadata_file) && !clean) {
    suppressWarnings(
      try(load(metadata_file), silent = TRUE)
    )
  }

  if (!identical(past_eeobject, ee_hash(eeobject, f_index))) {
    # 2. Select a specific EE Feature by EE FC index.
    feature <- ee$Feature(eeobject$toList(f_index + 1, f_index)$get(0))

    # 3. FeatureCollection metadata
    fc_properties_names <- eeobject$propertyNames()$getInfo()
    fc_properties_length <- length(fc_properties_names)
    fc_n_features <- eeobject$size()$getInfo()

    # 4. Feature metadata
    feature_info <- feature$getInfo()
    f_properties_names <- names(feature_info$properties)
    f_properties_length <- length(names(feature_info$properties))

    # 5. Geometry metadata
    geom_type <- toupper(feature_info$geometry$type)
    geom_info <- feature$geometry()$projection()$getInfo()
    geom_epsg <- as.numeric(gsub("EPSG:", "", geom_info$crs))
    geom_proj4string <- st_crs(geom_epsg)$proj4string
    geom_geotransform <- paste0(geom_info$transform, collapse = " ")

    ee_metadata <- list(
      name = "FeatureCollection",
      fc_feature_index = f_index,
      fc_nfeatures = fc_n_features,
      fc_properties_names = fc_properties_names,
      fc_properties_length = fc_properties_length,
      f_properties_names = f_properties_names,
      f_properties_length = f_properties_length,
      geom_type = geom_type,
      geom_epsg = geom_epsg,
      geom_geotransform = geom_geotransform,
      geom_proj4string = geom_proj4string
    )

    # 6. Save ee_metadata in /tmpdir
    past_eeobject <- ee_hash(eeobject, f_index)
    suppressWarnings(
      try(save(ee_metadata, past_eeobject, file = metadata_file), silent = TRUE)
    )
  }
  if (isFALSE(quiet)) {
    # 7. Display Results
    cat(rule(right = bold(paste0("Earth Engine FeatureCollection"))))
    cat(blue$bold("\nFeatureCollection Metadata:"))
    cat("\n - Class                      :", "ee$FeatureCollection")
    cat("\n - Number of Features         :", ee_metadata$fc_nfeatures)
    cat("\n - Number of Properties       :", ee_metadata$fc_properties_length)

    cat(blue$bold("\nFeature Metadata:"))
    cat("\n - Number of Properties       :", ee_metadata$f_properties_length)

    cat(blue$bold(sprintf("\nGeometry Metadata (f_index = %s):", ee_metadata$fc_feature_index)))
    cat("\n - EPSG (SRID)                :", ee_metadata$geom_epsg)
    cat("\n - proj4string                :", ee_metadata$geom_proj4string)
    cat("\n - Geotransform               :", ee_metadata$geom_geotransform)
    cat("\n - WKT                        :", ee_metadata$geom_type)
    cat("\n", rule())
  }
  invisible(ee_metadata)
}

#' @name ee_print
#' @export
ee_print.ee.image.Image <- function(eeobject,
                                    ...,
                                    img_band,
                                    clean = FALSE,
                                    quiet = FALSE) {
  # 1. Fetch and Return bandname about ee$Image
  img_bandNames <- eeobject$bandNames()$getInfo()
  img_nband <- length(img_bandNames)
  if (missing(img_band)) {
    img_band <- img_bandNames[1]
  }

  # 1. Search if Image metadata exist in the /tempdir
  past_eeobject <- NULL
  metadata_file <- sprintf("%s/%s", tempdir(), ee_hash(eeobject, img_band))
  if (file.exists(metadata_file) && !clean) {
    suppressWarnings(
      try(load(metadata_file), silent = TRUE) # it will load the past_eeobject
    )
  }

  if (!identical(past_eeobject, ee_hash(eeobject, img_band))) {
    # 3. Fetch and Return metadata about an EE image band
    selected_img <- eeobject$select(img_band)
    band_info <- selected_img$getInfo()
    band_properties <- band_info$properties
    band_metadata <- band_info$bands[[1]]
    band_metadata_epsg <- as.numeric(gsub("EPSG:", "", band_metadata$crs))
    band_metadata_geom <- ee_image_info(selected_img, quiet = TRUE)
    band_metadata_nominal_scale <- selected_img %>%
      ee$Image$projection() %>%
      ee$Projection$nominalScale() %>%
      ee$Projection$getInfo()

    ee_metadata <- list(
      name = "Image",
      img_nbands = length(img_bandNames),
      img_bands_names = paste0(img_bandNames, collapse = " "),
      img_pixel = band_metadata_geom$nrow * band_metadata_geom$ncol * img_nband,
      img_size = band_metadata_geom$image_size * img_nband,
      img_properties_names = names(band_properties),
      img_properties_length = length(band_properties),
      band_name = img_band,
      band_epsg = band_metadata_epsg,
      band_proj4string = st_crs(band_metadata_epsg)$proj4string,
      band_geotransform = paste0(band_metadata$crs_transform, collapse = " "),
      band_scale_x = band_metadata$crs_transform[1],
      band_scale_y = band_metadata$crs_transform[5],
      band_nominal_scale = band_metadata_nominal_scale,
      band_dimensions = c(band_metadata_geom$nrow, band_metadata_geom$ncol),
      band_datatype = toupper(band_metadata$data_type$precision),
      band_pixel = band_metadata_geom$nrow * band_metadata_geom$ncol,
      band_size = band_metadata_geom$image_size
    )
    # 4. Save ee_metadata in /tmpdir
    past_eeobject <- ee_hash(eeobject, img_band)
    suppressWarnings(
      try(save(ee_metadata, past_eeobject, file = metadata_file),silent = TRUE)
    )
  }
  if (isFALSE(quiet)) {
    # 5. Display Results
    cat(rule(right = bold(paste0("Earth Engine Image"))))
    cat(blue$bold("\nImage Metadata:"))
    cat("\n - Class                      :", "ee$Image")
    cat("\n - Number of Bands            :", ee_metadata$img_nbands)
    cat("\n - Bands names                :", ee_metadata$img_bands_names)
    cat("\n - Number of Properties       :", ee_metadata$img_properties_length)
    cat("\n - Number of Pixels*          :", ee_metadata$img_pixel)
    cat("\n - Approximate size*          :", ee_humansize(ee_metadata$img_size))
    cat(blue$bold(sprintf("\nBand Metadata (img_band = %s):", ee_metadata$band_name)))
    cat("\n - EPSG (SRID)                :", ee_metadata$band_epsg)
    cat("\n - proj4string                :", ee_metadata$band_proj4string)
    cat("\n - Geotransform               :", ee_metadata$band_geotransform)
    cat("\n - Nominal scale (meters)     :", ee_metadata$band_nominal_scale)
    cat("\n - Dimensions                 :", ee_metadata$band_dimensions)
    cat("\n - Number of Pixels           :", ee_metadata$band_pixel)
    cat("\n - Data type                  :", ee_metadata$band_datatype)
    cat("\n - Approximate size           :", ee_humansize(ee_metadata$band_size))
    cat("\n", rule())
    cat("\n", "NOTE: (*) Properties calculated considering a constant geotransform and data type.")
  }
  invisible(ee_metadata)
}

#' @name ee_print
#' @export
ee_print.ee.imagecollection.ImageCollection <- function(eeobject,
                                                        ...,
                                                        img_index = 0,
                                                        img_band,
                                                        clean = FALSE,
                                                        quiet = FALSE) {
  # 1. Select a specific EE Image by EE IC index.
  img <- ee$Image(eeobject$toList(img_index + 1, img_index)$get(0))

  # 2. Fetch and Return metadata about the Image
  img_bandNames <- img$bandNames()$getInfo()
  if (missing(img_band)) {
    img_band <- img_bandNames[1]
  }

  # 3. Search if Image metadata exist in the /tempdir
  past_eeobject <- NULL
  metadata_file <- sprintf("%s/%s", tempdir(), ee_hash(eeobject, img_index, img_band))

  ## If metadata_file exist in /tempdir use it
  if (file.exists(metadata_file) && !clean) {
    suppressWarnings(
      try(load(metadata_file), silent = TRUE)
    )
  }

  ## If the query haven't been called before, it will create the metadata
  ## dataset, otherwise load from /temdir
  if (!identical(past_eeobject, ee_hash(eeobject, img_index, img_band))) {
    # 4. Fetch and Return ee$ImageCollection metadata
    ic_properties <- eeobject$propertyNames()$getInfo()
    ic_n_properties <- length(ic_properties)
    ic_n_img <- eeobject$size()$getInfo()

    # 3. Fetch and Return ee$Image metadata
    ee_mtd_img <- ee_print(img, img_band = img_band, quiet = TRUE, clean = TRUE)

    ee_metadata <- list(
      name = "ImageCollection",
      ic_image_index = img_index,
      ic_nimages = ic_n_img,
      ic_properties_names = ic_properties,
      ic_properties_length = ic_n_properties,
      ic_pixel = ee_mtd_img$band_pixel * ee_mtd_img$img_nbands * ic_n_img,
      ic_size = ee_mtd_img$img_size * ic_n_img,
      img_nbands = length(img_bandNames),
      img_bands_names = paste0(img_bandNames, collapse = " "),
      img_pixel = ee_mtd_img$band_pixel * ee_mtd_img$img_nbands,
      img_size = ee_mtd_img$img_size,
      img_properties_names = ee_mtd_img$img_properties_names,
      img_properties_length = ee_mtd_img$img_properties_length,
      band_name = ee_mtd_img$band_name,
      band_epsg = ee_mtd_img$band_epsg,
      band_proj4string = ee_mtd_img$band_proj4string,
      band_geotransform = ee_mtd_img$band_geotransform,
      band_scale_x = ee_mtd_img$band_scale_x,
      band_scale_y = ee_mtd_img$band_scale_y,
      band_nominal_scale = ee_mtd_img$band_nominal_scale,
      band_dimensions = ee_mtd_img$band_dimensions,
      band_datatype = ee_mtd_img$band_datatype,
      band_pixel = ee_mtd_img$band_pixel,
      band_size = ee_mtd_img$band_size
    )
    # 4. Save ee_metadata in /tmpdir
    past_eeobject <- ee_hash(eeobject, img_index, img_band)
    suppressWarnings(
      try(save(ee_metadata, past_eeobject, file = metadata_file),silent = TRUE)
    )
  }
  if (isFALSE(quiet)) {
    # 5. Display Results
    cat(rule(right = bold(paste0("Earth Engine ImageCollection"))))
    cat(blue$bold("\nImageCollection Metadata:"))
    cat("\n - Class                      :", "ee$ImageCollection")
    cat("\n - Number of Images           :", ee_metadata$ic_nimages)
    cat("\n - Number of Properties       :", ee_metadata$ic_properties_length)
    cat("\n - Number of Pixels*          :", ee_metadata$ic_pixel)
    cat("\n - Approximate size*          :", ee_humansize(ee_metadata$ic_size))

    cat(blue$bold(sprintf("\nImage Metadata (img_index = %s):", ee_metadata$ic_image_index)))
    cat("\n - Number of Bands            :", ee_metadata$img_nbands)
    cat("\n - Bands names                :", ee_metadata$img_bands_names)
    cat("\n - Number of Properties       :", ee_metadata$img_properties_length)
    cat("\n - Number of Pixels*          :", ee_metadata$img_pixel)
    cat("\n - Approximate size*          :", ee_humansize(ee_metadata$img_size))

    cat(blue$bold(sprintf("\nBand Metadata (img_band = '%s'):", ee_metadata$band_name)))
    cat("\n - EPSG (SRID)                :", ee_metadata$band_epsg)
    cat("\n - proj4string                :", ee_metadata$band_proj4string)
    cat("\n - Geotransform               :", ee_metadata$band_geotransform)
    cat("\n - Nominal scale (meters)     :", ee_metadata$band_nominal_scale)
    cat("\n - Dimensions                 :", ee_metadata$band_dimensions)
    cat("\n - Number of Pixels           :", ee_metadata$band_pixel)
    cat("\n - Data type                  :", ee_metadata$band_datatype)
    cat("\n - Approximate size           :", ee_humansize(ee_metadata$band_size))
    cat("\n", rule())
    cat("\n", "NOTE: (*) Properties calculated considering a constant geotransform and data type.")
  }
  invisible(ee_metadata)
}


#' @export
print.ee.computedobject.ComputedObject <-
  function(x, type = getOption("rgee.print.option"), ...) {
  if (type == "json") {
    str(x)
  } else if (type == "simply") {
    cat(paste0("EarthEngine Object: ", x$name()))
  } else if (type == "ee_print") {
    ee_print(x)
  }
}

#' Create a hash function digests for Earth Engine objects
#' @noRd
ee_hash <- function(eeobject, ...) {
  gsub(
    pattern = "[^0-9A-Za-z///' ]",
    replacement = "_" ,
    x =  eeobject$serialize() ,
    ignore.case = TRUE
  ) %>%
    digest(algo = "md5") %>%
    paste(..., sep = "_")
}
