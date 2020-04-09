#' Print and return metadata about Spatial Earth Engine Objects
#'
#' Print and return metadata about Spatial Earth Engine Objects. By default
#' ee_print will create a table printing all the available metadata.
#'
#' @param eeobject Earth Engine Object. Available for: Geometry, Feature,
#' FeatureCollection, Image or ImageCollection.
#' @param clean Logical. If TRUE, the cache will be cleaned.
#' @param max_display Set the max number of properties to display.
#' @param quiet logical. Suppress info message
#' @importFrom sf st_crs
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
ee_print <- function(eeobject, clean = FALSE, max_display = 0, quiet = FALSE) {
  UseMethod("ee_print")
}

#' @name ee_print
#' @export
ee_print.ee.geometry.Geometry <- function(eeobject, clean = FALSE,
                                          max_display = 0) {
  past_eeobject <- NULL
  fc_metadata <- sprintf("%s/%s", tempdir(), deparse(substitute(eeobject)))
  if (file.exists(fc_metadata) && !clean) {
    load(fc_metadata)
  }
  if (!identical(past_eeobject, eeobject$serialize())) {
    geom <- eeobject$getInfo()
    projection_properties <- eeobject$projection()$getInfo()

    name <- "Geometry"
    geom_type <- toupper(geom$type)
    geom_ngeom <- length(geom$coordinates)
    geom_epsg <- as.numeric(gsub("EPSG:", "", projection_properties$crs))
    geom_proj4string <- st_crs(geom_epsg)$proj4string
    geom_geotransform <- paste0(projection_properties$transform, collapse = " ")
    geom_geodesic <- eeobject$geodesic()$getInfo()

    mtd <- list(
      name = name,
      geom_type = geom_type,
      geom_ngeom = geom_ngeom,
      geom_epsg = geom_epsg,
      geom_proj4string = geom_proj4string,
      geom_geotransform = geom_geotransform,
      geom_geodesic = geom_geodesic
    )
    past_eeobject <- eeobject$serialize()
    save(mtd, past_eeobject, file = fc_metadata)
  }
  cat(cli::rule(),"\n")
  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("Geometry type                  : %s \n", mtd$geom_type))
  cat(sprintf("Number of geometries           : %s \n", mtd$geom_ngeom))
  cat(sprintf("EPSG (SRID)                    : %s \n", mtd$geom_epsg))
  cat(sprintf("proj4string                    : %s \n", mtd$geom_proj4string))
  cat(sprintf("Geodesic                       : %s \n", mtd$geom_geodesic))
  cat(cli::rule(),"\n")
  invisible(mtd)
}


#' @name ee_print
#' @export
ee_print.ee.feature.Feature <- function(eeobject, clean = FALSE,
                                        max_display = 0) {
  past_eeobject <- NULL
  fc_metadata <- sprintf("%s/%s", tempdir(), deparse(substitute(eeobject)))
  if (file.exists(fc_metadata) && !clean) {
    load(fc_metadata)
  }
  if (!identical(past_eeobject, eeobject$serialize())) {
    feature <- eeobject$getInfo()
    projection_properties <- eeobject$geometry()$projection()$getInfo()

    name <- "Feature"
    ft_type <- feature$geometry$type
    ft_ngeom <- length(feature$geometry$coordinates)
    ft_epsg <- as.numeric(gsub("EPSG:", "", projection_properties$crs))
    ft_proj4string <- st_crs(ft_epsg)$proj4string
    ft_geotransform <- paste0(projection_properties$transform, collapse = " ")
    ft_geodesic <- eeobject$geometry()$geodesic()$getInfo()
    ft_properties_length <- length(feature$properties)
    ft_properties_names <- names(feature$properties)

    mtd <- list(
      name = name,
      ft_type = ft_type,
      ft_ngeom = ft_ngeom,
      ft_epsg = ft_epsg,
      ft_proj4string = ft_proj4string,
      ft_geotransform = ft_geotransform,
      ft_geodesic = ft_geodesic,
      ft_properties_length = ft_properties_length,
      ft_properties_names = ft_properties_names
    )
    past_eeobject <- eeobject$serialize()
    save(mtd, past_eeobject, file = fc_metadata)
  }
  cat(cli::rule(),"\n")
  cat(sprintf("Class                      : %s \n", mtd$name))
  cat(sprintf("Geometry type              : %s \n", mtd$ft_type))
  cat(sprintf("Number of geometries       : %s \n", mtd$ft_ngeom))
  cat(sprintf("EPSG (SRID)                : %s \n", mtd$ft_epsg))
  cat(sprintf("proj4string                : %s \n", mtd$ft_proj4string))
  cat(sprintf("Geodesic                   : %s \n", mtd$ft_geodesic))
  cat(sprintf("Feature properties         : %s \n", mtd$ft_properties_length))
  if (max_display != 0) {
    cat(ee_create_table(mtd$ft_properties_names, max_display))
  }
  cat(cli::rule(),"\n")
  invisible(mtd)
}

#' @name ee_print
#' @export
ee_print.ee.featurecollection.FeatureCollection <- function(eeobject,
                                                            clean = FALSE,
                                                            max_display = 0) {
  past_eeobject <- NULL
  fc_metadata <- sprintf("%s/%s", tempdir(), deparse(substitute(eeobject)))
  if (file.exists(fc_metadata) && !clean) {
    load(fc_metadata)
  }
  if (!identical(past_eeobject, eeobject$serialize())) {
    # FeatureCollection properties
    fc_properties_names <- eeobject$propertyNames()$getInfo()
    fc_properties_length <- length(fc_properties_names)

    # Feature properties
    feature <- eeobject$first()$getInfo()
    f_properties_names <- names(feature$properties)
    f_properties_length <- length(names(feature$properties))

    # Geometry properties
    geometry_type <- toupper(feature$geometry$type)
    projection_properties <- eeobject$first()$geometry()$projection()$getInfo()
    epsg <- as.numeric(gsub("EPSG:", "", projection_properties$crs))
    fc_proj4string <- st_crs(epsg)$proj4string
    geotransform <- paste0(projection_properties$transform, collapse = " ")

    nfeatures <- eeobject$size()$getInfo()

    mtd <- list(
      name = "FeatureCollection",
      fc_nfeatures = nfeatures,
      fc_geometry_type = geometry_type,
      fc_epsg = epsg,
      fc_geotransform = geotransform,
      fc_proj4string = fc_proj4string,
      fc_properties_length = fc_properties_length,
      fc_properties_names = fc_properties_names,
      f_properties_length = f_properties_length,
      f_properties_names = f_properties_names
    )
    past_eeobject <- eeobject$serialize()
    save(mtd, past_eeobject, file = fc_metadata)
  }
  cat(cli::rule(),"\n")
  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("Number of features             : %s \n", mtd$fc_nfeatures))
  cat(sprintf("Geometry type*                 : %s \n", mtd$fc_geometry_type))
  cat(sprintf("EPSG (SRID)*                   : %s \n", mtd$fc_epsg))
  cat(sprintf("proj4string*                   : %s \n", mtd$fc_proj4string))
  cat(sprintf(
    "FeatureCollection properties  : %s \n",
    mtd$fc_properties_length
  ))
  if (max_display != 0) {
    cat(ee_create_table(mtd$fc_properties_names, max_display))
  }
  cat(sprintf(
    "Feature properties*           : %s \n",
    mtd$f_properties_length
  ))
  if (max_display != 0) {
    cat(ee_create_table(mtd$f_properties_names, max_display))
  }
  cat(cli::rule(),"\n")
  cat("* Properties calculated from the first element (ee.Feature)")
  invisible(mtd)
}

#' @name ee_print
#' @export
ee_print.ee.image.Image <- function(eeobject,
                                    band,
                                    clean = FALSE,
                                    quiet = FALSE) {
  # 1. Search if Image metadata exist in the /tempdir
  band_exist <- FALSE
  past_eeobject <- NULL
  metadata_file <- sprintf("%s/%s", tempdir(), deparse(substitute(eeobject)))
  if (file.exists(metadata_file) && !clean) {
    suppressWarnings(
      try(load(metadata_file), silent = TRUE) # it will load the past_eeobject
    )
    band_exist <- ee_metadata$band_name
  }

  ## Get band name
  img_bandNames <- eeobject$bandNames()$getInfo()
  if (missing(band)) {
    band <- img_bandNames[1]
  }

  if (!identical(past_eeobject, eeobject$serialize()) & band_exist != band) {

    # 2. Fetch and Return ee$Image metadata
    img_bandNames <- eeobject$bandNames()$getInfo()
    img_nband <- length(img_bandNames)
    if (missing(band)) {
      band <- img_bandNames[1]
    }

    # 3. Fetch and Return ee$Image band metadata
    selected_img <- eeobject$select(band)
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
      img_size = ee_humansize(band_metadata_geom$image_size * img_nband),
      img_properties_names = names(band_properties),
      img_properties_length = length(band_properties),
      band_name = band,
      band_epsg = band_metadata_epsg,
      band_proj4string = st_crs(band_metadata_epsg)$proj4string,
      band_geotransform = paste0(band_metadata$crs_transform, collapse = " "),
      band_scale_x = band_metadata$crs_transform[1],
      band_scale_y = band_metadata$crs_transform[5],
      band_nominal_scale = band_metadata_nominal_scale,
      band_dimensions = c(band_metadata_geom$nrow, band_metadata_geom$ncol),
      band_datatype = toupper(band_metadata$data_type$precision),
      band_pixel = band_metadata_geom$nrow * band_metadata_geom$ncol * img_nband
    )
    # 4. Save ee_metadata in /tmpdir
    past_eeobject <- eeobject$serialize()
    suppressWarnings(
      try(save(ee_metadata, past_eeobject, file = metadata_file),silent = TRUE)
    )
  }
  if (isFALSE(quiet)) {
    # 5. Display Results
    cat(rule(right = bold(paste0("Earth Engine Image"))))
    cat(blue$bold("\nImage Property:"))
    cat("\n - Class                      :", "ee$Image")
    cat("\n - Number of Bands            :", ee_metadata$img_nbands)
    cat("\n - Bands names                :", ee_metadata$img_bands_names)
    cat("\n - Approximate Image size     :", ee_metadata$img_size)
    cat("\n - Number of Image Properties :", ee_metadata$img_properties_length)
    cat(blue$bold(sprintf("\nBand Property (%s):", band)))
    cat("\n - EPSG (SRID)                :", ee_metadata$band_epsg)
    cat("\n - proj4string                :", ee_metadata$band_proj4string)
    cat("\n - Geotransform               :", ee_metadata$band_geotransform)
    cat("\n - Nominal scale (meters)     :", ee_metadata$band_nominal_scale)
    cat("\n - Dimensions                 :", ee_metadata$band_dimensions)
    cat("\n - Number of Pixels           :", ee_metadata$band_pixel)
    cat("\n - Data type                  :", ee_metadata$band_datatype)
    cat("\n", rule())
  }
  invisible(ee_metadata)
}

#' @name ee_print
#' @export
ee_print.ee.imagecollection.ImageCollection <- function(eeobject,
                                                        clean = FALSE,
                                                        max_display = 0) {
  past_eeobject <- NULL
  fc_metadata <- sprintf("%s/%s", tempdir(), deparse(substitute(eeobject)))
  if (file.exists(fc_metadata) & !clean) {
    load(fc_metadata)
  }
  if (!identical(past_eeobject, eeobject$serialize())) {
    ic <- eeobject$getInfo()
    bands <- unlist(lapply(ic$features[[1]]$bands, "[[", "id"))
    base_ic <- ic$features[[1]]
    scale <- ee$Image(eeobject$first())$
      select(bands[1])$
      projection()$
      nominalScale()$
      getInfo()

    geom_meta_img <- ee_image_info(eeobject, quiet = TRUE)

    name <- "ImageCollection"
    ic_nbands <- length(bands)
    ic_bands_names <- bands
    ic_epsg <- as.numeric(gsub("EPSG:", "", base_ic$bands[[1]]$crs))
    ic_proj4string <- st_crs(ic_epsg)$proj4string
    ic_geotransform <- paste0(base_ic$bands[[1]]$crs_transform, collapse = " ")
    ic_scale <- scale
    ic_dimensions <- do.call(
      sprintf,
      as.list(c("%s x %s (nrow x ncol):", base_ic$bands[[1]]$dimensions))
    )
    xp <- base_ic$bands[[1]]$dimensions[1]
    yp <- base_ic$bands[[1]]$dimensions[2]
    npixels <- xp * yp
    ic_npixels <- format(
      npixels,
      big.mark = "'",
      small.interval = 3
    )
    ic_datatype <- toupper(base_ic$bands[[1]]$data_type$precision)
    ic_properties_ic <- names(ic$properties)
    ic_properties_i <- names(ic$features[[1]]$properties)
    ic_nimages <- eeobject$size()$getInfo()

    mtd <- list(
      name = name,
      ic_nimages = ic_nimages,
      ic_nbands = ic_nbands,
      ic_bands_names = ic_bands_names,
      ic_epsg = ic_epsg,
      ic_proj4string = ic_proj4string,
      ic_geotransform = ic_geotransform,
      ic_scale = ic_scale,
      ic_dimensions = ic_dimensions,
      ic_npixels = ic_npixels,
      ic_datatype = ic_datatype,
      ic_properties_i = ic_properties_i,
      ic_properties_ic = ic_properties_ic
    )
    past_eeobject <- eeobject$serialize()
    save(mtd, past_eeobject, file = fc_metadata)
  }
  cat(cli::rule(),"\n")
  cat(sprintf("Class                      : %s \n", mtd$name))
  cat(sprintf("Number of Images           : %s \n", mtd$ic_nimages))
  cat(sprintf("Number of Bands*           : %s \n", mtd$ic_nbands))
  cat(sprintf("Bands names*               : %s \n", paste0(mtd$ic_bands_names,
    collapse = " "
  )))
  cat(sprintf("EPSG (SRID)*               : %s \n", mtd$ic_epsg))
  cat(sprintf("proj4string*               : %s \n", mtd$ic_proj4string))
  cat(sprintf("Geotransform*              : [%s] \n", mtd$ic_geotransform))
  cat(sprintf("Scale (m)*                 : %s \n", mtd$ic_scale))
  cat(sprintf("Dimensions*                : %s \n", mtd$ic_dimensions))
  cat(sprintf("Number of pixels           : %s \n", mtd$ic_npixels))
  cat(sprintf("Data type*                 : %s \n", mtd$ic_datatype))
  cat(sprintf(
    "Image Properties*          : %s \n",
    length(mtd$ic_properties_i)
  ))
  # if (max_display != 0) {
  #   cat(ee_create_table(mtd$ic_properties_i, max_display))
  # }
  cat(sprintf(
    "ImageCollection Properties : %s \n",
    length(mtd$ic_properties_ic)
  ))
  if (max_display != 0) {
    cat(ee_create_table(mtd$ic_properties_ic, max_display))
  }
  cat(cli::rule(),"\n")
  cat("* Properties calculated from the first element (ee.Image)")
  invisible(mtd)
}

#' Create a friendly viz of EE properties
#' @noRd
ee_create_table <- function(x, max_display) {
  ncol <- 2
  if (length(x) == 0) {
    return("")
  }
  if (length(x) > max_display) x <- x[1:max_display]
  rgx <- c(seq(1, length(x), ncol), length(x) + 1)
  ngroups <- length(rgx) - 1

  space <- max(mapply(nchar, x)) + 2

  x_spaced <- rep(NA, length(x))
  sa <- (space - nchar(x)) / 2

  count <- 1
  for (z in seq_len(length(x))) {
    if (sa[z] %% 2 == 0) final_space <- rep(sa[z], 2)
    if (sa[z] %% 2 != 0) final_space <- c(floor(sa[z]), ceiling(sa[z]))
    x_spaced[count] <- sprintf(
      "%s%s%s",
      paste0(rep(" ", final_space[1]), collapse = ""),
      x[z],
      paste0(rep(" ", final_space[2]), collapse = "")
    )
    count <- count + 1
  }

  for (t in 1:ngroups) {
    cat(sprintf(
      "                               |%s|\n",
      paste0(x_spaced[rgx[t]:(rgx[t + 1] - 1)], collapse = "|")
    ))
  }
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
