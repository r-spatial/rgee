#' Print and return metadata about Earth Engine Objects
#'
#' @param eeobject Earth Engine Object. Available for: Geometry, Feature,
#' FeatureCollection, Image or ImageCollection.
#' @param ncol Set how many columns should be created for display Earth Engine properties
#'  names.
#' @param max_display Set the max number of properties to display.
#' @details For avoiding computing time extremely large and "EEException" related with memory consuming
#' (\href{https://developers.google.com/earth-engine/ic_info}{See this Earth Engine Guide})
#' just the first element of: ee_Image, ee_ImageCollection, ee_Feature
#' and ee_FeatureCollection will be used to generated Earth Engine object properties.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_initialize()
#' eeobject <- ee_ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
#'   filter(ee_Filter()$eq("WRS_PATH", 44))$
#'   filter(ee_Filter()$eq("WRS_ROW", 34))$
#'   filterDate("2014-03-01", "2014-08-01")
#' ee_print(eeobject, ncol = 2, max_display = 4)
#' }
#' @export
ee_print <- function(eeobject, ncol = 2, max_display = 10) {
  UseMethod("ee_print")
}

#' @name ee_print
#' @export
ee_print.ee.geometry.Geometry <- function(eeobject, ncol = 2, max_display = 10) {

  geom <- eeobject$getInfo()
  projection_properties <- eeobject$projection()$getInfo()

  name <- "ee_Geometry"
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
  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("Geometry type                  : %s \n", mtd$geom_type))
  cat(sprintf("nGeometries                    : %s \n", mtd$geom_ngeom))
  cat(sprintf("EPSG (SRID)                    : %s \n", mtd$geom_epsg))
  cat(sprintf("proj4string                    : %s \n", mtd$geom_proj4string))
  cat(sprintf("Geotransform                   : [%s] \n", mtd$geom_geotransform))
  cat(sprintf("Geodesic                       : %s \n", mtd$geom_geodesic))
  mtd
}


#' @name ee_print
#' @export
ee_print.ee.feature.Feature <- function(eeobject, ncol = 2, max_display = 10) {
  metadata_list <- create_ee_Featurelist(eeobject)
  feature <- eeobject$getInfo()
  projection_properties <- eeobject$geometry()$projection()$getInfo()

  name <- "ee_Feature"
  ft_type <- feature$geometry$type
  ft_ngeom <- length(feature$geometry)
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

  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("Geometry type                  : %s \n", mtd$ft_type))
  cat(sprintf("nGeometries                    : %s \n", mtd$ft_ngeom))
  cat(sprintf("EPSG (SRID)                    : %s \n", mtd$ft_epsg))
  cat(sprintf("proj4string                    : %s \n", mtd$ft_proj4string))
  cat(sprintf("Geotransform                   : [%s] \n", mtd$ft_geotransform))
  cat(sprintf("Geodesic                       : %s \n", mtd$ft_geodesic))
  cat(sprintf("nFeature properties            : %s \n", mtd$ft_properties_length))
  if (max_display != 0) cat(ee_create_table(mtd$ft_properties_names, ncol, max_display))
}

#' @name ee_print
#' @export
ee_print.ee.featurecollection.FeatureCollection <- function(eeobject, ncol = 2, max_display = 10) {
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

  metadata_list <- list(
    name = "ee_FeatureCollection",
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

  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("nFeatures                      : %s \n", mtd$fc_nfeatures))
  cat(sprintf("Geometry type                  : %s \n", mtd$fc_geometry_type))
  cat(sprintf("EPSG (SRID)                    : %s \n", mtd$fc_epsg))
  cat(sprintf("proj4string                    : %s \n", mtd$fc_proj4string))
  cat(sprintf("Geotransform                   : [%s] \n", mtd$fc_geotransform))
  cat(sprintf("nFeatureCollection properties  : %s \n", mtd$fc_properties_length))
  if (max_display != 0) cat(ee_create_table(mtd$fc_properties_names, ncol, max_display))
  cat(sprintf("nFeature properties            : %s \n", mtd$f_properties_length))
  if (max_display != 0) cat(ee_create_table(mtd$f_properties_names, ncol, max_display))
  invisible(mtd)
}

#' @name ee_print
#' @export
ee_print.ee.image.Image <- function(eeobject, ncol = 2, max_display = 10) {
  img <- eeobject$getInfo()
  bands <- unlist(lapply(img$bands, "[[", "id"))
  base_img <- img$bands[[1]]
  scale <- eeobject$select(bands[1])$projection()$nominalScale()$getInfo()

  name <- "ee_Image"
  img_nbands <- length(bands)
  img_bands_names <- bands
  img_epsg <- as.numeric(gsub("EPSG:", "", base_img$crs))
  img_proj4string <- st_crs(img_epsg)$proj4string
  img_geotransform <- paste0(base_img$crs_transform, collapse = " ")
  img_scale <- scale
  img_dimensions <- try(do.call(sprintf, as.list(c("nrow:%s  ncol:%s", base_img$dimensions))))
  img_npixels <- format(base_img$dimensions[1] * base_img$dimensions[2],
                        big.mark = "'", small.interval = 3)
  img_datatype <- toupper(base_img$data_type$precision)
  img_properties_names <- names(img$properties)
  img_properties_length <- length(img$properties)

  if (class(img_dimensions) == "try-error") {
    img_dimensions <- "N/A"
    img_properties_names <- "N/A"
    img_properties_length <- 0
    img_npixels <- "N/A"
    warning("Image geometry properties are doubtful, is it a world bounding image?")
  }

  mtd <- list(
    name = name,
    img_nbands = img_nbands,
    img_bands_names = img_bands_names,
    img_epsg = img_epsg,
    img_proj4string = img_proj4string,
    img_geotransform = img_geotransform,
    img_scale = img_scale,
    img_dimensions = img_dimensions,
    img_npixels = img_npixels,
    img_datatype = img_datatype,
    img_properties_names = img_properties_names,
    img_properties_length = img_properties_length
  )
  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("#Bands                         : %s \n", mtd$img_nbands))
  cat(sprintf("Bands names                    : %s \n", paste0(mtd$img_bands_names, collapse = " ")))
  cat(sprintf("EPSG (SRID)                    : %s \n", mtd$img_epsg))
  cat(sprintf("proj4string                    : %s \n", mtd$img_proj4string))
  cat(sprintf("Geotransform                   : [%s] \n", mtd$img_geotransform))
  cat(sprintf("Scale (m)                      : %s \n", mtd$img_scale))
  cat(sprintf("Dimensions                     : %s \n", mtd$img_dimensions))
  cat(sprintf("#Pixels by image               : %s \n", mtd$img_npixels))
  cat(sprintf("Data type                      : %s \n", mtd$img_datatype))
  cat(sprintf("Image Properties               : %s \n", mtd$img_properties_length))
  if (max_display != 0) cat(ee_create_table(mtd$img_properties_names, ncol, max_display))

}

#' @name ee_print
#' @export
ee_print.ee.imagecollection.ImageCollection <- function(eeobject, ncol = 2, max_display = 10) {
  ic <- eeobject$getInfo()
  bands <- unlist(lapply(ic$bands, "[[", "id"))
  base_ic <- ic$features[[1]]
  scale <- ee_Image(eeobject$first())$
    select(bands[1])$
    projection()$
    nominalScale()$
    getInfo()

  name <- "ee_ImageCollection"
  ic_nbands <- length(bands)
  ic_bands_names <- bands
  ic_epsg <- as.numeric(gsub("EPSG:", "", base_ic$bands[[1]]$crs))
  ic_proj4string <- st_crs(ic_epsg)$proj4string
  ic_geotransform <- paste0(base_ic$bands[[1]]$crs_transform, collapse = " ")
  ic_scale <- scale
  ic_dimensions <- do.call(sprintf, as.list(c("nrow:%s  ncol:%s", base_ic$bands[[1]]$dimensions)))
  ic_npixels <- format(base_ic$bands[[1]]$dimensions[1] * base_ic$bands[[1]]$dimensions[2],
                       big.mark = "'", small.interval = 3)
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

  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("nImages                        : %s \n", mtd$ic_nimages))
  cat(sprintf("nBands                         : %s \n", mtd$ic_nbands))
  cat(sprintf("Bands names                    : %s \n", paste0(mtd$ic_bands_names, collapse = " ")))
  cat(sprintf("EPSG (SRID)                    : %s \n", mtd$ic_epsg))
  cat(sprintf("proj4string                    : %s \n", mtd$ic_proj4string))
  cat(sprintf("Geotransform                   : [%s] \n", mtd$ic_geotransform))
  cat(sprintf("Scale (m)                      : %s \n", mtd$ic_scale))
  cat(sprintf("Dimensions                     : %s \n", mtd$ic_dimensions))
  cat(sprintf("nPixels by image               : %s \n", mtd$ic_npixels))
  cat(sprintf("Data type                      : %s \n", mtd$ic_datatype))
  cat(sprintf("Image Properties               : %s \n", length(mtd$ic_properties_i)))
  if (max_display != 0) cat(ee_create_table(mtd$ic_properties_i, ncol, max_display))
  cat(sprintf("ImageCollection Properties     : %s \n", length(mtd$ic_properties_ic)))
  if (max_display != 0) cat(ee_create_table(mtd$ic_properties_ic, ncol, max_display))
}

#' Create a friendly viz of EE properties
#' @noRd
ee_create_table <- function(x, ncol, max_display) {
  if (length(x) > max_display) x <- x[1:max_display]
  rgx <- c(seq(1, length(x), ncol), length(x) + 1)
  ngroups <- length(rgx) - 1

  space <- max(mapply(nchar, x)) + 2

  x_spaced <- rep(NA, length(x))
  sa <- (space - nchar(x)) / 2

  count <- 1
  for (z in 1:length(x)) {
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
    cat(sprintf("                               |%s|\n",
                paste0(x_spaced[rgx[t]:(rgx[t + 1] - 1)], collapse = "|")
    ))
  }
}
