#' Return Information and Metadata of Earth Engine Objects
#' @param ... Availabe for ee_Geometry, ee_Feature, ee_FeatureCollection, ee_Image or ee_ImageCollection.
#' @export
ee_print <- function(...) {
  parameters <- c(as.list(environment()), list(...))
  fc_metadata <- sprintf("%s/%s",tempdir(),deparse(substitute(...)))

  if (file.exists(fc_metadata)) {
    load(fc_metadata)
    if (identical(metadata_list$filename,as.character(parameters[[1]]))) {
      ee_print_ee(metadata_list)
    } else {
      metadata_list <- ee_createEElist(...)
      ee_print_ee(metadata_list)
      metadata_list$filename <- as.character(parameters[[1]])
      save(metadata_list,file = fc_metadata)
    }
  } else {
    metadata_list <- ee_createEElist(...)
    ee_print_ee(metadata_list)
    metadata_list$filename <- as.character(...)
    save(metadata_list,file = fc_metadata)
  }
}

ee_createEElist <- function(eeobject) {
  if (identical(eeobject$name(),"Image")) {
    metadata_list <- create_ee_Imagelist(eeobject)
  } else if (identical(eeobject$name(),"ImageCollection")){
    metadata_list <- create_ee_ImageCollectionlist(eeobject)
  } else if (identical(eeobject$name(),"Geometry")){
    metadata_list <- create_ee_Geometry(eeobject)
  } else if (identical(eeobject$name(),"Feature")){
    metadata_list <- create_ee_Feature(eeobject)
  } else if (identical(eeobject$name(),"FeatureCollection")){
    metadata_list <- create_ee_FeatureCollectionlist(eeobject)
  } else {
    stop("ee_createEElist only support: ee_Geometry, ee_Feature, ee_FeatureCollection, ee_Image and ee_ImageCollection.")
  }
}

ee_print_ee <- function(mtd) {
  if (identical(mtd$name,"ee_Image")) {
    print_ee_Image(mtd)
  } else if (identical(mtd$name,"ee_ImageCollection")){
    print_ee_FeatureCollection(mtd)
  } else if (identical(mtd$name,"ee_Geometry")){
    print_ee_FeatureCollection(mtd)
  } else if (identical(mtd$name,"ee_Feature")){
    print_ee_FeatureCollection(mtd)
  } else if (identical(mtd$name,"ee_FeatureCollection")){
    print_ee_FeatureCollection(mtd)
  } else {
    stop("ee_print only support: ee_Geometry, ee_Feature, ee_FeatureCollection, ee_Image and ee_ImageCollection.")
  }
}



#' Save ee_Image metadata in a temporal file
#' @importFrom sf st_crs
#' @noRd
create_ee_Imagelist <- function(eeobject) {
  # Image properties
  img <- eeobject$getInfo()
  bands <- unlist(lapply(img$bands, '[[', 'id'))
  base_img <- img$bands[[1]]
  scale <- eeobject$select(bands[1])$projection()$nominalScale()$getInfo()

  name <- "ee_Image"
  img_nbands <- length(bands)
  img_bands_names <- bands
  img_epsg <- as.numeric(gsub("EPSG:","",base_img$crs))
  img_proj4string <- st_crs(img_epsg)$proj4string
  img_geotransform <- paste0(base_img$crs_transform, collapse = " ")
  img_scale <- scale
  img_dimensions <-  do.call(sprintf, as.list(c("nrow:%s  ncol:%s", base_img$dimensions)))
  img_npixels <- format(base_img$dimensions[1] * base_img$dimensions[2],
                        big.mark="'",small.interval=3)
  img_datatype <- toupper(base_img$data_type$precision)

  metadata_list <- list(name = name,
                        img_nbands=img_nbands,
                        img_bands_names = img_bands_names,
                        img_epsg = img_epsg,
                        img_proj4string = img_proj4string,
                        img_geotransform = img_geotransform,
                        img_scale = img_scale,
                        img_dimensions = img_dimensions,
                        img_npixels = img_npixels,
                        img_datatype = img_datatype)
}

#' Save ee_ImageCollection metadata in a temporal file
#' @noRd
#'
create_ee_ImageCollection_list <- function(eeobject){}

#' Save ee_Geometry metadata in a temporal file
#' @noRd
#'
create_ee_Geometry_list <- function(eeobject){}

#' Save ee_Feature metadata in a temporal file
#' @noRd
#'
create_ee_Feature_list <- function(eeobject){}


#' Save ee_FeatureCollection metadata in a temporal file
#' @noRd
#'
create_ee_FeatureCollectionlist <- function(eeobject) {
  # FeatureCollection properties
  fc_properties_names <- eeobject$propertyNames()$getInfo()
  fc_properties_length <- length(fc_properties_names)

  # Feature properties
  #OBS: Metadata generate just considering the first element
  feature <- eeobject$first()$getInfo()
  f_properties_names <- names(feature$properties)
  f_properties_length <- length(names(feature$properties))

  # Geometry properties
  geometry_type <- toupper(feature$geometry$type)
  projection_properties <- eeobject$first()$geometry()$projection()$getInfo()
  epsg <- as.numeric(gsub("EPSG:","",projection_properties$crs))
  fc_proj4string <- st_crs(epsg)$proj4string
  geotransform <-paste0(projection_properties$transform,collapse = " ")

  nfeatures <- eeobject$size()$getInfo()

  metadata_list <- list(name = "ee_FeatureCollection",
                        fc_nfeatures=nfeatures,
                        fc_geometry_type = geometry_type,
                        fc_epsg = epsg,
                        fc_geotransform = geotransform,
                        fc_proj4string = fc_proj4string,
                        fc_properties_length = fc_properties_length,
                        fc_properties_names = fc_properties_names,
                        f_properties_length = f_properties_length,
                        f_properties_names = f_properties_names)
}


#' ee_Image show function
#' @noRd
print_ee_Image <- function(mtd) {
  cat(sprintf("Class                          : %s \n", mtd$name))
  cat(sprintf("#Bands                         : %s \n", mtd$img_nbands))
  cat(sprintf("Bands names                    : %s \n", paste0(mtd$img_bands_names,collapse = " ")))
  cat(sprintf("EPSG (SRID)                    : %s \n",mtd$img_epsg))
  cat(sprintf("proj4string                    : %s \n",mtd$img_proj4string))
  cat(sprintf("Geotransform                   : [%s] \n",mtd$img_geotransform))
  cat(sprintf("Scale (m)                      : %s \n",mtd$img_scale))
  cat(sprintf("Dimensions                     : %s \n",mtd$img_dimensions))
  cat(sprintf("#Pixels                        : %s \n",mtd$img_npixels))
  cat(sprintf("Data type                      : %s \n",mtd$img_datatype))
}

#' Show ee_ImageCollection metadata
#' @noRd
print_ee_ImageCollection <- function(mtd) { }

#' Show ee_Geometry metadata
#' @noRd
print_ee_Geometry <- function(mtd) { }

#' Show ee_Feature metadata
#' @noRd
print_ee_Feature <- function(mtd) { }

#' Show ee_FeatureCollection metadata
#' @noRd
print_ee_FeatureCollection <- function(mtd) {
  cat(sprintf("class                          : %s \n", mtd$name))
  cat(sprintf("nfeatures                      : %s \n", mtd$fc_nfeatures))
  cat(sprintf("geometry type                  : MULTI%s \n",mtd$fc_geometry_type))
  cat(sprintf("epsg (SRID)                    : %s \n",mtd$fc_epsg))
  cat(sprintf("Geotransform                   : [%s] \n",mtd$fc_geotransform))
  cat(sprintf("FeatureCollection properties   : %s \n",mtd$fc_properties_length))
  cat(ee_create_table(mtd$fc_properties_names))
  cat(sprintf("Feature properties             : %s \n",mtd$f_properties_length))
  cat(ee_create_table(mtd$f_properties_names))
  invisible(mtd)
}

#' Create a table for being used in print_ee_*
#' @noRd
ee_create_table <- function(x) {
  ncol <- 2
  rgx <- c(seq(1,length(x),ncol),length(x)+1)
  ngroups <- length(rgx) - 1

  space <- max(mapply(nchar, x))+2

  x_spaced <- rep(NA,length(x))
  sa <- (space - nchar(x))/2

  count = 1
  for (z in 1:length(x)) {
    if (sa[z]%%2 == 0) final_space <- rep(sa[z],2)
    if (sa[z]%%2 != 0) final_space <- c(floor(sa[z]),ceiling(sa[z]))
    x_spaced[count] <- sprintf("%s%s%s",
                               paste0(rep(" ",final_space[1]),collapse = ""),
                               x[z],
                               paste0(rep(" ",final_space[2]),collapse = ""))
    count = count + 1
  }

  for (t in 1:ngroups) {
    cat(sprintf("                               |%s|\n",
                paste0(x_spaced[rgx[t]:(rgx[t+1]-1)],collapse = "|")))
  }
}
