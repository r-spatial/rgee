#' Create an R spatial gridded object from an EE thumbnail image
#'
#' Wrapper function around \code{ee$Image$getThumbURL} to create a stars or
#' RasterLayer R object from a
#' \href{https://developers.google.com/earth-engine/image_visualization#thumbnail-images}{EE thumbnail image}.
#'
#' @param image EE Image object to be converted into a stars object.
#' @param region EE Geometry Rectangle (\code{ee$Geometry$Rectangle}) specifying
#' the region to export.The CRS needs to be the same as the \code{x} argument,
#' otherwise, it will be forced.
#' @param dimensions Numeric vector of length 2. Thumbnail dimensions in pixel
#' units. If a single integer is provided, it defines the size of the
#' image's larger aspect dimension and scales the smaller dimension
#' proportionally. Defaults to 512 pixels for the larger image aspect dimension.
#' @param vizparams A list that contains the visualization parameters.
#' See details.
#' @param raster Logical. Should the thumbnail image be saved as a
#' RasterStack object?
#' @param quiet logical; suppress info messages.
#' @details
#'
#' \code{vizparams} set up the details of the thumbnail image. With
#' `ee_as_thumbnail` only is possible export one-band (G) or three-band
#' (RGB) images. Several parameters can be passed on to control color,
#' intensity, the maximum and minimum values, etc. The table below provides
#' all the parameters that admit `ee_as_thumbnail`.
#'
#' \tabular{lll}{
#' \strong{Parameter} \tab \strong{Description} \tab \strong{Type}\cr
#' \strong{bands}     \tab Comma-delimited list of
#' three band names to be mapped to RGB \tab  list \cr
#' \strong{min}       \tab  Value(s) to map to 0 \tab
#' number or list of three numbers, one for each band \cr
#' \strong{max}       \tab  Value(s) to map to 1 \tab
#' number or list of three numbers, one for each band \cr
#' \strong{gain}      \tab  Value(s) by which to multiply each pixel value \tab
#' number or list of three numbers, one for each band \cr
#' \strong{bias}      \tab  Value(s) to add to each Digital Number (DN)
#' value \tab number or list of three numbers, one for each band \cr
#' \strong{gamma}     \tab  Gamma correction factor(s) \tab
#' number or list of three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings
#' (single-band images only) \tab  comma-separated list of hex strings \cr
#' \strong{opacity}   \tab  The opacity of the layer
#' (0.0 is fully transparent and 1.0 is fully opaque) \tab
#' number \cr
#' }
#'
#' @return An stars or Raster object depending on the \code{raster} argument.
#' @family image download functions
#'
#' @importFrom methods as
#' @importFrom reticulate py_to_r
#' @importFrom utils download.file zip str
#' @examples
#' \dontrun{
#' library(raster)
#' library(stars)
#' library(rgee)
#'
#' ee_Initialize()
#'
#' nc <- st_read(system.file("shp/arequipa.shp", package = "rgee"))
#' dem_palette <- c(
#'   "#008435", "#1CAC17", "#48D00C", "#B3E34B", "#F4E467",
#'   "#F4C84E", "#D59F3C", "#A36D2D", "#C6A889", "#FFFFFF"
#' )
#'
#' ## DEM data -SRTM v4.0
#' image <- ee$Image("CGIAR/SRTM90_V4")
#' world_region <- ee$Geometry$Rectangle(
#'   coords = c(-180,-60,180,60),
#'   proj = "EPSG:4326",
#'   geodesic = FALSE
#' )
#'
#' ## world - elevation
#' world_dem <- ee_as_thumbnail(
#'   image = image,
#'   region = world_region,
#'   dimensions = 1024,
#'   vizparams = list(min = 0, max = 5000)
#' )
#'
#' world_dem[world_dem <= 0] <- NA
#' world_dem <- world_dem * 5000
#'
#' plot(
#'   x = world_dem, col = dem_palette, breaks = "equal",
#'   reset = FALSE, main = "SRTM - World"
#' )
#'
#' ## Arequipa-Peru
#' arequipa_region <- nc %>%
#'   st_bbox() %>%
#'   st_as_sfc() %>%
#'   sf_as_ee()
#'
#' arequipa_dem <- ee_as_thumbnail(
#'   image = image,
#'   region = arequipa_region$buffer(1000)$bounds(),
#'   dimensions = 512,
#'   vizparams = list(min = 0, max = 5000)
#' )
#'
#' arequipa_dem <- arequipa_dem * 5000
#' st_crs(arequipa_dem) <- 4326
#' plot(
#'   x = arequipa_dem[nc], col = dem_palette, breaks = "equal",
#'   reset = FALSE, main = "SRTM - Arequipa"
#' )
#'
#' suppressWarnings(plot(
#'   x = nc, col = NA, border = "black", add = TRUE,
#'   lwd = 1.5
#' ))
#' dev.off()
#'
#' ## LANDSAT 8
#' img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
#'   select(c("B4", "B3", "B2"))
#' Map$centerObject(img)
#' Map$addLayer(img, list(min = 0, max = 5000, gamma = 1.5))
#'
#' ## Teton Wilderness
#' l8_img <- ee_as_thumbnail(
#'   image = img,
#'   region = img$geometry()$bounds(),
#'   dimensions = 1024,
#'   vizparams = list(min = 0, max = 5000, gamma = 1.5),
#'   raster = TRUE
#' )
#' crs(l8_img) <- "+proj=longlat +datum=WGS84 +no_defs"
#' plotRGB(l8_img, stretch = "lin")
#' }
#' @export
ee_as_thumbnail <- function(image, region, dimensions, vizparams = NULL,
                            raster = FALSE, quiet = FALSE) {
  message_deprecated <- c(
    "Downloading images via \"ee_as_thumbnail\" will not be available for rgee version 1.0.8 ." ,
    " Use ee_as_raster(..., via = \"drive\"), or ee_as_raster(..., via = \"gcs\") instead."
  )
  .Deprecated("ee_as_thumbnail",msg = message_deprecated)
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  if (!requireNamespace("stars", quietly = TRUE)) {
    stop("package stars required, please install it first")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("package raster required, please install it first")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("package jsonlite required, please install it first")
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("package png required, please install it first")
  }

  # check viz parameters
  ee_check_vizparam(vizparams)

  # is image an ee.image.Image?
  if (!any(class(image) %in% "ee.image.Image")) {
    stop("image argument is not an ee$image$Image")
  }

  # is region an ee.geometry.Geometry?
  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  # From ee$Geometry$Rectangle to sf
  sf_region <- ee_as_sf(x = region)["geometry"]

  ## region is a ee$Geometry$Rectangle?
  if (any(class(region) %in% "ee.geometry.Geometry")) {
    npoints <- nrow(sf::st_coordinates(sf_region))
    if (npoints != 5) {
      stop(
        stop("region needs to be a ee$Geometry$Rectangle.")
      )
    }
  }

  # is dimensions missing?
  if (missing(dimensions)) {
    dimensions <- 512L
    if (!quiet) {
      message("dimensions param is missing. Assuming 512",
              " for the larger image aspect dimension.")
    }
  }

  # it is a large image?
  if (max(dimensions) > 2048) {
    if (!quiet) {
      message(
        "For large image is preferible use rgee::ee_download_*(...)",
        "or rgee::ee_as_*(...)"
      )
    }
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = jsonlite::parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "thumbnail"
  )
  if (is.null(image_id))  image_id <- "thumbnail"

  # Metadata of the Geometry to display
  ## is geodesic?
  is_geodesic <- ee_utils_py_to_r(region$geodesic()$getInfo())
  ## is_evenodd?
  query_params <- unlist(jsonlite::parse_json(region$serialize())$scope)
  is_evenodd <- as.logical(
    query_params[grepl("evenOdd", names(query_params))]
  )
  if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
    is_evenodd <- TRUE
  }

  # bbox and CRS of the geometry
  init_offset <- sf::st_bbox(sf_region)
  ee_crs <- sf::st_crs(sf_region)$epsg

  if (!quiet) {
    ee_geometry_message(region = region,
                        sf_region = sf_region[["geometry"]])
  }

  # Preparing parameters
  vizparams$dimensions <- dimensions
  vizparams$region <- region
  vizparams$format <- "png"
  if (is.null(vizparams$min)) {
    vizparams$min <- 0
  }
  if (is.null(vizparams$max)) {
    vizparams$min <- 1
  }

  # Creating thumbnail in png format
  if (!quiet) {
    cat(
      "Getting the thumbnail image ... please wait\n"
    )
  }
  thumbnail_url <- image$getThumbURL(vizparams)

  # Reading the png image
  z <- tempfile()
  download.file(thumbnail_url, z, mode = "wb", quiet = TRUE)
  raw_image <- png::readPNG(z)

  # matrix to array
  if (length(dim(raw_image)) == 2) {
    dim(raw_image) <- c(dim(raw_image), 1)
  }

  # It is a RGB or gray image?
  if (dim(raw_image)[3] == 1) {
    bands <- 1
  } else if (dim(raw_image)[3] == 2) {
    bands <- 1
  } else if (dim(raw_image)[3] == 3) {
    bands <- 3
  } else if (dim(raw_image)[3] == 4) {
    bands <- 3
  }

  # Create a stars object for RGB images
  if (bands == 3) {
    band_name <- c("R", "G", "B")
    stars_png <- mapply(read_png_as_stars,
                        seq_len(bands),
                        band_name,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(mtx = raw_image)
    )
    add <- function(x) Reduce(c, x)

    stars_png %>%
      add() %>%
      merge()  %>%
      stars::st_set_dimensions(names = c("x", "y", "band")) -> stars_png

    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- init_offset[1]
    attr_dim$y$offset <- init_offset[2]
    attr_dim$x$delta <- (init_offset[3] - init_offset[1]) / attr_dim$x$to
    attr_dim$y$delta <- (init_offset[4] - init_offset[2]) / attr_dim$y$to

    attr(stars_png, "dimensions") <- attr_dim
    sf::st_crs(stars_png) <- ee_crs
    if (isFALSE(raster)) {
      thumbnail_stars <- stars::st_as_stars(as(stars_png, "Raster"))
      names(thumbnail_stars) <- image_id
      thumbnail_stars <- stars::st_set_dimensions(
        .x = thumbnail_stars,
        which =  3,
        values = band_name
      )
      thumbnail_stars
    } else {
      thumbnail_raster <- as(stars_png, "Raster")
      names(thumbnail_raster) <- band_name
      thumbnail_raster
    }
  } else if (bands == 1) {
    # Create a stars object for single band image
    stars_png <- mapply(read_png_as_stars,
                        bands,
                        image_id,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(mtx = raw_image)
    )[[1]]
    stars_png <- stars::st_set_dimensions(.x = stars_png, names = c("x", "y"))

    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- init_offset[1]
    attr_dim$y$offset <- init_offset[2]
    attr_dim$x$delta <- (init_offset[3] - init_offset[1]) / attr_dim$x$to
    attr_dim$y$delta <- (init_offset[4] - init_offset[2]) / attr_dim$y$to
    attr(stars_png, "dimensions") <- attr_dim
    sf::st_crs(stars_png) <- ee_crs
    if (isFALSE(raster)) {
      thumbnail_stars <- stars_png %>%
        as("Raster") %>%
        stars::st_as_stars()
      names(thumbnail_stars) <- image_id
      thumbnail_stars
    } else {
      thumbnail_raster <- stars_png %>%
        as("Raster")
      names(thumbnail_raster) <- image_id
      thumbnail_raster
    }
  } else {
    stop("Number of bands not supported")
  }
}

#' From R array to stars
#' @noRd
read_png_as_stars <- function(x, band_name, mtx) {
  rotate <- function(x) t(apply(x, 2, rev))
  rotate_x <- rotate(mtx[, , x])
  dim_x <- dim(rotate_x)
  array_x <- array(NA, dim = c(dim_x[1], dim_x[2], 1))
  array_x[, , 1] <- rotate_x
  stars_object <- stars::st_as_stars(array_x)
  stars_object <- stars_object[, , , 1, drop = TRUE]
  names(stars_object) <- band_name
  stars_object
}


#' Check the visualization parameters
#' @noRd
ee_check_vizparam <- function(x) {
  list_names <- c(
    "bands", "min", "max", "gain", "bias", "gamma", "palette","opacity"
  )
  check_listnames <- names(x) %in% list_names
  if (any(!check_listnames)) {
    stop(
      "The following visualization parameters are not valid: ",
      paste(names(x[!check_listnames]), collapse = ", ")
    )
  }
}




#' Interface to search into the Earth Engine Data Catalog
#'
#' R functions for searching in Earth Engine's public data archive.
#'
#' @param quiet logical. Suppress info message
#' @param ee_search_dataset data.frame generated by rgee::ee_search_Datasets()
#' or a character which represents the EE dataset ID.
#' @param stardate Character. Start date of dataset availability.
#' @param enddate Character. End date of dataset availability.
#' @param provider Character. Name of the dataset's provider. See
#' ee_search_provider_list()
#' @param type Character. "Image", "ImageCollection" or a "table".
#' @param ... Character vector. tags
#' @param logical_operator Character. Available just for rgee::ee_search_tags
#' and rgee::ee_search_title. 'AND' represents inclusiveness between tags in
#' searching and 'OR' exclusiveness.
#' @param upgrade Logical. If the dataset needs to be upgraded.
#' @param maxdisplay Numeric. Maximum number of tabs to display in their browser
#' @param path_dataset Path of the dataset. By default it will loaded
#' automatically.
#' @name ee_search-tools
#' @return A data.frame where rows represents public data archive.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' # ee_search_provider_list()
#' # ee_search_title_list()
#' myquery <- ee_search_dataset() %>%
#'   ee_search_type("Image") %>%
#'   ee_search_provider("WWF") %>%
#'   ee_search_tags("srtm", "flow", "direction", "dem") %>%
#'   ee_search_title("15", "Flow", logical_operator = "AND") %>%
#'   ee_search_display()
#' }
#' @export
ee_search_dataset <- function(quiet = FALSE,
                              upgrade = FALSE,
                              path_dataset = NULL) {
  message_deprecated <- c(
    "\"ee_search_dataset\" will not be available for rgee version 1.0.8 ."
  )
  .Deprecated("ee_search_dataset", msg = message_deprecated)

  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  ee_search_dataset_file <- sprintf(
    "%s/ee_search_dataset.csv",
    ee_path
  )
  if (file.exists(ee_search_dataset_file) & !upgrade) {
    ee_search_dataset <- read.csv(ee_search_dataset_file,
                                  stringsAsFactors = FALSE)
  } else {
    if (is.null(path_dataset)) {
      user_samapriya <- "https://raw.githubusercontent.com/csaybar/"
      ee_template <- "%sEarth-Engine-Datasets-List/master/%s"
      ee_search_dataset_uri <- sprintf(ee_template, user_samapriya,
                                       find_eedataset())
    } else {
      ee_search_dataset_uri <- path_dataset
    }
    ee_search_dataset <- read.csv(ee_search_dataset_uri,
                                  stringsAsFactors = FALSE)
    if (!quiet) {
      cat("Downloading(Upgrading) the Earth Engine catalog ... please wait\n")
    }
    write.csv(
      x = ee_search_dataset,
      file = ee_search_dataset_file,
      row.names = FALSE
    )
  }
  return(ee_search_dataset)
}

#' @name ee_search-tools
#' @export
ee_search_startdate <- function(ee_search_dataset, stardate) {
  message_deprecated <- c(
    "\"ee_search_startdate\" will not be available for rgee version 1.0.8 ."
  )
  .Deprecated("ee_search_startdate", msg = message_deprecated)

  m <- gregexpr("[\\w']+", ee_search_dataset$start_date, perl = TRUE)
  ee_start_date <- ee_search_dataset$start_date %>%
    regmatches(m) %>%
    lapply(fix_date)
  m <- do.call(c, m)
  stardate <- as.Date(stardate)
  ee_search_dataset_q <- ee_search_dataset[which(ee_start_date > stardate), ]
  rownames(ee_search_dataset_q) <- NULL
  return(ee_search_dataset_q)
}

#' @name ee_search-tools
#' @export
ee_search_enddate <- function(ee_search_dataset, enddate = Sys.Date()) {
  message_deprecated <- c(
    "\"ee_search_enddate\" will not be available for rgee version 1.0.8 ."
  )
  .Deprecated("ee_search_enddate", msg = message_deprecated)
  m <- gregexpr("[\\w']+", ee_search_dataset$end_date, perl = TRUE)
  ee_end_date <- ee_search_dataset$end_date %>%
    regmatches(m) %>%
    lapply(fix_date)
  m <- do.call(c, m)
  enddate <- as.Date(enddate)
  ee_search_dataset_q <- ee_search_dataset[which(ee_end_date < enddate), ]
  rownames(ee_search_dataset_q) <- NULL
  return(ee_search_dataset_q)
}

#' @name ee_search-tools
#' @export
ee_search_type <- function(ee_search_dataset, type) {
  message_deprecated <- c(
    "\"ee_search_type\" will not be available for rgee version 1.0.8 ."
  )
  .Deprecated("ee_search_type", msg = message_deprecated)
  ee_search_dataset_type <- tolower(ee_search_dataset$type)
  type <- tolower(type)
  if (type %in% unique(ee_search_dataset_type)) {
    ee_search_dataset_q <- ee_search_dataset[ee_search_dataset_type %in% type, ]
    rownames(ee_search_dataset_q) <- NULL
    return(ee_search_dataset_q)
  } else {
    stop("type argument is not valid")
  }
}

#' @name ee_search-tools
#' @export
ee_search_provider <- function(ee_search_dataset, provider) {
  message_deprecated <- c(
    "\"ee_search_provider\" will not be available for rgee version 1.0.8 ."
  )
  .Deprecated("ee_search_provider", msg = message_deprecated)
  if (provider %in% unique(ee_search_dataset$provider)) {
    condition <- ee_search_dataset$provider %in% provider
    ee_search_dataset_q <- ee_search_dataset[condition,]
    rownames(ee_search_dataset_q) <- NULL
    return(ee_search_dataset_q)
  } else {
    stop("provider argument is not valid")
  }
}

#' @name ee_search-tools
#' @export
ee_search_provider_list <- function(ee_search_dataset) {
  message_deprecated <- c(
    "\"ee_search_provider_list\" will not be available for rgee version 1.0.8 ."
  )
  .Deprecated("ee_search_provider_list", msg = message_deprecated)
  return(unique(ee_search_dataset$provider))
}

#' @name ee_search-tools
#' @export
ee_search_tags <- function(ee_search_dataset, ..., logical_operator = "OR") {
  message_deprecated <- c(
    "\"ee_search_tags\" will not be available for rgee version 1.0.8 ."
  )
  .Deprecated("ee_search_tags", msg = message_deprecated)
  tags <- tolower(c(...))
  ee_tags <- tolower(ee_search_dataset$tags)
  if (logical_operator == "OR") {
    cond <- mapply(function(x) grepl(x, ee_tags), tags) %>% apply(1, any)
  } else if (logical_operator == "AND") {
    cond <- mapply(function(x) grepl(x, ee_tags), tags) %>% apply(1, all)
  } else {
    stop("logical_operator argument is not valid")
  }
  ee_search_dataset_q <- ee_search_dataset[cond, ]
  rownames(ee_search_dataset_q) <- NULL
  return(ee_search_dataset_q)
}

#' @name ee_search-tools
#' @export
ee_search_title <- function(ee_search_dataset, ..., logical_operator = "OR") {
  message_deprecated <- c(
    "\"ee_search_title\" will not be available for rgee version 1.0.8."
  )
  .Deprecated("ee_search_title", msg = message_deprecated)
  tags <- tolower(c(...))
  ee_title <- tolower(ee_search_dataset$title)
  if (logical_operator == "OR") {
    cond <- mapply(function(x) grepl(x, ee_title), tags) %>% apply(1, any)
  } else if (logical_operator == "AND") {
    cond <- mapply(function(x) grepl(x, ee_title), tags) %>% apply(1, all)
  } else {
    stop("logical_operator argument is not valid")
  }
  ee_search_dataset_q <- ee_search_dataset[cond, ]
  rownames(ee_search_dataset_q) <- NULL
  return(ee_search_dataset_q)
}


#' @name ee_search-tools
#' @export
ee_search_tagstitle <- function(ee_search_dataset, ...,
                                logical_operator = "OR") {
  message_deprecated <- c(
    "\"ee_search_tagstitle\" will not be available for rgee version 1.0.8."
  )
  .Deprecated("ee_search_tagstitle", msg = message_deprecated)

  tags <- tolower(c(...))
  ee_title <- tolower(ee_search_dataset$title)
  ee_tags <- tolower(ee_search_dataset$tags)
  if (logical_operator == "OR") {
    cond_1 <- mapply(function(x) grepl(x, ee_title), tags) %>% apply(1, any)
    cond_2 <- mapply(function(x) grepl(x, ee_tags), tags) %>% apply(1, any)
    cond_3 <- mapply(any, cond_1, cond_2)
  } else if (logical_operator == "AND") {
    cond_1 <- mapply(function(x) grepl(x, ee_title), tags) %>% apply(1, all)
    cond_2 <- mapply(function(x) grepl(x, ee_tags), tags) %>% apply(1, all)
    cond_3 <- mapply(any, cond_1, cond_2)
  } else {
    stop("logical_operator argument is not valid")
  }
  ee_search_dataset_q <- ee_search_dataset[cond_3, ]
  rownames(ee_search_dataset_q) <- NULL
  return(ee_search_dataset_q)
}

#' @name ee_search-tools
#' @export
ee_search_title_list <- function(ee_search_dataset) {
  message_deprecated <- c(
    "\"ee_search_title_list\" will not be available for rgee version 1.0.8."
  )
  .Deprecated("ee_search_title_list", msg = message_deprecated)
  return(unique(ee_search_dataset$provider))
}

#' Change the date format
#' @noRd
fix_date <- function(x) {
  month <- x[1]
  day <- x[2]
  year <- x[3]
  if (nchar(year) == 2 & as.integer(year) > 50) {
    year <- 1900 + as.integer(year)
  } else if (nchar(year) == 2 & as.integer(year) <= 50) {
    year <- 2000 + as.integer(year)
  } else {
    year <- as.integer(year)
  }
  final_date <- as.Date(sprintf("%s-%s-%s", year, month, day))
  return(final_date)
}

#' @name ee_search-tools
#' @export
ee_search_display <- function(ee_search_dataset, maxdisplay = 10) {
  message_deprecated <- c(
    "\"ee_search_title_list\" will not be available for rgee version 1.0.8.",
    " Use ee_utils_search_display instead."
  )
  .Deprecated("ee_search_title_list", msg = message_deprecated)

  if (is.character(ee_search_dataset)) {
    tag_name <- gsub("\\/", "_", ee_search_dataset)
  } else {
    tag_name <- gsub("\\/", "_", ee_search_dataset$id)
  }
  db_catalog <- "https://developers.google.com/earth-engine/datasets/catalog/"
  catalog_uri <- paste0(db_catalog, tag_name) %>%
    "["(1:maxdisplay) %>%
    na.omit() %>%
    as.character()
  for (uri in catalog_uri) {
    browseURL(uri)
  }
  invisible(TRUE)
}

#' Find the EE Dataset List on GitHub
#' @noRd
find_eedataset <- function() {
  message_deprecated <- c(
    "\"ee_search_title_list\" will not be available for rgee version 1.0.8."
  )
  .Deprecated("ee_search_title_list", msg = message_deprecated)

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("package httr required, please install it first")
  }
  git_repo <- "https://api.github.com/repos/csaybar/Earth-Engine-Datasets-List"
  req <- httr::GET(sprintf("%s/git/trees/master?recursive=1", git_repo))
  httr::stop_for_status(req)
  filelist <- lapply(httr::content(req)$tree, "[", "path")
  filelist <- unlist(filelist, use.names = FALSE)
  filelist[grepl("eed", filelist)]
}
