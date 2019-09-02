#' Is x a vector or a raster?
#' @noRd
image_or_vector <- function(x) {
  isvector <- try(read_sf(x),silent = T)
  if (any(class(isvector) %in% 'try-error')) {
    israster <- try(read_stars(x),silent = T)
    if (any(class(israster) %in% 'try-error')) {
      return(NULL)
    }
    return("stars")
  }
  return("sf")
}

#' Pass a foreign Image format to GEOTIFF
#' @param gee_asset_file filename in google earth engine asset
#' @noRd
ee_raster_to_tif <- function(x) {
  #is_a_tif <- mapply(grepl, x ,MoreArgs = list(pattern = "\\.tif$"))
  tif <- grepl(pattern = "\\.tif$",x)
  if (tif) {
    return(x)
  } else {
    if (!requireNamespace('raster', quietly = TRUE)) {
      stop('The image (', x, ') is not a GEOTIFF file, The raster package',
           ' is required to fix it internally.',call. = FALSE)
    } else {
      tif_file <- tempfile()
      newtif <- paste0(tif_file,".tif")
      raster::writeRaster(raster::raster(x), newtif)
      return(newtif)
    }
  }
}

#' Pass a foreign vector format to ESRI shapefile
#' @param gee_asset_file filename in google earth engine asset
#' @noRd
ee_vector_to_shapefile <- function(x) {
  #is_a_tif <- mapply(grepl, x ,MoreArgs = list(pattern = "\\.shp$"))
  shp <- grepl(pattern = "\\.shp$",x)
  if (shp) {
    return(x)
  } else {
    shp_file <- tempfile()
    newshp <- paste0(tif_file,".shp")
    write_sf(read_sf(x), newshp)
    return(newshp)
  }
}

#' Create a zip file from a ESRI shapefile
#' @param x .shp fullname
#' @param SHP_EXTENSIONS are suffix: c("dbf", "prj", "shp", "shx")
#' @noRd
create_shp_zip <- function(x, SHP_EXTENSIONS = c("dbf", "prj", "shp", "shx")){
  temp_dir <- tempdir()
  shp_basename <- gsub("\\.shp$","",x)
  shp_filenames <- sprintf("%s.%s", shp_basename, SHP_EXTENSIONS)
  zipname <- sprintf("%s.zip", shp_basename)
  setwd(dirname(zipname))
  zip(zipfile = zipname, files = basename(shp_filenames))
  temp_zip <- sprintf("%s/%s",temp_dir,basename(zipname))
  if (getwd() != dirname(zipname)) {
    file.copy(from = zipname, to = temp_zip, overwrite = TRUE)
    file.remove(zipname)
  }
  return(temp_zip)
}

#' Upload local files to google cloud storage
#'
#' Upload images or tables into google cloud storage for EE asset ingestion tasks.
#'
#' @param x 	filename (character), sf or stars object.
#' @param bucket bucketname you are uploading to
#' @param selenium_params Optional parameters when bucket is NULL. See details
#' @param quiet logical; suppress info message
#' @importFrom getPass getPass
#' @details laalla
#' @return
#' \itemize {
#'  \item upload_url: URL for the new :class:`Request` object.
#'  \item destination_path: Is the destination path in the earth engine asset.
#'  \item asset_filename: Filename (inside earth engine)
#' }
#' @export
ee_upload_file_to_gcs <- function(x,
                                  bucket = NULL,
                                  selenium_params = getOption("rgee.selenium.params"),
                                  quiet = FALSE) {
  if (image_or_vector(x) == "sf") {
    x %>%
      ee_vector_to_shapefile() %>%
      create_shp_zip -> x
    x_type = 'shapefile'
  } else {
    x <- ee_raster_to_tif(x)
    x_type = 'tif'
  }

  if (is.null(bucket)) {
    oauth_func_path <- system.file("python/ee_selenium_functions.py", package = "rgee")
    ee_selenium_functions <- rgee:::ee_source_python(oauth_func_path)

    tempdir_gee <- tempdir()
    session_temp <- sprintf("%s/rgee_session_by_selenium.Rdata", tempdir_gee)

    # Geeting cookies from https://code.earthengine.google.com/
    if (file.exists(session_temp) & cache == cache) {
      session <- ee_selenium_functions$load_py_object(session_temp)
    } else {
      if (!quiet) cat(sprintf("GMAIL ACCOUNT: %s\n", gmail_account))
      password <- getPass("GMAIL PASSWORD:")
      if (!quiet) {
        if (!showpassword) {
          cat("GMAIL PASSWORD:",paste(rep("*",nchar(password)),collapse = ""),"\n")
        } else {
          cat("GMAIL PASSWORD:",password,"\n")
        }
      }
      if (!quiet) cat("Acquiring uploading permissions ... please wait\n")
      session <- ee_selenium_functions$ee_get_google_auth_session_py(username = gmail_account,
                                                                     password = password,
                                                                     dirname =  ee_get_earthengine_path())
      cookies_names = names(ee_py_to_r(session$cookies$get_dict()))
      if (!quiet) cat(sprintf("cookies catched: [%s]",paste0(cookies_names,collapse = ", ")))
      if (ncookies>7) {
        ee_selenium_functions$save_py_object(session, session_temp)
      } else {
        warnings("The number of cookies is suspiciously low.")
      }
    }
    expected_cookies_name <- c("APISID", "CONSENT", "HSID", "NID", "SACSID",
                               "SAPISID", "SID", "SIDCC", "SSID")
    # Geting URL ingestion
    upload_url <- tryCatch(expr = {
      upload_url <- ee_py_to_r(ee_selenium_functions$ee_get_upload_url_py(session))
      count <- 1
      while (is.null(upload_url) & count < 5) {
        upload_url <- ee_py_to_r(ee_selenium_functions$ee_get_upload_url_py(session))
        count <- count + 1
      }
      if (is.null(upload_url)) {
        stop("Maybe due slow internet connection or a wrong google account password",
             "Expected cookies names need similar to this: \n [",
             paste0(expected_cookies_name,collapse = ", "),"]")
      } else {
        if (nchar(upload_url) > 500) {
          stop("Maybe due slow internet connection or a wrong google account password",
               "Expected cookies names need similar to this: \n [",
               paste0(expected_cookies_name,collapse = ", "),"]")
        }
      }
      upload_url
    }, error = function(e) {
      message("Error: Cleaning cache ... , was not possible get the URL to upload the data",
              ", run rgee::ee_upload_file_to_gcs again.")
      file.remove(session_temp)
    })

    if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", x))
    gcs_uri <- ee_selenium_functions$ee_file_to_gcs_py(session, x, x_type, upload_url)
    return(gcs_uri)
  } else {
    if (!requireNamespace('googleCloudStorageR', quietly = TRUE)) {
      stop('The googleCloudStorageR package is required to use rgee::ee_download_gcs',
           call. = FALSE)
    } else {
      ee_path <- path.expand("~/.config/earthengine")
      user <- read.table(sprintf("%s/rgee_sessioninfo.txt",ee_path),header = TRUE)[['user']]
      ee_Initialize(user = paste0(user,"@gmail.com"),gcs = TRUE)
      gcs_global_bucket(bucket = bucket)
      gcs_auth(getOption("rgee.gcs.auth"))
      setwd(dirname(x))
      gcs_upload(x, name = basename(x))
      gcs_uri <- sprintf("gs://%s/%s", bucket, basename(x))
      return(gcs_uri)
    }
  }
}

#' Load info for an asset, given an asset id.
#' @noRd
ee_verify_filename <- function(filename) {
  filename <- gsub("\\..+$","",filename)
  ee_path_dirname <- dirname(filename)
  m <- gregexpr("[\\w']+", ee_path_dirname, perl=TRUE)
  folder <- ee_path_dirname %>%
    regmatches(m) %>%
    '[['(1) %>%
    paste(collapse = "/")
  response <- ee$data$getInfo(folder)
  if (is.null(response)) {
    message <- c("%s is not a valid destination.",
                 "Make sure full path is provided e.g. users/user/nameofcollection",
                 'or projects/myproject/myfolder/newcollection and that you have',
                 "write access there.")
    stop(sprintf(message,asset_id))
  }
  ee_filename <- sprintf("%s/%s",folder, basename(filename))
  return(ee_filename)
}

#' Create the *.JSON (manifest) using import json (0.1.189)
#' @noRd
#' @importFrom rgdal showWKT
ee_create_manifest_future <- function(x,
                                      asset_id,
                                      uris,
                                      start_time=NULL,
                                      end_time=NULL,
                                      properties = NULL,
                                      pyramiding_policy = "MEAN") {

  affine_transform <- attr(x,"dimensions")
  shear <- x %>% attr("dimensions") %>% attr("raster")
  nbands <- (affine_transform$band$to - affine_transform$band$from) +1L
  if (length(nbands)==0) nbands <- 1
  band_names <- affine_transform$band$values
  if (is.null(band_names)) band_names <- sprintf("b%s",1:nbands)

  name <- sprintf("projects/earthengine-legacy/assets/%s",asset_id)
  tilesets <- list(
    crs = showWKT(st_crs(x)$proj4string),
    sources = list(
      list(
        uris=uris,
        affine_transform = list(
          scale_x = affine_transform$x$delta,
          shear_x = shear$affine[1],
          translate_x = affine_transform$x$offset,
          shear_y =  shear$affine[2],
          scale_y = affine_transform$y$delta,
          translate_y = affine_transform$y$offset
        )
      )
    )
  )

  bands <- list()
  for (b in 1:length(band_names)) {
    bands[[b]] <- list(id=band_names[b],tileset_band_index=as.integer((b-1)))
  }

  if (is.null(end_time)) end_time <- 0L
  if (is.null(start_time)) start_time <- 0L

  manifest <- list(
    name = name,
    tilesets = list(tilesets),
    bands = bands,
    pyramiding_policy = pyramiding_policy,
    end_time=list(seconds=end_time),
    start_time=list(seconds=start_time),
    properties = properties)

  if (is.null(properties)) manifest[['properties']] = NULL
  if (is.null(start_time)) manifest[['start_time']] = NULL
  if (is.null(end_time)) manifest[['end_time']] = NULL
  return(manifest)
}

#' Pass a file of gcs to ee asset
#' @noRd
ee_gcs_to_asset  <- function(gs_uri,filename, type = 'table' ,properties=NULL) {
  oauth_func_path <- system.file("python/ee_selenium_functions.py", package = "rgee")
  ee_selenium_functions <- rgee:::ee_source_python(oauth_func_path)
  tempdir_gee <- tempdir()

  if (!is.null(properties[['time_start']])) {
    properties[['time_start']] <- as.numeric(py$ee_Date_value(properties[['time_start']]))
    names(properties)[which(names(properties) %in% 'time_start')]  <- 'system:time_start'
  }
  if (!is.null(properties[['time_end']])) {
    properties[['time_end']] <- as.numeric(py$ee_Date_value(properties[['time_end']]))
    names(properties)[which(names(properties) %in% 'time_end')]  <- 'system:time_end'
  }
  if (!is.null(properties[['missingData']])) {
    missingData <- properties[['missingData']]
    properties[['missingData']] <- NULL
  }

  if (type == 'image') {
    main_payload <- list(id=filename,
                         tilesets = list(
                           list(
                             sources = list(
                               list(
                                 primaryPath= gs_uri,
                                 additionalPaths = list()
                               )
                             )
                           )
                         ),
                         properties = properties)

    main_payload[['missingData']] <- list(value = missingData)

    json_path <- sprintf("%s/manifest.json", tempdir_gee)
    ee_selenium_functions$ee_create_json_py(towrite = json_path,manifest = main_payload)
    system(sprintf("earthengine upload image --manifest '%s'", json_path))
  } else if(type == 'table') {
    system(sprintf("earthengine upload table --asset_id %s '%s'", filename, gs_uri))
  }
}

#' Create a folder or ImageCollection into GEE assets (DEPRECATED)
#' @param path_asset a character vector containing a single path name.
#' @param asset_type a character vector containing the asset type. 'folder' or 'imagecollection'.
#' @param quiet logical; suppress info message.
#' @export
ee_create_asset_path  <- function(path_asset, asset_type='folder',quiet=FALSE) {

  path_asset <- path_asset %>%
    str_extract_all(boundary("word"),simplify = TRUE) %>%
    paste(collapse = "/")

  asset_path_exist <- is.null(ee$data$getInfo(path_asset))
  if (asset_path_exist) {
    if (asset_type == 'folder') {
      ee$data$createAsset(list(type=ee$data$ASSET_TYPE_FOLDER), path_asset)
    } else if (asset_type == 'imagecollection') {
      ee$data$createAsset(list(type=ee$data$ASSET_TYPE_IMAGE_COLL), path_asset)
    } else {
      stop('Invalid asset_type parameter')
    }
    if (!quiet) cat('GEE asset:',path_asset,'created\n')
  }
  else {
    if (!quiet) cat("GEE asset:",path_asset,"already exists\n")
  }
}
