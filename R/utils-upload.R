#' Load info for an asset, given an asset id.
#' @noRd
#' @importFrom stringr str_extract_all boundary
verify_path_for_upload <- function(asset_id){
  folder <- asset_id %>%
    str_extract_all(boundary("word"),simplify = TRUE) %>%
    paste(collapse = "/")
  response <- ee$data$getInfo(folder)
  if (is.null(response)) {
    message <- c("%s is not a valid destination.",
                 "Make sure full path is provided e.g. users/user/nameofcollection",
                 'or projects/myproject/myfolder/newcollection and that you have',
                 "write access there.")
    stop(sprintf(message,asset_id))
  }
  return(folder)
}

#' Is a property key?
#' @noRd
allowed_property_key <- function(prop){
  google_prop_n <- c("description","provider_url","tags","title","time_start","time_end")
  google_special_properties <- sprintf('system:%s',google_prop_n)
  if (any(prop == google_special_properties) | grepl("^[A-Za-z0-9_-]+$",prop)) {
    return(TRUE)
  } else {
    gsp_c <- paste(google_special_properties,collapse = ", ")
    stop(sprintf("Property name %s is invalid.", prop),
         sprintf(" Special properties [%s] are allowed; other property",gsp_c),
         " keys must contain only letters, digits and underscores.")
  }
}

#' Passing a data_frame row to list(json)
#' @noRd
dataframe_row_to_json <- function(drow) {
  if (is.null(drow)) {
    return(NULL)
  } else {
    flat_metadata <- c(as.vector(drow %>% as.matrix),colnames(drow))
    isok <- all(mapply(allowed_property_key,flat_metadata)) # all elements are ok?
    return(as.list(drow))
  }
}

#' Download geckover in their sytem
#' @noRd
ee_download_selenium_driver <- function(version='latest') {
  oauth_func_path <- system.file("Python/ee_download_geckover.py", package = "rgee")
  ee_source_python(oauth_func_path)
  if (version == 'latest') version = NULL
  directory = dirname(gd_cre_path())
  os_type = Sys.info()[['sysname']]
  if (os_type == "Linux") {
    geckodown_linux(directory,version)
  } else if(os_type == "Windows") {
    geckodown_win(directory,version)
  } else {
    cat("Experimental: Only windows and derived linux OS are soported")
    geckodown_linux(directory,version)
  }
}

#' Check is selenium-firefox is correctly installed
#' @noRd
ee_check_selenium_driver <- function(driverdir){
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  if (missing(driverdir)) {
    driverdir = dirname(gd_cre_path())
  }
  print(ee_check_selenium_firefox(driverdir))
}


#' Check is selenium-firefox is correctly installed
#' @noRd
ee_get_google_auth_session <- function(username, password,dirname, quiet=FALSE) {
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  if (!quiet) cat("Acquiring upload permissions ... please wait\n")
  session <- ee_get_google_auth_session_py(username, password,dirname)
  if (!quiet) cat("Permissions adquired\n")
  return(session)
}

#' Create a folder or ImageCollection into GEE assets
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



#' get the cookies of https://code.earthengine.google.com using Selenium
#' @noRd
#' @importFrom getPass getPass
ee_getsession <- function(gmail_account, quiet = FALSE) {
  if (!quiet) cat(sprintf("GMAIL ACCOUNT: %s\n", gmail_account))
  password <- getPass("GMAIL PASSWORD:")
  if (!quiet) cat("GMAIL PASSWORD:",paste(rep("*",nchar(password)),collapse = ""),"\n")
  driverdir <- dirname(gd_cre_path())
  google_session <- ee_get_google_auth_session(gmail_account, password, driverdir,quiet=quiet)
  return(google_session)
}

#' Pass your local filename to google cloud storages (gs://earthengine-uploads/*)
#' @noRd
#'
ee_file_to_gcs <- function(session, filename,ftype,upload_url){
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_file_to_gcs_py(session, filename,ftype,upload_url)
}

#' Check upload URL
#' @noRd
ee_get_upload_url <- function(session) {
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  upload_url <- ee_get_upload_url_py(session)
  if (is.null(upload_url)) {
    stop('upload_url error: Maybe due slow internet connection, try again!')
  } else {
    if (nchar(upload_url) > 500) {
      stop('upload_url error: Maybe due slow internet connection, try again!')
    }
    return(upload_url)
  }
}


#' Compile the manifest (*.JSON) using import json
#' @noRd
ee_create_json <- function(manifest, path) {
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_create_json_py(towrite = path,manifest = manifest)
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


#' Create the *.JSON (manifest) using import json (0.1.175)
#' @noRd
#' @importFrom rgdal showWKT
ee_create_manifest <- function(x,
                               asset_id,
                               uris,
                               properties = NULL,
                               pyramiding_policy = "MEAN") {

  main_payload <- list(id=asset_id,
                       tilesets = list(
                         list(
                           sources = list(
                             list(
                               primaryPath= uris,
                               additionalPaths = list()
                             )
                           )
                         )
                       ),
                       properties = properties)
  if (is.null(properties)) main_payload[['properties']] = list("system:time_start"=0L)
  return(main_payload)
}


#' Is x a vector or a raster? (deprecated)
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
