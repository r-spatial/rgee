#' Upload sf or stars objects to GEE asset
#'
#' Send either images or vectors to Google Earth Engine asset through Selenium
#'
#' @param x sf or stars object.
#' @param gmail_account character; google account name (gmail address).
#' @param gee_asset_path character; images destination path, full path for upload to Google Earth Engine,
#' e.g. users/pinkiepie/myponycollection.
#' @param start_time start period of acquiring data.
#' @param end_time end period of acquiring data.
#' @param properties data.frame; \href{https://developers.google.com/earth-engine/image_manifest}{Manifest upload}.
#' See details
#' @param pyramiding_policy character; determines how each pixel at a given level of the pyramid is
#' computed from the aggregation. "MEAN" (default), "MODE" and "SAMPLE".
#' @param quiet logical; suppress info message
#' @param type TODO
#' @param one_by_one TODO
#' @param ... ignored
#' @importFrom methods is as
#' @examples
#' \dontrun{
#' library(rgee)
#' }
#' @export
ee_upload_toasset <- function(x,
                              gmail_account,
                              gee_asset_path,
                              type = "Float32",
                              start_time=NULL,
                              end_time=NULL,
                              properties = NULL,
                              pyramiding_policy = "MEAN",
                              one_by_one = TRUE,
                              quiet=FALSE, ...) {
  UseMethod("ee_upload_toasset")
}

#' @name ee_upload_toasset
#' @export
ee_upload_toasset.default <- function(x, ...) {
  stop("x should be a sf or stars object")
}



#' @name ee_upload_toasset
#' @export
ee_upload_toasset.sf <- function(x,
                                 gmail_account,
                                 gee_asset_path,
                                 type = "Float32",
                                 start_time=NULL,
                                 end_time=NULL,
                                 properties = NULL,
                                 pyramiding_policy = "MEAN",
                                 one_by_one = TRUE,
                                 quiet=FALSE, ...) {
  print('TODO')
}

#' @name ee_upload_toasset
#' @export
ee_upload_toasset.stars <- function(x,
                                    gmail_account,
                                    gee_asset_path,
                                    type = "Float32",
                                    start_time=NULL,
                                    end_time=NULL,
                                    properties = NULL,
                                    pyramiding_policy = "MEAN",
                                    one_by_one = TRUE,
                                    quiet=FALSE, ...) {

  # Is the destination path available in asset?
  destination_path <- verify_path_for_upload(gee_asset_path)

  # Saving cookies of https://code.earthengine.google.com/ into a requests.Session() object
  session <- ee_getsession(gmail_account = gmail_account, quiet = quiet)

  tempdir_gee <- tempdir()

  for (ii in 1:length(x)) {
    # Save the stars object as a GEOTIFF file
    if (is(x,'stars_proxy')) {
      filename <- x[[ii]]
    } else {
      filename <- sprintf("%s/%s.tif",tempdir_gee,gsub("\\..+","",names(x[ii])))
      suppressWarnings(write_stars(x[i],filename,type=type))
    }

    # Uploading stars object to a gs://earthengine-uploads/
    filename_img <- gsub("\\..+","",basename(filename))
    asset_full_path <- sprintf("%s/%s",destination_path, filename_img)
    if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n",names(x)[ii]))
    gsid <- ee_file_to_gcs(session, filename)

    # Creating manisfest and uploading to gee asset
    manifest <- ee_create_manifest(x = x,
                                   asset_id = asset_full_path,
                                   uris = gsid,
                                   start_time = start_time,
                                   end_time = end_time,
                                   properties = properties,
                                   pyramiding_policy = pyramiding_policy)

    json_path <- sprintf("%s/manifest.json",tempdir_gee)
    ee_create_json(manifest,json_path)
    system(sprintf("earthengine upload image --manifest '%s'",json_path))
    if(one_by_one) ee_monitoring()
  }
}

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
  google_prop_n <- c("description","provider_url","tags","title")
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

#' Passing of data_frame to list(json)
#' @noRd
metadata_tojson <- function(metadata) {
  flat_metadata <- c(as.vector(metadata %>% as.matrix),colnames(metadata))
  isok <- all(mapply(allowed_property_key,flat_metadata)) # all elements are ok?
  if (ncol(metadata)>1) {
    json_metadata <- lapply(1:nrow(metadata),function(x) as.list(metadata[x,]))
  } else {
    json_metadata <- list(list(metadata[[1]]))
    names(json_metadata[[1]]) <- names(metadata)
  }
  names(json_metadata) <- metadata[,1]
  return(json_metadata)
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
    py$geckodown_linux(directory,version)
  } else if(os_type == "Windows") {
    py$geckodown_win(directory,version)
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
ee_file_to_gcs <- function(session, filename){
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_file_to_gcs_py(session, filename)
}

#' Compile the manifest (*.JSON) using import json
#' @noRd
ee_create_json <- function(manifest, path) {
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_create_json_py(towrite = path,manifest = manifest)
}

#' Create the *.JSON (manifest) using import json
#' @noRd
#' @importFrom rgdal showWKT
ee_create_manifest <- function(x,
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
