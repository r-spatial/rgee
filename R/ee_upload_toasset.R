#' Upload a either an image or vector into Google Earth Engine using Selenium
#' @author Cesar Aybar
#' @export
ee_upload_toasset <- function(x,user,destination_path,metadata,nodata) {
  UseMethod("ee_upload_toasset")
}

ee_upload_toasset.default <- function(x,user,destination_path,metadata,nodata) {
  stop("x should be either an Image or a vector")
}


ee_upload_toasset.sf <- function() {
}


ee_upload_toasset.sfc <- function() {
}

ee_upload_toasset.sfg <- function() {
}

ee_upload_toasset.stars <- function(x,user,destination_path,metadata,nodata) {



  #gsid = __upload_file_gee(session=google_session,
  #                         file_path='amarakaeri')
}



#' Load info for an asset, given an asset id.
#' @importFrom stringr str_extract_all boundary
#' @noRd
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
  google_prop_n <- c("description","provider_url","tags","time_end","time_start","title")
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
ee_get_google_auth_session <- function(username, password,dirname) {
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  cat("Getting upload authorization through Selenium ... please wait\n")
  session <- ee_get_google_auth_session_py(username, password,dirname)
  cat("Done!")
  return(session)
}

#' Check the GEE asset
#' @noRd
ee_create_asset_path  <- function(destination_path) {
  asset_path_exist <- is.null(ee$data$getInfo(destination_path))
  if (asset_path_exist) {
    ee$data$createAsset(list(type=ee$data$ASSET_TYPE_FOLDER), destination_path)
    cat('GEE asset:',destination_path,'created')
  }
  else {
    cat("GEE asset:",destination_path,"already exists, connecting ...")
  }
}

#' get the cookies of https://code.earthengine.google.com using Selenium
#' @noRd
#' @importFrom getPass getPass
#' @export
ee_getsession <- function(user,filename,destination_path) {
  if (!file.exists(filename)) stop("filename does not exist")
  cat(sprintf("GMAIL ACCOUNT: %s\n",user))
  password <- getPass("GMAIL PASSWORD:")
  cat("GMAIL PASSWORD:",paste(rep("*",nchar(password)),collapse = ""),"\n")
  driverdir <- dirname(gd_cre_path())
  google_session <- ee_get_google_auth_session(user, password, driverdir)
  return(google_session)
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
    return("raster")
  }
  return("vector")
}

#' Return a gs://earthengine-uploads/* file
#' @noRd
#' @export
ee_upload_file <- function(session, filename){
  oauth_func_path <- system.file("Python/ee_selenium_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_upload_file_py(session, filename)
}

#'  Compile the manifest (*.JSON) using import json
ee_create_json <- function(manifest, path) {
  py$ee_create_json(towrite = path,manifest = manifest)
}

#'  Create the *.JSON (manifest) using import json
#' @importFrom rgdal showWKT
ee_create_manifest <- function(x,
                               asset_id,
                               uris,
                               start_time=NULL,
                               end_time=NULL,
                               properties = NULL,
                               pyramiding_policy = "MEAN"
) {

  affine_transform <- attr(x,"dimensions")
  shear <- x %>% attr("dimensions") %>% attr("raster")
  nbands <- (affine_transform$band$to - affine_transform$band$from) +1
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
    start_time=list(seconds=start_time)
    )

  if (is.null(properties)) {
    return(manifest)
  } else {
    manifest$properties = properties
    return(manifest)
  }
}



#' Upload a Image to the asset
#' @importFrom stars write_stars
#' @export
ee_upload_istars_to_asset <- function(x, type, user,destination_path,metadata) {
  destination_path <- verify_path_for_upload(destination_path)

  upload_tempdir <- tempdir()
  filename <- sprintf("%s/%s.tif",upload_tempdir,gsub("\\..+","",names(x)))
  suppressWarnings(write_stars(x,filename,type=type))
  session <- ee_getsession(user = user,filename = filename,destination_path = destination_path)
  ee_create_asset_path(destination_path)
  filename_img <- gsub("\\..+","",basename(filename))
  asset_full_path <- sprintf("%s/%s",destination_path, filename_img)
  cat(sprintf("Uploading %s to gs://earthengine-uploads/..",names(x)))
  gsid <- ee_upload_file(session, filename)
  manifest <- ee_create_manifest(x,asset_full_path,gsid)
  json_path <- sprintf("%s/manifest.json",upload_tempdir)
  ee_create_json(manifest,json_path)
  system(sprintf("earthengine upload image --manifest '%s'",json_path))
}
