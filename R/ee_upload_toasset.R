#' Upload to the asset
#' @noRd
#' @importFrom getPass getPass
#'
ee_upload <- function(user,filename,destination_path,metadata,nodatavalue) {
  submitted_tasks_id = list()
  verify_path_for_upload(destination_path)
  if (!file.exists(filename)) stop("filename does not exist")

  metadata_json <- metadata_tojson(metadata)
  print(sprintf("gmail account: %s",user))
  password <- getPass("gmail password:")
  #google_session  #__get_google_auth_session(user, password)
  driverdir = dirname(gd_cre_path())
  driverdir
  ee_get_google_auth_session(user, password, dirname)
}


ee_upload_toasset <- function() {
}

ee_upload_toasset.default <- function() {
}


ee_upload_toasset.character <- function() {
}

ee_upload_toasset.dataframe <- function() {
}


ee_upload_toasset.Spatial <- function() {
}

ee_upload_toasset.Raster <- function() {
}


ee_upload_toasset.sf <- function() {
}


ee_upload_toasset.sfc <- function() {
}

ee_upload_toasset.sfg <- function() {
}


ee_upload_toasset.stars <- function() {
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
}

#' Is a property key?
#' @noRd
allowed_property_key <- function(prop){
  google_prop_n <- c("description","provider_url","tags","time_end","time_start","title")
  google_special_properties <- sprintf('system:%s',google_prop)
  if (any(prop == google_special_properties) | grepl("^[A-Za-z0-9_]+$",prop)) {
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
  flat_metadata <- as.vector(metadata %>% as.matrix)
  col_names <- all(mapply(allowed_property_key,colnames(flat_metadata))) # all elements are ok?
  json_metadata <- lapply(1:nrow(metadata),function(x) as.list(metadata[x,]))
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
