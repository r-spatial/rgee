#' Upload sf or stars objects to GEE asset
#'
#' Send either images or vectors to Google Earth Engine asset using Selenium
#'
#' @param x character, sf or stars object.
#' @param gmail_account character. Google account name.
#' @param gee_asset_path character. images destination path, full path for upload to Google Earth Engine,
#' e.g. users/pinkiepie/myponycollection.
#' @param asset_filename asset filename
#' @param properties data.frame; \href{https://developers.google.com/earth-engine/image_manifest}{Manifest upload}.
#' See details
#' @param pyramiding_policy character; determines how each pixel at a given level of the pyramid is
#' computed from the aggregation. "MEAN" (default), "MODE" and "SAMPLE".
#' @param quiet logical; suppress info message
#' @param type TODO
#' @param showpassword TODO
#' @param cache TODO
#' @param one_by_one TODO
#' @param ... ignored
#' @importFrom methods is as
#' @details  TALKING ABOUT SELENIUM
#' @examples
#' \dontrun{
#' library(rgee)
#' }
#' @export
ee_upload <- function(x, ...) {
  UseMethod("ee_upload")
}

#' @name ee_upload
#' @export
ee_upload.character <- function(x, ...,
                                asset_filenames,
                                properties = NULL,
                                pyramiding_policy = "MEAN",
                                one_by_one = TRUE,
                                bucket = NULL,
                                selenium_params = getOption("rgee.selenium.params"),
                                quiet = FALSE) {

  SHP_EXTENSIONS <- c("dbf", "prj", "shp", "shx")
  tempdir_gee <- tempdir()


  if (!length(asset_filenames) == length(x)) stop("asset_filenames and x needs to",
                                                  " have the same lenght")

  if (is.null(bucket)) {
    if (!quiet) cat("Creating a temporary gcs bucket using selenium \n")
    session_temp <- sprintf("%s/rgee_session_by_selenium.Rdata", tempdir_gee)
    # URL to upload the data
    ee_session <- rgee:::ee_get_session(gmail_account = selenium_params[['gmail_account']],
                                        session_temp =  session_temp,
                                        showpassword = selenium_params[['showpassword']],
                                        cache = selenium_params[['cache']],
                                        quiet = quiet)
  } else {
    if (!requireNamespace('googleCloudStorageR', quietly = TRUE)) {
      stop('The googleCloudStorageR package is required to use rgee::ee_download_gcs',
           call. = FALSE)
    } else {
      print('TODO')
    }
  }

  count <- 1
  for (z in seq_along(x)) {
    to_upload_file <- x[z]
    if (image_or_vector(to_upload_file) == "sf") {
      # Creating zip file to upload
      vector_fullname_in_local <- to_upload_file %>%
        ee_vector_to_shapefile %>%
        create_shp_zip(SHP_EXTENSIONS)
      vector_fullname_in_asset <- asset_filenames[z]
      # Uploading zip file to a gs://earthengine-uploads/
      if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", vector_fullname_in_local))
      gsid <- rgee:::ee_file_to_gcs(session, vector_fullname_in_local, "shapefile", upload_url)


      system(sprintf("earthengine upload table --asset_id %s  '%s'", asset_full_path, gsid))
      if (one_by_one) ee_monitoring()
      file.remove(vector_fullname_in_local)
    } else if (image_or_vector(eee) == "stars") {

      # Uploading stars object to a gs://earthengine-uploads/
      fullname_image_in_local <- ee_raster_to_tif(x[z])
      fullname_image_in_asset <- asset_filenames[z]
      upload_url <- tryCatch(expr =  rgee:::ee_get_upload_url(ee_session),
                             error = function(e) {
                               message('Error: Cleaning cache ... , was not possible get the URL to upload the data')
                               file.remove(session_temp)})

      if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", fullname_image_in_local))
      gsid <- rgee:::ee_file_to_gcs(session = ee_session,
                                    filename = fullname_image_in_local,
                                    ftype = "tif",
                                    upload_url = upload_url)

      # Creating manisfest and uploading to gee asset
      manifest <- ee_create_manifest(
        x = eee,
        asset_id = asset_full_path,
        uris = gsid,
        properties = dataframe_row_to_json(properties[count, ]),
        pyramiding_policy = pyramiding_policy
      )
      json_path <- sprintf("%s/manifest.json", tempdir_gee)
      ee_create_json(manifest, json_path)
      system(sprintf("earthengine upload image --manifest '%s'", json_path))
      if (one_by_one) ee_monitoring()
    } else {
      stop(sprintf("%s is neither a vector nor tif", eee))
    }
    count <- count + 1
  }
}

#' TODO: BUG in line 112 check: preliminarly I change filename by  filenames
#' @name ee_upload
#' @export
ee_upload.sf <- function(x, ...,
                                 gmail_account,
                                 gee_asset_file,
                                 cache = TRUE,
                                 quiet = FALSE) {


  oauth_func_path <- system.file("python/ee_selenium_functions.py", package = "rgee")
  ee_selenium_functions <- rgee:::ee_source_python(oauth_func_path)
  # Is the destination path available in asset?
  gee_asset_path <- dirname(gee_asset_file)
  asset_filename <- basename(gee_asset_file)

  destination_path <-  rgee:::verify_path_for_upload(gee_asset_path)
  # Saving cookies of https://code.earthengine.google.com/ into a requests.Session() object
  tempdir_gee <- tempdir()
  session_temp <- sprintf("%s/rgee_session_by_selenium.Rdata", tempdir_gee)
  if (file.exists(session_temp) & cache == TRUE) {
    ee_selenium_functions$load_py_object(session_temp)
  } else {
    session <- rgee:::ee_getsession(gmail_account = gmail_account, quiet = FALSE)
    ee_selenium_functions$save_py_object(session, session_temp)
  }
  upload_url <- tryCatch(expr =  rgee:::ee_get_upload_url(session),
                         error = function(e) file.remove(session_temp))

  # Creating zip file to upload
  extensions <- c("dbf", "prj", "shp", "shx")
  shpname <- sprintf("%s/%s.shp", tempdir_gee, asset_filename)
  filenames <- sprintf("%s.%s", asset_filename, extensions)
  zipname <- sprintf("%s/%s.zip", tempdir_gee, asset_filename)
  suppressWarnings(write_sf(x, filenames[3]))
  setwd(tempdir_gee)
  zip(zipfile = zipname, files = filenames)

  # Uploading zip file to a gs://earthengine-uploads/
  asset_full_path <- sprintf("%s/%s", destination_path, asset_filename)
  if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", asset_filename))
  gsid <- rgee:::ee_file_to_gcs(session, zipname, "shapefile", upload_url)
  system(sprintf("earthengine upload table --asset_id %s  '%s'", asset_full_path, gsid))
}

#' @name ee_upload
#' @export
ee_upload.stars <- function(x, ...,
                                    gmail_account,
                                    gee_asset_file,
                                    type = "Float32",
                                    properties = NULL,
                                    pyramiding_policy = "MEAN",
                                    one_by_one = TRUE,
                                    cache = TRUE,
                                    quiet = FALSE) {

  tempdir_gee <- tempdir()
  session_temp <- sprintf("%s/rgee_session_by_selenium.Rdata", tempdir_gee)
  essn_vars <- ee_generate_url(gee_asset_file, gmail_account)
  upload_url <- essn_vars$upload_url
  destination_path <- essn_vars$destination_path
  asset_filename <- essn_vars$asset_filename

  for (i in 1:length(x)) {
    # Save the stars object as a GEOTIFF file
    filename <- sprintf("%s/%s.tif", tempdir_gee, gsub("\\..+", "", names(x[i])))
    suppressWarnings(write_stars(x[i], filename, type = type))

    # Uploading stars object to a gs://earthengine-uploads/
    filename_img <- gsub("\\..+", "", basename(filename))
    asset_full_path <- sprintf("%s/%s", destination_path, basename(filename_img))
    if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", names(x)[i]))
    gsid <- rgee:::ee_file_to_gcs(session, filename, ftype = "tif", upload_url = upload_url)

    # Creating manisfest and uploading to gee asset
    manifest <- rgee:::ee_create_manifest(
      x = x[i],
      asset_id = asset_full_path,
      uris = gsid,
      properties = rgee:::dataframe_row_to_json(properties[i, ]),
      pyramiding_policy = pyramiding_policy
    )
    json_path <- sprintf("%s/manifest.json", tempdir_gee)
    rgee:::ee_create_json(manifest, json_path)
    system(sprintf("earthengine upload image --manifest '%s'", json_path))
    if (one_by_one) ee_monitoring()
  }
}

#' @name ee_upload
#' @export
ee_upload.stars_proxy <- function(x, ...,
                                          gmail_account,
                                          asset_filename,
                                          properties = NULL,
                                          pyramiding_policy = "MEAN",
                                          one_by_one = TRUE,
                                          cache = TRUE,
                                          quiet = FALSE) {
  tempdir_gee <- tempdir()
  session_temp <- sprintf("%s/rgee_session_by_selenium.Rdata", tempdir_gee)
  filenames <- unlist(x)
  # Is the destination path available in asset?
  destination_path <- verify_path_for_upload(gee_asset_path)

  # Saving cookies of https://code.earthengine.google.com/ into a requests.Session() object
  essn_vars <- ee_generate_url(gee_asset_file, gmail_account, session_temp)
  upload_url <- essn_vars$upload_url
  destination_path <- essn_vars$destination_path
  filename_img <- essn_vars$asset_filename

  for (i in 1:length(x)) {
    # Read each stars_proxy object
    x <- read_stars(filenames[i], proxy = T)
    filename <- x[[1]]

    # Uploading stars object to a gs://earthengine-uploads/
    asset_full_path <- gee_asset_file
    if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", names(x)))
    gsid <- rgee:::ee_file_to_gcs(
      session = session,
      filename = filename,
      ftype = "tif",
      upload_url = upload_url
    )

    # Creating manisfest and uploading to gee asset
    manifest <- ee_create_manifest(
      x = x,
      asset_id = asset_full_path,
      uris = gsid,
      properties = dataframe_row_to_json(properties[i, ]),
      pyramiding_policy = pyramiding_policy
    )

    json_path <- sprintf("%s/manifest.json", tempdir_gee)
    ee_create_json(manifest, json_path)
    system(sprintf("earthengine upload image --manifest '%s'", json_path))
    if (one_by_one) ee_monitoring()
  }
}



#' GET the cookies of https://code.earthengine.google.com using Selenium
#' @param gmail_account gmail account
#' @param session_temp session python object
#' @param showpassword Display google account password
#' @param cache If it is TRUE, session_temp is used.
#' @param quiet logical; suppress info message
#' @importFrom getPass getPass
#' @return
#' \itemize {
#'  \item upload_url: URL for the new :class:`Request` object.
#'  \item destination_path: Is the destination path in the earth engine asset.
#'  \item asset_filename: Filename (inside earth engine)
#' }
#' @noRd
ee_get_session <- function(gmail_account,session_temp, showpassword,  cache, quiet) {
  oauth_func_path <- system.file("python/ee_selenium_functions.py", package = "rgee")
  ee_selenium_functions <- rgee:::ee_source_python(oauth_func_path)

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

    session <- rgee:::ee_get_google_auth_session(username = gmail_account,
                                          password =  password,
                                          dirname = ee_get_earthengine_path(),
                                          quiet=quiet)
    ncookies <- length(ee_py_to_r(session$cookies$get_dict()))
    if (ncookies>10) ee_selenium_functions$save_py_object(session, session_temp)
  }
  return(session)
}

