#' Upload sf or stars objects to GEE asset
#'
#' Send either images or vectors to Google Earth Engine asset through Selenium
#'
#' @param x sf or stars object.
#' @param gmail_account character; google account name (gmail address).
#' @param gee_asset_path character; images destination path, full path for upload to Google Earth Engine,
#' e.g. users/pinkiepie/myponycollection.
#' @param asset_filename asset filename
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
ee_upload_toasset <- function(x, ...) {
  UseMethod("ee_upload_toasset")
}

#' @name ee_upload_toasset
#' @export
ee_upload_toasset.character <- function(x, ...,
                                        gmail_account,
                                        gee_asset_path,
                                        properties = NULL,
                                        pyramiding_policy = "MEAN",
                                        one_by_one = TRUE,
                                        quiet = FALSE) {
  # Is the destination path available in asset?
  destination_path <- verify_path_for_upload(gee_asset_path)

  # Saving cookies of https://code.earthengine.google.com/ into a requests.Session() object
  session <- ee_getsession(gmail_account = gmail_account, quiet = quiet)
  upload_url <- ee_get_upload_url(session)
  extensions <- c("dbf", "prj", "shp", "shx")
  tempdir_gee <- tempdir()


  count <- 1
  for (eee in x) {
    if (image_or_vector(eee) == "sf") {

      # Creating zip file to upload
      asset_filename <- gsub("\\..+", "", basename(eee))
      filenames <- sprintf("%s.%s", asset_filename, extensions)
      zipname <- sprintf("%s/%s.zip", tempdir_gee, asset_filename)
      setwd(dirname(eee))
      zip(zipfile = zipname, files = filenames)

      # Uploading zip file to a gs://earthengine-uploads/
      asset_full_path <- sprintf("%s/%s", destination_path, asset_filename)
      if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", basename(eee)))
      gsid <- ee_file_to_gcs(session, zipname, "shapefile", upload_url)
      system(sprintf("earthengine upload table --asset_id %s  '%s'", asset_full_path, gsid))
      if (one_by_one) ee_monitoring()
    } else if (image_or_vector(eee) == "stars") {

      # Uploading stars object to a gs://earthengine-uploads/
      filename_img <- gsub("\\..+", "", basename(eee))
      asset_full_path <- sprintf("%s/%s", destination_path, basename(filename_img))
      if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", eee))
      gsid <- ee_file_to_gcs(session, eee, ftype = "tif", upload_url = upload_url)

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

#' @name ee_upload_toasset
#' @export
ee_upload_toasset.sf <- function(x, ...,
                                 gmail_account,
                                 asset_filename,
                                 gee_asset_path,
                                 quiet = FALSE) {
  # Is the destination path available in asset?
  destination_path <- verify_path_for_upload(gee_asset_path)

  # Saving cookies of https://code.earthengine.google.com/ into a requests.Session() object
  session <- ee_getsession(gmail_account = gmail_account, quiet = quiet)
  upload_url <- ee_get_upload_url(session)
  tempdir_gee <- tempdir()

  # Creating zip file to upload
  extensions <- c("dbf", "prj", "shp", "shx")
  shpname <- sprintf("%s/%s.shp", tempdir_gee, asset_filename)
  filenames <- sprintf("%s.%s", asset_filename, extensions)
  zipname <- sprintf("%s/%s.zip", tempdir_gee, asset_filename)
  suppressWarnings(write_sf(x, filename))
  setwd(tempdir_gee)
  zip(zipfile = zipname, files = filenames)


  # Uploading zip file to a gs://earthengine-uploads/
  asset_full_path <- sprintf("%s/%s", destination_path, asset_filename)
  if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", asset_filename))
  gsid <- ee_file_to_gcs(session, zipname, "shapefile", upload_url)
  system(sprintf("earthengine upload table --asset_id %s  '%s'", asset_full_path, gsid))
}

#' @name ee_upload_toasset
#' @export
ee_upload_toasset.stars <- function(x, ...,
                                    gmail_account,
                                    gee_asset_path,
                                    type = "Float32",
                                    properties = NULL,
                                    pyramiding_policy = "MEAN",
                                    one_by_one = TRUE,
                                    quiet = FALSE) {

  # Is the destination path available in asset?
  destination_path <- verify_path_for_upload(gee_asset_path)

  # Saving cookies of https://code.earthengine.google.com/ into a requests.Session() object
  session <- ee_getsession(gmail_account = gmail_account, quiet = quiet)
  upload_url <- ee_get_upload_url(session)
  tempdir_gee <- tempdir()

  for (i in 1:length(x)) {
    # Save the stars object as a GEOTIFF file
    filename <- sprintf("%s/%s.tif", tempdir_gee, gsub("\\..+", "", names(x[i])))
    suppressWarnings(write_stars(x[i], filename, type = type))

    # Uploading stars object to a gs://earthengine-uploads/
    filename_img <- gsub("\\..+", "", basename(filename))
    asset_full_path <- sprintf("%s/%s", destination_path, basename(filename_img))
    if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", names(x)[i]))
    gsid <- ee_file_to_gcs(session, filename, ftype = "tif", upload_url = upload_url)

    # Creating manisfest and uploading to gee asset
    manifest <- ee_create_manifest(
      x = x[i],
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

#' @name ee_upload_toasset
#' @export
ee_upload_toasset.stars_proxy <- function(x, ...,
                                          gmail_account,
                                          gee_asset_path,
                                          properties = NULL,
                                          pyramiding_policy = "MEAN",
                                          one_by_one = TRUE,
                                          quiet = FALSE) {
  filenames <- unlist(x)
  # Is the destination path available in asset?
  destination_path <- verify_path_for_upload(gee_asset_path)

  # Saving cookies of https://code.earthengine.google.com/ into a requests.Session() object
  session <- ee_getsession(gmail_account = gmail_account, quiet = quiet)
  upload_url <- ee_get_upload_url(session)
  tempdir_gee <- tempdir()

  for (i in 1:length(x)) {
    # Read each stars_proxy object
    x <- read_stars(filenames[i], proxy = T)
    filename <- x[[1]]

    # Uploading stars object to a gs://earthengine-uploads/
    filename_img <- gsub("\\..+", "", basename(filename))
    asset_full_path <- sprintf("%s/%s", destination_path, basename(filename_img))
    if (!quiet) cat(sprintf("Uploading %s to gs://earthengine-uploads/ \n", names(x)))
    gsid <- ee_file_to_gcs(
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
