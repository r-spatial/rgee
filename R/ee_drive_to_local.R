#' @noRd
ee_drive_to_local <- function(task,
         dsn,
         overwrite = TRUE,
         consider = TRUE,
         public = FALSE,
         metadata = FALSE,
         quiet = FALSE) {

  # Check packages
  ee_check_packages("ee_drive_to_local", "googledrive")

  # Check credentials

  ee_user <- ee_exist_credentials()

  if (is.na(ee_user[["drive_cre"]])& is.na(ee_user[["drive_cred_path"]])) {
    drive_credential <- ee_create_credentials_drive(ee_user$email)
    ee_save_credential(pdrive = drive_credential)
    message(
      "Google Drive credentials were not loaded.",
      " Running ee_Initialize(user = '", ee_user[["email"]], "', drive = TRUE)",
      " to fix."
    )
  }

  # global parameter of a task
  gd_folder <- basename(ee$batch$Task$status(task)[["destination_uris"]])
  gd_ExportOptions <- task[["config"]][["fileExportOptions"]]
  gd_filename <- gd_ExportOptions[["driveDestination"]][["filenamePrefix"]]

  # Select a google drive file considering the filename and folder
  count <- 1
  files_gd <- try(googledrive::drive_find(
    q = sprintf("'%s' in parents", gd_folder),
    q = sprintf("name contains '%s'", gd_filename)
  ), silent = TRUE)

  while (any(class(files_gd) %in% "try-error") & count < 5) {
    files_gd <- try(googledrive::drive_find(
      q = sprintf("'%s' in parents", gd_folder),
      q = sprintf("name contains '%s'", gd_filename)
    ), silent = TRUE)
    count <- count + 1
  }

  if (public) {
    googledrive::with_drive_quiet({
      files_gd <- googledrive::drive_share_anyone(
        file = files_gd
      )
    })
  }

  # (Problem) Google Drive support files with the same name
  if (nrow(files_gd) > 0) {
    ee_getTime <- function(x) {
      gd_file_date <- files_gd[["drive_resource"]][[x]][["createdTime"]]
      as.POSIXct(gd_file_date)
    }
    createdTime <- vapply(seq_len(nrow(files_gd)), ee_getTime, 0)
    files_gd <- files_gd[order(createdTime, decreasing = TRUE), ]
    if (isTRUE(consider)) {
      choices <- c(files_gd[["name"]],'last','all')
      if (nrow(files_gd) == 1) {
        file_selected <- 1
      } else {
        file_selected <- menu(
          choices = choices,
          title = paste0(
            "Multiple files with the same name",
            " (sorted according to the created time argument):"
          )
        )
      }
      if (choices[file_selected] == 'last') {
        files_gd <- files_gd[1,]
      } else if (choices[file_selected] == 'all') {
        files_gd <- files_gd
      } else {
        files_gd <- files_gd[file_selected, ]
      }
    } else if (consider == "last") {
      files_gd <- files_gd[1, ]
    } else if (consider == "all") {
      files_gd <- files_gd
    } else {
      stop("consider argument was not defined properly.")
    }
  } else {
    stop(
      "File does not exist in Google Drive.",
      " Please verify if the task finished properly."
    )
  }

  # Choose the right file using the driver_resource["originalFilename"]
  fileformat <- toupper(gd_ExportOptions[["fileFormat"]])

  if (missing(dsn)) {
    ee_tempdir <- tempdir()
    filenames_local <- sprintf("%s/%s", ee_tempdir, basename(files_gd$name))
  } else {
    pattern <- "(.*)(\\..*)$"
    element_len <- length(files_gd$name)
    # Neccesary for large GEOTIFF and TFRecord files
    if (task$task_type == "EXPORT_IMAGE" & element_len > 1) {
      file_ft <- sprintf(
        "-%04d%s",
        seq_len(element_len),
        sub(pattern, "\\2", files_gd$name)
      )
    } else {
      file_ft <- sub(pattern, "\\2", files_gd$name)
    }
    dsn_n <- sub(pattern,"\\1",basename(dsn))
    filenames_local <- sprintf("%s/%s%s",dirname(dsn), dsn_n, file_ft)
  }
  # it is necessary for ESRI shapefiles
  filenames_local <- ee_sort_localfiles(filenames_local, fileformat)
  to_download <- sort_drive_files(files_gd, fileformat)

  # if (nrow(to_download) > 4) {
  #   stop(
  #     "Impossible to download multiple geometries as SHP.",
  #     " Try to define the fileFormat argument as GEO_JSON"
  #   )
  # }
  for (index in seq_len(nrow(to_download))) {
    if (!quiet) {
      googledrive::with_drive_quiet({
        googledrive::drive_download(
          file = to_download[index, ],
          path = filenames_local[index],
          overwrite = overwrite
        )
      })
    } else {
      googledrive::drive_download(
        file = to_download[index, ],
        path = filenames_local[index],
        overwrite = overwrite
      )
    }
  }

  if (metadata) {
    list(
      dsn = filenames_local,
      metadata = list(
        ee_id = task$id,
        drive_name = to_download$name,
        drive_id = to_download$id,
        drive_download_link = sprintf(
          "https://drive.google.com/uc?id=%s&export=download",
          to_download$id)
      )
    )
  } else {
    filenames_local
  }
}
