#' Interface for manage the Earth Engine Asset
#'
#' R functions for managing the Earth Engine Asset. The interface allows users
#' to create and eliminate folders, move and copy assets, set and delete
#' properties, handle access control lists, and manage and/or cancel tasks.
#'
#' @name ee_manage-tools
#' @param path_asset Character. Name of the EE asset (Table, Image, Folder or
#' ImageCollection).
#' @param asset_type Character. The asset type to create ('Folder' or
#' 'ImageCollection').
#' @param final_path Character. Output filename
#' (e.g users/datacolecfbf/ic_moved)
#' @param add_properties List. Set of parameters to established as a property
#' of an EE object. See details.
#' @param del_properties Character. Names of properties to be deleted. See
#' details.
#' @param editor Character vector. Define editor users in the IAM Policy.
#' @param viewer Character vector. Define viewer users in the IAM Policy.
#' @param all_users_can_read Logical. All users can see the asset element.
#' @param cache Logical. If TRUE, the task report will be saved
#' in the /temp directory and used when the function .
#' @param quiet Logical. Suppress info message.
#' @importFrom stats na.omit
#' @importFrom utils write.csv read.csv
#' @details
#' If the argument `del_properties` is 'ALL',
#' \link[=rgee]{ee_manage_delete_properties} will delete all
#' the properties.
#' @author Samapriya Roy, adapted to R by csaybar.
#' @examples
#' \donttest{
#'
#' library(rgee)
#'
#' ee_Initialize()
#' ee_user_info()
#'
#' # Change datacolecfbf by your EE user to be able to reproduce
#'
#' # 1. Create a folder or Image Collection
#' # Change path asset according to your specific user
#' ee_manage_create("users/datacolecfbf/rgee")
#'
#' # 1. List all the elements inside a folder or a ImageCollection
#' ee_manage_assetlist(path_asset = "users/datacolecfbf/rgee")
#'
#' # 2. Create a Folder or a ImageCollection
#' ee_manage_create(
#'   path_asset = "users/datacolecfbf/rgee/rgee_folder",
#'   asset_type = "Folder"
#' )
#'
#' ee_manage_create(
#'   path_asset = "users/datacolecfbf/rgee/rgee_ic",
#'   asset_type = "ImageCollection"
#' )
#'
#' ee_manage_assetlist(path_asset = "users/datacolecfbf/rgee")
#'
#' # 3. Shows Earth Engine quota
#' ee_manage_quota()
#'
#' # 4. Move an EE object to another folder
#' ee_manage_move(
#'   path_asset = "users/datacolecfbf/rgee/rgee_ic",
#'   final_path = "users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved"
#' )
#'
#' ee_manage_assetlist(path_asset = "users/datacolecfbf/rgee/rgee_folder")
#'
#' # 5. Set properties to an EE object.
#' ee_manage_set_properties(
#'   path_asset = "users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved",
#'   add_properties = list(message = "hello-world", language = "R")
#' )
#'
#' ic_id <- "users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved"
#' test_ic <- ee$ImageCollection(ic_id)
#' test_ic$getInfo()
#'
#' # 6. Delete properties
#' ee_manage_delete_properties(
#'   path_asset = "users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved",
#'   del_properties = c("message", "language")
#' )
#' test_ic$getInfo()
#'
#' # 7. Create a report based on all the tasks
#' # that are running or have already been completed.
#' ee_manage_task()
#'
#' # 8. Cancel all the running task
#' ee_manage_cancel_all_running_task()
#'
#' # 9. Delete EE objects or folders
#' ee_manage_delete("users/datacolecfbf/rgee/")
#' }
#' @export
ee_manage_create <- function(path_asset, asset_type = "Folder", quiet = FALSE) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())

  # Is the same EE user?
  user <- read.table(file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
                     header = TRUE,
                     stringsAsFactors = FALSE)

  # Get the user path from path_asset
  folders <- strsplit(path_asset,"/")[[1]][1:2]
  path_asset_root_folder <- sprintf("%s/%s",folders[1],folders[2])

  # path_asset is identical to the path from ee_get_assethome()?
  if (!identical(path_asset_root_folder, user$user)) {
    stop('The root folder "',path_asset_root_folder,'" is invalid')
  }

  # Fix typos in path_asset
  path_asset <- ee_verify_filename(path_asset, strict = FALSE)
  asset_path_exist <- is.null(ee$data$getInfo(path_asset))

  if (asset_path_exist) {
    if (asset_type == "Folder") {
      new_path <- path_asset
      # repeat (do-while) is necessary for dealing nested folder
      repeat {
        # It is possible create an asset folder?
        nested_folder <- try(
          ee$data$createAsset(
            list(type = ee$data$ASSET_TYPE_FOLDER),
            new_path), silent = TRUE
        )
        #  If nested_folder class is "try-error", try the path_asset dirname.
        #            path_asset                            new_path
        #   users/datacolecfbf/cs/cs/rgee   --->  users/datacolecfbf/cs/rgee
        if (class(nested_folder) == "try-error") {
          new_path <- dirname(new_path)
        } else {
          if (!quiet) cat("GEE asset:", new_path, "created\n")
          # If new_path == path_asset break repeat
          if (identical(new_path, path_asset)) {
            break
          }
          #            path_asset                            new_path
          # users/datacolecfbf/cs/cs/rgee   <---  users/datacolecfbf/cs/rgee
          new_path <- path_asset
        }
      }
    } else if (asset_type == "ImageCollection") {
      # Attempt to create a ImageCollection
      create_ic_asset <- try(
        expr = ee$data$createAsset(
          value = list(type = ee$data$ASSET_TYPE_IMAGE_COLL),
          opt_path = path_asset
        ), silent = TRUE
      )
      # If the Folder where the ImageCollection
      # will be saved does not exist, create it!
      if (class(create_ic_asset) == "try-error") {
        dir_path <- dirname(path_asset)
        # Creating the folder
        ee_manage_create(
          path_asset = dir_path,
          asset_type = "Folder",
          quiet = quiet
        )
        # Creating the ImageCollection
        ee_manage_create(
          path_asset = path_asset,
          asset_type = "ImageCollection",
          quiet = quiet
        )
      }
    } else {
      stop("Invalid asset_type parameter")
    }
  } else {
    if (!quiet) cat("GEE asset:", path_asset, "already exists\n")
  }
  invisible(TRUE)
}


#' @name ee_manage-tools
#' @export
ee_manage_delete <- function(path_asset, quiet = FALSE) {
  # path_asset exist?
  path_asset <- ee_verify_filename(path_asset, strict = TRUE)
  response <- ee$data$getInfo(path_asset)
  if (is.null(response)) stop("path_asset does not exist!")

  # If is a nested Folder or an ImageCollection,
  # firstly remove upper elements
  if (response$type %in% ee_manage_handle_names()) {
    list_files <- ee$data$getList(list(id = path_asset))
    items <- unlist(lapply(list_files, "[[", "id")) %>%
      ee_remove_project_chr()
    mapply(ee_manage_delete, items)
  }
  # Remove the empty folder or ImageCollection
  ee$data$deleteAsset(path_asset)
  if (!quiet) cat("EE object deleted:", path_asset, "\n")
  invisible(TRUE)
}

#' @name ee_manage-tools
#' @export
ee_manage_assetlist <- function(path_asset, quiet = FALSE) {
  if (missing(path_asset)) {
    stop("path_asset was not specified")
  }
  # path_asset exist?
  path_asset <- ee_verify_filename(path_asset, strict = TRUE)
  response <- ee$data$getInfo(path_asset)
  if (is.null(response)) stop("path_asset does not exist!")

  # Get the ID and CLASS from all the elements of path_asset
  list_files <- ee$data$getList(list(id = path_asset))
  ids <- unlist(lapply(list_files, "[[", "id")) %>%
    ee_remove_project_chr()
  type <- unlist(lapply(list_files, "[[", "type"))

  # Handle results and present it in a df
  df_path <- data.frame(
    ID = ids,
    TYPE = type,
    stringsAsFactors = FALSE
  )

  if (nrow(df_path) != 0L) {
    df_path <- rbind(
      df_path[df_path$TYPE %in% ee_manage_handle_names("ImageCollection"), ],
      df_path[df_path$TYPE %in% ee_manage_handle_names("Folder"), ],
      df_path[df_path$TYPE %in% ee_manage_handle_names("Image"), ],
      df_path[df_path$TYPE %in% ee_manage_handle_names("Table"), ]
    )
  }
  return(df_path)
}

#' @name ee_manage-tools
#' @export
ee_manage_quota <- function(quiet = FALSE) {
  oauth_func_path <- system.file("python/ee_manage.py", package = "rgee")
  ee_quota <- ee_source_python(oauth_func_path)
  ID <- ee$data$getAssetRoots()[[1]]$id %>%
    ee_remove_project_chr()
  quota <- ee_utils_py_to_r(ee_quota$quota(ID))
  total_msg <- ee_humansize(as.numeric(quota[1]))
  used_msg <- ee_humansize(as.numeric(quota[2]))
  if (!quiet) {
    cat(sprintf(" Total Quota: %s \n Used Quota: %s", total_msg, used_msg))
  }
  invisible(quota)
}

#' @name ee_manage-tools
#' @export
ee_manage_copy <- function(path_asset, final_path, quiet = FALSE) {
  # path_asset exist?
  path_asset <- ee_verify_filename(path_asset, strict = TRUE)
  # Fix typos in final_path
  final_path <- ee_verify_filename(final_path, strict = FALSE)

  # Is a EE active asset, a Folder or ImageCollection?
  header <- ee$data$getInfo(path_asset)[["type"]]
  eeasset_objects <- c("Image", "Table", "FeatureCollection")

  if (header %in% ee_manage_handle_names(eeasset_objects)) {
    # Images and Tables
    ee$data$copyAsset(path_asset, final_path)
    if (!quiet) cat("Done\n")
  } else if (header %in% ee_manage_handle_names("ImageCollection")) {
    # List all the images inside the ImageCollection
    to_copy_list <- ee$data$getList(params = list(id = path_asset)) %>%
      lapply("[[", "id") %>%
      unlist() %>%
      ee_remove_project_chr()

    # Create an empty ImageCollection
    ee_manage_create(
      path_asset = final_path,
      asset_type = "ImageCollection"
    )

    if (!quiet) {
      cat(
        "Copying a total of", length(to_copy_list),
        " elements ..... please wait\n"
      )
    }

    # Copy each Image to the empty ImageCollection
    folder_destination <- sprintf("%s/%s", final_path, basename(to_copy_list))
    for (z in seq_along(to_copy_list)) {
      ee$data$copyAsset(to_copy_list[z], folder_destination[z])
    }
    if (!quiet) cat("Done\n")
  } else if (header %in% ee_manage_handle_names("Folder")) {
    # List all the elements inside the Folder
    to_copy_list <- ee$data$getList(params = list(id = path_asset)) %>%
      lapply("[[", "id") %>%
      unlist() %>%
      ee_remove_project_chr()

    # Create an empty Folder
    ee_manage_create(
      path_asset = final_path,
      asset_type = "Folder"
    )

    if (!quiet) {
      cat(
        "Copying a total of", length(to_copy_list),
        " elements ..... please wait\n"
      )
    }

    # Copy each asset active to the empty Folder
    folder_destination <- sprintf("%s/%s", final_path, basename(to_copy_list))
    for (z in seq_along(to_copy_list)) {
      ee$data$copyAsset(to_copy_list[z], folder_destination[z])
    }
    if (!quiet) cat("Done\n")
  } else {
    stop("Unsupported EE asset object")
  }
  invisible(TRUE)
}

#' @name ee_manage-tools
#' @export
ee_manage_move <- function(path_asset, final_path, quiet = FALSE) {
  # path_asset exist?
  path_asset <- ee_verify_filename(path_asset, strict = TRUE)
  # Fix typos in final_path
  final_path <- ee_verify_filename(final_path, strict = FALSE)

  # Is a Folder or ImageCollection?
  header <- ee$data$getInfo(path_asset)[["type"]]
  eeasset_objects <- c("Image", "Table", "FeatureCollection")

  if (header %in% ee_manage_handle_names(eeasset_objects)) {
    ee$data$renameAsset(path_asset, final_path)
    if (!quiet) cat("Done\n")
  } else if (header %in% ee_manage_handle_names("ImageCollection")) {
    # List all the images inside the ImageCollection
    to_copy_list <- ee$data$getList(params = list(id = path_asset)) %>%
      lapply("[[", "id") %>%
      unlist() %>%
      ee_remove_project_chr()
    # Create an empty ImageCollection
    ee_manage_create(
      path_asset = final_path,
      asset_type = "ImageCollection"
    )

    if (!quiet) {
      cat(
        "Copying a total of", length(to_copy_list),
        " elements ..... please wait\n"
      )
    }

    # Copy each Image to the empty ImageCollection
    folder_destination <- sprintf("%s/%s", final_path, basename(to_copy_list))
    for (z in seq_along(to_copy_list)) {
      ee$data$renameAsset(to_copy_list[z], folder_destination[z])
    }
    ee_manage_delete(path_asset, quiet = quiet)
    if (!quiet) cat("Done\n")

  } else if (header %in% ee_manage_handle_names("Folder")) {
    # List all the elements inside the Folder
    to_copy_list <- ee$data$getList(params = list(id = path_asset)) %>%
      lapply("[[", "id") %>%
      unlist() %>%
      ee_remove_project_chr()
    # Create an empty Folder
    ee_manage_create(
      path_asset = final_path,
      asset_type = "Folder"
    )

    if (!quiet) {
      cat(
        "Copying a total of", length(to_copy_list),
        " elements ..... please wait\n"
      )
    }

    # Copy each asset active to the empty Folder
    folder_destination <- sprintf("%s/%s", final_path, basename(to_copy_list))
    for (z in seq_along(to_copy_list)) {
      ee$data$renameAsset(to_copy_list[z], folder_destination[z])
    }
    if (!quiet) cat("Done\n")
  } else {
    stop("Unsupported EE asset object")
  }
  invisible(TRUE)
}

#' @name ee_manage-tools
#' @export
ee_manage_set_properties <- function(path_asset, add_properties) {
  # path_asset exist?
  path_asset <- ee_verify_filename(path_asset, strict = TRUE)

  # Is a Folder?
  header <- ee$data$getInfo(path_asset)[["type"]]
  eeasset_objects <- c("Image", "ImageCollection", "FeatureCollection", "Table")

  if (header %in% ee_manage_handle_names(eeasset_objects)) {
    ee$data$setAssetProperties(path_asset, add_properties)
  } else {
    stop("Impossible assign properties to a Folder")
  }
  invisible(TRUE)
}

#' @name ee_manage-tools
#' @export
ee_manage_delete_properties <- function(path_asset,
                                        del_properties = "ALL") {
  # path_asset exist?
  path_asset <- ee_verify_filename(path_asset, strict = TRUE)

  # Is a Folder?
  header <- ee$data$getInfo(path_asset)[["type"]]
  eeasset_objects <- c("Image", "ImageCollection", "FeatureCollection", "Table")

  if (header %in% ee_manage_handle_names(eeasset_objects)) {
    if ("ALL" %in%  del_properties) {
      properties_todelete <- names(ee$data$getAsset(path_asset)$properties)
    } else {
      properties_todelete <- del_properties
    }
    del_list <- list()
    del_list[properties_todelete] <- list(NULL)
    ee$data$setAssetProperties(path_asset, del_list)
  } else {
    stop("Impossible delete properties to a Folder")
  }
  invisible(TRUE)
}

#' @name ee_manage-tools
#' @export
ee_manage_asset_access <- function(path_asset,
                                   editor = NULL,
                                   viewer = NULL,
                                   all_users_can_read = TRUE,
                                   quiet = FALSE) {
  bindings_template <- getOption('rgee.manage.setIamPolicy')
  bindings_template$bindings[[2]]$members <- paste0('user:', editor)
  if (isTRUE(all_users_can_read)) {
    bindings_template$bindings[[3]]$members <- c(viewer,'allUsers')
  } else {
    bindings_template$bindings[[3]]$members <- paste0('user:', viewer)
  }

  # Error arise when users use ee_Initialize without email argument.
  # Unfortunately, email argument needs to be in the form of
  # ee_Initialize(email = "xxx@gmail.com") if it is not realize the
  # code will not work. Enhance in future versions of rgee.
  tryCatch(
    expr = ee$data$setIamPolicy(path_asset, bindings_template),
    error =  function(e) stop(
    "Please run again ee_Initialize specifying",
    " the full address of your email",
    " (e.g. ee_Initialize(email = data.colec.fbf@gmail.com))"
    )
  )
  invisible(TRUE)
}

#' @name ee_manage-tools
#' @export
ee_manage_task <- function(cache = FALSE) {
  ee_temp <- tempdir()
  # Load the ee_manage python module
  oauth_func_path <- system.file("python/ee_manage.py", package = "rgee")
  ee_manage_py <- ee_source_python(oauth_func_path)
  manage_task_file <- sprintf("%s/ee_manage_task_file.csv", ee_temp)

  if (!isTRUE(cache)) {
    py_names <- c(
      "tid", "tstate", "tdesc", "ttype", "tcreate",
      "tdiffstart", "tdiffend", "error_message"
    )
    df_names <- c(
      "ID", "State", "DestinationPath", "Type", "Start",
      "DeltaToCreate(s)", "DeltaToCompletedTask(s)", "ErrorMessage"
    )
    status <- ee_utils_py_to_r(ee_manage_py$genreport())
    if (length(status) == 0) {
      message("No recent task to report")
      df_order <- data.frame(message = "No recent task to report")
      write.csv(df_order, manage_task_file, row.names = FALSE)
      return(invisible(df_order))
    }
    order_name <- names(status[[1]])
    df_status <- data.frame(
      matrix(unlist(status), nrow = length(status), byrow = TRUE),
      stringsAsFactors = FALSE
    )
    colnames(df_status) <- order_name
    df_order <- df_status[py_names]
    colnames(df_order) <- df_names
    df_order$DestinationPath <- sub(".*:\\s", "", df_order$DestinationPath)
    write.csv(df_order, manage_task_file, row.names = FALSE)
  } else {
    df_order <- read.csv(manage_task_file, stringsAsFactors = FALSE)
  }
  return(df_order)
}

#' @name ee_manage-tools
#' @export
ee_manage_cancel_all_running_task <- function() {
  # Retrieves a list of RUNNING and READY tasks.
  all_task <- ee$data$getTaskList()
  all_task_state <- unlist(lapply(all_task, "[[", "state"))
  running_ready_tasks <- all_task_state  %in%  c("RUNNING" , "READY")

  if (any(running_ready_tasks)) {
    stop("There are no tasks running")
  } else {
    running <- all_task[which(running_ready_tasks)]
    for (z in seq_along(running)) {
      ee$data$cancelTask(running[[z]][["id"]])
    }
  }
  invisible(TRUE)
}

#' Verify is the EE path asset is correct
#' @noRd
ee_verify_filename <- function(path_asset, strict = TRUE) {
  ee_path_dirname <- gsub("\\..+$", "", path_asset)
  m <- gregexpr("[\\w']+", ee_path_dirname, perl = TRUE)
  folder <- ee_path_dirname %>%
    regmatches(m) %>%
    "[["(1) %>%
    paste(collapse = "/") %>%
    ee_remove_project_chr()
  response <- ee$data$getInfo(folder)
  if (is.null(response) & strict) {
    message <- c(
      "%s is not a valid destination.",
      " Make sure a correct full path is provided",
      " (e.g. either users/user/nameofcollection",
      " or projects/myproject/myfolder/newcollection).")
    stop(sprintf(message, path_asset))
  }
  return(folder)
}

#' Change the unit of measurement of bytes
#' @param x Integer. Number of bytes.
#' @return Number of bytes in a more human-comprehensible way
#' @noRd
ee_humansize <- function(x, suffixes = c("B", "KB", "MB", "GB", "TB", "PB")) {
  count <- 0
  while (x >= 1024 & (count < length(suffixes) - 1)) {
    x <- x / 1024
    count <- count + 1
  }
  if (suffixes[count + 1] == "B") {
    sprintf("%s %s", sprintf("%s", x), suffixes[count + 1])
  } else {
    sprintf("%s %s", sprintf("%.2f", x), suffixes[count + 1])
  }
}


#' Remove EE projects info
#' @param x Character (path_asset)
#' @noRd
ee_remove_project_chr <- function(x) {
  new_x <- gsub("projects/earthengine/legacy/assets/", "", x)
  gsub("projects/earthengine-legacy/assets/", "", new_x)
}

#' EE asset object type
#'
#' The earth engine API constantly change of name to the EE assets
#' object. This function was created to help to handle it.
#'
#' @param type EE asset object to consider
#' @noRd
ee_manage_handle_names <- function(type = c("Folder", "ImageCollection")) {
  names <- NULL
  if ("Folder" %in% type) {
    names <- c(names, c("FOLDER", "Folder", "folder"))
  }
  if ("Image" %in% type) {
    names <- c(names, c("IMAGE", "Image", "image"))
  }
  if ("ImageCollection" %in% type) {
    names <- c(
      names,
      c("ImageCollection", "IMAGE_COLLECTION", "imagecollection")
    )
  }
  if ("Feature" %in% type) {
    names <- c(names, c("feature", "Feature", "FEATURE"))
  }
  if ("FeatureCollection" %in% type) {
    names <- c(
      names, c("featurecollection", "FeatureCollection", "FEATURE_COLLECTION")
    )
  }
  if ("Table" %in% type) {
    names <- c(names, c("table", "TABLE", "Table"))
  }
  names
}

#' @name ee_manage-tools
#' @export
ee_manage_asset_size <- function(path_asset, quiet = FALSE) {
  info_data <- ee$data$getInfo(path_asset)
  size_file <- as.numeric(info_data$sizeBytes)
  if (!quiet) {
    cat('Type            :', info_data$type,'\n')
    cat('Size (in Bytes) :', size_file)
  }
  invisible(size_file)
}
