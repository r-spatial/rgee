#' Interface for manage the Earth Engine Asset
#'
#' R functions for managing the Earth Engine Asset
#'
#' @name ee_manage-tools
#' @param path_asset Character. Name of the EE asset e.g. users/user/nameofcollection.
#' @param asset_type Character. The asset type ('folder' or 'imagecollection').
#' @param quiet Logical. Suppress info message.
#' @param final_path Character. Name of the final EE asset to copy or move.
#' @param properties List. Set of parameters to established as a property of an EE object. See details.
#' @param property Character. Name of a specific property to be deleted.
#' @param cache Logical. TRUE save the report in the /temp directory.
#' @param acl  A list describing the asset's Access Control List. See getOption("rgee.manage.getAssetAcl").
#' @importFrom stats na.omit
#' @importFrom utils write.csv read.csv
#'
#' @examples
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # Change google account to be able to reproduce
#' ee_manage_create('users/aybar1994/rgee')
#'
#' # 1. List all the elements inside a folder or a ImageCollection
#' ee_manage_assetlist(path_asset = 'users/aybar1994/rgee')
#'
#' # 2. Create a Folder or a ImageCollection
#' ee_manage_create('users/aybar1994/rgee/rgee_folder',asset_type = 'folder')
#' ee_manage_create('users/aybar1994/rgee/rgee_ic',asset_type = 'imagecollection')
#' ee_manage_assetlist('users/aybar1994/rgee')
#'
#' # 3. Shows your Earth Engine quota
#v ee_manage_quota()
#'
#' # 4. Estimate the size of a Image, ImageCollection, Table or Folder.
#' ee_manage_size('users/aybar1994/rgee')
#'
#' # 5. Move a EE object to another folder
#' ee_manage_move(path_asset = 'users/aybar1994/rgee/rgee_ic',
#'                final_path = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
#' ee_manage_assetlist('users/aybar1994/rgee/rgee_folder')
#'
#' # 6. Set properties to an EE object.
#' ee_manage_set_properties(path_asset = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved',
#'                          properties = list(message='hello-world',language = 'R'))
#' test_ic <- ee$ImageCollection('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
#' test_ic$getInfo()
#'
#' # 7. Delete properties
#' ee_manage_delete_properties(path_asset = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved',
#'                             property = c("message","language"))
#' test_ic$getInfo()
#'
#' # 8. Share EE objects -- Create a public dataset
#' ee_manage_assets_access('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
#' ee$data$getAssetAcl('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
#'
#' # 9. Create a report based on all tasks that is running or has finished
#' ee_manage_task()
#'
#' # 10. Cancel all the running task
#' ee_manage_cancel_all_running_taks()
#'
#' # 11. Delete EE objects or folders
#' ee_manage_delete('users/aybar1994/rgee/')
#' @export
ee_manage_create = function(path_asset, asset_type='folder',quiet=FALSE) {
  asset_type = tolower(asset_type)
  path_asset = ee_verify_filename(path_asset,strict = FALSE)
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

#' @name ee_manage-tools
#' @export
ee_manage_delete = function(path_asset, quiet=FALSE) {
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  response = ee$data$getInfo(path_asset)
  if (is.null(response)) stop("path_asset does not exist!")
  if (response$type %in% c('Folder','ImageCollection')) {
    list_files = ee$data$getList(list(id=path_asset))
    items = unlist(lapply(list_files, '[[','id'))
    mapply(ee_manage_delete, items)
  }
  ee$data$deleteAsset(path_asset)
  if (!quiet) cat('EE object deleted:',path_asset,'\n')
}

#' @name ee_manage-tools
#' @export
ee_manage_assetlist = function(path_asset, quiet=FALSE) {
  if (missing(path_asset)) path_asset = ee$data$getAssetRoots()[[1]]$id
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  response = ee$data$getInfo(path_asset)
  if (is.null(response)) stop("path_asset does not exist!")
  list_files = ee$data$getList(list(id=path_asset))
  ids = unlist(lapply(list_files, '[[',1))
  type = unlist(lapply(list_files, '[[',2))
  df_path = data.frame(ID = ids, TYPE = type,stringsAsFactors = FALSE)
  df_path = rbind(df_path[df_path$TYPE == 'ImageCollection',],
                  df_path[df_path$TYPE == 'Folder',],
                  df_path[df_path$TYPE == 'Image',],
                  df_path[df_path$TYPE == 'Table',])
  return(df_path)
}


#' @name ee_manage-tools
#' @export
ee_manage_quota = function() {
  oauth_func_path <- system.file("python/ee_manage.py", package = "rgee")
  ee_quota <- ee_source_python(oauth_func_path)
  ID <- ee$data$getAssetRoots()[[1]]$id
  quota = ee_py_to_r(ee_quota$quota(ID))
  total_msg = ee_humansize(as.numeric(quota[1]))
  used_msg = ee_humansize(as.numeric(quota[2]))
  cat(sprintf(' Total Quota: %s \n Used Quota: %s',total_msg, used_msg))
  invisible(quota)
}

#' @name ee_manage-tools
#' @export
ee_manage_size = function(path_asset) {
  if (missing(path_asset)) path_asset = ee$data$getAssetRoots()[[1]]$id
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  header=ee$data$getInfo(path_asset)[['type']]
  if (header=="ImageCollection") {
    ic = ee$ImageCollection(path_asset)
    nelements = ic$size()$getInfo()
    asset_size = sum(ic$aggregate_array('system:asset_size')$getInfo())
  } else if (header=="FeatureCollection") {
    fc = ee$FeatureCollection(path_asset)
    nelements = fc$size()$getInfo()
    asset_size = fc$get('system:asset_size')$getInfo()
  } else if (header=="Image") {
    img = ee$Image(path_asset)
    nelements = 1
    asset_size = img$get('system:asset_size')$getInfo()
  }else if (header=="Folder") {
    #After 0.1.175 needs add --no-use_cloud_api
    nelements = length(system(sprintf("earthengine ls %s", path_asset), intern = TRUE))
    asset_size = system(sprintf("earthengine du %s -s", path_asset), intern = TRUE)
    asset_size <- gsub("([0-9]+).*$", "\\1", asset_size) %>%
      as.numeric() %>%
      ee_humansize()
  }
  msg_01 = sprintf("- Size: %s\n", asset_size)
  msg_02 = sprintf("- # elements: %s\n",nelements)
  cat(header,":\n", msg_01, msg_02)
}

#' @name ee_manage-tools
#' @export
ee_manage_copy = function(path_asset,final_path,quiet = FALSE) {
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  final_path = ee_verify_filename(final_path,strict = FALSE)
  header = ee$data$getInfo(path_asset)[['type']]
  if (header %in% c("Image","ImageCollection","FeatureCollection")) {
    ee$data$copyAsset(path_asset, final_path)
    if (!quiet) cat('Done\n')
  } else if(header == 'Folder') {
    to_copy_list = ee$data$getList(params=list(id = path_asset))  %>%
      lapply('[[',1) %>%
      unlist()
    if (!quiet) cat('Copying a total of', length(to_copy_list), ' elements ..... please wait\n')
    folder_destination = sprintf("%s/%s",final_path,basename(to_copy_list))
    for (z in seq_along(to_copy_list)) {
      ee$data$copyAsset(to_copy_list[z], folder_destination[z])
    }
    if (!quiet) cat('Done\n')
  }
}

#' @name ee_manage-tools
#' @export
ee_manage_move = function(path_asset, final_path, quiet = FALSE) {
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  final_path = ee_verify_filename(final_path,strict = FALSE)
  header = ee$data$getInfo(path_asset)[['type']]
  if (header %in% c("Image","ImageCollection","FeatureCollection")) {
    ee$data$renameAsset(path_asset, final_path)
    if (!quiet) cat('Done\n')
  } else if(header == 'Folder') {
    header_finalpath = ee$data$getInfo(final_path)[['type']]
    if (is.null(header_finalpath)) ee_manage_create(final_path,quiet = quiet)
    to_copy_list = ee$data$getList(params=list(id = path_asset))  %>%
      lapply('[[',1) %>%
      unlist()
    if (!quiet) cat('Moving a total of', length(to_copy_list), ' elements ..... please wait\n')
    folder_destination = sprintf("%s/%s",final_path,basename(to_copy_list))
    for (z in seq_along(to_copy_list)) {
      ee$data$renameAsset(to_copy_list[z], folder_destination[z])
    }
    ee_manage_delete(path_asset,quiet = quiet)
    if (!quiet) cat('Done\n')
  }
}

#' @name ee_manage-tools
#' @export
ee_manage_set_properties = function(path_asset, properties) {
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  oauth_func_path <- system.file("python/ee_selenium_functions.py", package = "rgee")
  ee_selenium_functions <- ee_source_python(oauth_func_path)
  header = ee$data$getInfo(path_asset)[['type']]
  if (header %in% c("Image","ImageCollection","FeatureCollection")) {
    if ("system:time_start" %in% names(properties)) {
      properties[["system:time_start"]] =
        ee_selenium_functions$ee_Date_value(properties[["system:time_start"]]) %>%
        ee_py_to_r() %>%
        as.numeric()
    }

    if ("system:time_end" %in% names(properties)) {
      properties[["system:time_end"]] =
        ee_selenium_functions$ee_Date_value(properties[["system:time_end"]]) %>%
        ee_py_to_r() %>%
        as.numeric()
    }
    ee$data$setAssetProperties(path_asset,properties)
  } else {
    stop("Impossible assign properties to a Folder")
  }
}

#' @name ee_manage-tools
#' @export
ee_manage_delete_properties = function(path_asset, property) {
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  oauth_func_path <- system.file("python/ee_selenium_functions.py", package = "rgee")
  ee_selenium_functions <- ee_source_python(oauth_func_path)
  header = ee$data$getInfo(path_asset)[['type']]
  if (header %in% c("Image","ImageCollection","FeatureCollection")) {
    del_list = list()
    del_list[property] = list(NULL)
    ee$data$setAssetProperties(path_asset,del_list)
  } else {
    stop("Impossible delete properties to a Folder")
  }
}

#' @name ee_manage-tools
#' @export
ee_manage_assets_access = function(path_asset, acl = getOption("rgee.manage.getAssetAcl"),quiet=FALSE) {
  acl_m = acl
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  header = ee$data$getInfo(path_asset)[['type']]
  if (is.null(acl_m[['all_users_can_read']]))  acl_m[['all_users_can_read']] = TRUE
  if (header %in% c("Image","ImageCollection","FeatureCollection")) {
    acl_m = lapply(acl_m, function(x) paste(x, collapse = '\" , \"'))
    df_acl = data.frame(key = names(acl_m),value=unlist(acl_m))
    df_acl_p = na.omit(df_acl[c("writers","readers"),])
    create_jsondump = function(...) {
      sprintf("{%s",
              paste0(
                sprintf('"%s": ["%s"]',...),
                collapse = ", "
              )
      )
    }
    acl_m = do.call(create_jsondump, df_acl_p) %>%
      paste0(
        sprintf(', "all_users_can_read": %s}',tolower(acl_m[['all_users_can_read']]))
      )
    ee$data$setAssetAcl(path_asset,acl_m)
    if (!quiet) cat('The ACL of',path_asset,'has been changed.\n')
  } else if (header=="Folder") {
    list_files = ee$data$getList(list(id=path_asset))
    items = unlist(lapply(list_files, '[[',1))
    mapply(ee_manage_assets_access, items,MoreArgs = list(acl = acl))
  }
  invisible(TRUE)
}

#' @name ee_manage-tools
#' @export
ee_manage_task = function(quiet, cache = TRUE) {
  oauth_func_path <- system.file("python/ee_manage.py", package = "rgee")
  ee_manage_py <- ee_source_python(oauth_func_path)
  ee_temp = tempdir()
  manage_task_file = sprintf("%s/ee_manage_task_file.csv", ee_temp)
  if (!file.exists(manage_task_file) & cache) {
    py_names = c('tid', 'tstate', 'tdesc', 'ttype', 'tcreate', 'tdiffstart', 'tdiffend', 'error_message')
    df_names = c("ID","State","DestinationPath", "Type","Start","DeltaToCreate(s)","DeltaToCompletedTask(s)","ErrorMessage")
    status = ee_py_to_r(ee_manage_py$genreport())
    order_name = names(status[[1]])
    df_status <- data.frame(matrix(unlist(status), nrow=length(status), byrow=TRUE),stringsAsFactors = FALSE)
    colnames(df_status) = order_name
    df_order = df_status[py_names]
    colnames(df_order) = df_names
    df_order$DestinationPath = sub(".*:\\s","",df_order$DestinationPath)
    write.csv(df_order,manage_task_file,row.names = FALSE)
  } else {
    df_order = read.csv(manage_task_file,stringsAsFactors = FALSE)
  }
  return(df_order)
}

#' @name ee_manage-tools
#' @export
ee_manage_cancel_all_running_taks = function() {
  all_taks = ee$data$getTaskList()
  running = all_taks[which(unlist(lapply(all_taks, '[[','state')) == 'RUNNING')]
  if (length(running) > 0) stop("There are not any tasks running")
  for (z in seq_along(running)) {
    ee$data$cancelTask(running[[z]][['id']])
  }
}

#' Verify is the EE path asset is correct
#' @noRd
ee_verify_filename <- function(path_asset, strict = TRUE) {
  ee_path_dirname <- gsub("\\..+$","",path_asset)
  m <- gregexpr("[\\w']+", ee_path_dirname, perl=TRUE)
  folder <- ee_path_dirname %>%
    regmatches(m) %>%
    '[['(1) %>%
    paste(collapse = "/")
  response <- ee$data$getInfo(folder)
  if (is.null(response) & strict) {
    message <- c("%s is not a valid destination.",
                 "Make sure full path is provided e.g. users/user/nameofcollection",
                 'or projects/myproject/myfolder/newcollection and that you have',
                 "write access there.")
    stop(sprintf(message,path_asset))
  }
  return(folder)
}



#' Change the unit of measurement of bytes
#' @param x Integer. Number of bytes.
#' @return Number of bytes in a more human-comprehensible way
#' @noRd
ee_humansize = function(x, suffixes = c('B', 'KB', 'MB', 'GB', 'TB', 'PB')) {
  count = 0
  while (x >= 1024 & (count < length(suffixes) - 1)) {
    x = x/1024
    count = count + 1
  }
  if (suffixes[count+1] == 'B') {
    sprintf('%s %s',sprintf('%s',x),suffixes[count+1])
  } else {
    sprintf('%s %s',sprintf('%.2f',x),suffixes[count+1])
  }
}
