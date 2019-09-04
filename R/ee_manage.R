#' Create a folder or ImageCollection into GEE assets
#' @name ee_check-tools
#' @param path_asset a character vector containing a single path name.
#' @param asset_type a character vector containing the asset type. 'folder' or 'imagecollection'.
#' @param quiet logical; suppress info message.
#'
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

#' @name ee_check-tools
#' @export
ee_manage_delete = function(path_asset, quiet=FALSE) {
  path_asset = ee_verify_filename(path_asset,strict = TRUE)
  response = ee$data$getInfo(path_asset)
  if (is.null(response)) stop("path_asset does not exist!")
  if (response$type %in% c('Folder','ImageCollection')) {
    list_files = ee$data$getList(list(id=path_asset))
    items = unlist(lapply(list_files, '[[',1))
    mapply(ee_manage_delete, items)
  }
  ee$data$deleteAsset(path_asset)
  if (!quiet) cat('EE object deleted:',path_asset,'\n')
}

#' @name ee_check-tools
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


#' @name ee_check-tools
#' @export
ee_manage_quota = function() {
  oauth_func_path <- system.file("python/ee_quota.py", package = "rgee")
  ee_quota <- ee_source_python(oauth_func_path)
  ID <- ee$data$getAssetRoots()[[1]]$id
  quota = ee_quota$quota(ID)
  total_msg = ee_humansize(as.numeric(quota[1]))
  used_msg = ee_humansize(as.numeric(quota[2]))
  cat(sprintf(' Total Quota: %s \n Used Quota: %s',total_msg, used_msg))
  invisible(quota)
}

#' @name ee_check-tools
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
    img = ee$Image(asset)
    nelements = 1
    asset_size = img$get('system:asset_size')$getInfo()
  }else if (header=="folder") {
    #After 0.1.175 needs add --no-use_cloud_api
    nelements = length(system(sprintf("earthengine ls %s", path_asset), intern = TRUE))
    asset_size = system(sprintf("earthengine du %s -s", path_asset), intern = TRUE) %>%
      gsub("([0-9]+).*$", "\\1", .) %>%
      as.numeric() %>%
      ee_humansize()
  }
  msg_01 = sprintf("- Size: %s\n", asset_size)
  msg_02 = sprintf("- # elements: %s\n",nelements)
  cat(header,":\n", msg_01,"\n", msg_02)
}

#' @name ee_check-tools
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

#' @name ee_check-tools
#' @exports
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
