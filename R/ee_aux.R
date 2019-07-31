#' Soy una bonita que aun no hace nada
#' @param x holi2
#' @importFrom geojsonio geojson_json
#' @export
ee_as_geom <- function(x) {
  oauth_func_path <- system.file("Python/ee_aux.py", package = "rgee")
  ee_source_python(oauth_func_path)
  geojson_sf <- geojson_json(x)
  ee_geometry2_py(geojson_sf)
}


#' Soy una bonita que aun no hace nada
#' @param task holi2
#' @param path holi2
#' @param overwrite holi2
#' @importFrom googledrive drive_find drive_download
#' @importFrom stars read_stars
#' @export
ee_Download <- function(task,path=NULL,overwrite=FALSE) {
  gd_filename <- task$config$driveFileNamePrefix
  query_gd <- sprintf("name contains '%s'",gd_filename)
  files_gd <- try(drive_find(q = query_gd))
  count <- 1
  while (any(class(files_gd) %in% "try-error") & count < 5) {
    files_gd <- try(drive_find(q = query_gd))
    count <- count + 1
  }
  orgl_filename <- files_gd$drive_resource[[1]]$originalFilename
  row_id <- mapply(grepl,task$id,orgl_filename) %>% which()
  to_download <- files_gd[row_id,]
  if(is.null(path)) path <- tempfile()
  download_mtd <- drive_download(to_download,path,overwrite = overwrite)
  read_stars(download_mtd$local_path)
}
