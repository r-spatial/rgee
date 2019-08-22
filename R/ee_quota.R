#' Returns quota usage details for the asset root with the given ID.
#' @author \href{Samapriya Roy}{https://github.com/samapriya}
#' @examples
#' \dontrun{
#' ee_quota()
#' }
#' @export
ee_quota <- function() {
  oauth_func_path <- system.file("python/ee_quota.py", package = "rgee")
  ee_source_python(oauth_func_path)
  id <- ee$data$getAssetRoots()[[1]]$id
  cat("",py$quota(id))
  invisible(TRUE)
}

