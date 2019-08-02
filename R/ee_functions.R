#' Constructs an Earth Engine image.
#' @param args This constructor accepts a variety of arguments:
#' \itemize{
#'   \item A string - an EarthEngine asset id,
#'   \item A string and a number - an EarthEngine asset id and version,
#'   \item A number - creates a constant image,
#'   \item An EEArray - creates a constant array image,
#'   \item A list - creates an image out of each element of the array and
#'         combines them into a single image,
#'   \item An ee.Image - returns the argument,
#'   \item Nothing - results in an empty transparent image.
#'}
#' @param version An optional asset version.
#' @export
ee_Image <- function(args=NULL, version=NULL){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_image_py(args=args, version=version)
}

#' Representation for an Earth Engine ImageCollection
#' @param args ImageCollections can be constructed from the following arguments:
#' \enumerate{
#' \item A string: assumed to be the name of a collection,
#' \item An array of images, or anything that can be used to construct an
#' image.
#' \item A single image.
#' \item A computed object - reinterpreted as a collection.
#' }
#' @export
ee_ImageCollection <- function(args) {
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_imagecollection_py(args)
}

#' Constructs an Earth Engine geometry.
#' @param geo_json The GeoJSON object describing the geometry or a
#' computed object to be reinterpred as a Geometry. Supports
#' CRS specifications as per the GeoJSON spec, but only allows named
#' (rather than "linked" CRSs). If this includes a 'geodesic' field,
#' and opt_geodesic is not specified, it will be used as opt_geodesic.
#' @param opt_proj An optional projection specification, either as an
#' ee.Projection, as a CRS ID code or as a WKT string. If specified,
#' overrides any CRS found in the geo_json parameter. If unspecified and
#' the geo_json does not declare a CRS, defaults to "EPSG:4326"
#' (x=longitude, y=latitude).
#' @param opt_geodesic Whether line segments should be interpreted as spherical
#' geodesics. If false, indicates that line segments should be
#' interpreted as planar lines in the specified CRS. If absent,
#' defaults to true if the CRS is geographic (including the default
#' EPSG:4326), or to false if the CRS is projected.
#' @param opt_evenOdd If true, polygon interiors will be determined by the even/odd
#' rule, where a point is inside if it crosses an odd number of edges to
#' reach a point at infinity. Otherwise polygons use the left-inside
#' rule, where interiors are on the left side of the shell's edges when
#' walking the vertices in the given order. If unspecified, defaults to
#' True.
#' @export
ee_Geometry <- function(geo_json, opt_proj=NULL, opt_geodesic=NULL,opt_evenOdd=NULL) {
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_geometry_py(geo_json, opt_proj=opt_proj, opt_geodesic=opt_proj,opt_evenOdd=opt_proj)
}


#' Constructs an Earth Engine Feature
#' Features can be constructed from one of the following arguments plus an
#' optional dictionary of properties:
#' \enumerate{
#'   \item An ee.Geometry.
#'   \item A GeoJSON Geometry.
#'   \item A GeoJSON Feature.
#'   \item A computed object - reinterpreted as a geometry if properties
#'         are specified, and as a feature if they aren't.
#' }
#'
#' @param geom A geometry or feature.
#' @param opt_properties A dictionary of metadata properties. If the first
#' parameter is a Feature (instead of a geometry), this is unused.
#' @export
ee_Feature <- function(geom, opt_properties=NULL){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_feature_py(geom, opt_properties=opt_properties)
}

#' Constructs an Earth Engine FeatureCollection
#' @param args constructor argument. One of:
#'   \enumerate {
#'     \item A string - assumed to be the name of a collection.
#'     \item A geometry.
#'     \item A feature.
#'     \item An array of features.
#'     \item A computed object - reinterpreted as a collection.
#' }
#' @param opt_column The name of the geometry column to use. Only useful with the
#' string constructor.
#' @export
ee_FeatureCollection <- function(args, opt_column=NULL) {
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  fc_object <- ee_featurecollection_py(args, opt_column=opt_column)
}

#' An object to represent collection filters.
#'
#' @param filter Filter|List, optional
#' @details
#' This constructor accepts the following args:
#' \enumerate{
#'  \item Another filter.
#'  \item A list of filters (which are implicitly AND ed together).
#'  \item A ComputedObject returning a filter. Users should not be making these;
#'  they are produced by the generator functions below.
#' }
#'
#' @return Filter EarthEngine Python class
#' @section Methods:
#' \itemize{
#' \item \strong{always():}
#' \item \strong{And():}
#' \item \strong{aside():}
#' \item \strong{calendarRange():}
#' \item \strong{contains():}
#' \item \strong{date():}
#' \item \strong{dateRangeContains():}
#' \item \strong{dayOfYear():}
#' \item \strong{disjoint():}
#' \item \strong{encode():}
#' \item \strong{eq():}
#' \item \strong{equals():}
#' \item \strong{freeze():}
#' \item \strong{geometry():}
#' \item \strong{getInfo():}
#' \item \strong{greaterThan():}
#' \item \strong{greaterThanOrEquals():}
#' \item \strong{gt():}
#' \item \strong{gte():}
#' \item \strong{hasGeometry():}
#' \item \strong{initialize():}
#' \item \strong{inList():}
#' \item \strong{intersects():}
#' \item \strong{isContained():}
#' \item \strong{isVariable():}
#' \item \strong{lessThan():}
#' \item \strong{lessThanOrEquals():}
#' \item \strong{listContains():}
#' \item \strong{lt():}
#' \item \strong{lte():}
#' \item \strong{maxDifference():}
#' \item \strong{metadata_():}
#' \item \strong{name():}
#' \item \strong{neq():}
#' \item \strong{never():}
#' \item \strong{Not():}
#' \item \strong{notEquals():}
#' \item \strong{notNull():}
#' \item \strong{Or():}
#' \item \strong{predicateCount():}
#' \item \strong{rangeContains():}
#' \item \strong{reset():}
#' \item \strong{serialize():}
#' \item \strong{stringContains():}
#' \item \strong{stringEndsWith():}
#' \item \strong{stringStartsWith():}
#' \item \strong{withinDistance():}
#' }
#' @export
ee_Filter <- function(filter){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_filter_py(filter)
}

#' Soy una bonita funcion que no hace nada 4 :)
#' @export
ee_Reducer <- function(){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_reducer_py()
}

#' Soy una bonita funcion que no hace nada 4 :)
#' @export
ee_Export <- function(){
  oauth_func_path <- system.file("Python/ee_functions.py", package = "rgee")
  ee_source_python(oauth_func_path)
  ee_export_py()
}
