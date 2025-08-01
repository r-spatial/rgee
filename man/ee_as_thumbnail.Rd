% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ee_as_thumbnail.R
\name{ee_as_thumbnail}
\alias{ee_as_thumbnail}
\title{Create an R spatial gridded object from an EE thumbnail image}
\usage{
ee_as_thumbnail(
  image,
  region,
  dimensions,
  vizparams = NULL,
  raster = FALSE,
  quiet = FALSE
)
}
\arguments{
\item{image}{EE Image object to be converted into a stars object.}

\item{region}{EE Geometry Rectangle (ee$Geometry$Rectangle) specifies the region to be exported.
The CRS must match the 'x' argument; otherwise, it will be forced.}

\item{dimensions}{Numeric vector of length 2 that specifies the dimensions of the thumbnail image in pixels.
If only one integer is provided, it determines the size of the larger dimension of the image and scales
the other dimension proportionally. Defaults to 512 pixels for the larger image aspect dimension.}

\item{vizparams}{A list containing the visualization parameters.See details.}

\item{raster}{Logical. Should the thumbnail image be saved as a
RasterStack object?}

\item{quiet}{logical; suppress info messages.}
}
\value{
An stars or Raster object depending on the \code{raster} argument.
}
\description{
Wrapper function around \code{ee$Image$getThumbURL} to create a stars or
RasterLayer R object from a
\href{ https://developers.google.com/earth-engine/guides/image_visualization}{EE thumbnail image}.
}
\details{
\code{vizparams} set up the details of the thumbnail image. With
\code{ee_as_thumbnail} allows exporting only one-band (G) or three-band
(RGB) images. Several parameters can be passed on to control color,
intensity, the maximum and minimum values, etc. The table below provides
all the parameters that admit \code{ee_as_thumbnail}.

\tabular{lll}{
\strong{Parameter}\tab \strong{Description}  \tab \strong{Type}\cr
\strong{bands}    \tab  Comma-delimited list of three band (RGB) \tab  list \cr
\strong{min}      \tab  Value(s) to map to 0 \tab  number or list of three
numbers, one for each band \cr
\strong{max}      \tab  Value(s) to map to 1 \tab  number or list of three
numbers, one for each band \cr
\strong{gain}     \tab  Value(s) by which to multiply each pixel value \tab
number or list of three numbers, one for each band \cr
\strong{bias}     \tab  Value(s) to add to each Digital Number
value \tab number or list of three numbers, one for each band \cr
\strong{gamma}    \tab  Gamma correction factor(s) \tab  number or list of
three numbers, one for each band \cr
\strong{palette}  \tab  List of CSS-style color strings
(single-band only) \tab  comma-separated list of hex strings \cr
\strong{opacity}   \tab  The opacity of the layer (from 0 to 1)  \tab  number \cr
}
}
\examples{
\dontrun{
library(raster)
library(stars)
library(rgee)

ee_Initialize()

nc <- st_read(system.file("shp/arequipa.shp", package = "rgee"))
dem_palette <- c(
  "#008435", "#1CAC17", "#48D00C", "#B3E34B", "#F4E467",
  "#F4C84E", "#D59F3C", "#A36D2D", "#C6A889", "#FFFFFF"
)

## DEM data -SRTM v4.0
image <- ee$Image("CGIAR/SRTM90_V4")
world_region <- ee$Geometry$Rectangle(
  coords = c(-180,-60,180,60),
  proj = "EPSG:4326",
  geodesic = FALSE
)

## world - elevation
world_dem <- ee_as_thumbnail(
  image = image,
  region = world_region,
  dimensions = 1024,
  vizparams = list(min = 0, max = 5000)
)

world_dem[world_dem <= 0] <- NA
world_dem <- world_dem * 5000

plot(
  x = world_dem, col = dem_palette, breaks = "equal",
  reset = FALSE, main = "SRTM - World"
)

## Arequipa-Peru
arequipa_region <- nc \%>\%
  st_bbox() \%>\%
  st_as_sfc() \%>\%
  sf_as_ee()

arequipa_dem <- ee_as_thumbnail(
  image = image,
  region = arequipa_region$buffer(1000)$bounds(),
  dimensions = 512,
  vizparams = list(min = 0, max = 5000)
)

arequipa_dem <- arequipa_dem * 5000
st_crs(arequipa_dem) <- 4326
plot(
  x = arequipa_dem[nc], col = dem_palette, breaks = "equal",
  reset = FALSE, main = "SRTM - Arequipa"
)

suppressWarnings(plot(
  x = nc, col = NA, border = "black", add = TRUE,
  lwd = 1.5
))
dev.off()

## LANDSAT 8
img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
  select(c("B4", "B3", "B2"))
Map$centerObject(img)
Map$addLayer(img, list(min = 0, max = 5000, gamma = 1.5))

## Teton Wilderness
l8_img <- ee_as_thumbnail(
  image = img,
  region = img$geometry()$bounds(),
  dimensions = 1024,
  vizparams = list(min = 0, max = 5000, gamma = 1.5),
  raster = TRUE
)
crs(l8_img) <- "+proj=longlat +datum=WGS84 +no_defs"
plotRGB(l8_img, stretch = "lin")
}
}
\seealso{
Other image download functions: 
\code{\link{ee_as_rast}()},
\code{\link{ee_as_raster}()},
\code{\link{ee_as_stars}()},
\code{\link{ee_imagecollection_to_local}()}
}
\concept{image download functions}
