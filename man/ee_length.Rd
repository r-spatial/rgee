% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ops-subsetting.R
\name{ee_length}
\alias{ee_length}
\alias{length.ee.image.Image}
\alias{length<-.ee.image.Image}
\title{Length of an Earth Engine Image Object}
\usage{
\method{length}{ee.image.Image}(x)

\method{length}{ee.image.Image}(x) <- value
}
\arguments{
\item{x}{an EE Image Object.}

\item{value}{a non-negative integer.}
}
\description{
Get or set the length of an Earth Engine Image.
}
\details{
If a vector is shortened, extra values are discarded and when a vector
is lengthened, it is padded out to its new length with ee$Image(0), with
band name of zzz_rgee_NA_\%02d.
}
