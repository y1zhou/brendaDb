% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{DownloadBrenda}
\alias{DownloadBrenda}
\title{Download the BRENDA text file.}
\usage{
DownloadBrenda(path = "./brenda_download.zip", ...)
}
\arguments{
\item{path}{The path to store the downloaded text file.}

\item{...}{Parameters to be passed to \code{unzip}.}
}
\value{
A text file downloaded to \code{path}.
}
\description{
Download the BRENDA text file.
}
\examples{
DownloadBrenda("/path/to/textfile")

}