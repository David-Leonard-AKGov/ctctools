% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chinookmodelfunctions.R
\name{writeCalibrationTable1}
\alias{writeCalibrationTable1}
\title{(Calibration Performance) Write Table1 To csv}
\usage{
writeCalibrationTable1(data.combined, results.path = ".")
}
\arguments{
\item{data.combined}{A dataframe. Output of \code{\link{importFCSCCC}}.}

\item{results.path}{A character vector of length one. The absolute path or
path relative to the working directory where a new directory named
"results" will be written (if not already present). All output files will
be written here. Default is the current working directory.}
}
\value{
Writes a csv file.
}
\description{
Write table 1 as a csv, by stock, calibration, & data.type.
}
\examples{
\dontrun{
writeCalibrationTable1(data.combined, results.path = model.list$results.path)
}
}
