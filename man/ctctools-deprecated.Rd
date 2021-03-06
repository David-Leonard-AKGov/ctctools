% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated_functions.R
\name{ctctools-deprecated}
\alias{ctctools-deprecated}
\alias{importData}
\alias{mergeData}
\alias{plotCompare}
\alias{writeTable1}
\alias{writeTable3}
\alias{writeTable4}
\alias{writeTable5}
\alias{write_table6.6}
\title{Deprecated functions in ctctools}
\usage{
importData(data.path.vec = NA, model.list = NULL)

mergeData(ccc.list, fcs.list, stocks.names = "all")

plotCompare(data.combined, savepng = FALSE, results.path = ".",
  point.col.df, ...)

writeTable1(data.combined, results.path = ".")

writeTable3(metrics, ranking.method, results.path = ".", ...)

writeTable4(metrics, ranking.method, results.path = ".", ...)

writeTable5(metrics, ranking.method, results.path = ".", ...)

write_table6.6(spfi.output, data.catch)
}
\description{
These functions still work but will be eventually be removed (defunct).
}
\details{
\itemize{
The deprecated functions include:
 \item \code{importData}
 \item \code{mergeData}
 \item \code{plotCompare}
 \item \code{writeTable1}
 \item \code{writeTable3}
 \item \code{writeTable4}
 \item \code{writeTable5}
 \item \code{write_table6.6}

}
}
\seealso{
\code{\link{importFCSCCC}} replaces \code{importData}.

\code{\link{mergeFCSCCC}} replaces \code{mergeData}.

\code{\link{plotFCSCCC}} replaces \code{plotCompare}.

\code{\link{writeCalibrationTable1}} replaces \code{writeTable1}.

\code{\link{writeCalibrationTable3}} replaces \code{writeTable3}.

\code{\link{writeCalibrationTable4}} replaces \code{writeTable4}.

\code{\link{writeCalibrationTable5}} replaces \code{writeTable5}.

\code{\link{writeSPFItable6.6}} replaces \code{write_table6.6}.
}
