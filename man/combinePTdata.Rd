% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isbm_functions.R
\name{combinePTdata}
\alias{combinePTdata}
\title{(ISBM/CYER) Combine the data from multiple .pt files.}
\usage{
combinePTdata(pt.list)
}
\arguments{
\item{pt.list}{A list. The output from \code{\link{readPT}}.}
}
\value{
A list of three data frames named \code{data.isbm.long},
  \code{data.isbm.ages}, and  \code{data.bper.long}. Each data frame is in
  long format and has the same strucutre as the long format data frames
  produced by \code{\link{readPT}}, but each data frame includes a
  \code{cwtstock} column as all stocks are combined into the same data frame.
}
\description{
(ISBM/CYER) Combine the data from multiple .pt files.
}
\examples{
\dontrun{
data.path <- "../data/isbm"
pt.filenames <- list.files(data.path, pattern = ".PT$")
pt.filepaths <- paste(data.path, pt.filenames, sep="/")
pt.list <- readPT(pt.filepaths)
pt.list.long <- combinePTdata(pt.list)
names(pt.list.long)
}
}
