% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fp_functions.R
\name{read_BSE}
\alias{read_BSE}
\title{Read BSE file}
\usage{
read_BSE(filename)
}
\arguments{
\item{filename}{}
}
\value{
A list comprising multiple elements, including unique representations of the tables in the .BSE file. The final element is a data frame of the stocks translation table.
}
\description{
Read BSE file
}
\examples{
\dontrun{
dat.bse <- read_BSE("2017BPC_PII_V1.22.BSE")
}
}
