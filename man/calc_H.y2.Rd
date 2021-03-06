% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spfi_functions.R
\name{calc_H.y2}
\alias{calc_H.y2}
\title{(SPFI) Calculate the harvest rate parameters grouped by year.}
\usage{
calc_H.y2(c.ty.sum, r.ty.sum, T.ty, N.y)
}
\arguments{
\item{c.ty.sum}{Output from \code{\link{calc_ty.sum}}.}

\item{r.ty.sum}{Output from \code{\link{calc_ty.sum}}.}

\item{T.ty}{Output from \code{\link{calc_T.ty}}.}

\item{N.y}{Output from \code{\link{calc_N.y}}.}
}
\value{
A data frame of the AEQ fishery harvest rate parameter estimates
  grouped by year.
}
\description{
This is equivalent to equation 7b in the draft SPFI document.
This function produces the same output as \code{\link{calc_H.y}}.
}
\examples{
\dontrun{
#do imputation for total abundance:
imputation <- "calc_APC"
imputation.list <- do.call(what = imputation, args = list(N.ty, data.catch = data.catch))
N.y <- imputation.list$imputation.results
H.y <- calc_H.y2(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, T.ty = T.ty, N.y = N.y)
}
}
