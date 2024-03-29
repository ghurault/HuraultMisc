% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprec-stan.R
\name{process_replications}
\alias{process_replications}
\title{Extract posterior predictive distribution}
\usage{
process_replications(
  fit,
  idx = NULL,
  parName,
  bounds = NULL,
  type = c("continuous", "discrete", "eti", "hdi"),
  ...
)
}
\arguments{
\item{fit}{Stanfit object.}

\item{idx}{Dataframe for translating the indices of the parameters into more informative variable (can be NULL).}

\item{parName}{Name of the parameter to extract.}

\item{bounds}{NULL or vector of length 2 representing the bounds of the distribution if it needs to be truncated.}

\item{type}{Indicates how the distribution is summarised.}

\item{...}{Parameters to be passed to \code{\link[=extract_distribution]{extract_distribution()}}.}
}
\value{
Dataframe.
}
\description{
Extract posterior predictive distribution
}
