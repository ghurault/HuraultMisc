% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distribution.R
\name{extract_pmf}
\alias{extract_pmf}
\title{Extract probability mass function from vector of samples}
\usage{
extract_pmf(x, support = NULL)
}
\arguments{
\item{x}{Vector of samples from a distribution.}

\item{support}{Vector of all possible values that the distribution can take. Can be NULL.}
}
\value{
Dataframe with columns: \code{Value}, \code{Probability.}
}
\description{
Extract probability mass function from vector of samples
}
\examples{
extract_pmf(round(rnorm(1e3, 0, 10))) \%>\% head()
}
