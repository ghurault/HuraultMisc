% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distribution.R
\name{extract_ci}
\alias{extract_ci}
\title{Extract confidence intervals from a vector of samples}
\usage{
extract_ci(x, CI_level = seq(0.1, 0.9, 0.1), type = c("eti", "hdi"))
}
\arguments{
\item{x}{Vector of samples from a distribution.}

\item{CI_level}{Vector containing the level of the confidence/credible intervals.}

\item{type}{"eti" for equal-tailed intervals and "hdi" for highest density intervals.}
}
\value{
Dataframe with columns: \code{Lower}, \code{Upper}, \code{Level.}
}
\description{
Extract confidence intervals from a vector of samples
}
\examples{
x <- rexp(1e4)
extract_ci(x, type = "eti")
extract_ci(x, type = "hdi")
}
