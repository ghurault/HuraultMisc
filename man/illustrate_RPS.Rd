% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/illustrations.R
\name{illustrate_RPS}
\alias{illustrate_RPS}
\title{Illustration of the Ranked Probability Score}
\usage{
illustrate_RPS(mu = 5, sigma = 1, observed = 6)
}
\arguments{
\item{mu}{Mean of the Gaussian forecast distribution}

\item{sigma}{Standard deviation of the Gaussian forecast distribution}

\item{observed}{Observed outcome}
}
\value{
Ggplot
}
\description{
Illustration of the RPS in the case of forecasts for a discrete "Severity" score, ranging from 0 to 10.
The forecast follow a (truncated between 0 and 10) Gaussian distribution, which is discretised to the nearest integer for RPS calculation.
}
\details{
The RPS is the mean square error between the cumulative outcome and cumulative forecast distribution (shaded are square).
The Ranked Probability Skill Score compares the RPS to a reference RPS (RPS0), \code{RPSS = 1 - RPS / RPS0}.
It can be interpreted as a normalised distance to a reference forecast:
RPSS = 0 means that the forecasts are not better than the reference and RPSS = 1 corresponds to perfect forecasts.
}
\examples{
illustrate_RPS()

}
