# Compute resolution ------------------------------------------------------

#' Compute resolution of forecasts, normalised by the uncertainty
#'
#' The resolution is computed as the mean squared distance to a base rate (reference forecast) and is then normalised by the uncertainty (maximum resolution).
#' This means the output is between 0 and 1, 1 corresponding to the maximum resolution.
#'
#' @param f Vector of forecasts
#' @param p0 Vector of base rate. In the case rate is usually the prevalence of a uniform forecast (e.g. 1 / number of categories) but can depend on the observation (hence the vector).
#'
#' @return Resolution
#' @export
#'
#' @examples
#' compute_resolution(seq(0, 1, .1), 0.5)
compute_resolution <- function(f, p0) {

  stopifnot(min(f) >= 0,
            max(f) <= 1,
            min(p0) >= 0,
            max(p0) <= 1)

  reso <- mean((f - p0)^2)
  uncertainty <- mean(p0 * (1 - p0))
  reso / uncertainty
}

