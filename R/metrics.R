# Compute calibration -----------------------------------------------------

#' Estimate calibration given forecasts and corresponding outcomes
#'
#' @param forecast Vector of probability forecast
#' @param outcome Vector of observations (0 or 1)
#' @param res Resolution (bin's width). If NULL, automatic bin width selection with Sturges' method
#'
#' @return Dataframe with colums Forecast (bins), Frequency (frequency of outcomes in the bin), Lower (lower bound of the 95% CI) and Upper (upper bound of the 95% CI)
#' @export
#'
#' @examples
#' N <- 10000
#' f <- runif(N)
#' o <- sapply(1:N, function(i) {rbinom(1, 1, f[i])})
#' cal <- compute_calibration(f, o)
#' with(cal, plot(Forecast, Frequency))
#' abline(c(0, 1), col = "red")
compute_calibration <- function(forecast, outcome, res = NULL) {

  if (is.null(res)){res <- 1 / nclass.Sturges(as.vector(as.matrix(forecast)))} # Automatic bin width selection

  forecast <- round(forecast / res) * res
  bins <- seq(0, 1, res)

  count_f <- table(factor(forecast, levels = bins)) # Number of forecast in each bin
  count_a <- table(factor(forecast[outcome == 1], levels = bins)) # Number of outcomes in each bin

  out <- Hmisc::binconf(count_a, count_f, alpha = 0.05, method = "exact")
  out <- as.data.frame(out)
  colnames(out)[1] <- "Frequency"
  out <- cbind(data.frame(Forecast = bins), out)
  rownames(out) <- NULL

  na.omit(out)
}

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

  reso <- mean((f - p0)^2)
  uncertainty <- mean(p0 * (1 - p0))
  reso / uncertainty
}
