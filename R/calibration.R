# Compute calibration -----------------------------------------------------

#' Estimate calibration given forecasts and corresponding outcomes
#'
#' @param forecast Vector of probability forecast
#' @param outcome Vector of observations (0 or 1)
#' @param prec Bins' precision (determine the number of bins). If NULL, automatic bin width selection with Sturges' method
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
compute_calibration <- function(forecast, outcome, prec = NULL) {

  if (is.null(prec)){prec <- 1 / nclass.Sturges(as.vector(as.matrix(forecast)))} # Automatic bin width selection

  forecast <- round(forecast / prec) * prec
  bins <- seq(0, 1, prec)

  count_f <- table(factor(forecast, levels = bins)) # Number of forecast in each bin
  count_a <- table(factor(forecast[outcome == 1], levels = bins)) # Number of outcomes in each bin

  out <- Hmisc::binconf(count_a, count_f, alpha = 0.05, method = "exact")
  out <- as.data.frame(out)
  colnames(out)[1] <- "Frequency"
  out <- cbind(data.frame(Forecast = bins), out)
  rownames(out) <- NULL

  na.omit(out)
}
