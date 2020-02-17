# Compute calibration -----------------------------------------------------

#' Estimate calibration given forecasts and corresponding outcomes
#'
#' @param forecast Vector of probability forecasts
#' @param outcome Vector of observations (0 or 1)
#' @param method Moethod used to estimate calibration, either "smoothing" or "binning"
#' @param CI Confidence level (e.g. 0.95). CI not computed if NULL (CI can be expensive to compute for LOWESS).
#' @param binwidth Binwidth when calibration is estimated by binning.  If NULL, automatic bin width selection with Sturges' method.
#' @param ... Arguments of stats::loess function (e.g.span)
#'
#' @return Dataframe with colums Forecast (bins), Frequency (frequency of outcomes in the bin), Lower (lower bound of the CI) and Upper (upper bound of the CI)
#' @export
#' @import stats
#'
#' @examples
#' N <- 1e4
#' f <- rbeta(N, 1, 1)
#' o <- sapply(f, function(x) {rbinom(1, 1, x)})
#' lapply(c("binning", "smoothing"),
#'        function(m) {
#'          cal <- compute_calibration(f, o, method = m)
#'          with(cal, plot(Forecast, Frequency, type = "l"))
#'          abline(c(0, 1), col = "red")
#'        })
compute_calibration <- function(forecast, outcome, method = c("smoothing", "binning"), CI = NULL , binwidth = NULL, ...) {

  method <- match.arg(method)

  stopifnot(max(forecast) <= 1,
            min(forecast) >= 0,
            is.null(CI) || (CI < 1 && CI > 0))

  if (sum(outcome == 0 | outcome == 1, na.rm = TRUE) != length(outcome)) {
    stop(as.character(substitute(outcome)), " values should be only 0 or 1")
  }

  if (method == "smoothing") {

    fit <- loess(outcome ~ forecast, ...)

    out <- data.frame(Forecast = seq(0, 1, .01))
    if (!is.null(CI)) {
      tmp <- predict(fit, newdata = out$Forecast, se = TRUE)
      out$Frequency <- tmp$fit
      t <- .5 + CI / 2
      out$Lower <- with(tmp, pmax(0, fit - qt(t, df) * se.fit))
      out$Upper <- with(tmp, pmin(1, fit + qt(t, df) * se.fit))
      out <- rbind(data.frame(Forecast = 0, Frequency = 0, Lower = 0, Upper = 0),
                   na.omit(out)) # Remove missing and add 0
    } else {
      tmp <- predict(fit, newdata = out$Forecast, se = FALSE)
      out$Frequency <- tmp
      out <- rbind(data.frame(Forecast = 0, Frequency = 0),
                   na.omit(out)) # Remove missing and add 0
    }

  } else if (method == "binning") {

    stopifnot(is.null(binwidth) || (binwidth > 0 && binwidth < 1))

    if (is.null(binwidth)) {
      binwidth <- 1 / grDevices::nclass.Sturges(as.vector(as.matrix(forecast))) # Automatic bin width selection
    }
    forecast <- round(forecast / binwidth) * binwidth
    bins <- seq(0, 1, binwidth)

    count_f <- table(factor(forecast, levels = bins)) # Number of forecast in each bin
    count_a <- table(factor(forecast[outcome == 1], levels = bins)) # Number of outcomes in each bin

    if (!is.null(CI)){
      out <- Hmisc::binconf(count_a, count_f, alpha = 1 - CI, method = "exact")
      out <- as.data.frame(out)
      colnames(out)[1] <- "Frequency"
      out <- cbind(data.frame(Forecast = bins), out)
      rownames(out) <- NULL
      out <- na.omit(out)
    } else {
      out <- data.frame(Forecast = bins, Frequency = as.numeric(count_a / count_f))
    }

  }

  return(out)
}