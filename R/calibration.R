# Compute calibration -----------------------------------------------------

#' Estimate calibration given forecasts and corresponding outcomes
#'
#' @param forecast Vector of probability forecasts.
#' @param outcome Vector of observations (0 or 1).
#' @param method Method used to estimate calibration, either "smoothing" or "binning".
#' @param CI Confidence level (e.g. 0.95). CI not computed if NULL (CI can be expensive to compute for LOWESS).
#' @param binwidth Binwidth when calibration is estimated by binning.
#' If NULL, automatic bin width selection with Sturges' method.
#' @param ... Arguments of [stats::loess()] function (e.g. span)
#'
#' @return Dataframe with columns `Forecast` (bins), `Frequency` (frequency of outcomes in the bin),
#' `Lower` (lower bound of the CI) and `Upper` (upper bound of the CI).
#' @export
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
            is.null(CI) || (CI < 1 && CI > 0),
            all(outcome %in% c(0, 1, NA, NaN)))

  if (method == "smoothing") {

    fit <- stats::loess(outcome ~ forecast, ...)

    out <- data.frame(Forecast = seq(0, 1, .01))
    if (!is.null(CI)) {
      tmp <- stats::predict(fit, newdata = out$Forecast, se = TRUE)
      tval <- stats::qt(.5 + CI / 2, tmp$df)
      out <- out %>%
        mutate(Frequency = tmp$fit,
               SE = tmp$se.fit,
               Lower = .data$Frequency - tval * .data$SE,
               Lower = pmax(0, .data$Lower),
               Upper = .data$Frequency + tval * .data$SE,
               Upper = pmin(1, .data$Upper)) %>%
        bind_rows(tibble(Forecast = 0, Frequency = 0, Lower = 0, Upper = 0))
    } else {
      out <- out %>%
        mutate(Frequency = stats::predict(fit, newdata = out$Forecast, se = FALSE)) %>%
        bind_rows(tibble(Forecast = 0, Frequency = 0))
    }

  }

  if (method == "binning") {

    if (is.null(binwidth)) {
      binwidth <- 1 / grDevices::nclass.Sturges(as.vector(as.matrix(forecast))) # Automatic bin width selection
    } else {
      stopifnot(length(binwidth) == 1,
                binwidth > 0 & binwidth < 1)
    }
    forecast <- round(forecast / binwidth) * binwidth
    bins <- seq(0, 1, binwidth)

    count_f <- table(factor(forecast, levels = bins)) # Number of forecast in each bin
    count_a <- table(factor(forecast[outcome == 1], levels = bins)) # Number of outcomes in each bin

    if (!is.null(CI)){
      out <- Hmisc::binconf(count_a, count_f, alpha = 1 - CI, method = "exact", return.df = TRUE) %>%
        rename(Frequency = .data$PointEst) %>%
        mutate(Forecast = bins) %>%
        relocate(.data$Forecast)
      rownames(out) <- NULL
    } else {
      out <- data.frame(Forecast = bins, Frequency = as.numeric(count_a / count_f))
    }

  }
  out <- drop_na(out)

  return(out)
}
