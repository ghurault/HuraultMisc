test_that("Calibration curve are accurate", {

  # Perfectly calibrated data (10000 observations)
  file <- system.file("testdata", "calibration.csv",package="HuraultMisc", mustWork = TRUE)
  df <- read.csv(file)

  tol <- 0.05 # tolerance

  # Smoothing with default span
  x <- with(df, compute_calibration(Forecast, Outcome, method = "smoothing"))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol)
  # Smoothing with custom span
  x <- with(df, compute_calibration(Forecast, Outcome, method = "smoothing", span = 0.5))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol)
  # Check confidence intervals
  x <- with(df, compute_calibration(Forecast, Outcome, method = "smoothing", CI = .95))
  expect_gt(with(x, mean(Forecast > Lower & Forecast < Upper)), 0.9) # tolerance
  # Binning with automatic bin selection
  x <- with(df, compute_calibration(Forecast, Outcome, method = "binning"))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol)
  # Binning with custom bin selection
  x <- with(df, compute_calibration(Forecast, Outcome, method = "binning", binwidth = .1))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol)

})
