test_that("Calibration curve are accurate", {

  # Perfectly calibrated data (10000 observations)
  file <- system.file("testdata", "calibration.csv",package="HuraultMisc", mustWork = TRUE)
  df <- read.csv(file)

  # tolerances
  tol_rmse <- 0.05
  tol_ci <- 0.85

  # Smoothing with default span
  x <- with(df, compute_calibration(Forecast, Outcome, method = "smoothing", CI = 0.95))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol_rmse)
  expect_gt(with(x, mean(Forecast > Lower & Forecast < Upper)), tol_ci) # CI (with some tolerance)
  # Smoothing with custom span
  x <- with(df, compute_calibration(Forecast, Outcome, method = "smoothing", span = 0.5))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol_rmse)
  # Binning with automatic bin selection
  x <- with(df, compute_calibration(Forecast, Outcome, method = "binning", CI = 0.95))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol_rmse)
  expect_gt(with(x, mean(Forecast > Lower & Forecast < Upper)), tol_ci) # CI (with some tolerance)
  # Binning with custom bin selection
  x <- with(df, compute_calibration(Forecast, Outcome, method = "binning", binwidth = .1))
  expect_lt(with(x, sqrt(mean((Frequency - Forecast)^2))), tol_rmse)

  # Checking errors
  expect_error(compute_calibration(c(1.1, 0.5), c(1, 0), method = "smoothing"))
  expect_error(compute_calibration(c(0.9, 0.5), c(1.1, 0), method = "smoothing"))
  expect_error(with(df, compute_calibration(Forecast, Outcome, method = "smoothing", CI = 95)))
  expect_error(with(df, compute_calibration(Forecast, Outcome, method = "binning", binwidth = 2)))
  expect_error(with(df, compute_calibration(Forecast, Outcome, method = "kernel")))
})
