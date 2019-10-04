# Perfectly calibrated data

set.seed(2019)

N <- 1e4
Forecast <- rbeta(N, 2, 2)
Outcome <- sapply(1:N, function(i) {rbinom(1, 1, Forecast[i])})

# write.csv(data.frame(Forecast, Outcome), file = "inst/testdata/calibration.csv", row.names = FALSE)
