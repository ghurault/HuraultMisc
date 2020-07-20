test_that("compute_RPS returns the correct values for a uniform forecast", {
  for (k in 3:20) {
    p0 <- rep(1 / k, k)
    E_RPS <- mean(sapply(1:k, function(x) {compute_RPS(p0, x)}))
    expect_equal(E_RPS, (k + 1) / 6 / k)
  }
})

test_that("compute_RPS returns NA if inputs contain NA", {
  expect_equal(compute_RPS(c(.1, .4, .5), NA), NA)
  expect_equal(compute_RPS(c(NA, .2, NA), 2), NA)
})

test_that("compute_RPS identify incorrect inputs", {
  expect_error(compute_RPS(list(.1, .4, .5), 2))
  expect_error(compute_RPS(2, 2))
  expect_error(compute_RPS(c(.4, -.2, .8), 2))
  expect_error(compute_RPS(c(.4, .4, .3), 2))
  expect_error(compute_RPS(c(.4, .2, .4), 0))
})
