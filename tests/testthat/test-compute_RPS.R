test_that("compute_RPS returns the correct values", {

  for (k in 3:20) {
    p0 <- rep(1 / k, k) # uniform forecast
    E_RPS <- mean(sapply(1:k, function(x) {compute_RPS(p0, x)})) # expected value of uniform forecast
    expect_equal(E_RPS, (k + 1) / 6 / k)
  }

})

test_that("compute_RPS catches errors", {

  expect_error(compute_RPS(list(.1, .4, .5), 2))
  expect_error(compute_RPS(2, 2))
  expect_error(compute_RPS(c(.4, -.2, .8), 2))
  expect_error(compute_RPS(c(.4, .4, .3), 2))
  expect_error(compute_RPS(c(.4, .2, .4), 0))

})
