test_that("Plot returns ggplot object", {
  expect_is(illustrate_RPS(), "ggplot")
  expect_is(illustrate_forward_chaining(), "ggplot")
})

test_that("Errors are noticed", {
  expect_error(illustrate_RPS(observed = -1))
  expect_error(illustrate_RPS(mu = -10, sigma = 3))
})
