N <- 100
N_post <- 1e3
truth <- rep(0, N)
post_samples <- sapply(rnorm(N, 0, 1), function(x) {rnorm(N_post, x, 1)})

# Somewhat redundant tests to those in test-stan_workflow but the input is more controlled here

test_that("coverage is accurate", {
  cov_rmse <- with(compute_coverage(post_samples, truth),
       sqrt(mean((Nominal - Coverage)^2)))
  expect_lt(cov_rmse, 0.1)
})

test_that("compute_coverage catch errors", {
  expect_error(compute_coverage(post_samples, truth[-1]))
})

test_that("plot_coverage returns a ggplot object", {
  expect_is(plot_coverage(post_samples, truth), "ggplot")
})
