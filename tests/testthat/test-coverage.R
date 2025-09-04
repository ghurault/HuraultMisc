set.seed(2021)
N <- 100
N_post <- 1e3
truth <- rep(0, N)
post_samples <- sapply(rnorm(N, 0, 1), function(x) {
  rnorm(N_post, x, 1)
})

for (t in c("eti", "hdi")) {
  test_that("compute_coverage() is accurate", {
    cov_rmse <- with(
      compute_coverage(post_samples, truth, type = t),
      sqrt(mean((Nominal - Coverage)^2))
    )
    expect_lt(cov_rmse, 0.1)
  })

  test_that("plot_coverage() returns a ggplot object", {
    expect_is(plot_coverage(post_samples, truth, type = t), "ggplot")
  })

  test_that("compute_coverage() identifies input of wrong dimensions", {
    expect_error(compute_coverage(post_samples, truth[-1], type = t))
    expect_error(compute_coverage(post_samples[, -1], truth, type = t))
  })

  test_that("compute_coverage() identifies incorrect inputs", {
    expect_error(compute_coverage(post_samples, data.frame(truth)), type = t)
    expect_error(compute_coverage(post_samples, as.character(truth)), type = t)
    expect_error(compute_coverage(post_samples, truth, paste0(seq(0, 100, 5), "%")), type = t)
    expect_error(compute_coverage(post_samples, truth, seq(0, 100, 5)), type = t)
  })
}
