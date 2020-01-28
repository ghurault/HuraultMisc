test_that("empirical_pval works", {

  expect_equal(empirical_pval(seq(1, 100), 95, alternative = "greater"),
               (5 + 1) / (100 + 1))

  tol <- 0.1
  expect_lt(empirical_pval(rnorm(1e3), 2, alternative = "two.sided"), .05 + tol)
  expect_lt(empirical_pval(rnorm(1e3), -2, alternative = "less"), .025 + tol)

  expect_error(empirical_pval(matrix(rnorm(1e3), ncol = 10), 10))
  expect_error(empirical_pval(seq(1, 100), c(2, 3)))
  expect_error(empirical_pval(95, seq(1, 100)))
})

test_that("post_pred_pval works", {

  yrep <- matrix(rnorm(1e3), ncol = 10)
  y <- rep(2, 10)
  x <- post_pred_pval(yrep, y, plot = TRUE)

  expect_is(x, "list")
  expect_is(x[[1]], "numeric")
  expect_is(x[[2]], "ggplot")

  expect_warning(post_pred_pval(yrep, c(y, 2)))
  expect_error(post_pred_pval(yrep, y, test_statistic = "mean"))
  expect_error(post_pred_pval(yrep, y, plot = "no"))
})
