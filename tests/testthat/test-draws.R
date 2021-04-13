x <- rnorm(1e3)
X <- matrix(x, ncol = 10)
a1 <- array(rnorm(400), dim = c(100, 2, 2))
a2 <- array(rnorm(800), dim = c(100, 2, 2, 2))

dr <- list(sample(1:length(x), 1),
           sample(1:length(x), 10),
           sample(1:nrow(X), 1),
           sample(1:nrow(X), 10),
           11:20,
           21:30,
           31:40)
tmp <- list(extract_draws_from_array(x, dr[[1]]),
            extract_draws_from_array(x, dr[[2]]),
            extract_draws_from_array(X, dr[[3]]),
            extract_draws_from_array(X, dr[[4]]),
            extract_draws(list(x = x, X = X), dr[[5]]),
            extract_draws_from_array(a1, dr[[6]]),
            extract_draws_from_array(a2, dr[[7]]))
sol_nrow <- c(length(dr[[1]]),
              length(dr[[2]]),
              length(dr[[3]]) * ncol(X),
              length(dr[[4]]) * ncol(X),
              length(dr[[5]]) * (1 + ncol(X)),
              length(dr[[6]]) * 2 * 2,
              length(dr[[7]]) * 2 * 2 * 2)

test_that("extract_draws (and related) returns dataframe of correct size", {
  for (i in 1:length(tmp)) {
    expect_true(is.data.frame(tmp[[1]]))
    expect_true(all(colnames(tmp[[i]]) %in% c("Draw", "Index", "Value", "Parameter")))
    expect_equal(nrow(tmp[[i]]), sol_nrow[i])
  }
})

test_that("extract_draws (and related) returns correct draws", {
  for (i in 1:length(tmp)) {
    expect_equal(sort(unique(tmp[[i]][["Draw"]])), sort(dr[[i]]))
    expect_true(!any(is.na(tmp[[i]][["Value"]])))
  }
})

test_that("extract_draws (and related) identify incorrect inputs", {
  expect_error(extract_draws_from_array(list(1, 2), 1))
  expect_error(extract_draws_from_array(rnorm(1e2), 0))
  expect_error(extract_draws_from_array(rnorm(1e2), 1e4))
  expect_error(extract_draws(data.frame(rnorm(1e2)), 1))
})

# extract_parameters_from_draw (deprecated) --------------------------------------------------------------

# In test-stan
