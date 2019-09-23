test_that("Transformation to numeric works", {
  x <- rep(seq(0, 1, .1), each = 10)
  df0 <- data.frame(A = x)
  df0$A <- factor(df0$A)
  df1 <- factor_to_numeric(df0, "A")
  expect_equal(df1$A, x)
  expect_type(df1$A, "double")
  expect_warning(factor_to_numeric(data.frame(A = c(1, 2, "C")), "A"))
})

test_that("Dataframe is unchanged", {
  df0 <- data.frame(A = 1:2, B = 3:4, C = 5:6)
  df1 <- factor_to_numeric(df0, c("A", "C"))
  expect_equal(dim(df1), dim(df0))
  expect_equal(df1$B, df0$B)
})
