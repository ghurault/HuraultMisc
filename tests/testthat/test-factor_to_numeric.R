test_that("factor_to_numeric works", {

  x <- matrix(rbinom(3 * 100, 5, 0.5), ncol = 3)
  df0 <- data.frame(A = x[, 1], B = x[, 2], C = x[, 3])

  df1 <- df0
  df1[["A"]] <- factor(df1[["A"]])
  df1[["C"]] <- factor(df1[["C"]])

  df2 <- factor_to_numeric(df1, c("A", "C"))

  expect_equal(dim(df1), dim(df2))
  expect_equal(df1[["B"]], df2[["B"]])

  for (i in c("A", "C")) {
    expect_equal(df2[[i]], df0[[i]])
    expect_type(df2[[i]], "double")
  }
})

test_that("factor_to_numeric catch warnings", {
  expect_warning(factor_to_numeric(data.frame(A = c(1, 2, "C")), "A")) # cannot convert to numeric
  expect_warning(factor_to_numeric(data.frame(A = c(1, 2), B = c(2, 3)), "C")) # factor_name not in colnames(df)
})
