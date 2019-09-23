test_that("Changing column names work", {
  df0 <- data.frame(A = 1:2, B = 3:4, C = 5:6)
  df1 <- change_colnames(df0, c("A", "C"), c("Aa", "Cc"))
  expect_equal(colnames(df1)[1], "Aa")
  expect_equal(colnames(df1)[2], "B")
  expect_equal(colnames(df1)[3], "Cc")
})

test_that("Checking dataframe is unchanged", {
  df0 <- data.frame(A = 1:2, B = 3:4, C = 5:6)
  df1 <- change_colnames(df0, c("A", "C"), c("Aa", "Cc"))
  expect_equal(ncol(df1), ncol(df0))
  for (i in 1:ncol(df0)) {
    expect_equal(df0[, i], df1[, i])
  }
})

test_that("Test errors", {
  df0 <- data.frame(A = 1:2, B = 3:4, C = 5:6)
  expect_error(change_colnames(df0, c("A", "B"), c("Aa")))
})
