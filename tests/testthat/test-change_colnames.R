df0 <- data.frame(A = 1:2, B = 3:4, C = 5:6)
df1 <- change_colnames(df0, c("A", "C"), c("Aa", "Cc"))

test_that("change_colnames changes the column names", {
  expect_equal(colnames(df1)[1], "Aa")
  expect_equal(colnames(df1)[2], "B")
  expect_equal(colnames(df1)[3], "Cc")
})

test_that("changes_colnames leaves the content of the dataframe unchanged", {
  expect_equal(ncol(df1), ncol(df0))
  for (i in 1:ncol(df0)) {
    expect_equal(df0[, i], df1[, i])
  }
})

test_that("change_colnames catch errors", {
  arr <- array(rnorm(1e3), dim = c(10, 10), dimnames = list(paste0("R", 1:10), paste0("C", 1:10)))
  expect_error(change_colnames(arr, c("C1"), c("V1")))
  expect_error(change_colnames(df0, 1, "V1"))
  expect_error(change_colnames(df0, c("A", "B"), c("Aa")))
})
