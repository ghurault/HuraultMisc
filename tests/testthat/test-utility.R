# Test change_colnames ----------------------------------------------------

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

test_that("change_colnames catches incorrect inputs", {
  arr <- array(rnorm(1e3), dim = c(10, 10), dimnames = list(paste0("R", 1:10), paste0("C", 1:10)))
  expect_error(change_colnames(arr, c("C1"), c("V1")))
  expect_error(change_colnames(df0, 1, "V1"))
  expect_error(change_colnames(df0, c("A", "B"), c("Aa")))
})

# Test factor_to_numeric --------------------------------------------------

test_that("factor_to_numeric works", {

  x <- matrix(rbinom(3 * 100, 5, 0.5), ncol = 3)
  df0 <- data.frame(A = x[, 1], B = x[, 2], C = x[, 3])

  df1 <- df0
  df1[["A"]] <- factor(df1[["A"]])

  # One factor
  expect_equal(factor_to_numeric(df1, "A"), df0)
  expect_type(factor_to_numeric(df1, "A")$A, "double")

  # Multiple factors
  df1[["C"]] <- factor(df1[["C"]])
  df2 <- factor_to_numeric(df1, c("A", "C"))
  expect_equal(dim(df1), dim(df2))
  expect_equal(df1[["B"]], df2[["B"]])
  for (i in c("A", "C")) {
    expect_equal(df2[[i]], df0[[i]])
    expect_type(df2[[i]], "double")
  }
})

test_that("factor_to_numeric does not accept a list for df", {
  expect_error(factor_to_numeric(list(A = factor(1:10), B = "b"), "A"))
})

test_that("factor_to_numeric does not accept non-string for factor_name", {
  expect_error(factor_to_numeric(data.frame(A = c(1, 2), B = c(2, 3)), 1))
})

test_that("factor_to_numeric warns that variable cannot be converted to numeric", {
  expect_warning(factor_to_numeric(data.frame(A = factor(c(1, 2, "C"))), "A"))
})

test_that("factor_to_numeric warns that the input factor_name is not in df", {
  expect_warning(factor_to_numeric(data.frame(A = c(1, 2), B = c(2, 3)), "C"))
})

# Test extract_index ---------------------------------------------------

x <- c("sigma", "sigma[1]", "sigma1[1]", "sigma[1, 2]", "sigma[1,3]", "sigma[1][4]", "sigma[10,10]", "sigma[1][2][3]")
test_that("extract_index_1d returns a correct dataframe", {
  sol <- data.frame(Variable = c("sigma", "sigma", "sigma1", "sigma[1, 2]", "sigma[1,3]", "sigma[1]", "sigma[10,10]", "sigma[1][2]"),
                    Index = c(NA, 1, 1, NA, NA, 4, NA, 3))
  out <- extract_index_1d(x)
  expect_equal(out, sol)
  expect_is(out, "data.frame")
})

test_that("extract_index_nd returns a correct dataframe", {
  sol <- data.frame(Variable = c("sigma", "sigma", "sigma1", "sigma", "sigma", "sigma", "sigma", "sigma"))
  sol$Index <- list(NA, 1, 1, c(1, 2), c(1, 3), c(1, 4), c(10, 10), c(1, 2, 3))
  out <- extract_index_nd(x)
  expect_equal(out, sol)
  expect_is(out, "data.frame")
})

# Test logit --------------------------------------------------------------

test_that("logit works", {
  expect_equal(logit(0.5), 0)
  expect_equal(logit(c(0, 1)), c(-Inf, Inf))
  expect_warning(logit(2))
  expect_error(logit("a"))
})

test_that("inv_logit works", {
  expect_equal(inv_logit(0), 0.5)
  expect_equal(inv_logit(c(-Inf, Inf)), c(0, 1))
  expect_error(inv_logit("a"))
})

# Test approx_equal -------------------------------------------------------

test_that("approx_equal works", {
  expect_true(approx_equal(1, 1))
  expect_true(1 %~% (1 + 1e-16))
  expect_false(1 %~% 1.01)
  expect_error(1 %~% "a")
  expect_equal(approx_equal(1, c(1, 2)), c(TRUE, FALSE))
  expect_equal(approx_equal(c(1, 2), 1), c(TRUE, FALSE))
  expect_equal(approx_equal(c(1, 2), c(1, 2)), c(TRUE, TRUE))
})
