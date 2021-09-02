test_that("is_scalar() works", {
  expect_true(is_scalar(1))
  expect_true(is_scalar("a"))
  expect_false(is_scalar(c(1, 2)))
})

test_that("is_wholenumber() works", {
  expect_true(is_wholenumber(1))
  expect_true(is_wholenumber(1.0))
  expect_false(is_wholenumber(1.1))
  expect_true(is_scalar_wholenumber(1))
  expect_false(is_scalar_wholenumber(c(1, 2)))
  expect_equal(is_wholenumber(NA), NA)
})

test_that("is_stanfit() works", {
  expect_false(is_stanfit(matrix(rnorm(9), ncol = 3)))
  a <- 1
  class(a) <- "stanfit"
  expect_true(is_stanfit(a))
})
