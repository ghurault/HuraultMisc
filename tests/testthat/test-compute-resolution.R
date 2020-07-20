test_that("Computing resolution returns correct values", {
  expect_equal(compute_resolution(c(0, 1), 0.5), 1)
  expect_equal(compute_resolution(c(0.5, 0.5), 0.5), 0)
})

test_that("compute_resolution catches incorrect inputs", {
  expect_error(compute_resolution(c(-1, 1), 0.5))
  expect_error(compute_resolution(c(0, 1), -1))
})
