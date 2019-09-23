test_that("Plot returns ggplot object", {
  expect_is(illustrate_RPS(), "ggplot")
  expect_is(illustrate_forward_chaining(), "ggplot")
})
