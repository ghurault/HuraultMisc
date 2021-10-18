# Test type continuous ----------------------------------------------------

x <- rnorm(1e4)
X <- matrix(x, ncol = 100)

test_that("extract_distribution() works for vector with type continuous", {
  dist <- extract_distribution(x,
                               parName = "x",
                               type = "continuous",
                               support = c(-10, 10))
  expect_true(all(c("Value", "Density", "Index", "Variable") %in% colnames(dist)))
  expect_true(is.na(unique(dist[["Index"]])))
  expect_equal(range(dist[["Value"]]), c(-10, 10))
})

test_that("extract_distribution() works for matrix with type continuous", {
  dist <- extract_distribution(X,
                               parName = "X",
                               type = "continuous",
                               support = c(-10, 10))
  expect_true(all(c("Value", "Density", "Index", "Variable") %in% colnames(dist)))
  expect_equal(length(unique(dist[["Index"]])), 100)
  expect_equal(range(dist[["Value"]]), c(-10, 10))
})

# Test type discrete ------------------------------------------------------

test_that("extract_distribution() works for discrete numeric vector", {
  x <- round(x * 10)
  dist <- extract_distribution(x,
                               parName = "x",
                               type = "discrete",
                               support = min(x):max(x))
  expect_true(all(c("Value", "Probability", "Index", "Variable") %in% colnames(dist)))
  expect_equal(range(dist[["Value"]]), range(x))
})

test_that("extract_distribution() works for discrete non-numeric vector", {
  tmp <- list(
    extract_distribution(sample(LETTERS, 1e2, replace = TRUE),
                         type = "discrete",
                         support = LETTERS),
    extract_distribution(sample(LETTERS, 1e2, replace = TRUE),
                         type = "discrete") %>%
      expect_warning()
  )
  for (i in seq_along(tmp)) {
    expect_true(all(c("Value", "Probability") %in% colnames(tmp[[i]])))
  }
})

# Test type eti and hdi ---------------------------------------------------

CI_level <- list(seq(.1, .9, .1),
                 seq(.05, .95, .05))

for (t in c("eti", "hdi")) {
  test_that(paste0("extract_distribution() works for vector with type ", t), {
    for (i in 1:length(CI_level)) {
      dist <- extract_distribution(x, parName = "x", type = t, CI_level = CI_level[[i]])
      expect_true(all(c("Lower", "Upper", "Level", "Index", "Variable") %in% colnames(dist)))
      expect_equal(nrow(dist), length(CI_level[[i]]))
    }
  })
}

# Errors and warnings -----------------------------------------------------

test_that("extract_distribution() catches errors and warnings",{
  # Wrong input
  expect_error(extract_distribution(list(x), parName = "x"))
  # Wrong type
  expect_error(extract_distribution(x, parName = "x", type = "pdf"))
  # Wrong CI level
  expect_error(extract_distribution(x, parName = "x", type = "eti", CI_level = seq(5, 95, 10)))
  expect_error(extract_distribution(x, parName = "x", type = "eti", CI_level = c("50%", "95%")))
  # Transform not a function
  expect_error(extract_distribution(x, parName = "x", transform = "log"))
  # NULL support
  expect_warning(extract_distribution(x, parName = "x", type = "continuous"))
  expect_warning(extract_distribution(x, parName = "x", type = "discrete"))
  # Multiple parName
  expect_warning(extract_distribution(x, parName = c("x", "y")))
})
