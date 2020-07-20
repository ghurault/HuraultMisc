load(system.file("testdata", "env_hierarchical.Rdata", package = "HuraultMisc", mustWork = TRUE))
fit_fake <- readRDS(system.file("testdata", "fake_hierarchical.rds", package = "HuraultMisc", mustWork = TRUE))

test_that("extract_distribution works with different objects", {

  obj <- list(fit_fake,
              matrix(rnorm(1e3), ncol = 10),
              rnorm(1e3))

  for (i in 1:length(obj)) {
    dist <- extract_distribution(obj[[i]], parName = "y_rep", type = "continuous", support = c(-10, 10))
    expect_true("Density" %in% colnames(dist)) # check colnames
    # check index
    if (i == 1) {
      expect_equal(length(unique(dist[["Index"]])), N)
    } else if (i == 2) {
      expect_equal(length(unique(dist[["Index"]])), 10)
    } else if (i == 3) {
      expect_true(is.na(unique(dist[["Index"]])))
    }
  }
  expect_error(extract_distribution(rstan::extract(fit_fake, pars = "y_rep"), parName = "x", type = "eti")) # list input
})

test_that("extract_distribution works for different types", {

  # Continuous
  dist_cont <- extract_distribution(fit_fake, parName = "y_rep", type = "continuous", support = c(-10, 10))
  expect_true("Density" %in% colnames(dist_cont)) # check colnames
  expect_equal(range(dist_cont[["Value"]]), c(-10, 10)) # check support range

  # Discrete
  dist_disc <- extract_distribution(fit_fake, parName = "y_rep", type = "discrete", support = c(-10, 10))
  expect_true("Probability" %in% colnames(dist_disc)) # check colnames
  expect_equal(range(dist_disc[["Value"]]), c(-10, 10)) # check support range

  # Samples
  dist_samp <- extract_distribution(fit_fake, parName = "y_rep", type = "samples", nDraws = 100)
  expect_true("Draw" %in% colnames(dist_samp)) # check colnames
  expect_equal(nrow(dist_samp), 100 * N) # check nrow

  # ETI and HDI
  CI_level <- list(seq(.1, .9, .1),
                   seq(.05, .95, .05))
  for (i in 1:length(CI_level)) {
    for (t in c("hdi", "eti")) {
      dist_ci <- extract_distribution(fit_fake, parName = "y_rep", CI_level = CI_level[[i]], type = t)
      expect_equal(nrow(dist_ci), N * length(CI_level[[i]])) # check nrow
      expect_true("Level" %in% colnames(dist_ci)) # check colnames
    }
  }

  # Wrong type
  expect_error(extract_distribution(fit_fake, parName = "y_rep", type = "spaghetti"))

})

test_that("extract_distribution other failures and warnings", {
  expect_error(extract_distribution(fit_fake, parName = "y_rep", type = "hdi", CI_level = seq(5, 95, 10))) # error in CI_level
  expect_error(extract_distribution(fit_fake, parName = "y_rep", type = "hdi", CI_level = c("50%", "95%"))) # error in CI_level
  expect_warning(extract_distribution(fit_fake, parName = c("y_rep", "mu"))) # multiple parName
  expect_error(extract_distribution(fit_fake, parName = "y_rep", transform = "log")) # transform not a function
  expect_warning(extract_distribution(fit_fake, parName = "y_rep", type = "samples", nDraws = 1e5)) # nDraws warning
  expect_warning(extract_distribution(fit_fake, parName = "y_rep", type = "continuous", support = NULL)) # support warning
  expect_warning(extract_distribution(fit_fake, parName = "y_rep", type = "discrete", support = NULL)) # support warning
})

test_that("process_replications works", {
  idx <- observations_dictionary(data_fake)

  # test support
  pred_cont <- process_replications(fit_fake, idx = idx, parName = "y_rep", bounds = c(-10, 10), type = "continuous")
  expect_equal(range(pred_cont[["y_rep"]]), c(-10, 10)) # support works

  # test truncation
  pred_eti <- process_replications(fit_fake, idx = idx, parName = "y_rep", bounds = c(-5, 5), type = "eti", CI_level = .99)
  is_between <- function(x, lb, ub) {x >= lb & x <= ub}
  expect_true(!any(!c(is_between(pred_eti[["Lower"]], -5, 5), is_between(pred_eti[["Upper"]], -5, 5)), na.rm = TRUE))

})

test_that("process_replications failures and warnings", {
  expect_warning(process_replications(fit_fake, idx = NULL, parName = "y_rep", bounds = NULL)) # support warning from extract_distribution
  expect_error(process_replications(rnorm(1e3), idx = NULL, parName = "y_rep")) # fit is not a stanfit object
})
