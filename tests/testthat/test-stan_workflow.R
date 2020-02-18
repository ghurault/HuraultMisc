# Initialisation ----------------------------------------------------------

source("init-stan_workflow.R")

# Test summary_statistics -------------------------------------------------------------------

par_prior <- summary_statistics(fit_prior, param)
par_fake <- summary_statistics(fit_fake, param)

test_that("summary_statistics returns a correct dataframe", {
  expect_equal(dim(par_fake), dim(par_prior))
  expect_equal(nrow(par_fake), N_parameters)
  expect_equal(nrow(par_fake[par_fake$Variable == "mu", ]), N_subject)
  expect_equal(nrow(par_fake[par_fake$Variable == "y_rep", ]), N)
})

test_that("summary_statistics catches errors", {
  expect_error(summary_statistics(rnorm(1e3), ""))
  expect_error(summary_statistics(fit_fake, param, paste0(seq(5, 95, 5), "%")))
  expect_error(summary_statistics(fit_fake, "parameter_not_in_model"))
  expect_error(summary_statistics(fit_fake, 1))
})

# Test extract_distribution and process_replications ----------------------

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

# Test coverage -----------------------------------------------------------

truth <- rstan::extract(fit_prior, pars = "mu")[[1]][draw, ]
post_samples <- rstan::extract(fit_fake, pars = "mu")[[1]]

for (t in c("eti", "hdi")) {
  test_that("coverage is accurate", {
    cov_rmse <- with(compute_coverage(post_samples, truth, type = t),
                     sqrt(mean((Nominal - Coverage)^2)))
    expect_lt(cov_rmse, 0.25) # high tolerance because of potential variability between runs
  })
}

test_that("compute_coverage catch errors", {
  expect_error(compute_coverage(post_samples, truth[-1]))
  expect_error(compute_coverage(data.frame(post_samples), truth))
  expect_error(compute_coverage(post_samples, data.frame(truth)))
  expect_error(compute_coverage(post_samples, as.character(truth)))
  expect_error(compute_coverage(post_samples, truth, paste0(seq(0, 100, 5), "%")))
  expect_error(compute_coverage(post_samples, truth, seq(0, 100, 5)))
})

test_that("plot_coverage returns a ggplot object", {
  expect_is(plot_coverage(post_samples, truth), "ggplot")
})

# Test plot_prior_posterior -----------------------------------------------

test_that("plot_prior_posterior returns a ggplot object", {
  expect_is(plot_prior_posterior(par_prior, par_fake, param_pop), "ggplot")
})

test_that("plot_prior_posterior catches errors", {
  expect_error(plot_prior_posterior(par_prior, as.matrix(par_fake), param_pop))
  expect_error(plot_prior_posterior(as.matrix(par_prior), par_fake, param_pop))
  expect_error(plot_prior_posterior(as.matrix(par_prior), par_fake, param_pop))
  expect_error(plot_prior_posterior(par_prior, par_fake, as.list(param_pop)))
  expect_error(plot_prior_posterior(par_prior, par_fake, "parameter_not_in_model"))

  tmp1 <- par_prior
  tmp1[["5%"]] <- NULL
  tmp2 <- par_fake
  tmp2[["5%"]] <- NULL
  expect_error(plot_prior_posterior(tmp1, tmp2, param_pop))
})

# Test extract_draws (and related functions) ---------------------------------------

test_that("extract_draws and related function works", {
  x <- rnorm(1e3)
  X <- matrix(x, ncol = 10)

  dr <- list(sample(1:length(x), 1),
             sample(1:length(x), 10),
             sample(1:nrow(X), 1),
             sample(1:nrow(X), 10),
             11:20)
  tmp <- list(extract_draws_from_array(x, dr[[1]]),
              extract_draws_from_array(x, dr[[2]]),
              extract_draws_from_array(X, dr[[3]]),
              extract_draws_from_array(X, dr[[4]]),
              extract_draws(list(x = x, X = X), dr[[5]]))
  sol_nrow <- c(length(dr[[1]]),
                length(dr[[2]]),
                length(dr[[3]]) * ncol(X),
                length(dr[[4]]) * ncol(X),
                length(dr[[5]]) * (1 + ncol(X)))
  sol_ncol <- c(3, 3, 3, 3, 4)

  for (i in 1:length(tmp)) {
    # Size dataframe
    expect_equal(nrow(tmp[[i]]), sol_nrow[i])
    expect_equal(ncol(tmp[[i]]), sol_ncol[i])
    # Content dataframe
    expect_equal(sort(unique(tmp[[i]][["Draw"]])), sort(dr[[i]]))
    expect_true(!any(is.na(tmp[[i]][["Value"]])))
  }

  tmp <- extract_parameters_from_draw(fit_prior, param, 1)
  expect_equal(nrow(tmp), N_parameters)
  expect_equal(sort(param), sort(as.character(unique(tmp[["Parameter"]]))))
})

test_that("extract_draws and related functions catch warnings and errors", {
  expect_error(extract_draws_from_array(list(1, 2), 1))
  expect_error(extract_draws_from_array(rnorm(1e2), 0))
  expect_error(extract_draws_from_array(rnorm(1e2), 1e4))
  expect_error(extract_draws(data.frame(rnorm(1e2)), 1))

  expect_error(extract_parameters_from_draw(rnorm(1e3), "x", 1))
  expect_warning(extract_parameters_from_draw(fit_fake, "mu", c(1, 2)))
})

# Test PPC_group_distribution ----------------------------------------------------

test_that("PPC_group_distribution returns a ggplot object", {
  expect_is(PPC_group_distribution(fit_fake, "mu", 1), "ggplot")
  expect_is(PPC_group_distribution(fit_fake, "mu", 100), "ggplot")
})

test_that("PPC_group_distribution catches errors and warnings", {
  expect_error(PPC_group_distribution(fit_fake, 1))
  expect_error(PPC_group_distribution(fit_fake, "mu", 0))
  expect_error(PPC_group_distribution(fit_fake, "mu", 1e5))
  expect_error(PPC_group_distribution(matrix(rnorm(1e3), ncol = 10), "mu"))
  expect_error(PPC_group_distribution(fit_fake, c("mu", "y_rep")))
  expect_error(PPC_group_distribution(fit_fake, "parameter_not_in_model"))
})
