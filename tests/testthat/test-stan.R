# Initialisation ----------------------------------------------------------

load(system.file("testdata", "env_hierarchical.Rdata", package = "HuraultMisc", mustWork = TRUE))
fit_prior <- readRDS(system.file("testdata", "prior_hierarchical.rds", package = "HuraultMisc", mustWork = TRUE))
fit_fake <- readRDS(system.file("testdata", "fake_hierarchical.rds", package = "HuraultMisc", mustWork = TRUE))

# Test summary_statistics -------------------------------------------------------------------

par_prior <- summary_statistics(fit_prior, param)
par_fake <- summary_statistics(fit_fake, param)

test_that("summary_statistics returns the right content", {
  expect_equal(dim(par_fake), dim(par_prior))
  expect_equal(nrow(par_fake), N_parameters)
  expect_equal(nrow(par_fake[par_fake$Variable == "mu", ]), N_subject)
  expect_equal(nrow(par_fake[par_fake$Variable == "y_rep", ]), N)
  expect_equal(sort(par_fake$Index[par_fake$Variable == "mu"]), 1:N_subject)
  expect_true(all(is.na(par_fake$Index[par_fake$Variable %in% c("mu_pop", "sigma_pop")])))
})

test_that("summary_statistics catches wrong inputs", {
  expect_error(summary_statistics(rnorm(1e3), ""))
  expect_error(summary_statistics(fit_fake, param, paste0(seq(5, 95, 5), "%")))
  expect_error(summary_statistics(fit_fake, "parameter_not_in_model"))
  expect_error(summary_statistics(fit_fake, 1))
})

# Test plot_prior_posterior -----------------------------------------------

test_that("plot_prior_posterior returns a ggplot object", {
  expect_is(plot_prior_posterior(par_prior, par_fake, param_pop), "ggplot")
})

test_that("plot_prior_posterior identifies incorrect inputs", {
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

# Test check_model_sensitivity --------------------------------------------

test_that("check_model_sensitivity returns a ggplot object", {
  expect_is(check_model_sensitivity(par_prior, par_fake, c(param_pop, param_sub)), "ggplot")
})

test_that("check_model_sensitivity identifies incorrect inputs", {
  expect_error(check_model_sensitivity(par_prior, as.matrix(par_fake), param_pop))
  expect_error(check_model_sensitivity(as.matrix(par_prior), par_fake, param_pop))
  expect_error(check_model_sensitivity(as.matrix(par_prior), par_fake, param_pop))
  expect_error(check_model_sensitivity(par_prior, par_fake, as.list(param_pop)))
})

# Test PPC_group_distribution ----------------------------------------------------

test_that("PPC_group_distribution returns a ggplot object for different inputs", {
  expect_is(PPC_group_distribution(fit_fake, "mu", 1), "ggplot")
  expect_is(PPC_group_distribution(fit_fake, "mu", 100), "ggplot")
  expect_is(PPC_group_distribution(rstan::extract(fit_fake, pars = "mu")[[1]], "mu", 1), "ggplot")
  expect_is(PPC_group_distribution(matrix(rnorm(1e3), ncol = 10), "mu", 10), "ggplot")
})

test_that("PPC_group_distribution incorrect inputs", {
  expect_error(PPC_group_distribution(fit_fake, 1))
  expect_error(PPC_group_distribution(fit_fake, "mu", 0))
  expect_error(PPC_group_distribution(fit_fake, "mu", 1e5))
  expect_error(PPC_group_distribution(fit_fake, c("mu", "y_rep")))
  expect_error(PPC_group_distribution(fit_fake, "parameter_not_in_model"))
})
