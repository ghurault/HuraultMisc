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

test_that("compute_prior_influence returns correct values", {
  prior <- data.frame(Variable = c("a", "b", "b"),
                      Index = c(NA, 1, 2),
                      Mean = c(0, 1, NA),
                      sd = c(1, 2, NA)) # test that prior for b[2] does not matter
  post <- data.frame(Variable = c("a", "b", "b"),
                     Index = c(NA, 1, 2),
                     Mean = c(0, 1, 1.5),
                     sd = c(0.1, 1, 2))
  expected_output <- data.frame(Variable = c("a", "b", "b"),
                                Index = c(NA, 1, 2),
                                PostShrinkage = c(0.99, 0.75, 0),
                                DistPrior = c(0, 0, 0.25))
  expect_equal(compute_prior_influence(prior, post, c("a", "b")),
               expected_output)
})

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

# extract_distribution with stanfit object --------------------------------

test_that("extract_distribution works with stanfit object", {
  dist <- extract_distribution(fit_fake, parName = "y_rep", type = "continuous", support = c(-10, 10))
  expect_true(all(c("Value", "Density", "Index", "Variable") %in% colnames(dist)))
  expect_equal(length(unique(dist[["Index"]])), N)
  expect_equal(range(dist[["Value"]]), c(-10, 10))
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
