# Initialisation --------------------------------------------------------------------

# Fit a simple hierarchical model

# set.seed(1)

library(rstan)

# Data
N_subject <- 20
N_repeat <- c(2, 7, 3, 4, 8, 9, 9, 6, 3, 8, 5, 7, 6, 3, 2, 5, 5, 11, 4, 7) # rpois(N_subject, 5)
N <- sum(N_repeat)

# Parameters of the model
param_pop <- c("mu_pop", "sigma_pop")
param_sub <- c("mu")
param_obs <- c("y_rep")
param <- c(param_pop, param_sub, param_obs)

# Files
compiled_model <- readRDS(system.file("testdata", "hierarchical_compiled.rds", package = "HuraultMisc", mustWork = TRUE))

# Dataframe to translate observation parameters' indices into patient and time values
observations_dictionary <- function(data_stan) {
  tmp <- with(data_stan,
              data.frame(Subject = rep(1:N_subject, N_repeat),
                         Repeat = do.call(c, lapply(N_repeat, function(x) {1:x}))))
  tmp$Index <- 1:nrow(tmp)
  return(tmp)
}

# Fit and sample from prior ---------------------------------------------

data_prior <- list(N = N,
                   N_subject = N_subject,
                   N_repeat = N_repeat,
                   y = rep(1, N), # doesn't matter
                   run = 0)
fit_prior <- sampling(compiled_model, data = data_prior, refresh = 0)

# Check fit
# check_hmc_diagnostics(fit_prior)
# pairs(fit_prior, pars = param_pop)

# Fit fake data -----------------------------------------------------------

# Simulate fake data from prior
draw <- 2019
y_sim <- extract(fit_prior, pars = "y_rep")[[1]][draw, ]

data_fake <- list(N = N,
                  N_subject = N_subject,
                  N_repeat = N_repeat,
                  y = y_sim,
                  run = 1)
fit_fake <- sampling(compiled_model, data = data_fake, refresh = 0)

# Check fit
# check_hmc_diagnostics(fit_fake)
# pairs(fit_fake, pars = param_pop)

# Tests -------------------------------------------------------------------

par_prior <- summary_statistics(fit_prior, param)
par_fake <- summary_statistics(fit_fake, param)

truth <- rstan::extract(fit_prior, pars = "mu")[[1]][draw, ]
post_samples <- rstan::extract(fit_fake, pars = "mu")[[1]]

N_parameters <- N *length(param_obs) + N_subject * length(param_sub) + length(param_pop)

test_that("summary_statistics returns a correct dataframe", {
  expect_equal(dim(par_fake), dim(par_prior))
  expect_equal(nrow(par_fake), N_parameters)
  expect_equal(nrow(par_fake[par_fake$Variable == "mu", ]), N_subject)
  expect_equal(nrow(par_fake[par_fake$Variable == "y_rep", ]), N)
})

test_that("extract_distribution works with different objects", {

  # Stan input, continuous
  dist_stan <- extract_distribution(fit_fake, parName = "y_rep", type = "continuous", support = c(-10, 10))
  expect_true("Density" %in% colnames(dist_stan)) # check colnames

  # Matrix input, discrete
  dist_matrix <- extract_distribution(matrix(rnorm(1e3), ncol = 10), parName = "x", type = "discrete", support = -4:4)
  expect_equal(length(unique(dist_matrix[["Index"]])), 10) # check index length

  # Vector input, samples
  dist_vector <- extract_distribution(rnorm(1e3), parName = "x", type = "samples")
  expect_true(is.na(unique(dist_vector[["Index"]]))) # check index is NA
  expect_true("Draw" %in% colnames(dist_vector)) # check colnames

  # List input
  expect_error(extract_distribution(rstan::extract(fit_fake, pars = "y_rep"), parName = "x", type = "eti"))
})

idx <- observations_dictionary(data_fake)

pred_disc1 <- process_replications(fit_fake, idx, "y_rep", type = "discrete", bounds = c(-10, 10))
pred_eti <- process_replications(fit_fake, idx, "y_rep", type = "eti")
pred_hdi <- process_replications(fit_fake, idx, "y_rep", type = "hdi")

test_that("process_replications returns a correct dataframe", {
  expect_true("Probability" %in% colnames(pred_disc1)) # check colnames
  expect_equal(range(pred_disc1[["y_rep"]]), c(-10, 10)) # check support range
  expect_true("Level" %in% colnames(pred_eti)) # check colnames
  expect_true("Level" %in% colnames(pred_hdi)) # check colnames
})

test_that("extract_distribution or process_replications catch warnings and errors", {
  expect_warning(extract_distribution(fit_fake, parName = c("y_rep", "mu"))) # multiple parName
  expect_error(extract_distribution(rnorm(1e3), parName = "x", transform = "log")) # check error that transform not a function
  expect_error(process_replications(rnorm(1e3), idx = NULL, parName = "y_rep")) # check error that fit is not a stanfit object
  expect_warning(process_replications(fit_fake, idx, "y_rep", type = "samples", nDraws = 1e5)) # check nDraws warning
  expect_error(process_replications(fit_fake, idx, "y_rep", type = "spaghetti")) # check wrong type error
  expect_warning(process_replications(fit_fake, idx, "y_rep", type = "continuous", bounds = NULL)) # check support warning
  expect_warning(process_replications(fit_fake, idx, "y_rep", type = "discrete", bounds = NULL)) # check support warning
})

test_that("PPC_group_distribution returns a ggplot object", {
  expect_is(PPC_group_distribution(fit_fake, "mu", 1), "ggplot")
  expect_is(PPC_group_distribution(fit_fake, "mu", 100), "ggplot")
  expect_error(PPC_group_distribution(fit_fake, "mu", 0))
  expect_error(PPC_group_distribution(fit_fake, "mu", 1e5))
})

test_that("plot_prior_posterior returns a ggplot object", {
  expect_is(plot_prior_posterior(par_fake, par_prior, param_pop), "ggplot")
})

test_that("coverage is accurate", {
  cov_rmse <- with(compute_coverage(post_samples, truth),
                   sqrt(mean((Nominal - Coverage)^2)))
  expect_lt(cov_rmse, 0.25) # high tolerance because of potential variability between runs
})

test_that("compute_coverage catch errors", {
  expect_error(compute_coverage(post_samples, truth[-1]))
})

test_that("plot_coverage returns a ggplot object", {
  expect_is(plot_coverage(post_samples, truth), "ggplot")
})

test_that("extract_parameters_from_draw works", {
  tmp <- extract_parameters_from_draw(fit_prior, param, 1)
  expect_equal(nrow(tmp), N_parameters)
  expect_equal(sort(param), sort(as.character(unique(tmp[["Parameter"]]))))

  expect_warning(extract_parameters_from_draw(fit_prior, param, c(1, 2)))
  expect_warning(extract_parameters_from_draw(fit_prior, param, 0))
  expect_warning(extract_parameters_from_draw(fit_prior, param, 1e6))
})

test_that("parameters_intervals works", {

  CI_level <- list(seq(.1, .9, .1),
                   seq(.05, .95, .05))
  for (i in 1:length(CI_level)) {
    for (t in c("hdi", "eti")) {
      tmp <- parameters_intervals(fit_fake, param, CI_level[[i]], type = t)
      expect_equal(nrow(tmp), N_parameters * length(CI_level[[i]]))
      expect_equal(sort(param), sort(as.character(unique(tmp[["Variable"]]))))
    }
  }
  expect_error(parameters_intervals(fit_fake, param, c(seq(5, 95, 10))))

})
