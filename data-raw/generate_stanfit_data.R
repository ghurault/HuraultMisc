# In this script, we fit a simple hierarchical model and save the model outputs.
# They will be used for testing functions using Stan

# We are using cmdstanr v2.34.0 so that the csv files can be converted to
# rstan's stanfit object, for backward compatibility.
# cf. https://github.com/stan-dev/rstan/issues/1133#issuecomment-2466698446

# Initialisation ----------------------------------------------------------

rm(list = ls())

set.seed(2025)

cmdstanr::set_cmdstan_path(file.path(dirname(cmdstanr::cmdstan_path()), "cmdstan-2.34.0"))

library(cmdstanr)
library(here)

stan_code <- here("data-raw/hierarchical_model.stan") # Model path

# Data
N_subject <- 20
N_repeat <- c(2, 7, 3, 4, 8, 9, 9, 6, 3, 8, 5, 7, 6, 3, 2, 5, 5, 11, 4, 7) # rpois(N_subject, 5)
N <- sum(N_repeat)

# Parameters of the model
params <- list(
  Population = c("mu_pop", "sigma_pop"),
  Subject = c("mu"),
  Observation = c("y_rep")
)

N_parameters <- N * length(params$Observation) + N_subject * length(params$Subject) + length(params$Population)

# Dataframe to translate observation parameters' indices into patient and time values
observations_dictionary <- function(data_stan) {
  tmp <- with(
    data_stan,
    data.frame(
      Subject = rep(1:N_subject, N_repeat),
      Repeat = do.call(c, lapply(N_repeat, function(x) {
        1:x
      }))
    )
  )
  tmp$Index <- 1:nrow(tmp)
  return(tmp)
}

mdl <- cmdstan_model(stan_code)

# Sample from prior predictive distribution ---------------------------------------------

data_prior <- list(
  N = N,
  N_subject = N_subject,
  N_repeat = N_repeat,
  y = rep(1, N), # doesn't matter
  run = 0
)
fit_prior <- mdl$sample(
  data = data_prior,
  chains = 4,
  thin = 2,
  save_warmup = FALSE,
  refresh = 0
)

# Check fit
fit_prior$diagnostic_summary()
bayesplot::mcmc_pairs(fit_prior$draws(), pars = params$Population)

# Fit fake data -----------------------------------------------------------

# Simulate fake data from prior
draw <- 10
y_sim <- fit_prior$draws("y_rep", format = "draws_matrix")[draw, ]

data_fake <- list(
  N = N,
  N_subject = N_subject,
  N_repeat = N_repeat,
  y = as.vector(y_sim),
  run = 1
)
fit_fake <- mdl$sample(
  data = data_fake,
  chains = 4,
  thin = 2,
  save_warmup = FALSE,
  refresh = 0
)

# Check fit
fit_fake$diagnostic_summary()
bayesplot::mcmc_pairs(fit_fake$draws(), pars = params$Population)

# Save stanfit objects ----------------------------------------------------

fit_prior$save_output_files(here("inst", "testdata", "stanmodel_prior"))
fit_fake$save_output_files(here("inst", "testdata", "stanmodel_post"))

cfg <- list(
  Parameters = params,
  Data = data_fake
)

saveRDS(cfg, file = here("inst", "testdata", "stanmodel_config.rds"))
