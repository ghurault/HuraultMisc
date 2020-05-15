# In this script, we fit a simple hierarchical model and save the stanfit objects
# They will be used for testing functions using Stan

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Make sure workspace as we will save part of it

set.seed(1)

library(rstan)

# Model path
code <- "data-raw/hierarchical_model.stan"

# Data
N_subject <- 20
N_repeat <- c(2, 7, 3, 4, 8, 9, 9, 6, 3, 8, 5, 7, 6, 3, 2, 5, 5, 11, 4, 7) # rpois(N_subject, 5)
N <- sum(N_repeat)

# Parameters of the model
param_pop <- c("mu_pop", "sigma_pop")
param_sub <- c("mu")
param_obs <- c("y_rep")
param <- c(param_pop, param_sub, param_obs)
N_parameters <- N *length(param_obs) + N_subject * length(param_sub) + length(param_pop)

# Dataframe to translate observation parameters' indices into patient and time values
observations_dictionary <- function(data_stan) {
  tmp <- with(data_stan,
              data.frame(Subject = rep(1:N_subject, N_repeat),
                         Repeat = do.call(c, lapply(N_repeat, function(x) {1:x}))))
  tmp$Index <- 1:nrow(tmp)
  return(tmp)
}

compiled_model <- stan_model(code)

# Sample from prior predictive distribution ---------------------------------------------

data_prior <- list(N = N,
                   N_subject = N_subject,
                   N_repeat = N_repeat,
                   y = rep(1, N), # doesn't matter
                   run = 0)
fit_prior <- sampling(compiled_model,
                      data = data_prior,
                      chains = 4,
                      iter = 1000,
                      thin = 2,
                      save_warmup = FALSE,
                      refresh = 0)

# Check fit
check_hmc_diagnostics(fit_prior)
pairs(fit_prior, pars = param_pop)

# Fit fake data -----------------------------------------------------------

# Simulate fake data from prior
draw <- 10
y_sim <- extract(fit_prior, pars = "y_rep")[[1]][draw, ]

data_fake <- list(N = N,
                  N_subject = N_subject,
                  N_repeat = N_repeat,
                  y = y_sim,
                  run = 1)
fit_fake <- sampling(compiled_model,
                     data = data_fake,
                     chains = 4,
                     iter = 1000,
                     thin = 2,
                     save_warmup = FALSE,
                     refresh = 0)

# Check fit
check_hmc_diagnostics(fit_fake)
pairs(fit_fake, pars = param_pop)

# Save stanfit objects ----------------------------------------------------

save(list = setdiff(ls(), c("fit_prior", "fit_fake", "compiled_model", "code", "y_sim")),
     file = "inst/testdata/env_hierarchical.Rdata")
saveRDS(fit_prior,
        file = "inst/testdata/prior_hierarchical.rds")
saveRDS(fit_fake,
        file = "inst/testdata/fake_hierarchical.rds")
