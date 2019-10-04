# Precompiled stan model
# Can potentially compile model during build, but it still causes problem with the installed package size

library(rstan)
compiled_model <- stan_model(file = "inst/stan/hierarchical_model.stan")

# saveRDS(compiled_model, file = "inst/testdata/hierarchical_compiled.rds")
