# Precompiled stan model
# Can potentially compile model during build, but it still causes problem with the installed package size

compiled_model <- rstan::stan_model(file = "inst/stan/hierarchical_model.stan", save_dso = TRUE)

# saveRDS(compiled_model, file = "inst/testdata/hierarchical_compiled.rds")
