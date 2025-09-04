# Initialisation ----------------------------------------------------------

set.seed(2021)
options(warn = -1)

# Load test data
cfg <- readRDS(system.file("testdata", "stanmodel_config.rds", package = "HuraultMisc", mustWork = TRUE))
fit_prior <- rstan::read_stan_csv(
  dir(system.file("testdata", "stanmodel_prior", package = "HuraultMisc", mustWork = TRUE), full.names = TRUE)
)
fit_fake <- rstan::read_stan_csv(
  dir(system.file("testdata", "stanmodel_post", package = "HuraultMisc", mustWork = TRUE), full.names = TRUE)
)

N_parameters <- with(
  cfg,
  Data$N * length(Parameters$Observation) + Data$N_subject * length(Parameters$Subject) + length(Parameters$Population)
)

yrep <- rstan::extract(fit_prior, pars = "y_rep")[[1]]

# Helpers -----------------------------------------------------------------

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

# Test summary_statistics -------------------------------------------------------------------

par_prior <- summary_statistics(fit_prior, pars = unlist(cfg$Parameters))
par_fake <- summary_statistics(fit_fake, pars = unlist(cfg$Parameters))

test_that("summary_statistics returns the right content", {
  expect_equal(dim(par_fake), dim(par_prior))
  expect_equal(nrow(par_fake), N_parameters)
  expect_equal(nrow(par_fake[par_fake$Variable == "mu", ]), cfg$Data$N_subject)
  expect_equal(nrow(par_fake[par_fake$Variable == "y_rep", ]), cfg$Data$N)
  expect_equal(sort(par_fake$Index[par_fake$Variable == "mu"]), 1:cfg$Data$N_subject)
  expect_true(all(is.na(par_fake$Index[par_fake$Variable %in% cfg$Parameters$Population])))
})

test_that("summary_statistics catches wrong inputs", {
  expect_error(summary_statistics(rnorm(1e3), ""))
  expect_error(summary_statistics(fit_fake, cfg$Parameters, paste0(seq(5, 95, 5), "%")))
  expect_error(summary_statistics(fit_fake, "parameter_not_in_model"))
  expect_error(summary_statistics(fit_fake, 1))
})

# Test combine_prior_posterior --------------------------------------------

test_that("combine_prior_posterior works", {
  cpp1 <- combine_prior_posterior(par_prior, par_fake, pars = NULL, match_exact = TRUE)
  expect_true(is.data.frame(cpp1))
  expect_true(all(colnames(par_prior) %in% colnames(cpp1)))
  expect_true(all(colnames(par_fake) %in% colnames(cpp1)))
  expect_true("Distribution" %in% colnames(cpp1))
  expect_true(all(as.character(unique(cpp1[["Distribution"]])) %in% c("Prior", "Posterior")))
  expect_true(all(unique(cpp1[["Variable"]]) %in% unlist(cfg$Parameters)))

  cpp2 <- combine_prior_posterior(par_prior, par_fake, pars = "mu", match_exact = TRUE)
  expect_equal(as.character(unique(cpp2[["Variable"]])), "mu")

  par_prior2 <- par_prior %>%
    drop_na() %>%
    mutate(Variable = paste0(Variable, "[", Index, "]")) %>%
    select(-Index)
  par_fake2 <- par_fake %>%
    drop_na() %>%
    mutate(Variable = paste0(Variable, "[", Index, "]")) %>%
    select(-Index)

  cpp3 <- combine_prior_posterior(par_prior2, par_prior2, pars = "mu", match_exact = FALSE)
  expect_equal(as.character(unique(cpp2[["Variable"]])), "mu")
  expect_equal(nrow(cpp2), nrow(cpp3))

  expect_error(combine_prior_posterior(par_prior2, par_prior2, pars = "mu", match_exact = TRUE))
})

# Test plot_prior_posterior -----------------------------------------------

test_that("plot_prior_posterior returns a ggplot object", {
  expect_is(plot_prior_posterior(par_prior, par_fake, pars = cfg$Parameters$Population), "ggplot")
})

test_that("plot_prior_posterior identifies incorrect inputs", {
  expect_error(plot_prior_posterior(par_prior, as.matrix(par_fake), pars = cfg$Parameters$Population))
  expect_error(plot_prior_posterior(as.matrix(par_prior), par_fake, pars = cfg$Parameters$Population))
  expect_error(plot_prior_posterior(as.matrix(par_prior), par_fake, pars = cfg$Parameters$Population))
  expect_error(plot_prior_posterior(par_prior, par_fake, pars = as.list(cfg$Parameters$Population)))
  expect_error(plot_prior_posterior(par_prior, par_fake, pars = "parameter_not_in_model"))

  tmp1 <- par_prior
  tmp1[["5%"]] <- NULL
  tmp2 <- par_fake
  tmp2[["5%"]] <- NULL
  expect_error(plot_prior_posterior(tmp1, tmp2, pars = cfg$Parameters$Population))
})

# Test prior_influence --------------------------------------------

test_that("compute_prior_influence returns correct values", {
  prior <- data.frame(
    Variable = c("a", "b", "b"),
    Index = c(NA, 1, 2),
    Mean = c(0, 1, NA),
    sd = c(1, 2, NA)
  ) # test that prior for b[2] does not matter
  post <- data.frame(
    Variable = c("a", "b", "b"),
    Index = c(NA, 1, 2),
    Mean = c(0, 1, 1.5),
    sd = c(0.1, 1, 2)
  )
  expected_output <- data.frame(
    Variable = c("a", "b", "b"),
    Index = c(NA, 1, 2),
    PostShrinkage = c(0.99, 0.75, 0),
    DistPrior = c(0, 0, 0.25)
  )
  expect_equal(
    compute_prior_influence(prior, post, c("a", "b")),
    expected_output
  )
})

test_that("plot_prior_influence returns a ggplot object", {
  expect_is(plot_prior_influence(par_prior, par_fake, pars = unlist(cfg$Parameters[c("Population", "Subject")])), "ggplot")
  expect_is(check_model_sensitivity(par_prior, par_fake, pars = unlist(cfg$Parameters[c("Population", "Subject")])), "ggplot")
})

test_that("check_model_sensitivity identifies incorrect inputs", {
  expect_error(plot_prior_influence(par_prior, as.matrix(par_fake), pars = cfg$Parameters$Population))
  expect_error(plot_prior_influence(as.matrix(par_prior), par_fake, pars = cfg$Parameters$Population))
  expect_error(plot_prior_influence(as.matrix(par_prior), par_fake, pars = cfg$Parameters$Population))
  expect_error(plot_prior_influence(par_prior, par_fake, pars = as.list(cfg$Parameters$Population)))
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

# Test compute_rsquared ---------------------------------------------------

test_that("compute_rsquared runs", {
  rsq <- compute_rsquared(yrep)
  expect_true(dplyr::between(rsq, 0, 1))
})

test_that("compute_rsquared catches incorrect input", {
  expect_error(compute_rsquared(as.character(yrep)))
  expect_error(compute_rsquared(yrep[, 1]))
})

# extract_distribution with stanfit object --------------------------------

test_that("extract_distribution works with stanfit object", {
  dist <- extract_distribution(fit_fake, parName = "y_rep", type = "continuous", support = c(-10, 10))
  expect_true(all(c("Value", "Density", "Index", "Variable") %in% colnames(dist)))
  expect_equal(length(unique(dist[["Index"]])), cfg$Data$N)
  expect_equal(range(dist[["Value"]]), c(-10, 10))
})

idx <- observations_dictionary(cfg$Data)

test_that("process_replications output is in the correct support", {
  pred_cont <- process_replications(fit_fake, idx = idx, parName = "y_rep", bounds = c(-10, 10), type = "continuous")
  expect_equal(range(pred_cont[["y_rep"]]), c(-10, 10))
})

test_that("process_replications truncation works", {
  bd <- c(-5, 5)
  pred_eti <- process_replications(fit_fake, idx = idx, parName = "y_rep", bounds = bd, type = "eti", CI_level = .99)
  expect_true(!any(
    !c(
      dplyr::between(pred_eti[["Lower"]], bd[1], bd[2]),
      dplyr::between(pred_eti[["Upper"]], bd[1], bd[2])
    ),
    na.rm = TRUE
  ))
})

test_that("process_replications failures and warnings", {
  expect_warning(process_replications(fit_fake, idx = NULL, parName = "y_rep", bounds = NULL)) # support warning from extract_distribution
  expect_error(process_replications(rnorm(1e3), idx = NULL, parName = "y_rep")) # fit is not a stanfit object
})

# extract_parameters_from_draw (deprecated) --------------------------------------------------------------

tmp <- extract_parameters_from_draw(fit_fake, unlist(cfg$Parameters), 1)

test_that("extract_parameters_from_draw returns dataframe of correct size", {
  expect_equal(nrow(tmp), N_parameters)
})

test_that("extract_parameters_from_draw extract all parameters", {
  expect_equal(
    sort(as.character(unlist(cfg$Parameters))),
    sort(as.character(unique(tmp[["Parameter"]])))
  )
})

test_that("extract_parameters_from_draw catches incorrect inputs", {
  expect_error(extract_parameters_from_draw(rnorm(1e3), "x", 1)) # not stanfit
  expect_warning(extract_parameters_from_draw(fit_fake, "mu", c(1, 2))) # multiple draws
})
