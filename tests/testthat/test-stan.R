# Initialisation ----------------------------------------------------------

set.seed(2021)
options(warn = -1)

N_patient <- 5
t_max <- rpois(N_patient, 15)
N <- sum(t_max)
max_score <- 100

param <- EczemaPred::list_parameters("BinMC")
param$Test <- NULL
N_parameters <- N * length(param$PatientTime) + N_patient * length(param$Patient) + length(param$Population)

fit_prior <- EczemaPred::sample_prior_BinMC(N_patient = N_patient, t_max = t_max, max_score = max_score, chains = 1, refresh = 0)

yrep <- rstan::extract(fit_prior, pars = "y_rep")[[1]]
fd <- EczemaPred::get_index2(t_max) %>%
  mutate(Score = yrep[5, ])

fit_fake <- EczemaPred::fit_BinMC(train = fd, test = NULL, max_score = max_score, chains = 1, refresh = 0)

# Test summary_statistics -------------------------------------------------------------------

par_prior <- summary_statistics(fit_prior, pars = unlist(param))
par_fake <- summary_statistics(fit_fake, pars = unlist(param))

test_that("summary_statistics returns the right content", {
  expect_equal(dim(par_fake), dim(par_prior))
  expect_equal(nrow(par_fake), N_parameters)
  expect_equal(nrow(par_fake[par_fake$Variable == "p10", ]), N_patient)
  expect_equal(nrow(par_fake[par_fake$Variable == "y_rep", ]), N)
  expect_equal(sort(par_fake$Index[par_fake$Variable == "p10"]), 1:N_patient)
  expect_true(all(is.na(par_fake$Index[par_fake$Variable %in% param$Population])))
})

test_that("summary_statistics catches wrong inputs", {
  expect_error(summary_statistics(rnorm(1e3), ""))
  expect_error(summary_statistics(fit_fake, param, paste0(seq(5, 95, 5), "%")))
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
  expect_true(all(unique(cpp1[["Variable"]]) %in% unlist(param)))

  cpp2 <- combine_prior_posterior(par_prior, par_fake, pars = "p10", match_exact = TRUE)
  expect_equal(as.character(unique(cpp2[["Variable"]])), "p10")

  par_prior2 <- par_prior %>% drop_na() %>% mutate(Variable = paste0(Variable, "[", Index, "]")) %>% select(-Index)
  par_fake2 <- par_fake %>% drop_na() %>% mutate(Variable = paste0(Variable, "[", Index, "]")) %>% select(-Index)

  cpp3 <- combine_prior_posterior(par_prior2, par_prior2, pars = "p10", match_exact = FALSE)
  expect_equal(as.character(unique(cpp2[["Variable"]])), "p10")
  expect_equal(nrow(cpp2), nrow(cpp3))

  expect_error(combine_prior_posterior(par_prior2, par_prior2, pars = "p10", match_exact = TRUE))
})

# Test plot_prior_posterior -----------------------------------------------

test_that("plot_prior_posterior returns a ggplot object", {
  expect_is(plot_prior_posterior(par_prior, par_fake, pars = param$Population), "ggplot")
})

test_that("plot_prior_posterior identifies incorrect inputs", {
  expect_error(plot_prior_posterior(par_prior, as.matrix(par_fake), pars = param$Population))
  expect_error(plot_prior_posterior(as.matrix(par_prior), par_fake, pars = param$Population))
  expect_error(plot_prior_posterior(as.matrix(par_prior), par_fake, pars = param$Population))
  expect_error(plot_prior_posterior(par_prior, par_fake, pars = as.list(param$Population)))
  expect_error(plot_prior_posterior(par_prior, par_fake, pars = "parameter_not_in_model"))

  tmp1 <- par_prior
  tmp1[["5%"]] <- NULL
  tmp2 <- par_fake
  tmp2[["5%"]] <- NULL
  expect_error(plot_prior_posterior(tmp1, tmp2, pars = param$Population))
})

# Test prior_influence --------------------------------------------

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

test_that("plot_prior_influence returns a ggplot object", {
  expect_is(plot_prior_influence(par_prior, par_fake, pars = unlist(param[c("Population", "Patient")])), "ggplot")
  expect_is(check_model_sensitivity(par_prior, par_fake, pars = unlist(param[c("Population", "Patient")])), "ggplot")
})

test_that("check_model_sensitivity identifies incorrect inputs", {
  expect_error(plot_prior_influence(par_prior, as.matrix(par_fake), pars = param$Population))
  expect_error(plot_prior_influence(as.matrix(par_prior), par_fake, pars = param$Population))
  expect_error(plot_prior_influence(as.matrix(par_prior), par_fake, pars = param$Population))
  expect_error(plot_prior_influence(par_prior, par_fake, pars = as.list(param$Population)))
})

# Test PPC_group_distribution ----------------------------------------------------

test_that("PPC_group_distribution returns a ggplot object for different inputs", {
  expect_is(PPC_group_distribution(fit_fake, "p10", 1), "ggplot")
  expect_is(PPC_group_distribution(fit_fake, "p10", 100), "ggplot")
  expect_is(PPC_group_distribution(rstan::extract(fit_fake, pars = "p10")[[1]], "p10", 1), "ggplot")
  expect_is(PPC_group_distribution(matrix(rnorm(1e3), ncol = 10), "p10", 10), "ggplot")
})

test_that("PPC_group_distribution incorrect inputs", {
  expect_error(PPC_group_distribution(fit_fake, 1))
  expect_error(PPC_group_distribution(fit_fake, "p10", 0))
  expect_error(PPC_group_distribution(fit_fake, "p10", 1e5))
  expect_error(PPC_group_distribution(fit_fake, c("p10", "y_rep")))
  expect_error(PPC_group_distribution(fit_fake, "parameter_not_in_model"))
})

# extract_distribution with stanfit object --------------------------------

test_that("extract_distribution works with stanfit object", {
  dist <- extract_distribution(fit_fake, parName = "y_rep", type = "discrete", support = c(0, max_score))
  expect_true(all(c("Value", "Probability", "Index", "Variable") %in% colnames(dist)))
  expect_equal(length(unique(dist[["Index"]])), N)
  expect_equal(range(dist[["Value"]]), c(0, max_score))
})

idx <- EczemaPred::get_index2(t_max = t_max)

test_that("process_replications output is in the correct support", {
  pred_cont <- process_replications(fit_fake, idx = idx, parName = "y_rep", bounds = c(0, max_score), type = "discrete")
  expect_equal(range(pred_cont[["y_rep"]]), c(0, max_score))
})

test_that("process_replications truncation works", {
  bd <- c(0, round(max_score / 5))
  pred_eti <- process_replications(fit_fake, idx = idx, parName = "y_rep", bounds = bd, type = "eti", CI_level = .99)
  expect_true(!any(!c(dplyr::between(pred_eti[["Lower"]], bd[1], bd[2]),
                      dplyr::between(pred_eti[["Upper"]], bd[1], bd[2])),
                   na.rm = TRUE))
})

test_that("process_replications failures and warnings", {
  expect_warning(process_replications(fit_fake, idx = NULL, parName = "y_rep", bounds = NULL)) # support warning from extract_distribution
  expect_error(process_replications(rnorm(1e3), idx = NULL, parName = "y_rep")) # fit is not a stanfit object
})

# extract_parameters_from_draw (deprecated) --------------------------------------------------------------

tmp <- extract_parameters_from_draw(fit_fake, unlist(param), 1)

test_that("extract_parameters_from_draw returns dataframe of correct size", {
  expect_equal(nrow(tmp), N_parameters)
})

test_that("extract_parameters_from_draw extract all parameters", {
  expect_equal(sort(as.character(unlist(param))),
               sort(as.character(unique(tmp[["Parameter"]]))))
})

test_that("extract_parameters_from_draw catches incorrect inputs", {
  expect_error(extract_parameters_from_draw(rnorm(1e3), "x", 1)) # not stanfit
  expect_warning(extract_parameters_from_draw(fit_fake, "p10", c(1, 2))) # multiple draws
})
