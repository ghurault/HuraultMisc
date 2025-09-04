# Extract summary statistics -----------------------------------------------------------

#' Extract summary statistics
#'
#' @param fit Stanfit object.
#' @param pars Character vector of parameters to extract. Defaults to all parameters.
#' @param probs Numeric vector of quantiles to extract.
#'
#' @section Alternative:
#' The [tidybayes](https://mjskay.github.io/tidybayes/) package offers an alternative to this function, for example:
#' `fit %>% tidy_draws() %>% gather_variables() %>% mean_qi()`.
#' However, this does not provide information about `Rhat` or `Neff`, nor does it process the indexes.
#' The tidybayes package is more useful for summarising the distribution of a handful of parameters (using `tidybayes::spread_draws()`).
#'
#' @return Dataframe of posterior summary statistics
#' @export
summary_statistics <- function(fit, pars, probs = c(.05, .25, .5, .75, .95)) {
  stopifnot(is_stanfit(fit))

  par <- rstan::summary(fit, pars = pars, probs = probs)$summary %>%
    as.data.frame() %>%
    rename(Mean = .data$mean)
  par <- cbind(par, extract_index_1d(rownames(par))) # Extract index for 1d array/vectors
  rownames(par) <- NULL

  return(par)
}

# PPC distribution for single draw ----------------------------------------

#' Posterior Predictive Check for Stan model
#'
#' Plot the distribution density of parameters within a same group from a single/multiple draw of the posterior distribution.
#' In the case of a hierarchical model, we might look at the distribution of patient parameter and compare it to the prior for the population distribution.
#'
#' @param obj Matrix (rows: samples, cols: parameter) or Stanfit object.
#' @param parName Name of the observation-dependent (e.g. patient-dependent) parameter to consider (optional when `obj` is a matrix).
#' @param nDraws Number of draws to plot
#'
#' @return Ggplot of the distribution
#' @references A. Gelman, J. B. B. Carlin, H. S. S. Stern, and D. B. B. Rubin, Bayesian Data Analysis (Chapter 6), Third Edition, 2014.
#' @export
#' @import ggplot2
#'
#' @examples
#' X <- matrix(rnorm(1e3), ncol = 10)
#' PPC_group_distribution(X, "", 10)
PPC_group_distribution <- function(obj, parName = "", nDraws = 1) {
  stopifnot(is_stanfit(obj) || is.matrix(obj))

  if (is_stanfit(obj)) {
    stopifnot(
      is.character(parName),
      is_scalar(parName)
    )
    par <- rstan::extract(obj, pars = parName)[[1]] # parName in obj checked here
  } else {
    par <- obj
  }

  max_draw <- nrow(par)
  if (nDraws < 1 | nDraws > max_draw) {
    stop("nDraws should be between 1 and ", max_draw, " (number of posterior samples)")
  }

  tmp <- extract_draws(par, sample(1:max_draw, nDraws, replace = FALSE))
  tmp$Draw <- factor(tmp$Draw)

  ggplot(data = tmp, aes_string(x = "Value", group = "Draw")) +
    geom_density(colour = "#9ecae1") + # pastel blue
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    labs(x = parName, y = "Density") +
    theme_classic(base_size = 15)
}

# R-squared ---------------------------------------------------------------

#' Compute Bayesian R-squared from matrix of posterior replications
#'
#' The approach is not fully Bayesian and provides a global estimate rather than an estimate for each sample
#' (this is because the predictive means and residual variance are estimated from replications than given by the model).
#'
#' @param yrep Matrix with rows representing samples and columns representing observations
#'
#' @return Bayesian R-squared (scalar, between 0 and 1)
#'
#' @export
#'
#' @references [A. Gelman, B. Goodrich, J. Gabry, and A. Vehtari, “R-squared for Bayesian Regression Models,” Am. Stat., vol. 73, no. 3, pp. 307–309, Jul. 2019](https://doi.org/10.1080/00031305.2018.1549100)
#'
#' @examples
#' N <- 50
#' N_sample <- 1e2
#' y <- runif(N, 0, 10)
#' yrep <- do.call(
#'   cbind,
#'   lapply(
#'     1:N,
#'     function(i) {
#'       y[i] + rnorm(N_sample)
#'     }
#'   )
#' )
#' compute_rsquared(yrep)
compute_rsquared <- function(yrep) {
  stopifnot(
    is.matrix(yrep),
    is.numeric(yrep)
  )

  n_smp <- nrow(yrep)
  mean_yrep <- apply(yrep, 2, mean)
  var_fit <- stats::var(mean_yrep)

  err <- yrep - matrix(rep(mean_yrep, each = n_smp), nrow = n_smp)
  var_res <- mean(apply(err, 1, stats::var))

  return(var_fit / (var_fit + var_res))
}
