# Extract summary statistics -----------------------------------------------------------

#' Extract summary statistics
#'
#' @param fit Stanfit object
#' @param pars Character vector of parameters to extract. Defaults to all parameters.
#' @param probs Numeric vector of quantiles to extract
#'
#'@section Alternative:
#' The tidybayes package offers an alternative to this function, for example:
#' `fit %>% tidy_draws() %>% gather_variables %>% mean_qi()`.
#' However, this does not provide information about Rhat or Neff, nor does it process the indexes.
#' The tidybayes package is more useful for summarising the distribution of a handful of parameters (using [tidybayes::spread_draws()]).
#'
#' @return Dataframe of posterior summary statistics
#' @export
summary_statistics <- function(fit, pars, probs = c(.05, .25, .5, .75, .95)) {

  stopifnot(is_stanfit(fit))

  par <- rstan::summary(fit, pars = pars, probs = probs)$summary
  par <- as.data.frame(par)
  colnames(par)[colnames(par) == "mean"] <- "Mean"
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
#' @param obj Matrix (rows: samples, cols: parameter) or Stanfit object
#' @param parName Name of the observation-dependent (e.g. patient-dependent) parameter to consider (optional when obj is a matrix)
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
    stopifnot(is.character(parName),
              length(parName) == 1)
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
