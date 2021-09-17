# Empirical p-values ----------------------------------------------------------------

#' Compute empirical p-values
#'
#' @param t_rep Vector of samples from a distribution.
#' @param t Observation (numeric scalar).
#' @param alternative Indicates the alternative hypothesis: must be one of "two.sided", "greater" or "less".
#'
#' @return Empirical p-value.
#' @export
#'
#' @examples
#' empirical_pval(rnorm(1e2), 2)
empirical_pval <- function(t_rep, t, alternative = c("two.sided", "less", "greater")) {

  stopifnot(is.vector(t_rep, mode = "numeric"),
            length(t_rep) > 1,
            is.numeric(t),
            is_scalar(t))
  alternative <- match.arg(alternative)

  r <- sum(t_rep > t)
  n <- length(t_rep)
  p <- (r + 1) / (n + 1) # instead of r/n, since the r replicates and the data are assumed from the null

  if (alternative == "greater") {
    pval <- p
  } else if (alternative == "less") {
    pval <- 1 - p
  } else if (alternative == "two.sided") {
    pval <- 2 * min(p, 1 - p)
  }
  return(pval)
}

# Posterior predictive p-values -------------------------------------------

#' Posterior Predictive p-value
#'
#' Compute and plot posterior predictive p-value (Bayesian p-value) from samples of a distribution.
#' The simulations and observations are first summarised into a test statistics,
#' then the test statistic of the observations is compared to the test statistic of the empirical distribution.
#'
#' @param yrep Matrix of posterior replications with rows corresponding to samples and columns to simulated observations.
#' @param y Vector of observations.
#' @param test_statistic Function of the test statistic to compute the p-value for.
#' @param alternative Indicates the alternative hypothesis: must be one of `"two.sided"`, `"greater"` or `"less"`.
#' @param plot Whether to output a plot visualising the distribution of the test statistic.
#'
#' @return List containing the p-value and (optionally) a ggplot.
#' @export
#' @import ggplot2
#'
#' @examples
#' post_pred_pval(matrix(rnorm(1e3), ncol = 10), rnorm(10), plot = TRUE)
post_pred_pval <- function(yrep, y, test_statistic = mean, alternative = c("two.sided", "less", "greater"), plot = FALSE) {

  stopifnot(is.matrix(yrep),
            is.vector(y, mode = "numeric"),
            is.function(test_statistic),
            is.logical(plot),
            is_scalar(plot))
  alternative <- match.arg(alternative)

  if (ncol(yrep) != length(y)) {
    warning("not the same number of observations in yrep (ncol) and y: the test statistics might not be comparable")
  }

  t_rep <- apply(yrep, 1, test_statistic)
  t <- test_statistic(y)

  out <- list(pval = empirical_pval(t_rep, t, alternative))

  if (as.logical(plot)) {
    out[["plot"]] <- ggplot(data = data.frame(x = t_rep)) +
      geom_density(aes_string(x = "x"), fill = "#9ecae1", alpha = 0.8) +
      geom_vline(xintercept = t, size = 2) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = "test statistic") +
      theme_bw(base_size = 15)
  }
  return(out)
}
