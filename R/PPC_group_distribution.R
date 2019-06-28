#' Posterior Predictive Check for Stan model
#'
#' Plot the distribution density of parameters within a same group from a single/multiple draw of the posterior distribution.
#' In the case of a hierarchical model, we might look at the distribution of patient parameter and compare it to the prior for the population distribution.
#'
#' @param fit stanfit object
#' @param parName name of the observation-dependent (e.g. patient-dependent) parameter to consider
#' @param nDraws number of draws to plot
#'
#' @return Ggplot of the distribution
#' @export
#'
#' @examples
#'
#' @import ggplot2
PPC_group_distribution <- function(fit, parName, nDraws = 1) {

  tmp <- rstan::extract(fit, parName)[[1]]
  tmp <- tmp[sample(1:nrow(tmp), nDraws), ]
  if (nDraws == 1) {
    tmp <- data.frame(Patient = 1:length(tmp), Draw = 1, Parameter = tmp)
  } else {
    tmp <- reshape2::melt(tmp,
                          varnames = c("Draw", "Patient"),
                          value.name = "Parameter")
  }
  tmp$Draw <- factor(tmp$Draw)

  ggplot(data = tmp, aes(x = Parameter, group = Draw)) +
    geom_density(colour = "#9ecae1") + # pastel blue
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = parName, y = "Density") +
    theme_classic(base_size = 20)
}
