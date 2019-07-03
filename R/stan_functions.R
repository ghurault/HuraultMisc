# Extract summary statistics -----------------------------------------------------------

#' Extract summary statistics
#'
#' @param fit Stanfit object
#' @param param Parameters to extract
#' @param quant Quantiles to extract
#'
#' @return Dataframe of posterior summary statistics
#' @export
summary_statistics <- function(fit, param, quant = c(.05, .25, .5, .75, .95)) {
  # Extract parameters' summary
  #
  # Args:
  # fit: stanfit object
  # param: parameters to extract
  # quant: Quantiles to extract
  #
  # Returns: dataframe containing posterior summary statistics of the parameters

  par <- rstan::summary(fit, pars = param, probs = quant)$summary
  par <- as.data.frame(par)

  par$Variable <- rownames(par)
  rownames(par) <- NULL
  colnames(par)[colnames(par) == "mean"] <- "Mean"

  par$Index <- NA
  # Fill index column if needed
  for (parName in param) {
    id_var <- grep(paste(parName, "\\[", sep = ""), par$Variable) # index of variables of interest in par
    par$Index[id_var] <- as.numeric(sub(".*\\[(.*)\\].*", "\\1", par$Variable[id_var], perl = TRUE)) # extract index in brackets
    par$Variable[id_var] <- parName
  }
  return(par)
}

# Process replications ----------------------------------------------------

#' Extract posterior predictive distribution
#'
#' The Posterior predictive distribution is extracted as a "continuous" function (cf. density), as a "discrete" function (cf. probability) or as "samples" (draws from the distribution)
#'
#' @param fit Stanfit object
#' @param idx Dataframe for translating the indices of the replication parameters into more informative variable
#' @param parName Name of the replication parameter
#' @param type Indicates how the distribution is summarised. Values can take "continuous", "discrete" or "samples".
#' @param bounds Bounds of the distribution (vector of length 2)
#' @param nDensity Number of equally spaced points at which the density is to be estimated (better to use a power of 2). Applies when type = "continuous".
#' @param nDraws Number of draws from the distribution. Applies when type = "samples"
#'
#' @return Dataframe
#' @export
process_replications <- function(fit, idx, parName, type = "continuous", bounds, nDensity = 2^7, nDraws = 100) {

  pred <- rstan::extract(fit, pars = parName)[[1]]
  pred <- as.data.frame(pred)

  if (type == "samples"){
    smp <- sample(1:nrow(pred), nDraws)
  }

  tmp <- do.call("rbind",
                 lapply(1:ncol(pred), function(x) {
                   tmp <- pred[, x]
                   tmp <- tmp[!(tmp < min(bounds) | tmp > max(bounds))] # truncate the distribution
                   if (type == "continuous") {
                     d <- density(tmp, kernel = "gaussian", from = min(bounds), to = max(bounds), n = nDensity) # select a power of 2 for n, not too much or it takes memory
                     data.frame(S = d$x, Density = d$y, Index = x)
                   } else if (type == "discrete") {
                     tmp <- round(tmp)
                     d <- table(factor(tmp, levels = min(bounds):max(bounds)))
                     data.frame(S = min(bounds):max(bounds), Probability = as.numeric(d / sum(d)), Index = x)
                   } else if (type == "samples") {
                     data.frame(S = tmp[smp], Draw = 1:nDraws, Index = x)
                   } else {
                     stop("type should be either continuous, discrete or samples")
                   }
                 }))
  tmp <- change_colnames(tmp, "S", parName)
  tmp <- merge(tmp, idx, by = "Index", all = TRUE)
  tmp$Index <- NULL

  return(tmp)
}

# PPC distribution for single draw ----------------------------------------

#' Posterior Predictive Check for Stan model
#'
#' Plot the distribution density of parameters within a same group from a single/multiple draw of the posterior distribution.
#' In the case of a hierarchical model, we might look at the distribution of patient parameter and compare it to the prior for the population distribution.
#'
#' @param fit Stanfit object
#' @param parName Name of the observation-dependent (e.g. patient-dependent) parameter to consider
#' @param nDraws Number of draws to plot
#'
#' @return ggplot of the distribution
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
