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
#' @param bounds NULL or vector of length 2 representing the bounds of the distribution if it needs to be truncated.
#' @param nDensity Number of equally spaced points at which the density is to be estimated (better to use a power of 2). Applies when type = "continuous".
#' @param nDraws Number of draws from the distribution. Applies when type = "samples"
#'
#' @return Dataframe
#' @export
#' @import stats
process_replications <- function(fit, idx, parName, type = "continuous", bounds = NULL, nDensity = 2^7, nDraws = 100) {

  pred <- rstan::extract(fit, pars = parName)[[1]]
  if (is.null(bounds)) {
    # Even if the distribution is continuous, it needs to be truncated for type "continuous" or "discrete"
    bounds <- quantile(pred, probs = c(.001, 0.999))
  }
  pred <- as.data.frame(pred)

  if (type == "samples"){
    if (nDraws < 1 | nDraws > nrow(pred)) {
      stop("nDraws should be between 1 and ", nrow(pred), " (number of posterior samples)")
    } else {
      smp <- sample(1:nrow(pred), nDraws)
    }
  }

  tmp <- do.call("rbind",
                 lapply(1:ncol(pred), function(x) {
                   tmp <- pred[, x]
                   if (!is.null(bounds)) {
                     # truncate the distribution if bounds are provided
                     tmp <- tmp[!(tmp < min(bounds) | tmp > max(bounds))]
                   }
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
#' @import ggplot2
PPC_group_distribution <- function(fit, parName, nDraws = 1) {

  tmp <- rstan::extract(fit, parName)[[1]]
  if (nDraws < 1 | nDraws > nrow(tmp)) {
    stop("nDraws should be between 1 and ", nrow(tmp), " (number of posterior samples)")
  }
  tmp <- tmp[sample(1:nrow(tmp), nDraws), ]
  if (nDraws == 1) {
    tmp <- data.frame(Patient = 1:length(tmp), Draw = 1, Parameter = tmp)
  } else {
    tmp <- reshape2::melt(tmp,
                          varnames = c("Draw", "Patient"),
                          value.name = "Parameter")
  }
  tmp$Draw <- factor(tmp$Draw)

  ggplot(data = tmp, aes_string(x = "Parameter", group = "Draw")) +
    geom_density(colour = "#9ecae1") + # pastel blue
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = parName, y = "Density") +
    theme_classic(base_size = 20)
}

# Compare posterior to prior estimates ------------------------------------

#' Plot posterior estimates alongside prior estimates
#'
#' @param post Dataframe of posterior parameter estimates
#' @param prior Dataframe of prior parameter estimates
#' @param param Vector of parameter names to plot
#'
#' @return ggplot of parameter estimates
#' @export
#'
#' @import ggplot2
plot_prior_posterior <- function(post, prior, param) {

  post$Distribution <- "Posterior"
  prior$Distribution <- "Prior"
  tmp <- rbind(post[post[["Variable"]] %in% param, ],
               prior[prior[["Variable"]] %in% param, ])
  tmp$Distribution <- factor(tmp$Distribution, levels = c("Prior", "Posterior")) # to show posterior on top

  ggplot(data = tmp, aes_string(x = "Variable", y = "Mean", ymin = "`5%`", ymax = "`95%`", colour = "Distribution")) +
    geom_pointrange(position = position_dodge2(width = .3), size = 1.2) +
    scale_colour_manual(values = c("#E69F00", "#000000")) +
    coord_flip() +
    labs(colour = "", x = "", y = "Estimate") +
    theme_bw(base_size = 20) +
    theme(legend.position = "top")
}

# Plot coverage (not directly Stan-related) -----------------------------------------------------------

#' Plot coverage of CI for different confidence level.
#' Useful for fake data check.
#'
#' @param post_samples Matrix of posterior samples. Rows represent a sample and columns represent variables.
#' @param truth Vector of true parameter values (should be the same length as the number of columns in post_samples).
#' @param CI Vector of confidence levels.
#'
#' @return Ggplot of coverage as a function of the nominal coverage (confidence level), with 95% uncertainty interval
#' @export
#' @import ggplot2
#'
#' @examples
#' N <- 100
#' N_post <- 1e3
#' truth <- rep(0, N)
#' post_samples <- sapply(rnorm(N, 0, 1), function(x) {rnorm(N_post, x, 1)})
#' plot_coverage(post_samples, truth)
plot_coverage <- function(post_samples, truth, CI = seq(0, 1, 0.05)) {
  if (ncol(post_samples) != length(truth)) {
    stop("The number of columns in post_samples should be equal to the length of truth")
  }

  df <- do.call(rbind,
                lapply(1:ncol(post_samples),
                       function(i) {
                         # For each variable, compute Lower and Upper bounds for different confidence level, and check if the truth is in in the interval
                         tmp <- do.call(rbind,
                                        lapply(CI,
                                               function(ci) {
                                                 # Compute Lower and Upper bounds of CI for each confidence level
                                                 alpha <- 1 - ci
                                                 q <- quantile(post_samples[, i], probs = c(alpha / 2, 1 - alpha / 2))
                                                 data.frame(Nominal = ci, Variable = i, Lower = q[1], Upper = q[2])
                                               }))
                         rownames(tmp) <- NULL
                         tmp$Coverage <- (truth[i] > tmp$Lower & truth[i] < tmp$Upper)
                         return(tmp)
                       }))
  # Compute coverage and confidence levels (95% fot coverage)
  cov <- do.call(data.frame,
                 aggregate(Coverage ~ Nominal, df, function(x) {
                   with(binom.test(sum(x), length(x), alternative = "two.sided", conf.level = 0.95),
                        c(estimate, conf.int))
                 }))
  colnames(cov)[-1] <- c("Coverage", "Lower", "Upper")

  # Plot
  ggplot(data = cov, aes_string(x = "Nominal", y = "Coverage", ymin = "Lower", ymax = "Upper")) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, col = "red") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_bw(base_size = 15)
}
