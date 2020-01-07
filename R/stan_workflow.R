# Extract summary statistics -----------------------------------------------------------

#' Extract summary statistics
#'
#' @param fit Stanfit object
#' @param param Parameters to extract
#' @param quant Quantiles to extract
#'
#' @section Note:
#' Not sure how this function works for more than one-dimensional (e.g. matrices) parameters
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

# Extract parameters' CI --------------------------------------------------

#' Extract parameters' credible interval
#'
#' Extract parameters distribution as a series of credible interval (equal-tailed or highest density)
#'
#' @param fit Stanfit object
#' @param param Vector of parameters to extract
#' @param CI_level Vector containing the level of the credible intervals
#' @param type Should be one of hdi (highest density interval, i.e. narrowest) or eti (equal-tailed interval, i.e. centered around median)
#'
#' @section Note:
#' This function should not work for more than one-dimensional parameters
#'
#' @return (Long) Dataframe with columns: Variable, Index, Lower, Upper, Level
#' @export
parameters_intervals <- function(fit, param, CI_level = seq(0.1, 0.9, 0.1), type = c("hdi", "eti")) {

  if (max(CI_level) > 1 | min(CI_level) < 0) {
    stop("CI_level values must be between 0 and 1")
  }

  type <- match.arg(type)

  if (type == "eti") {
    bounds <- data.frame(lower = 0.5 - CI_level / 2,
                         upper = 0.5 + CI_level / 2,
                         level = CI_level)

    ss <- summary_statistics(fit, param, quant = c(rev(bounds[["lower"]]), bounds[["upper"]]))

    out <- do.call(rbind,
                   lapply(1:nrow(bounds),
                          function(i) {
                            lbl <- paste0(format(c(bounds$lower[i], bounds$upper[i]) * 100, trim = TRUE), "%")
                            tmp <- ss[, c("Variable", "Index", lbl)]
                            tmp <- change_colnames(tmp, lbl, c("Lower", "Upper"))
                            tmp[["Level"]] <- bounds$level[i]
                            return(tmp)
                          }))
  }

  if (type == "hdi") {
    par <- rstan::extract(fit, pars = param)

    out <- do.call(rbind,
                   lapply(1:length(par),
                          function(i) {
                            # Loop over parameters
                            tmp <- do.call(rbind,
                                           lapply(CI_level,
                                                  function(q) {
                                                    # Loop over CI width
                                                    ci <- HDInterval::hdi(par[[i]], credMass = q)
                                                    ci <- as.data.frame(t(ci))
                                                    colnames(ci) <- c("Lower", "Upper")
                                                    if (nrow(ci) > 1) {
                                                      ci[["Index"]] <- 1:nrow(ci)
                                                    } else {
                                                      ci[["Index"]] <- NA
                                                    }
                                                    ci[["Level"]] <- q
                                                    return(ci)
                                                  }))
                            tmp[["Variable"]] <- names(par)[i]
                            return(tmp)
                          }))
  }

  return(out)
}

# Process replications ----------------------------------------------------

# Replace by a convenience function to summarise a distribution given as samples to a pdf, pmf, draws, ci
# - extend function to hande other inputs than Stanfit object (cf. class(fit) == "stanfit"); notably make sure it works for one dimensional vector (cf. Index)
# - extend function so that parNames can be a list of parameters (and instead of calling the value the name of the parameters, have a column variable)
# - update README

# at the end, make changes to project to ultimately delete parameters_intervals

#' Extract a distribution represented by samples
#'
#'@description
#' The distribution can be extracted as:
#' \itemize{
#' \item a probability density function ("continuous").
#' \item a probability mass unction ("discrete").
#' \item draws from the distribution ("samples"), in that case, little processing is done.
#' \item a series of equal-tailed confidence/credible intervals ("eti").
#' \item a series of highest density confidence/credible intervals ("hdi").
#' }
#'
#' @param object Object specifying the distribution as samples: can be a Stanfit object, a matrix (columns represents parameters, rows samples) or a vector.
#' @param parName Name of the parameter to extract
#' @param type Indicates how the distribution is summarised.
#' @param support Support of the distribution. For type = "continuous", this must be the range of the distribution. For type = "discrete", this must be a vector of all possible values that the distribution can take. Can be NULL.
#' @param transform Function to apply to the samples
#' @param nDensity Number of equally spaced points at which the density is to be estimated (better to use a power of 2). Applies when type = "continuous".
#' @param nDraws Number of draws from the distribution. Applies when type = "samples"
#' @param CI_level Vector containing the level of the confidence/credible intervals
#'
#' @return Dataframe
#' @export
#' @import stats
extract_distribution <- function(object,
                                 parName,
                                 type = c("continuous", "discrete", "samples", "eti", "hdi"),
                                 support = NULL,
                                 transform = identity,
                                 nDensity = 2^7,
                                 nDraws = 100,
                                 CI_level = seq(0.1, 0.9, 0.1)) {

  type <- match.arg(type)

  if (class(object) == "stanfit") {
    ps <- rstan::extract(object, pars = parName)[[1]]
  } else if (class(object) %in% c("matrix", "array", "numeric")) {
    ps <- as.array(object)
  } else {
    stop("object of class ", class(object), " not supported")
  }

  parName <- as.character(parName)
  if (length(parName) > 1) {
    parName <- parName[1]
    warning("parName should be of length 1, taking the first element: ", parName)
  }

  if (class(transform) != "function") {
    stop(transform, " should be a function")
  }

  if (is.null(support)) {
    if (type == "continuous" | type == "discrete") {
      if (type == "continuous") {
        support <- quantile(ps, probs = c(.001, 0.999))
      } else if (type == "discrete") {
        support <- min(floor(ps)):max(ceiling(ps))
      }
      warning("support is NULL, taking the following values: ", paste(support, collapse = ", "))
    }
  }

  if (type == "samples") {
    if (nDraws < 1 | nDraws > nrow(ps)) {
      warning("nDraws should be between 1 and ", nrow(ps), " (number of posterior samples). nDraws set to 1")
      nDraws <- 1
    }
    smp <- sample(1:nrow(ps), nDraws)
  }

  summary_from_vector <- function(x) {
    # Summarise distribution stored in a vector
    #
    # Args:
    # x: Vector of samples from the distribution
    #
    # Returns:
    # Dataframe

    x <- transform(x)
    if (type == "continuous") {
      d <- density(x, kernel = "gaussian", from = min(support), to = max(support), n = nDensity) # select a power of 2 for n, not too much or it takes memory
      data.frame(Value = d$x,
                 Density = d$y)
    } else if (type == "discrete") {
      x <- round(x)
      d <- table(factor(x, levels = support))
      data.frame(Value = support,
                 Probability = as.numeric(d / sum(d)))
    } else if (type == "samples") {
      data.frame(Value = x[smp],
                 Draw = 1:nDraws)
    } else if (type == "eti") {
      data.frame(Lower = quantile(x, probs = 0.5 - CI_level / 2),
                 Upper = quantile(x, probs = 0.5 + CI_level / 2),
                 Level = CI_level)
    } else if (type == "hdi") {
      tmp <- sapply(CI_level, function(q) {HDInterval::hdi(x, credMass = q)})
      data.frame(Lower = tmp["lower", ],
                 Upper = tmp["upper", ],
                 Level = CI_level)
    }
  }

  if (!is.na(ncol(ps))) {
    out <- do.call("rbind",
                   lapply(1:ncol(ps),
                          function(i) {
                            # Loop over parameters in the matrix
                            tmp <- summary_from_vector(ps[, i])
                            tmp[["Index"]] <- i
                            return(tmp)
                          }))
  } else {
    out <- summary_from_vector(ps)
    out[["Index"]] <- NA
  }
  out[["Variable"]] <- parName

  return(out)
}

#' Extract posterior predictive distribution
#'
#' This function keeps the signature of the previous implementation but is now a particular case of extract_distribution.
#'
#' @param fit Stanfit object
#' @param idx Dataframe for translating the indices of the parameters into more informative variable (can be NULL)
#' @param parName Name of the parameter to extract
#' @param bounds NULL or vector of length 2 representing the bounds of the distribution if it needs to be truncated.
#' @param ... Parameters to be passed to extract_distribution
#'
#' @return Dataframe
#' @export
process_replications <- function(fit, idx = NULL, parName, bounds = NULL, ...) {

  if (class(fit) != "stanfit") {
    stop("fit must be a stanfit object and not of class ", class(fit))
  }

  if (is.null(bounds)) {
    support <- NULL
    transform <- identity
  } else {
    support <- min(bounds):max(bounds)
    transform <- function(x) {x[!(x < min(bounds) | x > max(bounds))]} # truncate
  }

  out <- extract_distribution(object = fit,
                              parName = parName,
                              support = support,
                              transform = transform,
                              ...)

  out <- change_colnames(out, "Value", parName)
  if (!is.null(idx) & "Index" %in% colnames(idx)) {
    out <- merge(out, idx, by = "Index", all = TRUE)
    out$Index <- NULL
  }
  return(out)
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

#' Coverage probability
#'
#' Compute and plot coverage of CI for different confidence level.
#' Useful for fake data check.
#'
#' @param post_samples Matrix of posterior samples. Rows represent a sample and columns represent variables.
#' @param truth Vector of true parameter values (should be the same length as the number of columns in post_samples).
#' @param CI Vector of confidence levels.
#'
#' @name coverage
#'
#' @return
#' compute_coverage returns a dataframe containing coverage (and 95\% uncertainty interval for the coverage) for different confidence level (nominal coverage).
#' plot_coverage returns a ggplot of the coverage as the function of the nominal coverage with 95\% uncertainty interval.
#'
#' @examples
#' N <- 100
#' N_post <- 1e3
#' truth <- rep(0, N)
#' post_samples <- sapply(rnorm(N, 0, 1), function(x) {rnorm(N_post, x, 1)})
#'
#' compute_coverage(post_samples, truth)
#' plot_coverage(post_samples, truth)
NULL

#' @rdname coverage
#' @export
compute_coverage <- function(post_samples, truth, CI = seq(0, 1, 0.05)) {

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
                   Hmisc::binconf(sum(x), length(x), alpha = 0.05, method = "exact")
                 }))
  colnames(cov)[-1] <- c("Coverage", "Lower", "Upper")

  return(cov)
}

#' @rdname coverage
#' @export
#' @import ggplot2
plot_coverage <- function(post_samples, truth, CI = seq(0, 1, 0.05)) {

  ggplot(data =  compute_coverage(post_samples, truth, CI),
         aes_string(x = "Nominal", y = "Coverage", ymin = "Lower", ymax = "Upper")) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_bw(base_size = 15)
}

# Parameters from a single draw -------------------------------------------

#' Extract parameters from a single draw
#'
#' @param fit Stanfit object
#' @param param Vector of parameter names
#' @param draw Index of the draw to extract the parameters from
#'
#' @return Dataframe
#' @export
extract_parameters_from_draw <- function(fit, param, draw = 1) {

  par <- rstan::extract(fit, pars = param)

  draw <- as.integer(draw)
  if (length(draw) > 1) {
    warning("draw should be a single number, taking the first element")
    draw <- draw[1]
  }
  if (dim(par[[1]])[1] < draw | draw < 1) {
    warning("draw should be between 1 and the number of draws in the stanfit object, taking the first draw")
    draw <- 1
  }

  do.call(rbind,
          lapply(1:length(par),
                 function(i) {
                   tmp <- par[[i]]
                   d <- dim(tmp)
                   if (length(d) == 1) {
                     data.frame(Parameter = names(par)[i], Value = tmp[draw], Index = NA)
                   } else if (length(d) == 2) {
                     data.frame(Parameter = names(par)[i], Value = tmp[draw, ], Index = 1:ncol(tmp))
                   } else {
                     stop("Parameters of more than one dimensions (e.g. matrix or array of array) are not supported yet")
                   }
                 }))
}
