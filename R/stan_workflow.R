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

  if (class(fit) != "stanfit") {
    stop(as.character(substitute(fit)), " must be a stanfit object")
  }

  if (!is.vector(param, mode = "character")) {
    stop(as.character(substitute(param)), " must be a character vector")
  }

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

# Extract distribution and process replications ----------------------------------------------------

#' Extract a distribution represented by samples
#'
#'@description
#' The distribution can be extracted as:
#' \itemize{
#' \item a probability density function ("continuous").
#' \item a probability mass unction ("discrete").
#' \item draws from the distribution ("samples"). In that case, the little processing that is being done is similar to extract_draws, except that only one parameter can be extracted.
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
#' @param CI_level Vector containing the level of the confidence/credible intervals. Applies when type = "eti" or type = "hdi".
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
  } else if (is.numeric(object) & (is.vector(object) | is.matrix(object) | is.array(object))) {
    ps <- as.array(object)
  } else {
    stop("object of class ", class(object), " not supported")
  }

  parName <- as.character(parName)
  if (length(parName) > 1) {
    parName <- parName[1]
    warning(as.character(substitute(parName)), " should be of length 1, taking the first element: ", parName)
  }

  if (type %in% c("eti", "hdi")) {
    if (!is.vector(CI_level, mode = "numeric")) {
      stop(as.character(substitute(CI_level)), " must be a numeric vector")
    }
    if (max(CI_level) > 1 | min(CI_level) < 0) {
      stop(as.character(substitute(CI_level)), " values must be between 0 and 1")
    }
  }

  if (!is.function(transform)) {
    stop(as.character(substitute(transform)), " should be a function")
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
      warning(as.character(substitute(nDraws)), " should be between 1 and ", nrow(ps), " (number of posterior samples). Set to 1")
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
      d <- density(x, kernel = "gaussian", from = min(support), to = max(support), n = as.integer(nDensity))
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
    stop(as.character(substitute(fit)), " must be a stanfit object and not of class ", class(fit))
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

# Extract parameters from draws -------------------------------------------

#' Extract parameters' draws from an array
#'
#' @param obj Vector or matrix (columns represents different parameters) of draws
#' @param draws Vector draws to extract
#'
#' @return Dataframe with columns: Draw, Index, Value
extract_draws_from_array <- function(obj, draws) {

  if (!(is.vector(obj, mode = "numeric") | is.matrix(obj) | is.array(obj))) {
    stop(as.character(substitute(obj)), " should be a vector or a matrix")
  }
  obj <- as.array(obj)

  draws <- as.integer(draws)
  if (dim(obj)[1] < max(draws) | min(draws) < 1) {
    stop(as.character(substitute(draws)), " values should be between 1 and the number of draws in the object: ", dim(obj)[1])
  }

  d <- dim(obj)
  if (length(d) == 1) {
    data.frame(Draw = draws, Index = NA, Value = obj[draws])
  } else if (length(d) == 2) {
    reshape2::melt(as.matrix(obj[draws, ]),
                   varnames = c("Draw", "Index"),
                   value.name = "Value")
  } else {
    stop("Parameters of more than one dimensions (e.g. matrix or array of array) are not supported yet")
  }
}

#' Extract parameters' draws
#'
#' @param obj Vector, matrix (columns represents different parameters) of draws or list of it
#' @param draws Vector of draws to extract
#'
#' @return Dataframe with columns: Draw, Index, Value, Parameter
#' @export
#'
#' @examples
#' x <- rnorm(1e3)
#' X <- matrix(x, ncol = 10)
#' extract_draws(x, sample(1:length(x), 10))
#' extract_draws(X, sample(1:nrow(X), 10))
#' extract_draws(list(x = x, X = X), 1:10)
extract_draws <- function(obj, draws) {

  if (!(inherits(obj, "list") | is.vector(obj, mode = "numeric") | is.matrix(obj) | is.array(obj))) {
    stop(as.character(substitute(obj)), " should be a vector or a matrix or a list of it")
  }

  if (is.list(obj)) {
    do.call(rbind,
            lapply(1:length(obj),
                   function(i) {
                     tmp <- extract_draws_from_array(obj[[i]], draws)
                     tmp[["Parameter"]] <- names(obj)[i]
                     return(tmp)
                   }))
  } else {
    extract_draws_from_array(obj, draws)
  }
}

#' Extract parameters from a single draw
#'
#' @param fit Stanfit object
#' @param param Vector of parameter names
#' @param draw Index of the draw to extract the parameters from
#'
#' @section Note:
#' Useful for to generate fake data.
#'
#' @return Dataframe
#' @export
extract_parameters_from_draw <- function(fit, param, draw) {

  if (class(fit) != "stanfit") {
    stop(as.character(substitute(fit)), " must be a stanfit object")
  }

  draw <- as.integer(draw)
  if (length(draw) != 1) {
    warning(as.character(draw), " should be a single number, taking the first element")
    draw <- draw[1]
  }

  par <- rstan::extract(fit, pars = param)
  extract_draws(par, draw)
}

# Compute and plot coverage (not directly Stan-related) -----------------------------------------------------------

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
    stop("The number of columns in ",
         as.character(substitute(post_samples)),
         " should be equal to the length of ",
         as.character(substitute(truth)),
         ": ", length(truth))
  }

  # For each variable, compute Lower and Upper bounds for different confidence level, and check if the truth is in in the interval
  df <- do.call(rbind,
                lapply(1:ncol(post_samples),
                       function(i) {
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

  if (class(fit) != "stanfit") {
    stop(as.character(substitute(fit)), " must be a stanfit object")
  }

  if (!is.character(parName)) {
    stop(as.character(substitute(parName)), "must be a string")
  }

  if (length(parName) != 1) {
    stop(as.character(substitute(parName)), " should be of length one")
  }

  par <- rstan::extract(fit, pars = parName)[[1]]

  max_draw <-nrow(par)
  if (nDraws < 1 | nDraws > max_draw) {
    stop("nDraws should be between 1 and ", max_draw, " (number of posterior samples)")
  }

  tmp <- extract_draws(par, sample(1:max_draw, nDraws, replace = FALSE))
  tmp$Draw <- factor(tmp$Draw)

  ggplot(data = tmp, aes_string(x = "Value", group = "Draw")) +
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
