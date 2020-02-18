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
#'@section Alternative:
#' The tidybayes package offers an alternative to this function, for example:
#'
#' fit \%>\% tidy_draws() \%>\% gather_variables \%>\% mean_qi()
#'
#' However, this does not provide information about Rhat or Neff, nor does it process the indexes.
#' The tidybayes package is more useful for summarising the distribution of a handful of parameters (using spread_draws).
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
#'
#' @section Alternative:
#' This function can notably be used to prepare the data for plotting fan charts when type = "eti" or "hdi".
#' In that case, the tidybayes package offers an alternative with stat_lineribbon.
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
    stopifnot(is.vector(CI_level, mode = "numeric"),
              min(CI_level) >=0 && max(CI_level) <= 1)
  }

  stopifnot(is.function(transform))

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

  stopifnot(is.vector(obj, mode = "numeric") || is.matrix(obj) || is.array(obj))

  obj <- as.array(obj)
  draws <- as.integer(draws)

  stopifnot(min(draws) >= 1,
            max(draws) <= dim(obj)[1])

  d <- dim(obj)
  if (length(d) == 1) {
    data.frame(Draw = draws, Index = NA, Value = obj[draws])
  } else if (length(d) == 2) {
    tmp <- obj[draws, ]
    if (length(draws) == 1) {
      data.frame(Draw = draws, Index = 1:length(tmp), Value = tmp)
    } else {
      rownames(tmp) <- draws
      reshape2::melt(tmp,
                     varnames = c("Draw", "Index"),
                     value.name = "Value")
    }
  } else {
    stop("Parameters of more than one dimensions (e.g. matrix or array of array) are not supported yet")
  }
}

#' Extract parameters' draws
#'
#' @param obj Vector, matrix (columns represents different parameters) of draws or list of it
#' @param draws Vector of draws to extract
#'
#' @return Dataframe with columns: Draw, Index, Value and (only if input is a list) Parameter
#' @export
#'
#' @examples
#' x <- rnorm(1e3)
#' X <- matrix(x, ncol = 10)
#' extract_draws(x, sample(1:length(x), 10))
#' extract_draws(X, sample(1:nrow(X), 10))
#' extract_draws(list(x = x, X = X), 1:10)
extract_draws <- function(obj, draws) {

  if (!(inherits(obj, "list") || is.vector(obj, mode = "numeric") || is.matrix(obj) || is.array(obj))) {
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
#' @section Alternative:
#' The tidybayes package offers an alternative to this function, for example:
#'
#' fit \%>\% tidy_draws() \%>\% gather_variables() \%>\% filter(.draw == draw & .variable \%in\% param)
#'
#' However, the tidybayes version is less efficient as all draws and parameters are extracted and then filtered (also the draw IDs are not the same).
#' Using tidybayes would be more recommended when we only want to extract specific parameters, and that it does not matter which draw are extracted (in that case using spread_draws).
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

  stopifnot(is.character(parName),
            length(parName) == 1)

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

