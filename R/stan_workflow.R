# Extract summary statistics -----------------------------------------------------------

#' Extract summary statistics
#'
#' @param fit Stanfit object
#' @param pars Character vector of parameters to extract
#' @param probs Numeric vector of quantiles to extract
#'
#'@section Alternative:
#' The tidybayes package offers an alternative to this function, for example:
#' `fit %>% tidy_draws() %>% gather_variables %>% mean_qi()`.
#' However, this does not provide information about Rhat or Neff, nor does it process the indexes.
#' The tidybayes package is more useful for summarising the distribution of a handful of parameters (using spread_draws).
#'
#' @return Dataframe of posterior summary statistics
#' @export
#' @md
summary_statistics <- function(fit, pars, probs = c(.05, .25, .5, .75, .95)) {

  stopifnot(class(fit) == "stanfit")

  par <- rstan::summary(fit, pars = pars, probs = probs)$summary
  par <- as.data.frame(par)
  colnames(par)[colnames(par) == "mean"] <- "Mean"
  par <- cbind(par, extract_index_1d(rownames(par))) # Extract index for 1d array/vectors
  rownames(par) <- NULL

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

  if (any(class(object) == "stanfit")) {
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

  if (!any(class(fit) == "stanfit")) {
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
#' Not exported.
#'
#' @param obj Array/Vector/Matrix (first dimension corresponds to draws)
#' @param draws Vector draws to extract
#' @param parName Optional name to give to the parameter
#'
#' @return Dataframe with columns: Draw, Index (for 1d vector), Value, Parameter
#' @noRd
extract_draws_from_array <- function(obj, draws, parName = "") {

  stopifnot(is.vector(obj, mode = "numeric") || is.matrix(obj) || is.array(obj))

  obj <- as.array(obj)
  draws <- as.integer(draws)

  stopifnot(min(draws) >= 1,
            max(draws) <= dim(obj)[1])

  d <- dim(obj)
  if (length(d) == 1) {
    out <- data.frame(Draw = draws, Index = NA, Value = obj[draws], Parameter = parName)
  } else if (length(d) == 2) {
    # Faster implementation than code for length(d) > 2
    tmp <- obj[draws, ]
    if (length(draws) == 1) {
      out <- data.frame(Draw = draws, Index = 1:length(tmp), Value = tmp)
    } else {
      rownames(tmp) <- draws
      out <- reshape2::melt(tmp,
                     varnames = c("Draw", "Index"),
                     value.name = "Value")
    }
    out$Parameter <- parName
  } else {
    id_lbl <- paste0("i", 1:(length(d) - 1))
    out <- reshape2::melt(obj,
                   varnames = c("Draw", id_lbl),
                   value.name = "Value")
    out <- out[out[["Draw"]] %in% draws, ]
    lbl <- do.call(c, lapply(1:nrow(out), function(i) {paste0(out[i, id_lbl], collapse = ",")}))
    lbl <- paste0(parName, "[", lbl, "]")
    out$Index <- NA
    out[, id_lbl] <- NULL
    out$Parameter <- lbl
  }
  return(out)
}

#' Extract parameters' draws
#'
#' @param obj Array/Vector/Matrix of draws (cf. first dimension) or list of it
#' @param draws Vector of draws to extract
#'
#' @return Dataframe with columns: Draw, Index, Value and Parameter
#' @export
#'
#' @examples
#' x <- rnorm(1e3)
#' X <- matrix(x, ncol = 10)
#' a <- array(rnorm(80), dim = c(10, 2, 2, 2))
#' extract_draws(x, sample(1:length(x), 10))
#' extract_draws(X, sample(1:nrow(X), 10))
#' extract_draws(a, sample(1:10, 5))
#' extract_draws(list(x = x, X = X, a = a), 1:10)
extract_draws <- function(obj, draws) {

  if (!(inherits(obj, "list") || is.vector(obj, mode = "numeric") || is.matrix(obj) || is.array(obj))) {
    stop(as.character(substitute(obj)), " should be a vector or a matrix or a list of it")
  }

  if (is.list(obj)) {
    do.call(rbind,
            lapply(1:length(obj),
                   function(i) {
                     extract_draws_from_array(obj[[i]], draws, names(obj)[i])
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

  if (!any(class(fit) == "stanfit")) {
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

  is_stanfit <- any(class(obj) == "stanfit")
  stopifnot(is_stanfit || is.matrix(obj))

  if (is_stanfit) {
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
