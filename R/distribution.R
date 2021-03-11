# Extracting different types of distribution ------------------------------

#' Extract probability density function from vector of samples
#'
#' Not exported.
#'
#' @param x Vector of samples from a distribution.
#' @param support Vector of length 2 corresponding to the range of the distribution. Can be NULL.
#' @param n_density Number of equally spaced points at which the density is to be estimated (better to use a power of 2).
#'
#' @return Dataframe with columns: Value, Density.
#'
#' @noRd
#'
#' @examples
#' extract_pdf(rnorm(1e4))
extract_pdf <- function(x, support = NULL, n_density = 2^7) {

  stopifnot(is.vector(x, mode = "numeric"),
            is.numeric(n_density))

  if (is.null(support)) {
    support <- stats::quantile(x, probs = c(.001, 0.999))
    warning("support is NULL, taking the range: ", paste(support, collapse = ", "))
  } else {
    stopifnot(is.vector(support, mode = "numeric"),
              length(support) >= 2)
  }

  d <- stats::density(x, kernel = "gaussian", from = min(support), to = max(support), n = as.integer(n_density))
  data.frame(Value = d$x, Density = d$y)
}

#' Extract probability mass function from vector of samples
#'
#' Not exported.
#'
#' @param x Vector of samples from a distribution.
#' @param support Vector of all possible values that the distribution can take. Can be NULL.
#'
#' @return Dataframe with columns: Value, Probability.
#'
#' @noRd
#'
#' @examples
#' extract_pmf(round(rnorm(1e4, 0, 10)))
extract_pmf <- function(x, support = NULL) {

  stopifnot(is.vector(x, mode = "numeric"))

  if (is.null(support)) {
    support <- min(floor(x)):max(ceiling(x))
    warning("support is NULL, taking the following values: ", paste(support, collapse = ", "))
  } else {
    stopifnot(is.vector(support, mode = "numeric"),
              length(support) >= 2)
    support <- sort(support)
  }

  if (!(all(x == round(x)))) {
    x <- round(x)
    warning("Non-integer values were rounded.")
  }

  d <- table(factor(x, levels = support))
  data.frame(Value = support,
             Probability = as.numeric(d / sum(d)))
}

#' Extract equal-tailed intervals from vector of samples
#'
#' Not exported.
#'
#' @param x Vector of samples from a distribution.
#' @param CI_level Vector containing the level of the confidence/credible intervals.
#'
#' @return Dataframe with columns: Lower, Upper, Level.
#'
#' @noRd
#'
#' @examples
#' extract_eti(rnorm(1e4))
extract_eti <- function(x, CI_level = seq(0.1, 0.9, 0.1)) {

  stopifnot(is.vector(x, mode = "numeric"),
            is.vector(CI_level, mode = "numeric"),
            min(CI_level) >=0 && max(CI_level) <= 1)

  data.frame(Lower = stats::quantile(x, probs = 0.5 - CI_level / 2),
             Upper = stats::quantile(x, probs = 0.5 + CI_level / 2),
             Level = CI_level)
}

#' Extract highest density intervals from a vector of samples
#'
#' Not exported.
#'
#' @param x Vector of samples from a distribution.
#' @param CI_level Vector containing the level of the confidence/credible intervals.
#'
#' @return Dataframe with columns: Lower, Upper, Level.
#'
#' @noRd
#'
#' @examples
#' extract_hdi(rexp(1e4))
extract_hdi <- function(x, CI_level = seq(0.1, 0.9, 0.1)) {

  stopifnot(is.vector(x, mode = "numeric"),
            is.vector(CI_level, mode = "numeric"),
            min(CI_level) >=0 && max(CI_level) <= 1)

  tmp <- sapply(CI_level, function(q) {HDInterval::hdi(x, credMass = q)})
  data.frame(Lower = tmp["lower", ],
             Upper = tmp["upper", ],
             Level = CI_level)
}

# Master function ----------------------------------------------------

#' Extract a distribution represented by samples
#'
#'@description
#' The distribution can be extracted as:
#' \itemize{
#' \item a probability density function ("continuous").
#' \item a probability mass function ("discrete").
#' \item a series of equal-tailed confidence/credible intervals ("eti").
#' \item a series of highest density confidence/credible intervals ("hdi").
#' }
#'
#' @param object Object specifying the distribution as samples: can be a Stanfit object,
#' a matrix (columns represents parameters, rows samples) or a vector.
#' @param parName Name of the parameter to extract.
#' @param type Indicates how the distribution is summarised.
#' @param support Support of the distribution.
#' For type = "continuous", this must be the range of the distribution.
#' For type = "discrete", this must be a vector of all possible values that the distribution can take.
#' Can be NULL.
#' @param transform Function to apply to the samples.
#' @param nDensity Number of equally spaced points at which the density is to be estimated (better to use a power of 2).
#' Applies when type = "continuous".
#' @param CI_level Vector containing the level of the confidence/credible intervals. Applies when type = "eti" or type = "hdi".
#'
#' @return Dataframe
#' @export
#'
#' @seealso [extract_draws()] for extracting draws of an object.
#'
#' @section Alternative:
#' This function can notably be used to prepare the data for plotting fan charts when type = "eti" or "hdi".
#' In that case, the ggdist package offers an alternative with [ggdist::stat_lineribbon()].
extract_distribution <- function(object,
                                 parName = "",
                                 type = c("continuous", "discrete", "eti", "hdi"),
                                 support = NULL,
                                 transform = identity,
                                 nDensity = 2^7,
                                 CI_level = seq(0.1, 0.9, 0.1)) {

  type <- match.arg(type)

  parName <- as.character(parName)
  if (length(parName) > 1) {
    parName <- parName[1]
    warning(as.character(substitute(parName)), " should be of length 1, taking the first element: ", parName)
  }

  if (is_stanfit(object)) {
    object <- rstan::extract(object, pars = parName)[[1]]
  }

  stopifnot(is.numeric(object),
            is.vector(object) || is.matrix(object))

  stopifnot(is.function(transform))

  if (type == "continuous") {
    extract_function <- function(x) {extract_pdf(x, support, nDensity)}
  } else if (type == "discrete") {
    extract_function <- function(x) {extract_pmf(x, support)}
  } else if (type == "eti") {
    extract_function <- function(x) {extract_eti(x, CI_level)}
  } else if (type == "hdi") {
    extract_function <- function(x) {extract_hdi(x, CI_level)}
  }

  if (!is.matrix(object)) {
    out <- extract_function(transform(object))
    out[["Index"]] <- NA
  } else {
    out <- lapply(1:ncol(object),
                  function(i) {
                    # Loop over parameters in the matrix
                    tmp <- extract_function(transform(object[, i]))
                    tmp[["Index"]] <- i
                    return(tmp)
                  }) %>%
      bind_rows()
  }

  out[["Variable"]] <- parName

  return(out)
}
