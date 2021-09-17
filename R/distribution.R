# Extracting different types of distribution ------------------------------

#' Extract probability density function from vector of samples
#'
#' @param x Vector of samples from a distribution.
#' @param support Vector of length 2 corresponding to the range of the distribution. Can be NULL.
#' @param n_density Number of equally spaced points at which the density is to be estimated (better to use a power of 2).
#'
#' @return Dataframe with columns: `Value`, `Density.`
#'
#' @export
#'
#' @examples
#' extract_pdf(rnorm(1e3)) %>% head()
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
#' @param x Vector of samples from a distribution.
#' @param support Vector of all possible values that the distribution can take. Can be NULL.
#'
#' @return Dataframe with columns: `Value`, `Probability.`
#'
#' @export
#'
#' @examples
#' extract_pmf(round(rnorm(1e3, 0, 10))) %>% head()
extract_pmf <- function(x, support = NULL) {

  stopifnot(is.vector(x))
  if (is.null(support)) {
    if (is.numeric(x)) {
      support <- min(floor(x)):max(ceiling(x))
    } else {
      support <- unique(x)
    }
    warning("support is NULL, taking the following values: ", paste(support, collapse = ", "))
  } else {
    stopifnot(is.vector(support),
              length(support) >= 2,
              mode(x) == mode(support))
  }

  if (is.numeric(support)) {
    support <- sort(support)
    # Round x and support to avoid double precision errors (e.g. when support is seq(0, 1, .1))
    dp <- ceiling(-log10(min(support - dplyr::lag(support), na.rm = TRUE)) + .5)
    support <- round(support, dp)
    x <- round(x, dp)
  }

  if (!all(x %in% support)) {
    warning("Some values in x are not in support")
  }

  out <- factor(x, levels = support) %>%
    table() %>%
    proportions() %>%
    as.numeric() %>%
    data.frame(Value = support, Probability = .)

  return(out)
}

#' Extract confidence intervals from a vector of samples
#'
#' @param x Vector of samples from a distribution.
#' @param CI_level Vector containing the level of the confidence/credible intervals.
#' @param type "eti" for equal-tailed intervals and "hdi" for highest density intervals.
#'
#' @return Dataframe with columns: `Lower`, `Upper`, `Level.`
#'
#' @export
#'
#' @examples
#' x <- rexp(1e4)
#' extract_ci(x, type = "eti")
#' extract_ci(x, type = "hdi")
extract_ci <- function(x, CI_level = seq(0.1, 0.9, 0.1), type = c("eti", "hdi")) {
  stopifnot(is.vector(x, mode = "numeric"),
            is.vector(CI_level, mode = "numeric"),
            min(CI_level) >=0 && max(CI_level) <= 1)
  type <- match.arg(type)

  if (type == "eti") {
    out <- data.frame(Lower = stats::quantile(x, probs = 0.5 - CI_level / 2),
                      Upper = stats::quantile(x, probs = 0.5 + CI_level / 2),
                      Level = CI_level)
  }
  if (type == "hdi") {
    tmp <- sapply(CI_level, function(q) {HDInterval::hdi(x, credMass = q)})
    out <- data.frame(Lower = tmp["lower", ],
               Upper = tmp["upper", ],
               Level = CI_level)
  }
  return(out)
}


# Master function ----------------------------------------------------

#' Extract a distribution represented by samples
#'
#'@description
#' The distribution can be extracted as:
#' \itemize{
#' \item a probability density function (`type = "continuous"`), cf. [extract_pdf()].
#' \item a probability mass function (`type = "discrete"`), cf. [extract_pmf()].
#' \item a series of equal-tailed confidence/credible intervals (`type = "eti"`), cf. [extract_ci()].
#' \item a series of highest density confidence/credible intervals (`type = "hdi"`), cf. [extract_ci()].
#' }
#'
#' @param object Object specifying the distribution as samples: can be a Stanfit object,
#' a matrix (columns represents parameters, rows samples) or a vector.
#' @param parName Name of the parameter to extract.
#' @param type Indicates how the distribution is summarised.
#' @param transform Function to apply to the samples.
#' @param ... Arguments to pass to [extract_pmf()], [extract_pdf()] or [extract_ci()] depending on `type`.
#'
#' @return Dataframe.
#' The columns depends on the method that is used (see specific function for details).
#'
#' @export
#'
#' @seealso [extract_draws()] for extracting draws of an object.
#'
#' @section Alternative:
#' This function can notably be used to prepare the data for plotting fan charts when `type = "eti"` or `"hdi"`.
#' In that case, the [`ggdist`](https://mjskay.github.io/ggdist/) package offers an alternative with `ggdist::stat_lineribbon()`.
#'
#' @examples
#' extract_distribution(runif(1e2), type = "continuous", support = c(0, 1))
extract_distribution <- function(object,
                                 parName = "",
                                 type = c("continuous", "discrete", "eti", "hdi"),
                                 transform = identity,
                                 ...) {

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
    extract_function <- function(x) {extract_pdf(transform(x), ...)}
  } else if (type == "discrete") {
    extract_function <- function(x) {extract_pmf(transform(x), ...)}
  } else if (type == "eti") {
    extract_function <- function(x) {extract_ci(transform(x), type = "eti", ...)}
  } else if (type == "hdi") {
    extract_function <- function(x) {extract_ci(transform(x), type = "hdi", ...)}
  }

  if (!is.matrix(object)) {
    out <- extract_function(object) %>%
      mutate(Index = NA)
  } else {
    out <- lapply(1:ncol(object),
                  function(i) {
                    # Loop over parameters in the matrix
                    extract_function(object[, i]) %>%
                      mutate(Index = i)
                  }) %>%
      bind_rows()
  }

  out[["Variable"]] <- parName

  return(out)
}
