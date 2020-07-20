# Extract distribution ----------------------------------------------------

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

  if (is_stanfit(object)) {
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
