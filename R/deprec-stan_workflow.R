#' Extract posterior predictive distribution
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

  .Deprecated("extract_distribution")

  stopifnot(is_stanfit(fit))

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

  .Deprecated("extract_draws")

  stopifnot(is_stanfit(fit))

  draw <- as.integer(draw)
  if (length(draw) != 1) {
    warning(as.character(draw), " should be a single number, taking the first element")
    draw <- draw[1]
  }

  par <- rstan::extract(fit, pars = param)
  extract_draws(par, draw)
}
