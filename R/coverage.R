# Can be part of Stan workflow

# Documentation -----------------------------------------------------------

#' Coverage probability
#'
#' Compute and plot coverage of CI for different confidence level.
#' Useful for fake data check.
#'
#' @param post_samples Matrix of posterior samples. Rows represent a sample and columns represent variables.
#' @param truth Vector of true parameter values (should be the same length as the number of columns in post_samples).
#' @param CI Vector of confidence levels.
#' @param type Type of confidence intervals: either "eti" (equal-tailed intervals) or "hdi" (highest density intervals)
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

# Compute coverage --------------------------------------------------------

#' @rdname coverage
#' @export
compute_coverage <- function(post_samples, truth, CI = seq(0, 1, 0.05), type = c("eti", "hdi")) {

  stopifnot(is.matrix(post_samples),
            is.vector(truth, mode = "numeric"),
            ncol(post_samples) == length(truth),
            is.vector(CI, mode = "numeric"),
            min(CI) >= 0 && max(CI) <= 1)
  type <- match.arg(type)

  if (type == "hdi") {
    CI <- CI[CI > 0 & CI < 1]
  }

  # For each variable, compute Lower and Upper bounds for different confidence level, and check if the truth is in in the interval
  df <- do.call(rbind,
                lapply(1:ncol(post_samples),
                       function(i) {

                         if (type == "eti") {
                           tmp <- do.call(rbind,
                                          lapply(CI,
                                                 function(lvl) {
                                                   # Compute Lower and Upper bounds of CI for each confidence level
                                                   alpha <- 1 - lvl
                                                   q <- quantile(post_samples[, i], probs = c(alpha / 2, 1 - alpha / 2))
                                                   data.frame(Nominal = lvl, Variable = i, Lower = q[1], Upper = q[2])
                                                 }))
                         } else if (type == "hdi") {
                           x <- sapply(CI, function(lvl) {HDInterval::hdi(post_samples[, i], credMass = lvl)})
                           tmp <- data.frame(Nominal = CI, Variable = i, Lower = x["lower", ], Upper = x["upper", ])
                         }
                         rownames(tmp) <- NULL
                         tmp$Coverage <- (truth[i] >= tmp$Lower & truth[i] <= tmp$Upper)
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

# Plot coverage -----------------------------------------------------------

#' @rdname coverage
#' @export
#' @import ggplot2
plot_coverage <- function(post_samples, truth, CI = seq(0, 1, 0.05), type = c("eti", "hdi")) {

  ggplot(data =  compute_coverage(post_samples, truth, CI, type),
         aes_string(x = "Nominal", y = "Coverage", ymin = "Lower", ymax = "Upper")) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, col = "red", linetype = "dashed") +
    annotate("text", x = c(.2, .8), y = c(.8, .2), label = c("Inefficient", "Inaccurate"), size = 7) + # inefficient or sub-optimal
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_bw(base_size = 15)
}
