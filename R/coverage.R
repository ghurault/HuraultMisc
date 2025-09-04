# Can be part of Stan workflow

# Documentation -----------------------------------------------------------

#' Coverage probability
#'
#' Compute and plot coverage of CI for different confidence level.
#' Useful for fake data check.
#'
#' @param post_samples Matrix of posterior samples. Rows represent a sample and columns represent variables.
#' @param truth Vector of true parameter values (should be the same length as the number of columns in `post_samples`).
#' @param CI Vector of confidence levels.
#' @param type Type of confidence intervals: either "eti" (equal-tailed intervals) or "hdi" (highest density intervals).
#'
#' @name coverage
#'
#' @return
#' `compute_coverage` returns a Dataframe containing coverage (and 95% uncertainty interval for the coverage) for different confidence level (nominal coverage).
#' `plot_coverage` returns a ggplot of the coverage as the function of the nominal coverage with 95% uncertainty interval.
#'
#' @examples
#' N <- 100
#' N_post <- 1e3
#' truth <- rep(0, N)
#' post_samples <- sapply(rnorm(N, 0, 1), function(x) {
#'   rnorm(N_post, x, 1)
#' })
#'
#' compute_coverage(post_samples, truth)
#' plot_coverage(post_samples, truth)
NULL

# Compute coverage --------------------------------------------------------

#' @rdname coverage
#' @export
#' @import tidyr
compute_coverage <- function(post_samples, truth, CI = seq(0, 1, 0.05), type = c("eti", "hdi")) {
  stopifnot(
    is.matrix(post_samples),
    is.vector(truth, mode = "numeric"),
    ncol(post_samples) == length(truth),
    is.vector(CI, mode = "numeric"),
    min(CI) >= 0 && max(CI) <= 1
  )
  type <- match.arg(type)

  CI <- CI[CI > 0 & CI < 1]

  # Compute lower and upper bounds for different confidence levels, for each variable
  df <- extract_distribution(post_samples,
    parName = "",
    type = type,
    CI_level = CI
  )

  # Check if the truth is in the interval
  df <- inner_join(df,
    data.frame(Truth = truth, Index = 1:length(truth)),
    by = "Index"
  ) %>%
    mutate(Coverage = (.data$Truth >= .data$Lower & .data$Truth <= .data$Upper))

  # Compute coverage and confidence levels (95% for coverage)
  cov <- df %>%
    group_by(.data$Level) %>%
    summarise(cv = Hmisc::binconf(sum(.data$Coverage),
      length(.data$Coverage),
      alpha = 0.05,
      method = "exact",
      return.df = TRUE
    )) %>%
    unpack(.data$cv) %>%
    rename(
      Nominal = .data$Level,
      Coverage = .data$PointEst
    )

  # Add extreme values
  cov <- bind_rows(
    cov,
    data.frame(
      Nominal = c(0, 1),
      Coverage = c(0, 1),
      Lower = c(0, 1),
      Upper = c(0, 1)
    )
  ) %>%
    arrange(.data$Nominal)

  return(cov)
}

# Plot coverage -----------------------------------------------------------

#' @rdname coverage
#' @export
#' @import ggplot2
plot_coverage <- function(post_samples, truth, CI = seq(0, 1, 0.05), type = c("eti", "hdi")) {
  col <- "#0072B2"

  ggplot(
    data = compute_coverage(post_samples, truth, CI, type),
    aes_string(x = "Nominal", y = "Coverage", ymin = "Lower", ymax = "Upper")
  ) +
    geom_line(colour = col, size = 1.5) +
    geom_ribbon(alpha = 0.5, fill = col) +
    geom_abline(intercept = 0, slope = 1, colour = "black", linetype = "dashed") +
    annotate("text", x = c(.2, .8), y = c(.8, .2), label = c("Inefficient", "Inaccurate"), size = 7) + # inefficient or sub-optimal
    labs(x = "Nominal coverage", y = "Observed coverage") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_bw(base_size = 15)
}
