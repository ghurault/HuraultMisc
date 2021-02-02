# RPS ---------------------------------------------------------------------

#' Illustration of the Ranked Probability Score
#'
#' Illustration of the RPS in the case of forecasts for a discrete "Severity" score, ranging from 0 to 10.
#' The forecast follow a (truncated between 0 and 10) Gaussian distribution, which is discretised to the nearest integer for RPS calculation.
#' The RPS is the mean square error between the cumulative outcome and cumulative forecast distribution (shaded are square).
#' The Ranked Probability Skill Score compares the RPS to a reference RPS (RPS0), RPSS = 1 - RPS / RPS0.
#' It can be interpreted as a normalise distance to a reference forecast: RPSS = 0 means that the forecasts are not better than the reference and RPSS = 1 corresponds to perfect forecasts.
#'
#' @param mu Mean of the Gaussian forecast distribution
#' @param sigma Standard deviation of the Gaussian forecast distribution
#' @param observed Observed outcome
#'
#' @return Ggplot
#' @export
#'
#' @examples
#' illustrate_RPS()
#'
#' @import ggplot2
illustrate_RPS <- function(mu = 5, sigma = 1, observed = 6) {

  stopifnot(observed >= 0,
            observed <= 10)

  if ((mu / sigma) < -2 | (mu - 10) / sigma > 2) {
    stop("If the distribution mass is too much outside [0, 10], there will be numerical errors")
  }

  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  lwidth <- 3 # linewidth
  x <- seq(0, 10, .01)

  # RPS calculation
  Z <- stats::pnorm(10, mu, sigma) - stats::pnorm(0, mu, sigma) # Normalisation constant (cf. truncation)
  cumForecast <- (stats::pnorm(pmin(0:10 + .5, 10), mu, sigma) -  stats::pnorm(pmax(0:10 - .5, 0), mu, sigma)) / Z # Discretise Gaussian
  cumForecast <- cumsum(cumForecast)
  cumOutcome <- rep(0, 11)
  cumOutcome[observed + 1] <- 1
  cumOutcome <- cumsum(cumOutcome)
  RPS <- sum((cumForecast - cumOutcome)^2) / (11 - 1)
  RPS0 <- 2 / 11 # expected RPS for uniformly distributed outcome and uniform forecast: (k+1)/(6*k) where k is the number of categories
  RPSS <- 1 - RPS / RPS0

  # PDF
  df1 <- data.frame(Severity = x, Density = stats::dnorm(x, mean = mu, sd = sigma) / Z)
  pdf <- ggplot(data = df1, aes_string(x = "Severity")) +
    geom_line(aes_string(y = "Density"), lwd = lwidth) +
    geom_vline(xintercept = observed, colour = cbbPalette[2], lwd = lwidth) +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(df1$Density) * 1.05)) +
    scale_x_continuous(breaks = 0:10) +
    scale_colour_manual(values = cbbPalette) +
    theme_classic(base_size = 15)

  # CDF
  df2 <- data.frame(x,
                    Forecast = stats::pnorm(x, mean = mu, sd = sigma) / Z,
                    Outcome = as.numeric(x > observed)) %>%
    mutate(Lower = pmin(.data$Forecast, .data$Outcome),
           Upper = pmax(.data$Forecast, .data$Outcome),
           Fill = "|Error|")
  cdf <- ggplot()+
    geom_line(data = df2 %>%
                select(x, .data$Forecast, .data$Outcome) %>%
                pivot_longer(-x, names_to = "Distribution", values_to = "CD"),
              aes_string(x = "x", y = "CD", colour = "Distribution"), lwd = lwidth) +
    geom_ribbon(data = df2, aes_string(x = "x", ymin = "Lower", ymax = "Upper", fill = "Fill"), alpha = .3) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = 0:10) +
    scale_colour_manual(values = cbbPalette) +
    scale_fill_manual(values = "grey12") +
    labs(x = "Severity", y = "Cumulative Density") +
    theme_classic(base_size = 15) +
    theme(legend.title = element_blank(), legend.position = "top")

  cowplot::plot_grid(pdf + labs(subtitle = paste("RPS = ", signif(RPS, 2), " ; RPSS = ", signif(RPSS, 3), sep = "")),
                     cdf,
                     nrow = 2)
}

# Forward chaining --------------------------------------------------------

#' Illustration forward chaining
#'
#' @param horizon Prediction horizon
#' @param n_it Number of iterations to display
#'
#' @return Ggplot
#' @export
#'
#' @examples
#' illustrate_forward_chaining()
#'
#' @import ggplot2
illustrate_forward_chaining <- function(horizon = 7, n_it = 5) {

  df <- do.call(bind_rows,
                lapply(1:n_it,
                       function(it) {
                         data.frame(Iteration = it,
                                    Day = 1:((it + 1) * horizon),
                                    Subset = c(rep("Train", it * horizon), rep("Test", horizon))
                         )}))
  lbl <- df %>%
    group_by(.data$Iteration, .data$Subset) %>%
    summarise(Day = stats::median(.data$Day)) %>%
    rename(Label = .data$Subset)

  ggplot() +
    geom_rect(data = df,
              aes_string(xmin = "Day - 1", xmax = "Day", ymin = "Iteration - .35" , ymax = "Iteration + .35", fill = "Subset"),
              alpha = .8) +
    geom_text(data = lbl,
              aes_string(x = "Day", y = "Iteration", label = "Label"),
              fontface = "bold", size = 8) +
    scale_y_continuous(breaks = 1:max(df[["Iteration"]]), trans = "reverse", expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, max(df[["Day"]]), horizon), expand = c(0, 0)) +
    scale_fill_manual(values = c("#009E73", "#F0E442")) +
    theme_classic(base_size = 15) +
    theme(axis.title.y = element_text(angle = 90,vjust = 0.5),
          legend.position = "none")
}
