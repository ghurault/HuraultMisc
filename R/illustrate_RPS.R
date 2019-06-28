#' Illustration of the Ranked Probability Score
#'
#' Illustrates what the RPS measure when we forecast "Severity" with a Gaussian distribution.
#' The RPS corresponds to the shaded area squared.
#'
#' @param mu mean of the Gaussian forecast distribution
#' @param sigma standard deviation of the Gaussian forecast distribution
#' @param observed Observed outcome
#'
#' @return Ggplot
#' @export
#'
#' @examples
#'
#' @import ggplot2
illustrate_RPS <- function(mu = 5, sigma = 1, observed = 6) {

  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  lwidth <- 3 # linewidth
  x <- seq(0, 10, .01)

  df1 <- data.frame(Severity = x, Density = dnorm(x, mean = mu, sd = sigma))
  pdf <- ggplot(data = df1, aes(x = Severity)) +
    geom_line(aes(y = Density), lwd = lwidth) +
    geom_vline(xintercept = observed, colour = cbbPalette[2], lwd = lwidth) +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(df1$Density) * 1.05)) +
    scale_x_continuous(breaks = 0:10) +
    scale_colour_manual(values = cbbPalette) +
    theme_classic(base_size = 20)

  df2 <- data.frame(x,
                    Forecast = pnorm(x, mean = mu, sd = sigma),
                    Outcome = as.numeric(x > observed))
  df2$Lower <- pmin(df2$Forecast, df2$Outcome)
  df2$Upper <- pmax(df2$Forecast, df2$Outcome)

  cdf <- ggplot()+
    geom_line(data = reshape2::melt(df2[, c("x", "Forecast", "Outcome")],
                                    id="x", variable.name = "Distribution", value.name = "CD"),
              aes(x = x, y = CD, colour = Distribution), lwd = lwidth) +
    geom_ribbon(data = df2, aes(x = x, ymin = Lower, ymax = Upper, fill = "|Error|"), alpha = .3) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = 0:10) +
    scale_colour_manual(values = cbbPalette) +
    scale_fill_manual(values = "grey12") +
    labs(x = "Severity", y = "Cumulative Density") +
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank(), legend.position = "top")

  cowplot::plot_grid(cdf, pdf, nrow = 2)
}
