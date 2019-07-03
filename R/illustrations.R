# RPS ---------------------------------------------------------------------

#' Illustration of the Ranked Probability Score
#'
#' Illustrates what the RPS measure when we forecast "Severity" with a Gaussian distribution.
#' The RPS corresponds to the shaded area squared.
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

  df <- do.call(rbind,
                lapply(1:n_it,
                       function(it) {
                         data.frame(Iteration = it,
                                    Day = 1:((it + 1) * horizon),
                                    Subset = c(rep("Train", it * horizon), rep("Test", horizon))
                         )}))
  lbl <- aggregate(Day ~ Iteration + Subset, df, median)
  colnames(lbl)[colnames(lbl) == "Subset"] <- "Label"

  ggplot() +
    geom_rect(data = df,
              aes(xmin = Day - 1, xmax = Day,ymin = Iteration - .35 , ymax = Iteration + .35, fill = Subset),
              alpha = .8) +
    geom_text(data = lbl,
              aes(x = Day, y = Iteration, label = Label),
              fontface = "bold", size = 8) +
    scale_y_continuous(breaks = 1:max(df$Iteration), trans = "reverse", expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, max(df$Day), horizon), expand = c(0, 0)) +
    scale_fill_manual(values = c("#009E73", "#F0E442")) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = 90,vjust = 0.5),
          legend.position = "none")

}
