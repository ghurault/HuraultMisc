#' Illustration forward chaining
#'
#' @param horizon prediction horizon
#' @param n_it number of iterations to display
#'
#' @return Ggplot
#' @export
#'
#' @examples
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
