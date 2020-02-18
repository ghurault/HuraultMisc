# Compare prior to posterior

# Plot prior vs posterior estimates  ------------------------------------

#' Plot posterior CI alongside prior CI
#'
#' @param post Dataframe of posterior parameter estimates
#' @param prior Dataframe of prior parameter estimates
#' @param param Vector of parameter names to plot
#'
#' @return ggplot of parameter estimates
#' @export
#'
#' @import ggplot2
plot_prior_posterior <- function(prior, post, param) {

  id_vars <-  c("Variable", "Mean", "5%", "95%")

  stopifnot(is.data.frame(post),
            is.data.frame(prior),
            is.vector(param, mode = "character"),
            all(id_vars %in% colnames(post)),
            all(id_vars %in% colnames(prior)))

  post <- post[post[["Variable"]] %in% param, ]
  post$Distribution <- "Posterior"

  prior <- prior[prior[["Variable"]] %in% param, ]
  prior$Distribution <- "Prior"

  stopifnot(nrow(post) > 0,
            nrow(prior) > 0)

  id_vars <-  c(id_vars, "Distribution")

  tmp <- rbind(post[, id_vars], prior[, id_vars])
  tmp[["Distribution"]] <- factor(tmp[["Distribution"]], levels = c("Prior", "Posterior")) # to show posterior on top

  ggplot(data = tmp, aes_string(x = "Variable", y = "Mean", ymin = "`5%`", ymax = "`95%`", colour = "Distribution")) +
    geom_pointrange(position = position_dodge2(width = .3), size = 1.2) +
    scale_colour_manual(values = c("#E69F00", "#000000")) +
    coord_flip() +
    labs(colour = "", x = "", y = "Estimate") +
    theme_bw(base_size = 20) +
    theme(legend.position = "top")
}

