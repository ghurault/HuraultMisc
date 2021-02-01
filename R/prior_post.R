# Compare prior to posterior

# Plot prior vs posterior estimates  ------------------------------------

#' Plot posterior CI alongside prior CI
#'
#' @param post Dataframe of posterior parameter estimates
#' @param prior Dataframe of prior parameter estimates
#' @param pars Vector of parameter names to plot. Defaults to all parameters presents in post and prior.
#'
#' @return Ggplot of parameter estimates
#' @export
#'
#' @import ggplot2
plot_prior_posterior <- function(prior, post, pars = NULL) {

  id_vars <-  c("Variable", "Mean", "5%", "95%")

  stopifnot(is.data.frame(post),
            is.data.frame(prior),
            is.vector(pars, mode = "character"),
            all(id_vars %in% colnames(post)),
            all(id_vars %in% colnames(prior)))

  if (is.null(pars)) {
    pars <- intersect(post[["Variable"]], prior[["Variable"]])
  }

  post <- post[post[["Variable"]] %in% pars, ]
  post$Distribution <- "Posterior"

  prior <- prior[prior[["Variable"]] %in% pars, ]
  prior$Distribution <- "Prior"

  stopifnot(nrow(post) > 0,
            nrow(prior) > 0)

  id_vars <-  c(id_vars, "Distribution")

  tmp <- rbind(post[, id_vars], prior[, id_vars])
  tmp[["Distribution"]] <- factor(tmp[["Distribution"]], levels = c("Prior", "Posterior")) # to show posterior on top
  tmp[["Variable"]] <- factor(tmp[["Variable"]], levels = rev(pars)) # show parameters in the order of pars

  ggplot(data = tmp, aes_string(x = "Variable", y = "Mean", ymin = "`5%`", ymax = "`95%`", colour = "Distribution")) +
    geom_pointrange(position = position_dodge2(width = .3), size = 1.2) +
    scale_colour_manual(values = c("#E69F00", "#000000")) +
    coord_flip() +
    labs(colour = "", x = "", y = "Estimate") +
    theme_bw(base_size = 15) +
    theme(legend.position = "top")
}

# Model sensitivity to priors -------------------------------------------------------

#' Compute diagnostics of how the posterior is influenced by the prior
#'
#' \itemize{
#' \item Posterior shrinkage (`PostShrinkage`) = 1 - Var(Post) / Var(Prior), capturing how much the model is learning.
#' Shrinkage near 0 indicates that the data provides little information beyond the prior.
#' Shrinkage near 1 indicates that the data is much more informative than the prior.
#' \item Mahalanobis distance between the mean posterior and the prior (`DistPrior`), capturing whether the prior "includes" the posterior.
#' }
#'
#' @param prior Dataframe of prior parameter estimates (with columns Variable, Index, Mean and sd, cf. output from summary_statistics)
#' @param post Dataframe of posterior parameter estimates (with columns Variable, Index, Mean and sd, , cf. output from summary_statistics)
#' @param pars Vector of parameters' name to check. Defaults to all parameters presents in post and prior.
#'
#' @return Dataframe with columns: Variable, Index, PostShrinkage, DistPrior
#' @export
#'
#' @md
#' @seealso [plot_prior_influence()] to directly plot PostShrink vs DistPrior
#' @references M. Betancourt, \href{https://betanalpha.github.io/assets/case_studies/principled_bayesian_workflow.html}{“Towards a Principled Bayesian Workflow”}, 2018.
compute_prior_influence <- function(prior, post, pars) {

  id_vars <- c("Variable", "Index", "Mean", "sd")

  stopifnot(is.data.frame(prior),
            is.data.frame(post),
            is.vector(pars, mode = "character"),
            all(id_vars %in% colnames(prior)),
            all(id_vars %in% colnames(post)))

  if (is.null(pars)) {
    pars <- intersect(post[["Variable"]], prior[["Variable"]])
  }

  prior <- prior[prior[["Variable"]] %in% pars, id_vars]
  post <- post[post[["Variable"]] %in% pars, id_vars]

  # Eliminate index for prior by taking the first one
  # Useful when the model has subject-parameters with the same distribution (and when prior does not contain as many subjects as post for computational reasons)
  prior[which(prior[["Index"]] == 1), "Index"] <- NA
  prior <- prior[is.na(prior[["Index"]]), ]
  prior[["Index"]] <- NULL

  out <- merge(prior,
               post,
               by = "Variable",
               all.y = TRUE, # cf. individual parameters
               suffixes = c(".Prior", ".Posterior"))

  out[["PostShrinkage"]] <- 1 - (out[["sd.Posterior"]] / out[["sd.Prior"]])^2
  out[["DistPrior"]] <- abs(out[["Mean.Posterior"]] - out[["Mean.Prior"]]) / out[["sd.Prior"]]
  out <- out[, c("Variable", "Index", "PostShrinkage", "DistPrior")]

  return(out)
}

#' Plot prior influence diagnostics
#'
#' Plot posterior shrinkage (capturing how much the model learns) vs Mahalanobis distance between the mean posterior and the prior (whether the prior "includes" the posterior).
#'
#' @param prior Dataframe of prior parameter estimates (with columns Variable, Index, Mean and sd, cf. output from summary_statistics)
#' @param post Dataframe of posterior parameter estimates (with columns Variable, Index, Mean and sd, , cf. output from summary_statistics)
#' @param pars Vector of parameters' name to check
#'
#' @return Ggplot
#' @export
#'
#' @import ggplot2
#'
#' @seealso [compute_prior_influence()] to return a dataframe with posterior shrinkage and prior/posterior distance.
#' @md
plot_prior_influence <-  function(prior, post, pars = NULL) {

  tmp <- compute_prior_influence(prior, post, pars)
  tmp[["Variable"]] <- factor(tmp[["Variable"]], levels = pars)

  p <- ggplot(data = tmp,
              aes_string(x = "PostShrinkage", y = "DistPrior", colour = "Variable")) +
    geom_point(alpha = 0.8, size = 2) +
    annotate("text",
             x = c(0.5, 0.1, 0.9),
             y = c(2.5, 0.25, 0.25),
             label = c("Prior/Observational\nconflict", "Poorly\nidentified", "Ideal"),
             size = 5) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(min(0, min(tmp[["PostShrinkage"]])), 1), expand = expansion(mult = c(0, 0.01))) +
    labs(x =  "Posterior shrinkage", y = "Prior/Posterior distance", colour = "") +
    theme_classic(base_size = 15)

  if (length(unique(tmp[["Variable"]])) <= 8) {
    p <- p + scale_colour_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
  }

  return(p)
}

#' @rdname plot_prior_influence
#' @export
check_model_sensitivity <- function(prior, post, pars = NULL) {
  .Deprecated("plot_prior_influence")
  plot_prior_influence(prior, post, pars)
}
