# Documentation -----------------------------------------------------------

#' Compare prior to posterior
#'
#' - `combine_prior_posterior` subsets and binds the prior and posterior dataframes.
#' - `plot_prior_posterior` plots posterior CI alongside prior CI.
#' - `compute_prior_influence` computes diagnostics of how the posterior is influenced by the prior.
#' - `plot_prior_influence` plots diagnostics from `compute_prior_influence`.
#'
#' @param prior Dataframe of prior parameter estimates.
#' The dataframe is expected to have columns `Variable`, `Mean.`
#' For `plot_prior_posterior()`, the columns `5%` and `95%` should also be present.
#' For `compute_prior_influence()` and `plot_prior_influence()`, the columns `Index` and `sd` should also be present.
#' @param post Dataframe of posterior parameter estimates, with same columns as `prior`.
#' @param pars Vector of parameter names to plot. Defaults to all parameters presents in `post` and `prior.`
#' @param match_exact Logical indicating whether parameters should be matched exactly (e.g. `p` does not match `p\[1\]`).
#' @param lb Name of the column in `prior` and `post` corresponding to lower bound of error bar
#' @param ub Name of the column in `prior` and `post` corresponding to upper bound of error bar
#' @param remove_index_prior Whether to remove the index variable for `prior` except the first one.
#' This is useful if a parameter with multiple index have the same prior distribution
#' (e.g. with subject parameters, when `prior` does not contain as many subjects as post for computational reasons).
#'
#' @return
#' - `combine_prior_posterior` returns a dataframe with the same columns as in prior and post and a column `Distribution`.
#' - `compute_prior_influence` returns a dataframe with columns: `Variable`, `Index`, `PostShrinkage`, `DistPrior.`
#' - `plot_prior_posterior` and `plot_prior_influence` returns a ggplot object
#'
#' @details
#' - Posterior shrinkage (`PostShrinkage = 1 - Var(Post) / Var(Prior)`), capturing how much the model is learning.
#' Shrinkage near 0 indicates that the data provides little information beyond the prior.
#' Shrinkage near 1 indicates that the data is much more informative than the prior.
#' - 'Mahalanobis' distance between the mean posterior and the prior (`DistPrior`), capturing whether the prior "includes" the posterior.
#'
#' @section Note:
#' For `plot_prior_posterior`, parameters with the same name but different indices are plotted together.
#' If their prior distribution is the same, it can be useful to only keep one index in `prior`.
#' If not, we can use `match_exact = FALSE` to plot `parameter[1]` and `parameter[2]` separately.
#'
#' @references [M. Betancourt, “Towards a Principled Bayesian Workflow”](https://betanalpha.github.io/assets/case_studies/principled_bayesian_workflow.html), 2018.
#'
#' @name prior_posterior
NULL

# Combine prior and posterior dataframe -----------------------------------

#' @rdname prior_posterior
#' @export
#' @import dplyr
combine_prior_posterior <- function(prior, post, pars = NULL, match_exact = TRUE) {

  stopifnot(is.data.frame(prior),
            is.data.frame(post),
            is.logical(match_exact),
            is_scalar(match_exact))

  if (is.null(pars)) {
    pars <- intersect(post[["Variable"]], prior[["Variable"]])
  }
  stopifnot(is.vector(pars, mode = "character"),
            length(pars) > 0)

  if (!is.null(pars) & !match_exact) {
    rg <- paste0("^", pars, "(\\[.+\\])?$")

    pars <- sapply(list(prior[["Variable"]],
                        post[["Variable"]]),
                   function(x) {
                     id <- sapply(rg, function(rgi) {grepl(rgi, x)})
                     id <- apply(id, 1, any)
                     return(x[id])
                   })
    pars <- unique(c(pars))
  }

  post <- post %>%
    filter(.data$Variable %in% pars) %>%
    mutate(Distribution = "Posterior")

  prior <- prior %>%
    filter(.data$Variable %in% pars) %>%
    mutate(Distribution = "Prior")

  stopifnot(nrow(post) > 0,
            nrow(prior) > 0)

  out <- bind_rows(prior, post)
  return(out)
}

# Plot prior vs posterior estimates  ------------------------------------

#' Plot posterior CI alongside prior CI
#'
#' @rdname prior_posterior
#' @export
#' @import ggplot2
plot_prior_posterior <- function(prior, post, pars = NULL, match_exact = TRUE, lb = "5%", ub = "95%") {

  id_vars <-  c("Variable", "Mean", lb, ub)

  stopifnot(is.data.frame(post),
            is.data.frame(prior),
            all(id_vars %in% colnames(post)),
            all(id_vars %in% colnames(prior)))

  id_vars <-  c(id_vars, "Distribution")

  tmp <- combine_prior_posterior(prior, post, pars = pars, match_exact = match_exact)[, id_vars]
  tmp[["Distribution"]] <- factor(tmp[["Distribution"]], levels = c("Prior", "Posterior")) # to show posterior on top
  if (!is.null(pars) & match_exact) {
    tmp[["Variable"]] <- factor(tmp[["Variable"]], levels = rev(pars)) # show parameters in the order of pars
  }

  tmp %>%
    rename(Lower = all_of(lb),
           Upper = all_of(ub)) %>%
    ggplot(aes_string(x = "Variable", y = "Mean", ymin = "Lower", ymax = "Upper", colour = "Distribution")) +
    geom_pointrange(position = position_dodge2(width = .3), size = 1.2) +
    scale_colour_manual(values = c("#E69F00", "#000000")) +
    coord_flip() +
    labs(colour = "", x = "", y = "Estimate") +
    theme_bw(base_size = 15) +
    theme(legend.position = "top")
}

# Model sensitivity to priors -------------------------------------------------------

#' @rdname prior_posterior
#' @export
#' @import dplyr
compute_prior_influence <- function(prior, post, pars = NULL, match_exact = TRUE, remove_index_prior = TRUE) {

  id_vars <- c("Variable", "Index", "Mean", "sd")

  stopifnot(is.data.frame(prior),
            is.data.frame(post),
            is.vector(pars, mode = "character"),
            all(id_vars %in% colnames(prior)),
            all(id_vars %in% colnames(post)))

  if (remove_index_prior) {
    prior[which(prior[["Index"]] == 1), "Index"] <- NA
    prior <- prior[is.na(prior[["Index"]]), ]
  }

  tmp <- combine_prior_posterior(prior = prior, post = post, pars = pars, match_exact = match_exact) %>%
    select(all_of(c(id_vars, "Distribution")))

  prior <- tmp %>% filter(.data$Distribution == "Prior") %>% select(-.data$Distribution, -.data$Index)
  post <- tmp %>% filter(.data$Distribution == "Posterior") %>% select(-.data$Distribution)

  out <- right_join(prior,
                    post,
                    by = "Variable",
                    suffix = c("_Prior", "_Posterior")) %>%
    mutate(PostShrinkage = 1 - (.data$sd_Posterior / .data$sd_Prior)^2,
           DistPrior = abs(.data$Mean_Posterior - .data$Mean_Prior) / .data$sd_Prior) %>%
    select(all_of(c("Variable", "Index", "PostShrinkage", "DistPrior")))

  return(out)
}

#' @rdname prior_posterior
#' @export
#' @import ggplot2
plot_prior_influence <-  function(prior, post, pars = NULL, match_exact = TRUE) {

  tmp <- compute_prior_influence(prior = prior, post = post, pars = pars, match_exact = match_exact)
  if (!is.null(pars) & match_exact) {
    tmp[["Variable"]] <- factor(tmp[["Variable"]], levels = pars)
  }

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
    p <- p + scale_colour_manual(values = c("#999999", HuraultMisc::cbbPalette[-1]))
  }

  return(p)
}

#' @rdname prior_posterior
#' @export
check_model_sensitivity <- function(prior, post, pars = NULL) {
  .Deprecated("plot_prior_influence")
  plot_prior_influence(prior, post, pars, match_exact = TRUE)
}
