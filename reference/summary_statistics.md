# Extract summary statistics

Extract summary statistics

## Usage

``` r
summary_statistics(fit, pars, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
```

## Arguments

- fit:

  Stanfit object.

- pars:

  Character vector of parameters to extract. Defaults to all parameters.

- probs:

  Numeric vector of quantiles to extract.

## Value

Dataframe of posterior summary statistics

## Alternative

The [tidybayes](https://mjskay.github.io/tidybayes/) package offers an
alternative to this function, for example:
`fit %>% tidy_draws() %>% gather_variables() %>% mean_qi()`. However,
this does not provide information about `Rhat` or `Neff`, nor does it
process the indexes. The tidybayes package is more useful for
summarising the distribution of a handful of parameters (using
`tidybayes::spread_draws()`).
