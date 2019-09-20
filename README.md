# HuraultMisc

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

HuraultMisc is my personal R package regrouping functions used across different projects.
The library mostly provides functions for data analysis.

``` r
devtools::install_github("ghurault/HuraultMisc")
library(HuraultMisc)
```

## List of functions

### Data wrangling

- `change_colnames(df, current_names, new_names)`
- `factor_to_numeric(df, factor_name)`

### Stan related function

- `summary_statistics(fit, param, quant = c(.05, .25, .5, .75, .95))`
- `process_replications(fit, idx, parName, type = "continuous", bounds, nDensity = 2^7, nDraws = 100)`
- `PPC_group_distribution(fit, parName, nDraws = 1)`
- `plot_prior_posterior(post, prior, param)`

### Evaluation metrics

- `compute_calibration(forecast, outcome, method = "smoothing", CI = NULL , binwidth = NULL, ...)`
- `compute_resolution(f, p0)`

### Figures

- `illustrate_RPS(mu = 5, sigma = 1, observed = 6)`
- `illustrate_forward_chaining(horizon = 7, n_it = 5)`
