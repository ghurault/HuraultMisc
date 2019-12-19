# HuraultMisc

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/ghurault/HuraultMisc.svg?branch=master)](https://travis-ci.org/ghurault/HuraultMisc)
[![Codecov test coverage](https://codecov.io/gh/ghurault/HuraultMisc/branch/master/graph/badge.svg)](https://codecov.io/gh/ghurault/HuraultMisc?branch=master)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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

### Stan/Bayesian workflow

- `summary_statistics(fit, param, quant = c(.05, .25, .5, .75, .95))`
- `process_replications(fit, idx = NULL, parName, type = c("continuous", "discrete", "samples"), bounds = NULL, nDensity = 2^7, nDraws = 100)`
- `PPC_group_distribution(fit, parName, nDraws = 1)`
- `plot_prior_posterior(post, prior, param)`
- `compute_coverage(post_samples, truth, CI = seq(0, 1, 0.05))`
- `plot_coverage(post_samples, truth, CI = seq(0, 1, 0.05))`
- `extract_parameters_from_draw(fit, param, draw = 1)`

### Evaluation metrics

- `compute_calibration(forecast, outcome, method = c("smoothing", "binning"), CI = NULL , binwidth = NULL, ...)`
- `compute_resolution(f, p0)`

### Figures

- `illustrate_RPS(mu = 5, sigma = 1, observed = 6)`
- `illustrate_forward_chaining(horizon = 7, n_it = 5)`
