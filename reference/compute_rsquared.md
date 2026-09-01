# Compute Bayesian R-squared from matrix of posterior replications

The function returns a global estimate rather than an estimate for each
sample, since the predictive means and residual variance are estimated
from replications instead of being computed on a per-sample basis.

## Usage

``` r
compute_rsquared(yrep)
```

## Arguments

- yrep:

  Matrix with rows representing samples and columns representing
  observations

## Value

Bayesian R-squared (scalar, between 0 and 1)

## References

Gelman, A. et al. (2019) "R-squared for Bayesian Regression Models", The
American Statistician, 73(3), pp. 307–309.
[doi:10.1080/00031305.2018.1549100](https://doi.org/10.1080/00031305.2018.1549100)
.

## Examples

``` r
N <- 50
N_sample <- 1e2
y <- runif(N, 0, 10)
yrep <- do.call(
  cbind,
  lapply(
    1:N,
    function(i) {
      y[i] + rnorm(N_sample)
    }
  )
)
compute_rsquared(yrep)
#> [1] 0.8943424
```
