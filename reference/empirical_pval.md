# Compute empirical p-values

Compute empirical p-values

## Usage

``` r
empirical_pval(t_rep, t, alternative = c("two.sided", "less", "greater"))
```

## Arguments

- t_rep:

  Vector of samples from a distribution.

- t:

  Observation (numeric scalar).

- alternative:

  Indicates the alternative hypothesis: must be one of "two.sided",
  "greater" or "less".

## Value

Empirical p-value.

## Examples

``` r
empirical_pval(rnorm(1e2), 2)
#> [1] 0.01980198
```
