# Extract probability mass function from vector of samples

Extract probability mass function from vector of samples

## Usage

``` r
extract_pmf(x, support = NULL)
```

## Arguments

- x:

  Vector of samples from a distribution.

- support:

  Vector of all possible values that the distribution can take. Can be
  NULL.

## Value

Dataframe with columns: `Value`, `Probability.`

## Examples

``` r
extract_pmf(round(rnorm(1e3, 0, 10))) %>% head()
#> Warning: support is NULL, taking the following values: -30, -29, -28, -27, -26, -25, -24, -23, -22, -21, -20, -19, -18, -17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25
#>   Value Probability
#> 1   -30       0.001
#> 2   -29       0.000
#> 3   -28       0.000
#> 4   -27       0.001
#> 5   -26       0.001
#> 6   -25       0.001
```
