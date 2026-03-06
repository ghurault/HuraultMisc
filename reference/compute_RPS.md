# Compute RPS for a single forecast

Compute RPS for a single forecast

## Usage

``` r
compute_RPS(forecast, outcome)
```

## Arguments

- forecast:

  Vector of length N (forecast).

- outcome:

  Index of the true outcome (between 1 and N).

## Value

RPS (numeric scalar)

## Examples

``` r
compute_RPS(c(.2, .5, .3), 2)
#> [1] 0.065
```
