# Approximate equal

Compute whether x and y are approximately equal given a tolerance level

## Usage

``` r
approx_equal(x, y, tol = .Machine$double.eps^0.5)

x %~% y
```

## Arguments

- x:

  Numeric scalar.

- y:

  Numeric scalar.

- tol:

  Tolerance.

## Value

Boolean

## Examples

``` r
approx_equal(1, 1)
#> [1] TRUE
1 %~% (1 + 1e-16)
#> [1] TRUE
1 %~% 1.01
#> [1] FALSE
```
