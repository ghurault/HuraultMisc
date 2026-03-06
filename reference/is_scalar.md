# Test whether x is of length 1

Test whether x is of length 1

## Usage

``` r
is_scalar(x)
```

## Arguments

- x:

  Object to be tested.

## Value

Logical

## Examples

``` r
is_scalar(1) # TRUE
#> [1] TRUE
is_scalar("a") # TRUE
#> [1] TRUE
is_scalar(c(1, 2)) # FALSE
#> [1] FALSE
```
