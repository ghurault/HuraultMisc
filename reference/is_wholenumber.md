# Test whether x is a whole number

- `is_wholenumber()` uses
  [`base::round()`](https://rdrr.io/r/base/Round.html) to test whether
  `x` is a whole number, it will therefore issue an error if `x` is not
  of mode numeric. If used in
  [`base::stopifnot()`](https://rdrr.io/r/base/stopifnot.html) for
  example, this won't be a problem but it may be in conditionals.

- `is_scalar_wholenumber()` comes with the additional argument
  `check_numeric` to check whether `x` is a numeric before checking it
  is a whole number.

## Usage

``` r
is_wholenumber(x, tol = .Machine$double.eps^0.5)

is_scalar_wholenumber(x, check_numeric = TRUE, ...)
```

## Arguments

- x:

  Object to be tested

- tol:

  Tolerance

- check_numeric:

  Whether to check whether `x` is a numeric

- ...:

  Arguments to pass to `is_wholenumber()`

## Value

Logical

## Examples

``` r
is_wholenumber(1) # TRUE
#> [1] TRUE
is_wholenumber(1.0) # TRUE
#> [1] TRUE
is_wholenumber(1.1) # FALSE
#> [1] FALSE
is_scalar_wholenumber(1) # TRUE
#> [1] TRUE
is_scalar_wholenumber(c(1, 2)) # FALSE
#> [1] FALSE
```
