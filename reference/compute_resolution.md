# Compute resolution of forecasts, normalised by the uncertainty

The resolution is computed as the mean squared distance to a base rate
(reference forecast) and is then normalised by the uncertainty (maximum
resolution). This means the output is between 0 and 1, 1 corresponding
to the maximum resolution.

## Usage

``` r
compute_resolution(f, p0)
```

## Arguments

- f:

  Vector of forecasts

- p0:

  Vector of base rate. In the case rate is usually the prevalence of a
  uniform forecast (e.g. 1 / number of categories) but can depend on the
  observation (hence the vector).

## Value

Vector of resolution values

## Examples

``` r
compute_resolution(seq(0, 1, .1), 0.5)
#> [1] 0.4
```
