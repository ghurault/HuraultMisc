# Extract probability density function from vector of samples

Extract probability density function from vector of samples

## Usage

``` r
extract_pdf(x, support = NULL, n_density = 2^7)
```

## Arguments

- x:

  Vector of samples from a distribution.

- support:

  Vector of length 2 corresponding to the range of the distribution. Can
  be NULL.

- n_density:

  Number of equally spaced points at which the density is to be
  estimated (better to use a power of 2).

## Value

Dataframe with columns: `Value`, `Density.`

## Examples

``` r
extract_pdf(rnorm(1e3)) %>% head()
#> Warning: support is NULL, taking the range: -2.82552732836382, 2.98762607106799
#>       Value     Density
#> 1 -2.825527 0.009668441
#> 2 -2.779754 0.010918358
#> 3 -2.733982 0.012171388
#> 4 -2.688209 0.013440794
#> 5 -2.642436 0.014751383
#> 6 -2.596663 0.016135277
```
