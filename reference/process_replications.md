# Extract posterior predictive distribution

Extract posterior predictive distribution

## Usage

``` r
process_replications(
  fit,
  idx = NULL,
  parName,
  bounds = NULL,
  type = c("continuous", "discrete", "eti", "hdi"),
  ...
)
```

## Arguments

- fit:

  Stanfit object.

- idx:

  Dataframe for translating the indices of the parameters into more
  informative variable (can be NULL).

- parName:

  Name of the parameter to extract.

- bounds:

  NULL or vector of length 2 representing the bounds of the distribution
  if it needs to be truncated.

- type:

  Indicates how the distribution is summarised.

- ...:

  Parameters to be passed to
  [`extract_distribution()`](https://ghurault.github.io/HuraultMisc/reference/extract_distribution.md).

## Value

Dataframe.
