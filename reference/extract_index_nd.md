# Extract multiple indices inside bracket(s) as a list

Extract multiple indices inside bracket(s) as a list

## Usage

``` r
extract_index_nd(x, dim_names = NULL)
```

## Arguments

- x:

  Character vector.

- dim_names:

  Optional character vector of dimension names. If `dim_names` is not
  `NULL`, if the elements of `x` don't have the same number of indices,
  the missing indices will be set to `NA`.

## Value

Dataframe with columns:

- `Variable`, containing `x` where brackets have been removed

- `Index`, a list containing values within the brackets. If `dim_names`
  is not NULL, `Index` is replaced by columns with names `dim_names`
  containing numeric values.

## Examples

``` r
extract_index_nd(c("sigma", "sigma[1]", "sigma[1, 1]", "sigma[1][2]"))
#>   Variable Index
#> 1    sigma    NA
#> 2    sigma     1
#> 3    sigma  1, 1
#> 4    sigma  1, 2
```
