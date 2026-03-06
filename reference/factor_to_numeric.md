# Change the type of the column of a dataframe from factor to numeric

Change the type of the column of a dataframe from factor to numeric

## Usage

``` r
factor_to_numeric(df, factor_name)
```

## Arguments

- df:

  Dataframe.

- factor_name:

  Vector of names of factors to change to numeric.

## Value

Same dataframe with type of the given columns changed to numeric.

## Examples

``` r
df <- data.frame(A = rep(1:5, each = 10))
df$A <- factor(df$A)
str(df)
#> 'data.frame':    50 obs. of  1 variable:
#>  $ A: Factor w/ 5 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
df <- factor_to_numeric(df, "A")
str(df)
#> 'data.frame':    50 obs. of  1 variable:
#>  $ A: num  1 1 1 1 1 1 1 1 1 1 ...
```
