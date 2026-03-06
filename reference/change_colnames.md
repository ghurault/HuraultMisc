# Change column names of a dataframe

Change column names of a dataframe

## Usage

``` r
change_colnames(df, current_names, new_names)
```

## Arguments

- df:

  Dataframe

- current_names:

  Vector of column names to change.

- new_names:

  Vector of new names.

## Value

Dataframe with new column names

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(A = 1:2, B = 3:4, C = 5:6)
df <- change_colnames(df, c("A", "C"), c("Aa", "Cc"))
} # }
```
