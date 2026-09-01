# Extract confidence intervals from a vector of samples

Extract confidence intervals from a vector of samples

## Usage

``` r
extract_ci(x, CI_level = seq(0.1, 0.9, 0.1), type = c("eti", "hdi"))
```

## Arguments

- x:

  Vector of samples from a distribution.

- CI_level:

  Vector containing the level of the confidence/credible intervals.

- type:

  "eti" for equal-tailed intervals and "hdi" for highest density
  intervals.

## Value

Dataframe with columns: `Lower`, `Upper`, `Level.`

## Examples

``` r
x <- rexp(1e4)
extract_ci(x, type = "eti")
#>          Lower     Upper Level
#> 45% 0.60387979 0.8041503   0.1
#> 40% 0.51277904 0.9191887   0.2
#> 35% 0.43565206 1.0576732   0.3
#> 30% 0.35773128 1.2167601   0.4
#> 25% 0.28157390 1.4009135   0.5
#> 20% 0.21849885 1.6294038   0.6
#> 15% 0.16033345 1.9508311   0.7
#> 10% 0.10515143 2.3397921   0.8
#> 5%  0.05083657 3.0648175   0.9
extract_ci(x, type = "hdi")
#>          Lower     Upper Level
#> 1 9.466778e-03 0.1128918   0.1
#> 2 9.015746e-03 0.2270329   0.2
#> 3 3.330184e-03 0.3598353   0.3
#> 4 2.840767e-05 0.5127920   0.4
#> 5 1.445235e-03 0.7024139   0.5
#> 6 2.840767e-05 0.9192023   0.6
#> 7 2.840767e-05 1.2173282   0.7
#> 8 2.840767e-05 1.6295235   0.8
#> 9 2.840767e-05 2.3397944   0.9
```
