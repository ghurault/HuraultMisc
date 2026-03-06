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
#> 45% 0.60429677 0.8048989   0.1
#> 40% 0.51348024 0.9194756   0.2
#> 35% 0.43656849 1.0590203   0.3
#> 30% 0.35796380 1.2174887   0.4
#> 25% 0.28221729 1.4014074   0.5
#> 20% 0.21883635 1.6290178   0.6
#> 15% 0.16051743 1.9467274   0.7
#> 10% 0.10518431 2.3381349   0.8
#> 5%  0.05083657 3.0634668   0.9
extract_ci(x, type = "hdi")
#>          Lower     Upper Level
#> 1 9.466778e-03 0.1128918   0.1
#> 2 7.768045e-03 0.2259538   0.2
#> 3 2.992155e-03 0.3597081   0.3
#> 4 2.840767e-05 0.5135054   0.4
#> 5 1.344425e-03 0.7028017   0.5
#> 6 2.840767e-05 0.9196154   0.6
#> 7 5.498014e-05 1.2178687   0.7
#> 8 2.840767e-05 1.6290190   0.8
#> 9 5.498014e-05 2.3397944   0.9
```
