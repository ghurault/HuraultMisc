# Extract parameters' draws

Extract parameters' draws

## Usage

``` r
extract_draws(obj, draws)
```

## Arguments

- obj:

  Array/Vector/Matrix of draws (cf. first dimension) or list of it.

- draws:

  Vector of draws to extract.

## Value

Dataframe with columns: `Draw`, `Index`, `Value` and `Parameter`.

## Examples

``` r
x <- rnorm(1e3)
X <- matrix(x, ncol = 10)
a <- array(rnorm(80), dim = c(10, 2, 2, 2))
extract_draws(x, sample(1:length(x), 10))
#>    Draw Index      Value Parameter
#> 1     7    NA  0.4032161          
#> 2   407    NA -1.7488916          
#> 3   885    NA  0.4211679          
#> 4   262    NA  0.7744411          
#> 5    48    NA -0.2813901          
#> 6   283    NA  0.3550465          
#> 7   501    NA  0.3287806          
#> 8   563    NA -0.2667667          
#> 9   669    NA -1.1722332          
#> 10   52    NA -0.4979259          
extract_draws(X, sample(1:nrow(X), 10)) %>% head()
#>   Draw Index      Value Parameter
#> 1   65     1 -1.3927678          
#> 2  100     1  0.7057710          
#> 3   58     1 -0.3442753          
#> 4   66     1 -0.2047468          
#> 5   68     1 -1.2569568          
#> 6   94     1  0.4742961          
extract_draws(a, sample(1:10, 5)) %>% head()
#>    Draw      Value Index Parameter
#> 2     2 -0.6055398    NA   [1,1,1]
#> 3     3 -1.4453143    NA   [1,1,1]
#> 6     6  0.4751212    NA   [1,1,1]
#> 7     7 -0.1655246    NA   [1,1,1]
#> 8     8 -2.0692935    NA   [1,1,1]
#> 12    2 -0.5048666    NA   [2,1,1]
extract_draws(list(x = x, X = X, a = a), 1:10) %>% head()
#>   Draw Index      Value Parameter
#> 1    1    NA  1.4983973         x
#> 2    2    NA -2.5571066         x
#> 3    3    NA  0.3367835         x
#> 4    4    NA -2.2876689         x
#> 5    5    NA  0.6422652         x
#> 6    6    NA -1.2952980         x
```
