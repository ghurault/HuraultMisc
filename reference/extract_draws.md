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
#> 1   822    NA  2.2224587          
#> 2   262    NA -1.1923732          
#> 3   108    NA  1.0717368          
#> 4   376    NA -0.1464564          
#> 5   330    NA -0.3640846          
#> 6   488    NA -0.7806759          
#> 7   247    NA -0.6757397          
#> 8   406    NA  0.2403175          
#> 9   778    NA -1.0893823          
#> 10  715    NA -1.0148679          
extract_draws(X, sample(1:nrow(X), 10)) %>% head()
#>   Draw Index       Value Parameter
#> 1   63     1  0.79545121          
#> 2   25     1  0.19289988          
#> 3   77     1 -0.23992836          
#> 4   74     1 -0.48430317          
#> 5   94     1  1.25177858          
#> 6   55     1 -0.09720455          
extract_draws(a, sample(1:10, 5)) %>% head()
#>    Draw       Value Index Parameter
#> 1     1 -0.04495294    NA   [1,1,1]
#> 4     4 -1.35775429    NA   [1,1,1]
#> 7     7 -0.56766973    NA   [1,1,1]
#> 9     9  0.32367786    NA   [1,1,1]
#> 10   10  0.86817330    NA   [1,1,1]
#> 11    1 -0.60429145    NA   [2,1,1]
extract_draws(list(x = x, X = X, a = a), 1:10) %>% head()
#>   Draw Index       Value Parameter
#> 1    1    NA -0.08531927         x
#> 2    2    NA  0.74911722         x
#> 3    3    NA  0.44072593         x
#> 4    4    NA  0.13282516         x
#> 5    5    NA  1.85330059         x
#> 6    6    NA -0.41630264         x
```
