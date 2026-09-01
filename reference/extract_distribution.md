# Extract a distribution represented by samples

The distribution can be extracted as:

- a probability density function (`type = "continuous"`), cf.
  [`extract_pdf()`](https://ghurault.github.io/HuraultMisc/reference/extract_pdf.md).

- a probability mass function (`type = "discrete"`), cf.
  [`extract_pmf()`](https://ghurault.github.io/HuraultMisc/reference/extract_pmf.md).

- a series of equal-tailed confidence/credible intervals
  (`type = "eti"`), cf.
  [`extract_ci()`](https://ghurault.github.io/HuraultMisc/reference/extract_ci.md).

- a series of highest density confidence/credible intervals
  (`type = "hdi"`), cf.
  [`extract_ci()`](https://ghurault.github.io/HuraultMisc/reference/extract_ci.md).

## Usage

``` r
extract_distribution(
  object,
  parName = "",
  type = c("continuous", "discrete", "eti", "hdi"),
  transform = identity,
  ...
)
```

## Arguments

- object:

  Object specifying the distribution as samples: can be a Stanfit
  object, a matrix (columns represents parameters, rows samples) or a
  vector.

- parName:

  Name of the parameter to extract.

- type:

  Indicates how the distribution is summarised.

- transform:

  Function to apply to the samples.

- ...:

  Arguments to pass to
  [`extract_pmf()`](https://ghurault.github.io/HuraultMisc/reference/extract_pmf.md),
  [`extract_pdf()`](https://ghurault.github.io/HuraultMisc/reference/extract_pdf.md)
  or
  [`extract_ci()`](https://ghurault.github.io/HuraultMisc/reference/extract_ci.md)
  depending on `type`.

## Value

Dataframe. The columns depends on the method that is used (see specific
function for details).

## Alternative

This function can notably be used to prepare the data for plotting fan
charts when `type = "eti"` or `"hdi"`. In that case, the
[`ggdist`](https://mjskay.github.io/ggdist/) package offers an
alternative with `ggdist::stat_lineribbon()`.

## See also

[`extract_draws()`](https://ghurault.github.io/HuraultMisc/reference/extract_draws.md)
for extracting draws of an object.

## Examples

``` r
extract_distribution(runif(1e2), type = "continuous", support = c(0, 1))
#>           Value   Density Index Variable
#> 1   0.000000000 0.2310927    NA         
#> 2   0.007874016 0.2500776    NA         
#> 3   0.015748031 0.2698285    NA         
#> 4   0.023622047 0.2903139    NA         
#> 5   0.031496063 0.3115288    NA         
#> 6   0.039370079 0.3334813    NA         
#> 7   0.047244094 0.3561333    NA         
#> 8   0.055118110 0.3794820    NA         
#> 9   0.062992126 0.4035627    NA         
#> 10  0.070866142 0.4283568    NA         
#> 11  0.078740157 0.4538700    NA         
#> 12  0.086614173 0.4801357    NA         
#> 13  0.094488189 0.5071800    NA         
#> 14  0.102362205 0.5349967    NA         
#> 15  0.110236220 0.5636011    NA         
#> 16  0.118110236 0.5930477    NA         
#> 17  0.125984252 0.6232913    NA         
#> 18  0.133858268 0.6543178    NA         
#> 19  0.141732283 0.6861199    NA         
#> 20  0.149606299 0.7186330    NA         
#> 21  0.157480315 0.7517607    NA         
#> 22  0.165354331 0.7854003    NA         
#> 23  0.173228346 0.8194197    NA         
#> 24  0.181102362 0.8536247    NA         
#> 25  0.188976378 0.8878133    NA         
#> 26  0.196850394 0.9217322    NA         
#> 27  0.204724409 0.9551117    NA         
#> 28  0.212598425 0.9876694    NA         
#> 29  0.220472441 1.0190732    NA         
#> 30  0.228346457 1.0489609    NA         
#> 31  0.236220472 1.0770563    NA         
#> 32  0.244094488 1.1030454    NA         
#> 33  0.251968504 1.1264868    NA         
#> 34  0.259842520 1.1472101    NA         
#> 35  0.267716535 1.1649842    NA         
#> 36  0.275590551 1.1795126    NA         
#> 37  0.283464567 1.1906442    NA         
#> 38  0.291338583 1.1983768    NA         
#> 39  0.299212598 1.2026614    NA         
#> 40  0.307086614 1.2033857    NA         
#> 41  0.314960630 1.2008300    NA         
#> 42  0.322834646 1.1951628    NA         
#> 43  0.330708661 1.1865099    NA         
#> 44  0.338582677 1.1752386    NA         
#> 45  0.346456693 1.1617355    NA         
#> 46  0.354330709 1.1463528    NA         
#> 47  0.362204724 1.1294867    NA         
#> 48  0.370078740 1.1116371    NA         
#> 49  0.377952756 1.0932375    NA         
#> 50  0.385826772 1.0747321    NA         
#> 51  0.393700787 1.0565638    NA         
#> 52  0.401574803 1.0391230    NA         
#> 53  0.409448819 1.0228024    NA         
#> 54  0.417322835 1.0079468    NA         
#> 55  0.425196850 0.9947849    NA         
#> 56  0.433070866 0.9835333    NA         
#> 57  0.440944882 0.9744661    NA         
#> 58  0.448818898 0.9675751    NA         
#> 59  0.456692913 0.9629044    NA         
#> 60  0.464566929 0.9605240    NA         
#> 61  0.472440945 0.9603428    NA         
#> 62  0.480314961 0.9622127    NA         
#> 63  0.488188976 0.9660176    NA         
#> 64  0.496062992 0.9716381    NA         
#> 65  0.503937008 0.9787985    NA         
#> 66  0.511811024 0.9872931    NA         
#> 67  0.519685039 0.9969489    NA         
#> 68  0.527559055 1.0075005    NA         
#> 69  0.535433071 1.0187235    NA         
#> 70  0.543307087 1.0304217    NA         
#> 71  0.551181102 1.0423980    NA         
#> 72  0.559055118 1.0544670    NA         
#> 73  0.566929134 1.0664783    NA         
#> 74  0.574803150 1.0782861    NA         
#> 75  0.582677165 1.0897885    NA         
#> 76  0.590551181 1.1009017    NA         
#> 77  0.598425197 1.1115455    NA         
#> 78  0.606299213 1.1216750    NA         
#> 79  0.614173228 1.1312747    NA         
#> 80  0.622047244 1.1403269    NA         
#> 81  0.629921260 1.1488070    NA         
#> 82  0.637795276 1.1567479    NA         
#> 83  0.645669291 1.1641603    NA         
#> 84  0.653543307 1.1710358    NA         
#> 85  0.661417323 1.1773988    NA         
#> 86  0.669291339 1.1832637    NA         
#> 87  0.677165354 1.1886217    NA         
#> 88  0.685039370 1.1934526    NA         
#> 89  0.692913386 1.1977571    NA         
#> 90  0.700787402 1.2015064    NA         
#> 91  0.708661417 1.2046287    NA         
#> 92  0.716535433 1.2070980    NA         
#> 93  0.724409449 1.2088587    NA         
#> 94  0.732283465 1.2098206    NA         
#> 95  0.740157480 1.2099033    NA         
#> 96  0.748031496 1.2090525    NA         
#> 97  0.755905512 1.2071848    NA         
#> 98  0.763779528 1.2041634    NA         
#> 99  0.771653543 1.1999657    NA         
#> 100 0.779527559 1.1945193    NA         
#> 101 0.787401575 1.1877061    NA         
#> 102 0.795275591 1.1794845    NA         
#> 103 0.803149606 1.1698279    NA         
#> 104 0.811023622 1.1586708    NA         
#> 105 0.818897638 1.1459290    NA         
#> 106 0.826771654 1.1316380    NA         
#> 107 0.834645669 1.1157765    NA         
#> 108 0.842519685 1.0982561    NA         
#> 109 0.850393701 1.0791277    NA         
#> 110 0.858267717 1.0584076    NA         
#> 111 0.866141732 1.0360669    NA         
#> 112 0.874015748 1.0121059    NA         
#> 113 0.881889764 0.9866018    NA         
#> 114 0.889763780 0.9595880    NA         
#> 115 0.897637795 0.9310284    NA         
#> 116 0.905511811 0.9010651    NA         
#> 117 0.913385827 0.8697688    NA         
#> 118 0.921259843 0.8371865    NA         
#> 119 0.929133858 0.8034388    NA         
#> 120 0.937007874 0.7686737    NA         
#> 121 0.944881890 0.7330196    NA         
#> 122 0.952755906 0.6966100    NA         
#> 123 0.960629921 0.6596535    NA         
#> 124 0.968503937 0.6223314    NA         
#> 125 0.976377953 0.5848363    NA         
#> 126 0.984251969 0.5473860    NA         
#> 127 0.992125984 0.5101880    NA         
#> 128 1.000000000 0.4734611    NA         
```
