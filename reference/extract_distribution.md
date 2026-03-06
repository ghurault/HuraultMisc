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
#> 1   0.000000000 0.4168469    NA         
#> 2   0.007874016 0.4402654    NA         
#> 3   0.015748031 0.4633719    NA         
#> 4   0.023622047 0.4860562    NA         
#> 5   0.031496063 0.5082338    NA         
#> 6   0.039370079 0.5298304    NA         
#> 7   0.047244094 0.5507811    NA         
#> 8   0.055118110 0.5710347    NA         
#> 9   0.062992126 0.5906008    NA         
#> 10  0.070866142 0.6094878    NA         
#> 11  0.078740157 0.6277254    NA         
#> 12  0.086614173 0.6453371    NA         
#> 13  0.094488189 0.6624223    NA         
#> 14  0.102362205 0.6790644    NA         
#> 15  0.110236220 0.6953581    NA         
#> 16  0.118110236 0.7114011    NA         
#> 17  0.125984252 0.7273095    NA         
#> 18  0.133858268 0.7431898    NA         
#> 19  0.141732283 0.7591384    NA         
#> 20  0.149606299 0.7752446    NA         
#> 21  0.157480315 0.7915810    NA         
#> 22  0.165354331 0.8081887    NA         
#> 23  0.173228346 0.8250885    NA         
#> 24  0.181102362 0.8422760    NA         
#> 25  0.188976378 0.8597213    NA         
#> 26  0.196850394 0.8773450    NA         
#> 27  0.204724409 0.8950502    NA         
#> 28  0.212598425 0.9127125    NA         
#> 29  0.220472441 0.9301684    NA         
#> 30  0.228346457 0.9472460    NA         
#> 31  0.236220472 0.9637594    NA         
#> 32  0.244094488 0.9795121    NA         
#> 33  0.251968504 0.9942713    NA         
#> 34  0.259842520 1.0078377    NA         
#> 35  0.267716535 1.0200447    NA         
#> 36  0.275590551 1.0307240    NA         
#> 37  0.283464567 1.0397037    NA         
#> 38  0.291338583 1.0468319    NA         
#> 39  0.299212598 1.0520845    NA         
#> 40  0.307086614 1.0554203    NA         
#> 41  0.314960630 1.0568341    NA         
#> 42  0.322834646 1.0562863    NA         
#> 43  0.330708661 1.0539484    NA         
#> 44  0.338582677 1.0499431    NA         
#> 45  0.346456693 1.0444288    NA         
#> 46  0.354330709 1.0375564    NA         
#> 47  0.362204724 1.0295887    NA         
#> 48  0.370078740 1.0207820    NA         
#> 49  0.377952756 1.0113926    NA         
#> 50  0.385826772 1.0016841    NA         
#> 51  0.393700787 0.9919402    NA         
#> 52  0.401574803 0.9824303    NA         
#> 53  0.409448819 0.9734058    NA         
#> 54  0.417322835 0.9651113    NA         
#> 55  0.425196850 0.9577991    NA         
#> 56  0.433070866 0.9516290    NA         
#> 57  0.440944882 0.9467616    NA         
#> 58  0.448818898 0.9433283    NA         
#> 59  0.456692913 0.9414994    NA         
#> 60  0.464566929 0.9412841    NA         
#> 61  0.472440945 0.9427112    NA         
#> 62  0.480314961 0.9457859    NA         
#> 63  0.488188976 0.9505325    NA         
#> 64  0.496062992 0.9568744    NA         
#> 65  0.503937008 0.9647065    NA         
#> 66  0.511811024 0.9739290    NA         
#> 67  0.519685039 0.9844421    NA         
#> 68  0.527559055 0.9961152    NA         
#> 69  0.535433071 1.0087572    NA         
#> 70  0.543307087 1.0222031    NA         
#> 71  0.551181102 1.0362801    NA         
#> 72  0.559055118 1.0508147    NA         
#> 73  0.566929134 1.0655930    NA         
#> 74  0.574803150 1.0804251    NA         
#> 75  0.582677165 1.0951210    NA         
#> 76  0.590551181 1.1094737    NA         
#> 77  0.598425197 1.1233004    NA         
#> 78  0.606299213 1.1364288    NA         
#> 79  0.614173228 1.1486929    NA         
#> 80  0.622047244 1.1599102    NA         
#> 81  0.629921260 1.1699344    NA         
#> 82  0.637795276 1.1786672    NA         
#> 83  0.645669291 1.1860046    NA         
#> 84  0.653543307 1.1918448    NA         
#> 85  0.661417323 1.1960847    NA         
#> 86  0.669291339 1.1987388    NA         
#> 87  0.677165354 1.1997908    NA         
#> 88  0.685039370 1.1992461    NA         
#> 89  0.692913386 1.1970623    NA         
#> 90  0.700787402 1.1933589    NA         
#> 91  0.708661417 1.1882067    NA         
#> 92  0.716535433 1.1816900    NA         
#> 93  0.724409449 1.1738691    NA         
#> 94  0.732283465 1.1648809    NA         
#> 95  0.740157480 1.1548605    NA         
#> 96  0.748031496 1.1439276    NA         
#> 97  0.755905512 1.1321886    NA         
#> 98  0.763779528 1.1197589    NA         
#> 99  0.771653543 1.1067674    NA         
#> 100 0.779527559 1.0933093    NA         
#> 101 0.787401575 1.0794640    NA         
#> 102 0.795275591 1.0652870    NA         
#> 103 0.803149606 1.0508373    NA         
#> 104 0.811023622 1.0361385    NA         
#> 105 0.818897638 1.0211934    NA         
#> 106 0.826771654 1.0059724    NA         
#> 107 0.834645669 0.9904437    NA         
#> 108 0.842519685 0.9745519    NA         
#> 109 0.850393701 0.9582247    NA         
#> 110 0.858267717 0.9413608    NA         
#> 111 0.866141732 0.9238639    NA         
#> 112 0.874015748 0.9056464    NA         
#> 113 0.881889764 0.8866102    NA         
#> 114 0.889763780 0.8666482    NA         
#> 115 0.897637795 0.8456513    NA         
#> 116 0.905511811 0.8235813    NA         
#> 117 0.913385827 0.8003861    NA         
#> 118 0.921259843 0.7760345    NA         
#> 119 0.929133858 0.7504647    NA         
#> 120 0.937007874 0.7237521    NA         
#> 121 0.944881890 0.6959439    NA         
#> 122 0.952755906 0.6671139    NA         
#> 123 0.960629921 0.6373342    NA         
#> 124 0.968503937 0.6067562    NA         
#> 125 0.976377953 0.5755398    NA         
#> 126 0.984251969 0.5438510    NA         
#> 127 0.992125984 0.5118680    NA         
#> 128 1.000000000 0.4797936    NA         
```
