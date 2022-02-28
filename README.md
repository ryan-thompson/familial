
# familial

[![R-CMD-check](https://github.com/ryan-thompson/familial/workflows/R-CMD-check/badge.svg)](https://github.com/ryan-thompson/familial/actions)
[![codecov](https://codecov.io/gh/ryan-thompson/familial/branch/master/graph/badge.svg)](https://github.com/ryan-thompson/familial/actions)

## Overview

An R package for [familial inference](https://arxiv.org/abs/2202.12540).
Briefly, this package provides tests for hypotheses of the form

![
\\mathrm{H}\_0:\\mu(\\lambda)=\\mu_0\\text{ for some }\\lambda\\in\\Lambda\\quad\\text{vs.}\\quad\\mathrm{H}\_1:\\mu(\\lambda)\\neq\\mu_0\\text{ for all }\\lambda\\in\\Lambda,
](https://latex.codecogs.com/png.latex?%0A%5Cmathrm%7BH%7D_0%3A%5Cmu%28%5Clambda%29%3D%5Cmu_0%5Ctext%7B%20for%20some%20%7D%5Clambda%5Cin%5CLambda%5Cquad%5Ctext%7Bvs.%7D%5Cquad%5Cmathrm%7BH%7D_1%3A%5Cmu%28%5Clambda%29%5Cneq%5Cmu_0%5Ctext%7B%20for%20all%20%7D%5Clambda%5Cin%5CLambda%2C%0A "
\mathrm{H}_0:\mu(\lambda)=\mu_0\text{ for some }\lambda\in\Lambda\quad\text{vs.}\quad\mathrm{H}_1:\mu(\lambda)\neq\mu_0\text{ for all }\lambda\in\Lambda,
")

where
![\\{\\mu(\\lambda):\\lambda\\in\\Lambda\\}](https://latex.codecogs.com/png.latex?%5C%7B%5Cmu%28%5Clambda%29%3A%5Clambda%5Cin%5CLambda%5C%7D "\{\mu(\lambda):\lambda\in\Lambda\}")
is a family of centers, e.g., that induced by the Huber loss function
with parameter
![\\lambda](https://latex.codecogs.com/png.latex?%5Clambda "\lambda").
In contrast to classic statistical tests such as the
![t](https://latex.codecogs.com/png.latex?t "t") or sign tests for the
mean or median, familial tests do not depend on a single (sometimes
arbitrarily chosen) center.

Presently, `familial` supports tests of the Huber family of centers,
which includes the mean and median. Testing is carried out using a
Bayesian approach whereby the posterior probabilities of the competing
hypotheses
![\\mathrm{H}\_0](https://latex.codecogs.com/png.latex?%5Cmathrm%7BH%7D_0 "\mathrm{H}_0")
and
![\\mathrm{H}\_1](https://latex.codecogs.com/png.latex?%5Cmathrm%7BH%7D_1 "\mathrm{H}_1")
are from the Bayesian bootstrap. One- and two-sample tests are
supported, as are directional tests. Methods for visualizing output are
provided.

## Installation

To install the latest stable version from CRAN, run the following code:

``` r
install.packages('familial')
```

To install the latest development version from GitHub, run the following
code:

``` r
devtools::install_github('ryan-thompson/familial')
```

## Usage

The `center.test()` function performs a test of centers, with the
default being the Huber family of centers.

``` r
library(familial)
set.seed(1)

# One-sample test with point null
x <- MASS::galaxies
center.test(x, mu = 21000)
```

    ## -----------------------------------------------
    ## familial test of centers with huber family
    ## -----------------------------------------------
    ## mu = 21000 
    ## posterior probabilities: 
    ##    H0    H1 
    ## 0.542 0.458 
    ## optimal decision: indeterminate

``` r
# One-sample test with interval null
center.test(x, mu = c(20500, 21500))
```

    ## -----------------------------------------------
    ## familial test of centers with huber family
    ## -----------------------------------------------
    ## mu = 20500 21500 
    ## posterior probabilities: 
    ##    H0    H1 
    ## 0.959 0.041 
    ## optimal decision: H0

``` r
# Two-sample test
x <- MASS::cabbages[MASS::cabbages$Cult == 'c39', 'HeadWt']
y <- MASS::cabbages[MASS::cabbages$Cult == 'c52', 'HeadWt']
center.test(x, y)
```

    ## -----------------------------------------------
    ## familial test of centers with huber family
    ## -----------------------------------------------
    ## mu = 0 
    ## posterior probabilities: 
    ##    H0    H1 
    ## 0.008 0.992 
    ## optimal decision: H1

``` r
# Two-sample paired directional test
x <- MASS::anorexia[MASS::anorexia$Treat == 'FT', 'Postwt']
y <- MASS::anorexia[MASS::anorexia$Treat == 'FT', 'Prewt']
center.test(x, y, paired = T, alternative = 'greater')
```

    ## -----------------------------------------------
    ## familial test of centers with huber family
    ## -----------------------------------------------
    ## mu = 0 
    ## posterior probabilities: 
    ##    H0    H1 
    ## 0.006 0.994 
    ## optimal decision: H1

## Documentation

See the package
[vignette](https://CRAN.R-project.org/package=familial/vignettes/vignette.html)
or [reference
manual](https://CRAN.R-project.org/package=familial/familial.pdf).
