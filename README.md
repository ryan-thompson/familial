
# familial

[![R-CMD-check](https://github.com/ryan-thompson/familial/workflows/R-CMD-check/badge.svg)](https://github.com/ryan-thompson/familial/actions)
[![codecov](https://codecov.io/gh/ryan-thompson/familial/branch/master/graph/badge.svg)](https://github.com/ryan-thompson/familial/actions)

## Overview

An R package for testing familial hypotheses as described at \[paper
forthcoming\]. Familial hypotheses are statements of the form:

![
\\begin{aligned}
&\\mathrm{H}\_0:\\mu(\\lambda)=\\mu_0\\text{ for some }\\lambda\\in\\Lambda \\\\
&\\mathrm{H}\_1:\\mu(\\lambda)\\neq\\mu_0\\text{ for all }\\lambda\\in\\Lambda,
\\end{aligned}
](https://latex.codecogs.com/png.latex?%0A%5Cbegin%7Baligned%7D%0A%26%5Cmathrm%7BH%7D_0%3A%5Cmu%28%5Clambda%29%3D%5Cmu_0%5Ctext%7B%20for%20some%20%7D%5Clambda%5Cin%5CLambda%20%5C%5C%0A%26%5Cmathrm%7BH%7D_1%3A%5Cmu%28%5Clambda%29%5Cneq%5Cmu_0%5Ctext%7B%20for%20all%20%7D%5Clambda%5Cin%5CLambda%2C%0A%5Cend%7Baligned%7D%0A "
\begin{aligned}
&\mathrm{H}_0:\mu(\lambda)=\mu_0\text{ for some }\lambda\in\Lambda \\
&\mathrm{H}_1:\mu(\lambda)\neq\mu_0\text{ for all }\lambda\in\Lambda,
\end{aligned}
")

where
![\\{\\mu(\\lambda):\\lambda\\in\\Lambda\\}](https://latex.codecogs.com/png.latex?%5C%7B%5Cmu%28%5Clambda%29%3A%5Clambda%5Cin%5CLambda%5C%7D "\{\mu(\lambda):\lambda\in\Lambda\}")
is a family of population parameters. Presently, `familial` supports
tests for the center of a distribution via the Huber or the trimmed mean
families of location parameters. Testing is carried out using a Bayesian
approach whereby the posterior probabilities of the competing hypotheses
are computed using the Bayesian bootstrap. One- and two-sample tests are
supported, as are directional tests. Methods for visualizing output are
provided.

## Installation

<!-- To install the latest stable version from CRAN, run the following code: -->
<!-- ``` {r, eval = F} -->
<!-- install.packages('familial') -->
<!-- ``` -->

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
    ## 0.007 0.993 
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

See the package [reference manual](familial_0.1.0.pdf).

<!-- See the package [vignette](https://CRAN.R-project.org/package=familial/vignettes/vignette.html) or [reference manual](https://CRAN.R-project.org/package=familial/familial.pdf). -->
