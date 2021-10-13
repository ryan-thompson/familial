## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5
)

## -----------------------------------------------------------------------------
library(familial)
x <- MASS::mammals$brain / MASS::mammals$body
fit <- fit.family(x, family = 'huber')
plot(fit)

## -----------------------------------------------------------------------------
set.seed(1)
x <- MASS::galaxies
test <- center.test(x, mu = 21000)
print(test)

## -----------------------------------------------------------------------------
plot(test)

## -----------------------------------------------------------------------------
center.test(x, mu = c(20500, 21500))

## ----message = FALSE----------------------------------------------------------
x <- MASS::cabbages[MASS::cabbages$Cult == 'c39', 'HeadWt']
y <- MASS::cabbages[MASS::cabbages$Cult == 'c52', 'HeadWt']

test <- center.test(x, y, paired = FALSE)
print(test)

## -----------------------------------------------------------------------------
plot(test)

## -----------------------------------------------------------------------------
x <- MASS::anorexia[MASS::anorexia$Treat == 'FT', 'Postwt']
y <- MASS::anorexia[MASS::anorexia$Treat == 'FT', 'Prewt']

center.test(x, y, alternative = 'greater', paired = TRUE)

