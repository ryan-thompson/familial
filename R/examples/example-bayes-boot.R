set.seed(123)

boot <- bayes.boot(MASS::galaxies, fun = fit.family, nboot = 100)
head(boot)
