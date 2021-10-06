set.seed(1)

# Bootstrap
boot <- bayes.boot(MASS::galaxies, fun = fit.family, nboot = 100)
head(boot)
