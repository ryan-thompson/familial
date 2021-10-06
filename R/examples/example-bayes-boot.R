set.seed(1)

# Sequential bootstrap
boot <- bayes.boot(MASS::galaxies, fun = fit.family)
head(boot)

# Parallel bootstrap
cl <- parallel::makeCluster(2)
boot <- bayes.boot(MASS::galaxies, fun = fit.family)
parallel::stopCluster(cl)
head(boot)
