set.seed(123)

test <- center.test(MASS::galaxies, mu = 21000, nboot = 100)
print(test)
plot(test)

cl <- parallel::makeCluster(2)
test <- center.test(MASS::galaxies, mu = 21000, nboot = 100, cluster = cl)
parallel::stopCluster(cl)
print(test)
