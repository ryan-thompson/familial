# Familial test
test <- center.test(MASS::galaxies, mu = 21000, nboot = 100)
print(test)

# Familial test in parallel
cl <- parallel::makeCluster(2)
test <- center.test(MASS::galaxies, mu = 21000, nboot = 100, cluster = cl)
parallel::stopCluster(cl)
print(test)
