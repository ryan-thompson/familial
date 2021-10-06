set.seed(1)

# Familial test using Huber family with point null
test <- center.test(MASS::galaxies, mu = 21000, nboot = 100)
print(test)
plot(test)

# Familial test using Huber family with interval null
test <- center.test(MASS::galaxies, mu = c(20500, 21500), nboot = 100)
print(test)

# Familial test in parallel
cl <- parallel::makeCluster(2)
test <- center.test(MASS::galaxies, mu = c(20500, 21500), nboot = 100, cluster = cl)
parallel::stopCluster(cl)
print(test)
