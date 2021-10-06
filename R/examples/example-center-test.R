set.seed(1)

# Familial test using Huber family with point null
test <- center.test(MASS::geyser$waiting, mu = 70)
print(test)

# Familial test using Huber family with interval null
test <- center.test(MASS::galaxies, mu = c(20600, 30000))
plot(test)
