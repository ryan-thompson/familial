test_that('x must not contain NAs', {
  set.seed(123)
  x <- rnorm(10)
  x[1] <- NA
  expect_error(center.test(x))
})

test_that('y must not contain NAs', {
  set.seed(123)
  x <- rnorm(10)
  y <- rnorm(10)
  y[1] <- NA
  expect_error(center.test(x, y))
})

test_that('print function returns output', {
  set.seed(123)
  x <- rnorm(10)
  test <- center.test(x)
  expect_output(print(test))
})

test_that('plot function returns a plot', {
  set.seed(123)
  x <- rnorm(10)
  test <- center.test(x)
  expect_s3_class(plot(test), 'ggplot')
})

test_that('indeterminate result when center is zero', {
  set.seed(123)
  x <- rnorm(100)
  expect_equal(center.test(x, family = 'huber')$decision, 'indeterminate')
})

test_that('H0 is accepted when center is zero and H0 is an interval', {
  set.seed(123)
  x <- rnorm(100)
  expect_equal(center.test(x, family = 'huber', mu = c(- 1, 1))$decision, 'H0')
})

test_that('H1 is accepted when center is not zero', {
  set.seed(123)
  x <- rnorm(100, 1)
  expect_equal(center.test(x, family = 'huber')$decision, 'H1')
})

test_that('indeterminate result when difference in centers is zero', {
  set.seed(123)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber')$decision, 'indeterminate')
})

test_that('H0 is accepted when difference in centers is zero and H0 is an interval', {
  set.seed(123)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber', mu = c(- 1, 1))$decision, 'H0')
})

test_that('H1 is accepted when difference in centers is not zero', {
  set.seed(123)
  x <- rnorm(100, 1)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber')$decision, 'H1')
})

test_that('indeterminate result when center of differences is zero', {
  set.seed(123)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber', paired = TRUE)$decision, 'indeterminate')
})

test_that('H0 is accepted when center of differences is zero and H0 is an interval', {
  set.seed(123)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber', mu = c(- 1, 1), paired = TRUE)$decision, 'H0')
})

test_that('H1 is accepted when center of differences is not zero', {
  set.seed(123)
  x <- rnorm(100, 1)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber', paired = TRUE)$decision, 'H1')
})

test_that('H0 with null set (-Inf, 0) or (0, Inf) matches result for a one-sided test', {
  set.seed(123)
  x <- rnorm(100)
  set.seed(123)
  p1 <- center.test(x, mu = c(- Inf, 0))$prob[1]
  set.seed(123)
  p2 <- center.test(x, alternative = 'greater')$prob[1]
  expect_equal(p1, p2)
  set.seed(123)
  p1 <- center.test(x, mu = c(0, Inf))$prob[1]
  set.seed(123)
  p2 <- center.test(x, alternative = 'less')$prob[1]
  expect_equal(p1, p2)
})

test_that('sequential and parallel tests return same result', {
  set.seed(123)
  x <- rnorm(100)
  test.seq <- center.test(x)
  cl <- parallel::makeCluster(2)
  test.par <- center.test(x, cluster = cl)
  parallel::stopCluster(cl)
  expect_equal(test.seq$decision, test.par$decision)
})
