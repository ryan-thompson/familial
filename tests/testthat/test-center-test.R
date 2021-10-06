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

test_that('indeterminate result when x and y are distributed same and H0 is a point', {
  set.seed(123)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber')$decision, 'indeterminate')
  expect_equal(center.test(x, y, family = 'trimmed')$decision, 'indeterminate')
})

test_that('H0 is accepted when x and y are distributed different and H0 is an interval', {
  set.seed(123)
  x <- rnorm(100)
  y <- rnorm(100)
  expect_equal(center.test(x, y, family = 'huber', mu = c(- 1, 1))$decision, 'H0')
  expect_equal(center.test(x, y, family = 'trimmed', mu = c(- 1, 1))$decision, 'H0')
})

test_that('H0 is rejected when x and y are distributed different but paired', {
  set.seed(123)
  x <- rnorm(100)
  y <- 0.5 * x + 0.5 * rnorm(100, mean = 1)
  expect_equal(center.test(x, y, family = 'huber', paired = TRUE)$decision, 'H1')
  expect_equal(center.test(x, y, family = 'trimmed', paired = TRUE)$decision, 'H1')
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

test_that('print function returns output', {
  set.seed(123)
  x <- rnorm(100)
  test <- center.test(x)
  expect_output(print(test))
})

test_that('plot function returns a plot', {
  set.seed(123)
  x <- rnorm(100)
  path <- center.test(x)
  p <- plot(path)
  expect_s3_class(p, 'ggplot')
})
