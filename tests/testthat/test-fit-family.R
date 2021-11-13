test_that('x must not contain NAs', {
  set.seed(123)
  x <- rnorm(10)
  x[1] <- NA
  expect_error(fit.family(x))
})

test_that('w must not contain negative values', {
  set.seed(123)
  x <- rnorm(10)
  w <- abs(rnorm(10))
  w[1] <- - w[1]
  expect_error(fit.family(x, w))
})

test_that('w must contain at least one positive value', {
  set.seed(123)
  x <- rnorm(10)
  w <- rep(0, 10)
  expect_error(fit.family(x, w))
})

test_that('x and w must be same length', {
  set.seed(123)
  x <- rnorm(10)
  w <- abs(rnorm(9))
  expect_error(fit.family(x, w))
})

test_that('plot function returns a plot', {
  set.seed(123)
  x <- rnorm(100)
  path.x <- fit.family(x)
  expect_s3_class(plot(path.x), 'ggplot')
  expect_s3_class(plot(path.x, path.x), 'ggplot')
})

test_that('constant x is handled', {
  set.seed(123)
  x <- rep(1, 10)
  expect_equal(fit.family(x)$mu.hat, 1)
})

test_that('first and last points of huber family are mean and median for even samples', {
  set.seed(123)
  x <- rnorm(100)
  path <- fit.family(x, family = 'huber')
  expect_equal(head(path$mu, 1), mean(x))
  expect_equal(tail(path$mu, 1), median(x))
  w <- abs(rnorm(100))
  path.w <- fit.family(x, w, family = 'huber')
  expect_equal(head(path.w$mu, 1), weighted.mean(x, w))
  expect_equal(tail(path.w$mu, 1), weighted.median(x, w))
})

test_that('first and last points of huber family are mean and median for odd samples', {
  set.seed(123)
  x <- rnorm(99)
  path <- fit.family(x, family = 'huber')
  expect_equal(head(path$mu, 1), mean(x))
  expect_equal(tail(path$mu, 1), median(x))
  w <- abs(rnorm(99))
  path.w <- fit.family(x, w, family = 'huber')
  expect_equal(head(path.w$mu, 1), weighted.mean(x, w))
  expect_equal(tail(path.w$mu, 1), weighted.median(x, w))
})

test_that('first and last points of huber family are mean and median for samples with repeated observations', {
  set.seed(123)
  x <- rnorm(10)
  x <- rep(x, 10)
  path <- fit.family(x, family = 'huber')
  expect_equal(head(path$mu, 1), mean(x))
  expect_equal(tail(path$mu, 1), median(x))
  w <- abs(rnorm(100))
  path.w <- fit.family(x, w, family = 'huber')
  expect_equal(head(path.w$mu, 1), weighted.mean(x, w))
  expect_equal(tail(path.w$mu, 1), weighted.median(x, w))
})
