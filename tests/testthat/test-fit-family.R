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
  p.x <- plot(path.x)
  p.xx <- plot(path.x, path.x)
  expect_s3_class(p.x, 'ggplot')
  expect_s3_class(p.xx, 'ggplot')
})

test_that('plot function requires x and y to belong to same family', {
  set.seed(123)
  x <- rnorm(100)
  path.huber <- fit.family(x, family = 'huber')
  path.trimmed <- fit.family(x, family = 'trimmed')
  expect_error(plot(path.huber, path.trimmed))
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
  expect_equal(head(path.w$mu, 1), matrixStats::weightedMean(x, w))
  expect_equal(tail(path.w$mu, 1), matrixStats::weightedMedian(x, w, interpolate = F))
})

test_that('first and last points of huber family are mean and median for odd samples', {
  set.seed(123)
  x <- rnorm(99)
  path <- fit.family(x, family = 'huber')
  expect_equal(head(path$mu, 1), mean(x))
  expect_equal(tail(path$mu, 1), median(x))
  w <- abs(rnorm(99))
  path.w <- fit.family(x, w, family = 'huber')
  expect_equal(head(path.w$mu, 1), matrixStats::weightedMean(x, w))
  expect_equal(tail(path.w$mu, 1), matrixStats::weightedMedian(x, w, interpolate = F))
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
  expect_equal(head(path.w$mu, 1), matrixStats::weightedMean(x, w))
  expect_equal(tail(path.w$mu, 1), matrixStats::weightedMedian(x, w, interpolate = F))
})

test_that('first and last points of trimmed family are mean and median for even samples', {
  set.seed(123)
  x <- rnorm(100)
  path <- fit.family(x, family = 'trimmed')
  expect_equal(head(path$mu, 1), mean(x))
  expect_equal(tail(path$mu, 1), median(x))
  w <- abs(rnorm(100))
  path.w <- fit.family(x, w, family = 'trimmed')
  expect_equal(head(path.w$mu, 1), matrixStats::weightedMean(x, w))
  expect_equal(tail(path.w$mu, 1), matrixStats::weightedMedian(x, w, interpolate = F))
})

test_that('first and last points of trimmed family are mean and median for odd samples', {
  set.seed(123)
  x <- rnorm(99)
  path <- fit.family(x, family = 'trimmed')
  expect_equal(head(path$mu, 1), mean(x))
  expect_equal(tail(path$mu, 1), median(x))
  w <- abs(rnorm(99))
  path.w <- fit.family(x, w, family = 'trimmed')
  expect_equal(head(path.w$mu, 1), matrixStats::weightedMean(x, w))
  expect_equal(tail(path.w$mu, 1), matrixStats::weightedMedian(x, w, interpolate = F))
})

test_that('first and last points of trimmmed family are mean and median for samples with repeated
          observations', {
  set.seed(123)
  x <- rnorm(10)
  x <- rep(x, 10)
  path <- fit.family(x, family = 'trimmed')
  expect_equal(head(path$mu, 1), mean(x))
  expect_equal(tail(path$mu, 1), median(x))
  w <- abs(rnorm(100))
  path.w <- fit.family(x, w, family = 'trimmed')
  expect_equal(head(path.w$mu, 1), matrixStats::weightedMean(x, w))
  expect_equal(tail(path.w$mu, 1), matrixStats::weightedMedian(x, w, interpolate = F))
})
