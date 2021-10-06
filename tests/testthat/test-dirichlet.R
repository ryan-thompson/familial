test_that('object returned is n×d dimensional', {
  x <- rudirichlet(10, 10)
  expect_equal(nrow(x), 10)
  expect_equal(ncol(x), 10)
})
