test_that("simple_cumsum", {
  # simple_cumsum always returns a matrix, and cumsums by columns
  x <- matrix(c(1, 1, 10, 2, 2, 20), nrow = 2, byrow = TRUE)
  expected <- matrix(c(1, 1, 10, 3, 3, 30), nrow = 2, byrow = TRUE)
  expect_equal(simple_cumsum(x), expected)

  expect_equal(simple_cumsum(x[1, ]), expected[1, , drop = FALSE])
  expect_equal(simple_cumsum(x[2, ]), matrix(c(2, 2, 20), nrow = 1))

  expect_equal(simple_cumsum(x[1, , drop = FALSE]), expected[1, , drop = FALSE])
  expect_equal(simple_cumsum(x[2, , drop = FALSE]), matrix(c(2, 2, 20), nrow = 1))
})
