library(data.table)

test_that("test that `mx_to_qx` gives expected output", {
  output <- mx_to_qx(0.25, 45)
  expected <- 0.999987
  testthat::expect_equal(expected, output, tolerance = 0.001)
})

test_that("test that `mx_ax_to_qx` gives expected output", {
  output <- mx_ax_to_qx(.2, 3, 5)
  expected <- 0.7142857
  testthat::expect_equal(expected, output, tolerance = 0.001)
})

test_that("test that `qx_to_mx` gives expected output", {
  output <- qx_to_mx(.5, 5)
  expected <- 0.1386294
  testthat::expect_equal(expected, output, tolerance = 0.001)
})

test_that("test that `mx_qx_to_ax` gives expected output", {
  output <- mx_qx_to_ax(0.02, 0.095, 5)
  expected <- 2.368421
  testthat::expect_equal(expected, output, tolerance = 0.001)
})

test_that("test that `qx_ax_to_mx` gives expected output", {
  output <- qx_ax_to_mx(0.02, 2.5, 5)
  expected <- 0.004040404
  testthat::expect_equal(expected, output, tolerance = 0.001)
})

test_that("test that `mx_to_ax` gives expected output", {
  output <- mx_to_ax(0.05, 5)
  expected <- 2.395942
  testthat::expect_equal(expected, output, tolerance = 0.001)
})

test_that("test that inverse functionality works for mx ax qx functions", {
  output <- qx_to_mx(mx_to_qx(0.02, 5), 5)
  expected <- 0.02
  testthat::expect_equal(output, expected)

  output <- qx_ax_to_mx(mx_ax_to_qx(0.02, 2.4, 5), 2.4, 5)
  expected <- 0.02
  testthat::expect_equal(output, expected)

  output <- mx_qx_to_ax(mx = 0.02, qx = mx_ax_to_qx(0.02, 2.4, 5), length = 5)
  expected <- 2.4
  testthat::expect_equal(output, expected)
})
