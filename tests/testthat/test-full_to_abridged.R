library(data.table)

# test input data
dt <- data.table(
  location = "Canada",
  age = c(0:110),
  qx = .2,
  ax = .5
)
id_cols = c("age", "location")

# expected output
expected_dt <- data.table(
  location = "Canada",
  age = c(0, 1, seq(5, 110, 5)),
  qx = c(0.2, 0.5904, rep(0.67232, 21), 1),
  ax = c(0.5, 1.7249, rep(2.06306, 21), 0.5)
)

test_that("check `full_to_abridged` basic functionality", {
  output_dt <- full_to_abridged(dt, id_cols)
  testthat::expect_equivalent(expected_dt, output_dt, tolerance = 0.01)
})

test_that("check `full_to_abridged` works with odd ages", {
  # check error if abridged ages are bad
  testthat::expect_error(full_to_abridged(dt, id_cols,
                                         abridged_ages = c(-20, 0, 500)))

  # check functionality with non-standard ages
  output_dt <- full_to_abridged(dt, id_cols, abridged_ages = seq(0, 110, 10))
  testthat::expect_equal(nrow(output_dt), 12)

  # check functionality with oddly ordered ages
  output_dt <- full_to_abridged(dt, id_cols,
                               abridged_ages = c(110, 100, seq(0, 90, 10)))
  testthat::expect_equal(nrow(output_dt), 12)
})
