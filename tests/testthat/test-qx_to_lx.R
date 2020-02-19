testthat::context("qx_to_lx() tests")
library(data.table)

# set up standard input data.table
input_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age = rep(c(0, 1, seq(5, 20, 5)), 2),
  qx = c(rep(.1, 6), rep(.2, 6))
)
id_cols <- c("sex")

# set up expected output table
expected_dt <- data.table(
  sex = c(rep("female", 6), rep("male", 6)),
  age = rep(c(0, 1, seq(5, 20, 5)), 2),
  qx = c(rep(.1, 6), rep(.2, 6)),
  lx = c(1, 0.9, 0.81, 0.729, 0.6561, 0.59049,
         1, 0.8, 0.64, 0.512, 0.40960, 0.32768)
)

test_that("check `qx_to_lx()` basic functionality works", {
  output_dt <- qx_to_lx(input_dt, id_cols)
  testthat::expect_equal(output_dt, expected_dt)
})

test_that("check `qx_to_lx()` errors are thrown for different cases", {
  # Check error thrown when wrong argument types are given
  testthat::expect_error(qx_to_lx(input_dt, "hello", T))
  testthat::expect_error(qx_to_lx(input_dt, T, id_cols))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(qx_to_lx(non_unique_input_dt, id_cols = id_cols))
})

test_that("check resulting lx is monotonic by age", {
  output_dt <- qx_to_lx(input_dt, id_cols)
  output_females <- output_dt[sex == "female"]$lx
  testthat::expect_equal(T, all(output_females == cummin(output_females)))
})
