testthat::context("lx_to_dx tests")

library(data.table)

# set up standard input data.table
input_dt <- data.table(
  sex = rep("both", 4),
  age = c(0, 5, 10, 15),
  lx = c(1, 0.9, 0.7, 0.2)
)
id_cols <- c("sex")

# set up expected output table
expected_dx <- c(0.1, 0.2, 0.5, 0.2)

test_that("check `lx_to_dx()` basic functionality works", {
  output_dt <- lx_to_dx(input_dt, terminal_age = 15, id_cols)
  testthat::expect_equal(output_dt$dx, expected_dx)
})

test_that("check `qx_to_lx()` errors are thrown for different cases", {
  # check error thrown when wrong argument types are given
  testthat::expect_error(lx_to_dx(as.data.frame(input_dt), id_cols, 15, T))
  testthat::expect_error(lx_to_dx(input_dt, "hello", 15, T))
  testthat::expect_error(lx_to_dx(input_dt, id_cols, T, T))
  testthat::expect_error(lx_to_dx(input_dt, id_cols, 15, 15))

  # check error thrown when missing cols
  testthat::expect_error(lx_to_dx(input_dt[, c("sex", "lx")], id_cols, 15, T))
  testthat::expect_error(lx_to_dx(input_dt[, c("sex", "age")], id_cols, 15, T))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(lx_to_dx(non_unique_input_dt, id_cols, 15, T))

  # check error thrown if terminal_age < oldest age in data
  testthat::expect_error(lx_to_dx(input_dt, id_cols, 5))

  # check that extra age variable doesn't change results (like age_group_id)
  input_dt2 <- copy(input_dt)
  input_dt2$age_group_id <- c(1:4)
  output_dt2 <- lx_to_dx(input_dt2, terminal_age = 15, c("sex", "age_group_id"))
  testthat::expect_equal(output_dt2$dx, expected_dx)
})
