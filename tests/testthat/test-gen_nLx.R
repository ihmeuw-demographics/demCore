testthat::context("gen_nLx tests")

library(data.table)

# set up standard input data.table
dt <- data.table::data.table(
  sex = rep("both", 4),
  age = c(0, 5, 10, 15),
  age_int = c(5, 5, 5, 120),
  mx = c(0.1, 0.2, 0.3, 0.4),
  ax = c(2.5, 2.5, 2.5, 2.5)
)
dt[, qx := mx_ax_to_qx(mx, ax, age_int)]
dt <- qx_to_lx(dt, id_cols = c("sex"))
dt <- lx_to_dx(dt, id_cols = c("sex"), terminal_age = 15)
id_cols <- c("sex")

# set up expected output (rounded)
expected_nLx <- c(4.00, 2.00, 0.57, 0.07)

test_that("check `gen_nLx()` basic functionality works", {
  output_dt <- gen_nLx(dt, id_cols, terminal_age = 15)
  output_nLx <- round(output_dt$nLx, 2)
  testthat::expect_equal(output_nLx, expected_nLx)
})

test_that("check `gen_nLx()` errors are thrown for different cases", {
  # check error thrown when wrong argument types are given
  testthat::expect_error(gen_nLx(dt, "hello", 15))
  testthat::expect_error(gen_nLx(dt, id_cols, T))

  # check error thrown when terminal_age < max age in data
  testthat::expect_error(gen_nLx(dt, id_cols, 10))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(dt, dt)
  testthat::expect_error(gen_nLx(non_unique_input_dt, id_cols = id_cols, 15))
})
