library(data.table)

# set up standard input data.tables
dt <- data.table::data.table(
  sex = c(rep("female", 5), rep("male", 5)),
  qx = c(rep(.1, 5), rep(.2, 5)),
  age_start = rep(seq(15, 35, 5), 2),
  age_end = rep(seq(20, 40, 5), 2)
)
target_dt <- data.table::data.table(
  age_start = c(15),
  age_end = c(40)
)

# set up expected output table
expected_dt <- data.table(
  sex = c("female", "male"),
  age_start = c(15, 15),
  age_end = c(40, 40),
  qx = c(0.40951, 0.67232)
)

# test_that("check `agg_qx` basic functionality works", {
#   output_dt <- agg_qx(input_dt, age_start = 15, age_end = 40, id_cols = id_cols)
#   testthat::expect_equal(output_dt, expected_dt)
# })
#
# test_that("check `agg_qx` errors are thrown for different cases", {
#   # Check error thrown when wrong argument types are given
#   testthat::expect_error(agg_qx(input_dt, "hello", "hi", id_cols))
#   testthat::expect_error(agg_qx(input_dt, 15, 40, "hola"))
#
#   # Check error thrown when missing age_end in data
#   testthat::expect_error(agg_qx(input_dt[, c("sex", "age_start", "qx")],
#                                  age_start = 15, age_end = 40, id_cols = id_cols))
#
#   # Check that error thrown when age_end > largest age_end in data
#   testthat::expect_error(agg_qx(input_dt, age_start = 15, age_end = 70, id_cols = id_cols))
#
#   # Check that error thrown when data not square
#   testthat::expect_error(agg_qx(input_dt[1:8], age_start = 15, age_end = 40, id_cols = id_cols))
#
#   # Check that error thrown if age_start or age_end are not in data
#   testthat::expect_error(agg_qx(input_dt, age_start = 16, age_end = 40, id_cols = id_cols))
#   testthat::expect_error(agg_qx(input_dt, age_start = 15, age_end = 39, id_cols = id_cols))
#
#   # check error thrown when rows of input dt are not unique
#   non_unique_input_dt <- rbind(input_dt, input_dt)
#   testthat::expect_error(agg_qx(non_unique_input_dt, age_start = 15, age_end = 40, id_cols = id_cols))
# })
