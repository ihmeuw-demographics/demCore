library(data.table)

# ============================================================================
# Aggregation

# set up standard input data.tables
input_dt <- data.table::data.table(
  sex = c(rep("female", 5), rep("male", 5)),
  qx = c(rep(.1, 5), rep(.2, 5)),
  age_start = rep(seq(15, 35, 5), 2),
  age_end = rep(seq(20, 40, 5), 2)
)
age_mapping <- data.table::data.table(
  age_start = c(15),
  age_end = c(40)
)
id_cols = c("sex", "age_start", "age_end")

# set up expected output table
expected_dt <- data.table(
  sex = c("female", "male"),
  age_start = c(15, 15),
  age_end = c(40, 40),
  qx = c(0.40951, 0.67232)
)

test_that("test that `agg_qx` basic functionality works", {
  output_dt <- agg_qx(input_dt, id_cols = id_cols, age_mapping = age_mapping)
  testthat::expect_equal(output_dt, expected_dt)
})

test_that("test that `agg_qx` errors are thrown for different cases", {
  # Check error thrown when wrong argument types are given
  testthat::expect_error(agg_qx(age_mapping, id_cols, input_dt))
  testthat::expect_error(agg_qx(input_dt, age_mapping, id_cols))

  # Check error thrown when missing age_end in data
  testthat::expect_error(agg_qx(input_dt[, c("sex", "age_start", "qx")],
                                 id_cols = id_cols, age_mapping = age_mapping))

  # Check that error thrown when age_end > largest age_end in data
  age_mapping_test <- copy(age_mapping)
  testthat::expect_error(agg_qx(input_dt, id_cols = id_cols,
                                age_mapping = age_ma))

  # Check that error thrown when data not square
  testthat::expect_error(agg_qx(input_dt[1:8], age_start = 15, age_end = 40,
                                id_cols = id_cols))

  # Check that error thrown if age_start or age_end are not in data
  testthat::expect_error(agg_qx(input_dt, age_start = 16, age_end = 40,
                                id_cols = id_cols))
  testthat::expect_error(agg_qx(input_dt, age_start = 15, age_end = 39,
                                id_cols = id_cols))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(agg_qx(non_unique_input_dt, age_start = 15,
                                age_end = 40, id_cols = id_cols))
})


# ============================================================================
# Scaling

input_dt <- data.table::data.table(
  sex = "male",
  qx = c(0.03, 0.015, 0.005, 0.004, 0.001, 0.03, 0.05),
  age_start = c(0, 1, 2, 3, 4, 1, 0),
  age_end = c(1, 2, 3, 4, 5, 5, 5)
)

test_that("test that `scale_qx` gives correct age outputs", {
  dt <- scale_qx(input_dt, id_cols = c("sex", "age_start", "age_end"))
  testthat::expect_equal(nrow(dt), 7)
  testthat::expect_equal(c(0,0,1,1:4), dt$age_start)
  testthat::expect_equal(c(1,5,2,5,3,4,5), dt$age_end)
})

test_that("test that `scale_qx` gives values that aggregate correctly", {

  dt <- scale_qx(input_dt, id_cols = c("sex", "age_start", "age_end"))

  # <1, 1-4, <5
  val_1p0 <- 1 - dt[age_start == 0 & age_end == 1, qx]
  val_4p1 <- 1 - dt[age_start == 1 & age_end == 5, qx]
  val_5p0 <- 1 - dt[age_start == 0 & age_end == 5, qx]
  testthat::expect_equal(val_5p0, 0.95)
  testthat::expect_equal(val_5p0, val_1p0 * val_4p1)

  # 1, 2, 3, 4, and 1-4
  p1 <- 1 - dt[age_start == 1 & age_end == 2, qx]
  p2 <- 1 - dt[age_start == 2 & age_end == 3, qx]
  p3 <- 1 - dt[age_start == 3 & age_end == 4, qx]
  p4 <- 1 - dt[age_start == 4 & age_end == 5, qx]
  p1to4 <- 1 - dt[age_start == 1 & age_end == 5, qx]
  testthat::expect_equal(p1to4, p1*p2*p3*p4)

})

test_that("test that `scale_qx` works with non-integer age vals", {

  # create test intput
  input_dt2 <- copy(input_dt)
  input_dt2[, age_start := age_start / 10]
  input_dt2[, age_end := age_end / 10]

  # run function for both age sets
  dt <- scale_qx(input_dt2, id_cols = c("sex", "age_start", "age_end"))
  compare <- scale_qx(input_dt, id_cols = c("sex", "age_start", "age_end"))

  # check output qx is the same for integer and non-integer
  testthat::expect_equal(dt$qx, compare$qx)

})


