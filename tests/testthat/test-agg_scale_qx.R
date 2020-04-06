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
  age_start = c(0, 1, 2, 3, 4, 1, 0),
  age_end = c(1, 2, 3, 4, 5, 5, 5),
  qx = c(0.03, 0.015, 0.005, 0.004, 0.001, 0.03, 0.05)
)

expected_dt <- data.table::data.table(
  sex = "male",
  age_start = c(0, 1, 2, 3, 4, 1, 0),
  age_end = c(1, 2, 3, 4, 5, 5, 5),
  qx = c(0.025320566, 0.015126059, 0.005127338, 0.004127466, 0.001127850,
         0.025320566, 0.05)
)

test_that("test that `scale_qx` gives expected output", {
  output_dt <- scale_qx(input_dt, id_cols = c("sex", "age_start", "age_end"))
  setorderv(output_dt, cols = c("age_end", "age_start"))
  setorderv(expected_dt, cols = c("age_end", "age_start"))
  testthat::expect_equivalent(output_dt, expected_dt, tolerance = 0.00001)
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


