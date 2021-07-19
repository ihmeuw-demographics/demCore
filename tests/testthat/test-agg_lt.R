
# Tests with qx and ax inputs ---------------------------------------------

# test input data
dt <- data.table(
  location = "Canada",
  age_start = c(0:110),
  age_end = c(1:110, Inf),
  qx = c(rep(.2, 110), 1),
  ax = .5
)
dt[age_end == Inf, qx := 1]
id_cols = c("age_start", "age_end", "location")

age_mapping <- data.table(
  age_start = c(1, seq(5, 105, 5)),
  age_end = seq(5, 110, 5)
)

# expected output
expected_dt <- data.table(
  location = "Canada",
  age_start = c(1, seq(5, 105, 5)),
  age_end = seq(5, 110, 5),
  qx = c(0.5904, rep(0.67232, 21)),
  ax = c(1.7249, rep(2.06306, 21))
)

test_that("test that `agg_lt` gives expected output", {
  output_dt <- agg_lt(
    dt = dt,
    id_cols = id_cols,
    age_mapping = age_mapping
  )
  testthat::expect_equivalent(expected_dt, output_dt, tolerance = 0.01)
})

test_that("test that `agg_lt` works with odd ages", {
  # check error if age mapping is bad
  testthat::expect_error(
    agg_lt(
      dt = dt,
      id_cols = id_cols,
      age_mapping = data.table(
        age_start = c(-20, 0, 500),
        age_end = c(0, 500, Inf)
      )
    ),
    regexp = "missing"
  )

  # check functionality with non-standard ages
  output_dt <- agg_lt(
    dt = dt,
    id_cols = id_cols,
    age_mapping = data.table(
      age_start = seq(0, 100, 10),
      age_end = seq(10, 110, 10)
    )
  )
  testthat::expect_equal(nrow(output_dt), 11)
})

# Tests with only qx input ------------------------------------------------

# set up standard input data.tables
input_dt <- data.table::data.table(
  sex = c(rep("female", 5), rep("male", 5)),
  age_start = rep(seq(15, 35, 5), 2),
  age_end = rep(seq(20, 40, 5), 2),
  qx = c(rep(.1, 5), rep(.2, 5))
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
  qx = c(1 - ((1 - 0.1) ^ 5), 1 - ((1 - 0.2) ^ 5))
)

test_that("test that `agg_lt` with only 'qx' works", {
  output_dt <- agg_lt(input_dt, id_cols = id_cols, age_mapping = age_mapping)
  testthat::expect_equal(output_dt, expected_dt)
})

test_that("test that `agg_lt` errors are thrown for different cases", {
  # Check error thrown when wrong argument types are given
  testthat::expect_error(agg_lt(age_mapping, id_cols, input_dt))
  testthat::expect_error(agg_lt(input_dt, age_mapping, id_cols))

  # Check error thrown when missing age_end in data
  testthat::expect_error(agg_lt(input_dt[, c("sex", "age_start", "qx")],
                                id_cols = id_cols, age_mapping = age_mapping))

  # Check that error thrown when age_end > largest age_end in data
  age_mapping_test <- copy(age_mapping)
  testthat::expect_error(agg_lt(input_dt, id_cols = id_cols,
                                age_mapping = age_ma))

  # Check that error thrown when data not square
  testthat::expect_error(agg_lt(input_dt[1:8], age_start = 15, age_end = 40,
                                id_cols = id_cols))

  # Check that error thrown if age_start or age_end are not in data
  testthat::expect_error(agg_lt(input_dt, age_start = 16, age_end = 40,
                                id_cols = id_cols))
  testthat::expect_error(agg_lt(input_dt, age_start = 15, age_end = 39,
                                id_cols = id_cols))

  # check error thrown when rows of input dt are not unique
  non_unique_input_dt <- rbind(input_dt, input_dt)
  testthat::expect_error(agg_lt(non_unique_input_dt, age_start = 15,
                                age_end = 40, id_cols = id_cols))
})


# test that `agg_lt` with age_mapping not covering the entire range -------

# set up standard input data.tables
input_dt <- data.table::data.table(
  sex = c(rep("female", 5), rep("male", 5)),
  age_start = rep(seq(15, 35, 5), 2),
  age_end = rep(seq(20, 40, 5), 2),
  qx = c(rep(.1, 5), rep(.2, 5))
)
age_mapping <- data.table::data.table(
  age_start = c(20),
  age_end = c(30)
)
id_cols = c("sex", "age_start", "age_end")

# set up expected output table
expected_dt <- data.table(
  sex = c("female", "male"),
  age_start = c(20, 20),
  age_end = c(30, 30),
  qx = c(1 - ((1 - 0.1) ^ 2), 1 - ((1 - 0.2) ^ 2))
)

test_that("test that `agg_lt` with age_mapping not covering the entire range", {
  output_dt <- agg_lt(input_dt, id_cols = id_cols, age_mapping = age_mapping)
  testthat::expect_equal(output_dt, expected_dt)
})


# test that `agg_lt` returns input parameters -----------------------------

dt <- data.table::data.table(
  age_start = c(0:110),
  age_end = c(1:110, Inf),
  location = "Canada",
  qx = c(rep(.2, 110), 1),
  mx = 0.22
)
id_cols = c("age_start", "age_end", "location")
age_mapping = data.table::data.table(
  age_start = seq(0, 105, 5),
  age_end = seq(5, 110, 5)
)

test_that("test that `agg_lt` returns parameters we started with", {
  output_dt <- agg_lt(dt, id_cols = id_cols, age_mapping = age_mapping)
  testthat::expect_equal(
    names(output_dt), c("age_start", "age_end", "location", "qx", "mx")
  )
})
