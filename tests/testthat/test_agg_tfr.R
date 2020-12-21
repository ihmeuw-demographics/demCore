library(data.table)

# test dataset from Preston pg 96 Box 5.1
dt <- data.table::data.table(
  location = "USA", year = 1992,
  age_start = seq(10, 45, 5), age_end = seq(15, 50, 5),
  asfr = c(0.0014, 0.0607, 0.1146, 0.1174, 0.0802, 0.0325, 0.0059, 0.0003)
)

expected <- data.table::data.table(
  location = "USA", year = 1992,
  age_start = 10, age_end = 50,
  tf = c(2.064)
)

id_cols <- c("location", "year", "age_start", "age_end")
mapping <- data.table(age_start = 10, age_end = 50)

test_that("test that `agg_tf` gives expected output", {
  output_dt <- agg_tf(
    dt = dt,
    id_cols = id_cols,
    age_mapping = mapping
  )
  testthat::expect_equivalent(output_dt, expected, tolerance = 1e16)
})

test_that("test that `agg_tf` gives expected errors", {
  testthat::expect_error(
    agg_tf(
      dt = dt,
      id_cols = id_cols,
      age_mapping = data.table(age_start = 10, age_end = 18)
    ),
    regexp = "expected input data is missing."
  )
})
