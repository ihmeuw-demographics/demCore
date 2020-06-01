library(data.table)

# test input data
dt <- data.table(
  location = "Canada",
  age_start = c(0:110),
  age_end = c(1:110, Inf),
  qx = .2,
  ax = .5
)
dt[age_end == Inf, qx := 1]
id_cols = c("age_start", "age_end", "location")

age_mapping <- data.table(
  age_start = c(0, 1, seq(5, 110, 5)),
  age_end = c(1, seq(5, 110, 5), Inf)
)

# expected output
expected_dt <- data.table(
  location = "Canada",
  age_start = c(0, 1, seq(5, 110, 5)),
  age_end = c(1, seq(5, 110, 5), Inf),
  qx = c(0.2, 0.5904, rep(0.67232, 21), 1),
  ax = c(0.5, 1.7249, rep(2.06306, 21), 0.5)
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
    regexp = "missing intervals"
  )

  # check functionality with non-standard ages
  output_dt <-agg_lt(
    dt = dt,
    id_cols = id_cols,
    age_mapping = data.table(
      age_start = seq(0, 110, 10),
      age_end = c(seq(10, 110, 10), Inf)
    )
  )
  testthat::expect_equal(nrow(output_dt), 12)

  # check functionality with oddly ordered ages
  output_dt <- agg_lt(
    dt = dt,
    id_cols = id_cols,
    age_mapping = data.table(
      age_start = c(110, 100, seq(0, 90, 10)),
      age_end = c(Inf, 110, seq(10, 100, 10))
    )
  )
  testthat::expect_equal(nrow(output_dt), 12)
})
