library(data.table)

# test input
dt <- data.table::data.table(
  age = rep(c(0:2), 3),
  mx = c(0.09, 0.12, 0.08, 0.11, 0.14, 0.05, 0.4, 0.3, 0.07),
  sex = rep(c("male", "female", "both"), each = 3),
  location = rep("Canada", 9)
)
setorderv(dt, c("sex", "age"))

# expected output
expected <- copy(dt)
expected$ax <- c(0.34000, 1.35650, NA,
                 0.35000, 1.36100, NA,
                 0.28656, 1.39756, NA)

test_that("test that `gen_u5_ax` gives expected output", {
  output <- gen_u5_ax(dt, id_cols = c("age", "sex", "location"))
  setorderv(output, c("sex", "age"))
  setcolorder(output, c("age", "mx", "sex", "location", "ax"))
  testthat::expect_equivalent(output, expected, tolerance = 0.001)
})

test_that("test that `gen_u5_ax` gives errors when it should", {
  testthat::expect_error(gen_u5_ax(dt, id_cols = c("age", "sex", "year")))
  testthat::expect_error(gen_u5_ax(dt[age != 0], id_cols = c("age", "sex")))
})
