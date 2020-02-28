library(data.table)

# test input data
dt <- data.table::data.table(
  sex = rep("both", 4),
  age = c(0, 5, 10, 15),
  age_int = c(5, 5, 5, 120),
  mx = c(0.1, 0.2, 0.3, 0.4),
  ax = c(2.5, 2.5, 2.5, 2.5)
)
dt[, qx := mx_ax_to_qx(mx, ax, age_int)]

param_cols <- c("mx", "ax", "qx", "px",
                "lx", "dx", "nLx",
                "Tx", "ex")

test_that("check `lifetable` basic functionality", {
  dt <- lifetable(dt, id_cols = c("age", "sex"), terminal_age = 15)
  testthat::expect_equal(T, setequal(names(dt),
                                     c("sex", "age", "age_int",
                                      param_cols)))
  assertable::assert_values(dt, param_cols, "not_na", quiet = T)
})

test_that("check `lifetable` works when missing ax", {
  dt_no_ax <- dt[, .(sex, age, age_int, mx, qx)]
  dt <- lifetable(dt_no_ax, id_cols = c("age", "sex"), terminal_age = 15)
  testthat::expect_equal(T, setequal(names(dt),
                                     c("sex", "age", "age_int",
                                       param_cols)))
  assertable::assert_values(dt, param_cols, "not_na", quiet = T)
})

test_that("check `lifetable` works when missing mx", {
  dt_no_mx <- dt[, .(sex, age, age_int, ax, qx)]
  dt <- lifetable(dt_no_mx, id_cols = c("age", "sex"), terminal_age = 15)
  testthat::expect_equal(T, setequal(names(dt),
                                     c("sex", "age", "age_int",
                                       param_cols)))
  assertable::assert_values(dt, param_cols, "not_na", quiet = T)
})

test_that("check `lifetable` works when missing qx", {
  dt_no_qx <- dt[, .(sex, age, age_int, mx, ax)]
  dt <- lifetable(dt_no_qx, id_cols = c("age", "sex"), terminal_age = 15)
  testthat::expect_equal(T, setequal(names(dt),
                                     c("sex", "age", "age_int",
                                       param_cols)))
  assertable::assert_values(dt, param_cols, "not_na", quiet = T)
})
