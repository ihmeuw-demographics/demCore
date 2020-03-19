library(data.table)

# setup sample input data
data("exampleLT")
dt <- data.table::data.table()
for(d in 1:1000){
  dt_new <- copy(exampleLT)
  dt_new[, draw := d]
  dt_new[, mx := mx * rnorm(1, mean = 1, sd = 0.05)]
  dt_new[, ax := ax * rnorm(1, mean = 1, sd = 0.05)]
  dt_new[, qx := mx_ax_to_qx(mx, ax, age_length)]
  dt <- rbind(dt, dt_new, fill = T)
}
dt <- dt[!is.na(age)]
dt <- dt[, .SD, .SDcols = c("age", "draw", "age_length", "mx", "ax",
                            "qx")]
dt <- lifetable(dt, id_cols = c("age", "draw"), terminal_age = 95)

# run function
output_dt <- gen_summary_lt(dt, id_cols = c("age", "draw"),
                            lt_params = c("mx", "ax", "qx", "lx", "dx",
                                          "ex", "nLx", "Tx"))

test_that("test that `gen_summary_lt` gives expected columns", {
  testthat::expect_equivalent(names(output_dt), c("age",
                                           "life_table_parameter", "mean",
                                           "lower", "upper"))
})

test_that("test that `gen_sumamry_lt` gives lower <= mean <= upper", {
  testthat::expect_equal(nrow(output_dt[lower > mean]), 0)
  testthat::expect_equal(nrow(output_dt[mean > upper]), 0)
})

test_that("test that `gen_summary_lt` gives expected output vals", {
  # check that mean doesn't deviate substantially from expectation
  output_wide <- copy(output_dt)
  output_wide <- dcast(output_wide, age ~ life_table_parameter,
                       value.var = "mean")
  output_wide <- merge(output_wide, exampleLT, by = c("age"))
  testthat::expect_equal(output_wide$mx.x, output_wide$mx.y, tolerance = 0.01)
  testthat::expect_equal(output_wide$ax.x, output_wide$ax.y, tolerance = 0.01)
})

