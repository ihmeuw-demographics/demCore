library(data.table)

# setup sample input data
data("fNOR2010")
dt <- data.table::data.table()
for(d in 1:1000){
  dt_new <- copy(fNOR2010)
  dt_new[, draw := d]
  dt_new[, mx := mx * rnorm(1, mean = 1, sd = 0.05)]
  dt_new[, ax := ax * rnorm(1, mean = 1, sd = 0.05)]
  dt_new[, qx := mx_ax_to_qx(mx, ax, age_length)]
  dt <- rbind(dt, dt_new, fill = T)
}
dt <- dt[!is.na(age)]
dt <- dt[, .SD, .SDcols = c("age", "draw", "age_length", "location", "mx", "ax",
                            "qx")]
dt <- lifetable(dt, id_cols = c("age", "location", "draw"), terminal_age = 95)

test_that("check `gen_summary_lt` basic functionality", {
  # run function
  output_dt <- gen_summary_lt(dt, id_cols = c("age", "draw", "location"),
                             lt_params = c("mx", "ax", "qx", "lx", "dx",
                                           "ex", "nLx", "Tx"))
  # check columns in output
  testthat::expect_equivalent(names(output_dt), c("age", "location",
                                           "life_table_parameter", "mean",
                                           "lower", "upper"))
  # check lower <= mean <= upper
  testthat::expect_equal(nrow(output_dt[lower > mean]), 0)
  testthat::expect_equal(nrow(output_dt[mean > upper]), 0)

  # check that mean doesn't deviate substantially from expectation
  output_wide <- copy(output_dt)
  output_wide <- dcast(output_wide, age ~ life_table_parameter,
                       value.var = "mean")
  output_wide <- merge(output_wide, fNOR2010, by = c("age"))
  testthat::expect_equal(output_wide$mx.x, output_wide$mx.y, tolerance = 0.01)
  testthat::expect_equal(output_wide$ax.x, output_wide$ax.y, tolerance = 0.01)

})

