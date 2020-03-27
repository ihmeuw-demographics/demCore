library(data.table)

# setup sample input data
data("exampleLT")
base <- copy(exampleLT)
base <- demUtils::gen_length(base, col_stem = "age")
input_dt <- data.table::data.table()
for(d in 1:1000){
  dt_new <- copy(base)
  dt_new[, draw := d]
  dt_new[, mx := mx * rnorm(1, mean = 1, sd = 0.005)]
  dt_new[, ax := ax * rnorm(1, mean = 1, sd = 0.005)]
  dt_new[, qx := mx_ax_to_qx(mx, ax, age_length)]
  input_dt <- rbind(input_dt, dt_new, fill = T)
}
input_dt <- input_dt[!is.na(age_start)]
input_dt <- input_dt[, .SD, .SDcols = c("age_start", "age_end", "draw",
                                        "age_length", "mx", "ax", "qx")]
input_dt <- lifetable(input_dt, id_cols = c("age_start", "age_end", "draw"))

# run function
output_dt <- gen_summary_lt(
  input_dt,
  id_cols = c("age_start", "age_end", "draw"),
  lt_params = c("mx", "ax", "qx", "lx", "dx", "ex", "nLx", "Tx")
)

test_that("test that `gen_summary_lt` gives expected columns", {
  testthat::expect_equivalent(names(output_dt), c("age_start", "age_end",
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
  output_wide <- dcast(output_wide, age_start ~ life_table_parameter,
                       value.var = "mean")
  output_wide <- merge(output_wide, exampleLT, by = c("age_start"))
  testthat::expect_equal(output_wide$mx.x, output_wide$mx.y, tolerance = 0.01)
  testthat::expect_equal(output_wide$ax.x, output_wide$ax.y, tolerance = 0.01)
})

