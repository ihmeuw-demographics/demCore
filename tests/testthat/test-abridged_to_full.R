library(data.table)

# load test data
data("exampleLT")
data("fullLTpars")
fullLTpars <- fullLTpars[sex == "female", .(age_start, age_end, intercept,
                                            slope)]
id_cols <- c("age_start", "age_end")

# run function
dt1 <- abridged_to_full(
  dt = exampleLT,
  regression_fits = fullLTpars,
  id_cols = id_cols,
  regression_id_cols = c("age_start", "age_end"),
  lx_spline_start_age = 15,
  lx_spline_end_age = 100
)

test_that("test that `abridged_to_full` gives us expected ages", {
  testthat::expect_equal(dt1$age_start, 0:95)
})
