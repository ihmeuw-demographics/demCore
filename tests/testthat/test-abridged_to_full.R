library(data.table)

# load test data
data("exampleLT")
data("fullLTpars")
fullLTpars <- fullLTpars[sex == "female", .(age, intercept, slope)]
id_cols <- c("location", "age")

# run function
dt1 <- abridged_to_full(dt = exampleLT,
            regression_fits = fullLTpars,
            id_cols = id_cols,
            regression_id_cols = c("age"),
            terminal_age = 95)

test_that("test that `abridged_to_full` gives us expected ages", {
  testthat::expect_equal(dt1$age, 0:95)
})

test_that("test that `abridged_to_full` works without input 'age_length'", {
  input_dt2 <- copy(exampleLT)
  input_dt2[, age_length := NULL]
  dt2 <- abridged_to_full(dt = input_dt2,
                            regression_fits = fullLTpars,
                            id_cols = id_cols,
                            regression_id_cols = c("age"),
                            terminal_age = 95)
  testthat::expect_equal(dt2$age, 0:95)
})
