
library(data.table)

# load test data
data("fNOR2010")
data("fullLTpars")
fullLTpars <- fullLTpars[sex == "female", .(age, intercept, slope)]
id_cols <- c("location", "age")

# run function
dt <- gen_full_lt(dt = fNOR2010,
            regression_fits = fullLTpars,
            id_cols = id_cols,
            regression_id_cols = c("age"),
            terminal_age = 95)

test_that("check `gen_full_lt` gives us expected ages", {
  testthat::expect_equal(dt$age, 0:95)
})
