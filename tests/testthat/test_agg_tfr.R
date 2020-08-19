library(data.table)

asfr.usa <- c(0.0004, 0.0170, 0.0681, 0.0964, 0.1003, 0.0526, 0.0119, 0.0008,
              0.0001)
asfr.mli <- c(0.0026, 0.1514, 0.2706, 0.2820, 0.2501, 0.1885, 0.1007, 0.0297,
              0.0035)

dt <- data.table::data.table('asfr' = c(asfr.usa, asfr.mli),
                             'age_start' = rep(seq(10, 50, 5), 2),
                             'age_end' = rep(seq(15, 55, 5), 2),
                             'location' = c(rep('USA', 9), rep('MLI', 9)))

expected <- data.table::data.table('location' = c('USA', 'MLI'),
                                   'age_start' = c(10, 10),
                                   'age_end' = c(20, 20),
                                   'tf' = c(0.087, 0.77))
setorder(expected, location)


test_that("test that `agg_tf` gives expected output", {
  test_dt <- agg_tf(dt, c('location'), 10, 20)
  testthat::expect_equivalent(test_dt, expected, tolerance = 1e16)
})

test_that("test that `agg_tf` gives expected errors", {
  testthat::expect_error(agg_tf(dt, c('location'), 10, 18))
})