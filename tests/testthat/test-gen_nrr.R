library(data.table)

input <- data.table::data.table(
  age_start = seq(15, 45, 5),
  age_end = seq(20, 50, 5),
  sex = "female",
  asfr = c(),
  srb = c(),
  nLx = c()
)

# ~1.499 for ISR 2019
expected <- data.table::data.table(
  sex = "female",
  nrr = c()
)

testthat::test_that("`gen_nrr` gives expected output", {

  output <- gen_nrr(
    input,
    id_cols = c("age_start", "age_end", "sex"),
    reproductive_age_start = 15,
    reproductive_age_end = 50
  )
  testthat::expect_equal(output, expected)
})
