library(data.table)

input <- data.table::data.table(
  age_start = seq(15, 45, 5),
  age_end = seq(20, 50, 5),
  sex = "female",
  asfr = c(0.00002, 0.009, 0.1, 0.18, 0.19, 0.11, 0.03),
  srb = 1.057,
  nLx = c(4.61, 4.55, 4.48, 4.39, 4.30, 4.18, 4.03)
)

expected <- data.table::data.table(
  sex = "female",
  nrr = c(1.30)
)

testthat::test_that("`calculate_nrr` gives expected output", {

  output <- calculate_nrr(
    input,
    id_cols = c("age_start", "age_end", "sex"),
    reproductive_age_start = 15,
    reproductive_age_end = 50
  )
  testthat::expect_equivalent(output, expected, tolerance = 0.01)
})
