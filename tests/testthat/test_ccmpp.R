test_that("test that `leslie_matrix` gives expected output", {
  leslie <- leslie_matrix(
    survival = thailand_initial_estimates$survival[year_start == 1960 &
                                                     sex == "female", value],
    asfr = c(rep(0, 3),
             thailand_initial_estimates$asfr[year_start == 1960, value],
             rep(0, 7)),
    srb = thailand_initial_estimates$srb[year_start == 1960, value],
    n_ages = 17, int = 5, female = TRUE
  )
  testthat::expect_identical(dim(leslie), c(17L, 17L))
})

test_that("test that `ccmpp` gives expected output", {
  population <- ccmpp(
    inputs = thailand_initial_estimates,
    settings = list(
      years = seq(1960, 1995, 5),
      sexes = c("female", "male"),
      ages = seq(0, 80, 5),
      ages_survival = seq(0, 85, 5),
      ages_asfr = seq(15, 45, 5)
    )
  )
  assertable::assert_ids(
    data = population,
    id_vars = list(year = seq(1960, 2000, 5),
                   sex = c("female", "male"),
                   age_start = seq(0, 80, 5)),
    quiet = T
  )
  testthat::expect_identical(
    names(population),
    c("year", "sex", "age_start", "age_end", "value")
  )
})
