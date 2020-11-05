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
      ages_mortality = seq(0, 85, 5),
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

test_that("test that `ccmpp` gives expected output with immigration/emigration", {

  # create zero inputs for immigration and emigration
  inputs <- copy(thailand_initial_estimates)
  inputs$immigration <- inputs$net_migration
  inputs$emigration <- inputs$net_migration
  inputs$net_migration <- NULL

  population <- ccmpp(
    inputs = inputs,
    settings = list(
      years = seq(1960, 1995, 5),
      sexes = c("female", "male"),
      ages = seq(0, 80, 5),
      ages_mortality = seq(0, 85, 5),
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

test_that("test that `ccmpp` works with 'mx', 'ax', 'qx' inputs", {

  # create rough inputs for mx and ax
  lt <- copy(demCore::thailand_initial_estimates$survival)
  # px approximately equal to survivorship ratio
  lt[, qx := 1 - value]
  lt[, value := NULL]
  lt[is.infinite(age_end), qx := 1]
  # assign default ax values
  lt[, ax := 2.5]
  id_cols <- c("year_start", "year_end", "sex", "age_start", "age_end")
  demCore::lifetable(lt, id_cols = id_cols)

  # add inputs for mx and ax
  new_inputs <- copy(demCore::thailand_initial_estimates)
  new_inputs$mx <- lt[, .SD, .SDcols = c(id_cols, "mx")]
  setnames(new_inputs$mx, "mx", "value")
  new_inputs$ax <- lt[, .SD, .SDcols = c(id_cols, "ax")]
  setnames(new_inputs$ax, "ax", "value")
  new_inputs$survival <- NULL

  population <- ccmpp(
    inputs = new_inputs,
    settings = list(
      years = seq(1960, 1995, 5),
      sexes = c("female", "male"),
      ages = seq(0, 80, 5),
      ages_mortality = seq(0, 85, 5),
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
