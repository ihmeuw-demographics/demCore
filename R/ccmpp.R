#' @title Cohort Component Method of Population Projection (CCMPP)
#'
#' @description Implements the deterministic female dominant cohort component
#' method of population projection.
#'
#' @param inputs \[`list()`\]\cr
#'   \[`data.table()`\] for each ccmpp input. Requires 'srb', 'asfr', 'baseline',
#'   and 'survival'; and migration estimates provided as just 'net_migration' or
#'   'immigration' and 'emigration'. See **Section: Inputs** for more
#'   information on each of the required inputs.
#' @param settings \[`list()`\]\cr
#'   Named list of settings for running [ccmpp()] with. See
#'   **Section: Settings** for more information on each of the required
#'   settings.
#' @param value_col \[`character(1)`\]\cr
#'   Name of the column containing the value of interest in each of the
#'   `inputs`. Default is 'value'.
#' @param assert_positive_pop \[`logical(1)`\]\cr
#'   Whether or not to check that the projected population estimates are all
#'   greater than or equal to zero. Default is 'TRUE'.
#' @param validate_arguments \[`logical(1)`\]\cr
#'   Whether to validate that the input arguments are formatted correctly.
#'   Default is 'TRUE'.
#' @param gen_end_interval_col \[`logical(1)`\]\cr
#'   Whether to automatically generate the end of the interval columns (like
#'   'age'). Default is 'TRUE'.
#'
#' @return \[`data.table()`\] of year-sex-age specific population counts.
#'
#' @details
#' Given information on the starting population size, the population at
#' later time points is a function of mortality, fertility, sex-ratio at birth,
#' and net-migration.
#'
#' The cohort component method of population projection (CCMPP) projects a
#' sex-age specific population forward in time using leslie matrices
#' [leslie_matrix()] which encode information about fertility, sex-ratio at
#' birth, and mortality.
#'
#' \deqn{Population(t + \delta) = Leslie[t, t + \delta] * Population(t)}
#'
#' See `vignette("ccmpp", package = "demCore")` or linked references for more
#' details on this method.
#'
#' @section Inputs:
#' srb: \[`data.table()`\]\cr
#'   * year_start: \[`integer()`\] start of the calendar year interval
#'   (inclusive). Corresponds to 'years' `setting`.
#'   * year_end: \[`integer()`\] end of the calendar year interval (exclusive).
#'   * `value_col`: \[`numeric()`\] sex-ratio at birth estimates.
#'
#' asfr: \[`data.table()`\]\cr
#'   * year_start: \[`integer()`\] start of the calendar year interval
#'   (inclusive). Corresponds to 'years' `setting`.
#'   * year_end: \[`integer()`\] end of the calendar year interval (exclusive).
#'   * age_start: \[`integer()`\] start of the age group (inclusive).
#'   Corresponds to 'ages_asfr' setting.
#'   * age_end: \[`integer()`\] end of the age group (exclusive).
#'   * `value_col`: \[`numeric()`\] annual age-specific fertility rate estimates.
#'
#' baseline: \[`data.table()`\]\cr
#'   * year: \[`integer()`\] mid-year for population estimate.
#'   Corresponds to 'years' `setting`.
#'   * sex: \[`character()`\] either 'female' or 'male'. Corresponds to 'sexes'
#'   `setting`.
#'   * age_start: \[`integer()`\] start of the age group (inclusive).
#'   Corresponds to 'ages' `setting`.
#'   * age_end: \[`integer()`\] end of the age group (exclusive).
#'   * `value_col`: \[`numeric()`\] baseline year population count estimates.
#'
#' survival: \[`data.table()`\]\cr
#'   * year_start: \[`integer()`\] start of the calendar year interval
#'   (inclusive). Corresponds to 'years' `setting`.
#'   * year_end: \[`integer()`\] end of the calendar year interval (exclusive).
#'   * sex: \[`character()`\] either 'female' or 'male'. Corresponds to 'sexes'
#'   `setting`.
#'   * age_start: \[`integer()`\] start of the age group (inclusive).
#'   Corresponds to 'ages' `setting`.
#'   * age_end: \[`integer()`\] end of the age group (exclusive).
#'   * `value_col`: \[`numeric()`\] survivorship ratio estimates.
#'
#' net_migration: \[`data.table()`\]\cr
#'   * year_start: \[`integer()`\] start of the calendar year interval
#'   (inclusive). Corresponds to 'years' `setting`.
#'   * year_end: \[`integer()`\] end of the calendar year interval (exclusive).
#'   * sex: \[`character()`\] either 'female' or 'male'. Corresponds to 'sexes'
#'   `setting`.
#'   * age_start: \[`integer()`\] start of the age group (inclusive).
#'   Corresponds to 'ages' `setting`.
#'   * age_end: \[`integer()`\] end of the age group (exclusive).
#'   * `value_col`: \[`numeric()`\] annual net-migration proportion estimates.
#'
#' immigration: \[`data.table()`\]\cr
#'   * year_start: \[`integer()`\] start of the calendar year interval
#'   (inclusive). Corresponds to 'years' `setting`.
#'   * year_end: \[`integer()`\] end of the calendar year interval (exclusive).
#'   * sex: \[`character()`\] either 'female' or 'male'. Corresponds to 'sexes'
#'   `setting`.
#'   * age_start: \[`integer()`\] start of the age group (inclusive).
#'   Corresponds to 'ages' `setting`.
#'   * age_end: \[`integer()`\] end of the age group (exclusive).
#'   * `value_col`: \[`numeric()`\] annual immigration proportion estimates.
#'
#' emigration: \[`data.table()`\]\cr
#'   * year_start: \[`integer()`\] start of the calendar year interval
#'   (inclusive). Corresponds to 'years' `setting`.
#'   * year_end: \[`integer()`\] end of the calendar year interval (exclusive).
#'   * sex: \[`character()`\] either 'female' or 'male'. Corresponds to 'sexes'
#'   `setting`.
#'   * age_start: \[`integer()`\] start of the age group (inclusive).
#'   Corresponds to 'ages' `setting`.
#'   * age_end: \[`integer()`\] end of the age group (exclusive).
#'   * `value_col`: \[`numeric()`\] annual emigration proportion estimates.
#'
#' @section Settings:
#'   * years: \[`numeric()`\]\cr
#'   The start of each calendar year interval to project in [demCore::ccmpp()].
#'   Corresponds to the 'year_start' column in each of the year-specific inputs.
#'   * sexes: \[`character()`\]\cr
#'   The sexes being projected in [demCore::ccmpp()], must be either just
#'   'female', or 'female' and 'male'. Corresponds to the 'sex' column in each
#'   of the sex-specific inputs.
#'   * ages: \[`numeric()`\]\cr
#'   The ages being projected in [demCore::ccmpp()]. Corresponds to the
#'   'age_start' column in each of the standard age-specific inputs.
#'   * ages_survival: \[`numeric()`\]\cr
#'   The ages for which survivorship ratio estimates are available, includes one
#'   extra age group compared to the 'ages' setting. Corresponds to the
#'   'age_start' column in the 'survival' \[`data.table()`\] input.
#'   * ages_asfr: \[`numeric()`\]\cr
#'   The assumed female reproductive ages, subset of 'ages'. Corresponds to the
#'   'age_start' column in the age-specific fertility rate (asfr)
#'   \[`data.table()`\] input.
#'
#' @examples
#' population <- ccmpp(
#'   inputs = thailand_initial_estimates,
#'   settings = list(
#'     years = seq(1960, 1995, 5),
#'     sexes = c("female", "male"),
#'     ages = seq(0, 80, 5),
#'     ages_survival = seq(0, 85, 5),
#'     ages_asfr = seq(15, 45, 5)
#'   )
#' )
#'
#' @references
#' Preston, Samuel, Patrick Heuveline, and Michel Guillot. 2001. Demography:
#' Measuring and Modeling Population. Wiley.
#'
#' @seealso
#' [leslie_matrix()]
#'
#' vignette("ccmpp")
#'
#' @export
ccmpp <- function(inputs,
                  settings,
                  value_col = "value",
                  assert_positive_pop = TRUE,
                  validate_arguments = TRUE,
                  gen_end_interval_col = TRUE) {

  # Validate input arguments ------------------------------------------------

  assertthat::assert_that(
    assertthat::is.flag(validate_arguments),
    msg = "`validate_arguments` must be a logical flag"
  )

  if (validate_arguments) validate_ccmpp_inputs(inputs, settings, value_col)

  int <- unique(diff(settings$ages))
  projection_years <- c(settings$years, max(settings$years) + int)
  all_cols <- c("year_start", "year_end", "sex", "age_start", "age_end",
                value_col)
  pop_all_cols <- c("year", "sex", "age_start", "age_end", value_col)
  if (!gen_end_interval_col) {
    all_cols <- all_cols[!grepl("_end$", all_cols)]
    pop_all_cols <- pop_all_cols[!grepl("_end$", pop_all_cols)]
  }
  id_cols <- setdiff(all_cols, value_col)
  pop_id_cols <- setdiff(pop_all_cols, value_col)

  # Project population ------------------------------------------------------

  # convert inputs to matrices
  inputs_mdt <- lapply(names(inputs), function(input) {
    dt <- inputs[[input]]
    mdt <- dt_to_matrix(
      dt = dt,
      id_cols = setdiff(names(dt), value_col),
      value_col = value_col,
      validate_arguments = FALSE
    )
    return(mdt)
  })
  names(inputs_mdt) <- names(inputs)

  # initialize population matrix
  population_mdt <- list()
  for (s in settings$sexes) {
    population_mdt[[s]] <- matrix(
      NA,
      nrow = length(settings$ages),
      ncol = length(projection_years)
    )
    population_mdt[[s]][, 1] <- inputs_mdt[["baseline"]][[s]]
    dimnames(population_mdt[[s]]) <- list(settings$ages,
                                          projection_years)
  }

  # loop over number of projection time periods
  for (i in 1:length(settings$years)) {
    y <- projection_years[i]
    y_next <- projection_years[i + 1]

    # get vector of values for current year
    population_female <- population_mdt[["female"]][, i]
    survival_female <- inputs_mdt[["survival"]][["female"]][, i]
    net_migration_female <- inputs_mdt[["net_migration"]][["female"]][, i]
    srb <- inputs_mdt[["srb"]][, i]
    # add assumed zero asfr ages
    asfr <- c(rep(0, sum(settings$ages < min(settings$ages_asfr))),
              inputs_mdt[["asfr"]][, i],
              rep(0, sum(settings$ages > max(settings$ages_asfr))))

    # create leslie matrix for females
    leslie_female <- leslie_matrix(
      survival = survival_female,
      asfr = asfr,
      srb = srb,
      n_ages = length(settings$ages),
      int = int,
      female = TRUE
    )

    # calculate half the number of net migrants for females
    half_net_migrants_female <- net_migration_female * population_female * 0.5

    # project female population forward one projection period
    population_next_female <- leslie_female %*%
      (population_female + half_net_migrants_female) +
      half_net_migrants_female

    # add on projected population for next year to complete matrix
    population_mdt[["female"]][, i + 1] <- population_next_female[, 1]

    # project male population forward one projection period
    if ("male" %in% settings$sexes) {

      # get vector of values for current year
      population_male <- population_mdt[["male"]][, i]
      survival_male <- inputs_mdt[["survival"]][["male"]][, i]
      net_migration_male <- inputs_mdt[["net_migration"]][["male"]][, i]

      # create leslie matrix for males
      leslie_male <- leslie_matrix(
        survival = survival_male,
        asfr = asfr,
        srb = srb,
        n_ages = length(settings$ages),
        int = int,
        female = FALSE
      )
      # calculate half the number of net migrants for males
      half_net_migrants_male <- net_migration_male * population_male * 0.5

      # project male population forward one projection period
      population_next_male <- leslie_male %*%
        (population_male + half_net_migrants_male) +
        half_net_migrants_male

      # back-calculate total births in projection period
      total_births <- population_next_female[1, 1] /
        (survival_female[1] * (1 / (1 + srb)))
      # calculate youngest male population age group
      young_male_pop <- total_births * survival_male[1] * (srb / (1 + srb))
      # add onto migrants
      population_next_male[1, 1] <- population_next_male[1, 1] + young_male_pop

      # add on projected population for next year to complete matrix
      population_mdt[["male"]][, i + 1] <- population_next_male[, 1]
    }
  }

  # convert from matrix to dt
  population_dt <- matrix_to_dt(
    mdt = population_mdt,
    year_right_most_endpoint = NULL,
    gen_end_interval_col = gen_end_interval_col,
    value_col = value_col,
    validate_arguments = validate_arguments
  )
  if (gen_end_interval_col) population_dt[, year_end := NULL]
  setnames(population_dt, "year_start", "year")

  # check population values
  assertable::assert_values(population_dt, colnames = value_col, test = "not_na",
                            quiet = T)
  if (assert_positive_pop) {
    assertable::assert_values(population_dt, colnames = value_col, test = "gte",
                              test_val = 0, quiet = T)
  }

  # format output
  data.table::setcolorder(population_dt, pop_all_cols)
  data.table::setkeyv(population_dt, pop_id_cols)
  return(population_dt)
}

#' @title Make Leslie matrix
#'
#' @description Constructs the Leslie matrix needed for cohort component method
#' of population projection [`ccmpp()`].
#'
#' @param survival \[`numeric(n_ages + 1)`\]\cr
#'   Survivorship ratio, the proportion of people aged x - `int` that will be
#'   alive `int` years later in a stationary population
#' @param asfr \[`numeric(n_ages)`\]\cr
#'   Annual age specific fertility rates NOT yet multiplied by `int`. Must
#'   include both reproductive and non-reproductive age groups that are zero.
#' @param srb \[`numeric(1)`\]\cr
#'   Sex ratio at birth.
#' @param n_ages \[`integer(1)`\]\cr
#'   Number of age groups.
#' @param int \[`integer(1)`\]\cr
#'   Width of the age groups and projection intervals. Usually 1 or 5.
#' @param female \[`logical(1)`\]\cr
#'   Whether making Leslie matrix for projection of the female population
#'   (versus male population).
#'
#' @return \[`matrix(n_ages, n_ages)`\]
#'
#' @details
#' The Leslie matrix encodes two key pieces of information:
#' 1. The survivorship ratio is included in the off diagonal to age the
#' population in each age group \eqn{a} at time \eqn{t} to age group
#' \eqn{a + int} at time \eqn{t + int}.
#' 2. When projecting the female population, the first row includes information
#' about age-specific fertility rates, maternal survivorship ratios, birth
#' survivorship ratios and the sex-ratio at birth to calculate the initial
#' population size of the youngest age group at time \eqn{t + int}.
#'
#' See the references for more information.
#'
#' @examples
#' leslie <- leslie_matrix(
#'   survival = thailand_initial_estimates$survival[year_start == 1960 &
#'                                                   sex == "female", value],
#'   asfr = c(rep(0, 3),
#'            thailand_initial_estimates$asfr[year_start == 1960, value],
#'            rep(0, 7)),
#'   srb = thailand_initial_estimates$srb[year_start == 1960, value],
#'   n_ages = 17, int = 5, female = TRUE
#' )
#'
#' @references
#' Preston, Samuel, Patrick Heuveline, and Michel Guillot. 2001. Demography:
#' Measuring and Modeling Population. Wiley.
#'
#' @export
leslie_matrix <- function(survival,
                          asfr,
                          srb,
                          n_ages,
                          int,
                          female) {

  # Validate input arguments ------------------------------------------------

  # check `n_ages` and `int` argument
  assertthat::assert_that(
    assertthat::is.count(n_ages),
    assertthat::is.count(int),
    msg = "`n_ages` and `int` must be integers of length 1"
  )

  # check `survival` argument
  assertthat::assert_that(
    assertive::is_numeric(survival),
    length(survival) == n_ages + 1,
    msg = "`survival` must be a numeric of length `n_ages` + 1"
  )

  # check `asfr` argument
  assertthat::assert_that(
    assertive::is_numeric(asfr),
    length(asfr) == n_ages,
    msg = "`asfr` must be a numeric of length `n_ages`"
  )

  # check `srb` argument
  assertthat::assert_that(
    assertthat::is.scalar(srb),
    msg = "`srb` must be a numeric of length 1"
  )

  # check `female` argument
  assertthat::assert_that(
    assertthat::is.flag(female),
    msg = "`female` must be a logical  of length 1"
  )

  # Create Leslie matrix ----------------------------------------------------

  # initialize matrix
  leslie <- matrix(0, nrow = n_ages, ncol = n_ages)

  # first row includes asfr, srb, and birth survival
  # (used to calculate youngest female population age group)
  if (female) {
    k <- 1 / (1 + srb) * survival[1] * 0.5
    leslie[1, ] <- k * int * (asfr + (c(asfr[-1], 0) * survival[-1]))
  }

  # other rows include survivorship ratios
  leslie[2:n_ages, 1:(n_ages - 1)] <- diag(survival[-c(1, n_ages + 1)])
  leslie[n_ages, n_ages] <- survival[n_ages + 1]

  # label columns and rows
  age_labels <- seq(from = 0, by = int, length = n_ages)
  dimnames(leslie) <- list(age_labels, age_labels)

  return(leslie)
}

#' @title Validate the ccmpp input arguments
#'
#' @inheritParams ccmpp
#'
#' @return Invisibly returns `inputs` but throws error if not formatted
#'   correctly.
validate_ccmpp_inputs <- function(inputs,
                                  settings,
                                  value_col) {

  # check for all required settings
  required_settings <- c("years", "sexes", "ages", "ages_survival", "ages_asfr")
  assertthat::assert_that(
    all(required_settings %in% names(settings)),
    msg = paste0("Need all required settings: '",
                 paste0(required_settings, collapse = "', '"), "'.")
  )

  # check `settings$years` argument
  assertthat::assert_that(
    assertive::is_numeric(settings$years),
    length(unique(diff(settings$years))) == 1,
    unique(diff(settings$years)) != 0,
    msg = paste0("`settings$years` must be a numeric vector defining the start ",
                 "of evenly spaced calendar year intervals")
  )

  # check `settings$sexes` argument
  assertthat::assert_that(
    assertive::is_character(settings$sexes),
    "female" %in% settings$sexes,
    all(settings$sexes %in% c("female", "male")),
    msg = paste0("`settings$sexes` must be a character vector containing ",
                 "'female' and optionally 'male'")
  )

  # check `settings$ages` argument
  assertthat::assert_that(
    assertive::is_numeric(settings$ages),
    length(unique(diff(settings$ages))) == 1,
    unique(diff(settings$ages)) != 0,
    msg = paste0("`settings$ages` must be a numeric vector defining the start ",
                 "of evenly spaced age group intervals")
  )

  # check implied interval in `settings$years` and `settings$ages` is identical
  assertthat::assert_that(
    identical(unique(diff(settings$years)), unique(diff(settings$ages))),
    msg = paste0("`settings$years` & `settings$ages` must have the same interval ",
                 "length between each calendar year interval and between each ",
                 "age group")
  )
  int <- unique(diff(settings$ages))

  # check `settings$ages_survival` argument
  assertthat::assert_that(
    assertive::is_numeric(settings$ages_survival),
    length(unique(diff(settings$ages_survival))) == 1,
    unique(diff(settings$ages_survival)) != 0,
    all(settings$ages %in% settings$ages_survival),
    max(settings$ages_survival) == max(settings$ages) + int,
    msg = paste0("`settings$ages_survival` must be a numeric vector defining ",
                 "the start of evenly spaced age group intervals for the ",
                 "survivorship ratio estimates")
  )

  # check `settings$ages_asfr` argument
  assertthat::assert_that(
    assertive::is_numeric(settings$ages_asfr),
    length(unique(diff(settings$ages_asfr))) == 1,
    unique(diff(settings$ages_asfr)) != 0,
    all(settings$ages_asfr %in% settings$ages),
    msg = paste0("`settings$ages_asfr` must be a numeric vector defining ",
                 "the start of evenly spaced age group intervals for the ",
                 "asfr estimates")
  )

  # check `value_col` argument
  assertthat::assert_that(assertthat::is.string(value_col))

  # check `inputs` argument
  assertthat::assert_that(
    assertive::is_list(inputs),
    all(mapply(assertive::is_data.table, inputs)),
    msg = "`inputs` must be a list of data.tables"
  )

  # check all required columns are present in `inputs`
  components <- c("srb", "asfr", "baseline", "survival", "net_migration")
  all_cols <- c("year_start", "year_end", "sex", "age_start", "age_end",
                value_col)
  pop_all_cols <- c("year", "sex", "age_start", "age_end", value_col)
  component_cols <- list(
    "srb" = setdiff(all_cols, c("sex", "age_start", "age_end")),
    "asfr" = setdiff(all_cols, "sex"),
    "baseline" = pop_all_cols,
    "survival" = all_cols,
    "net_migration" = all_cols
  )
  component_ids <- list(
    "srb" = list(year_start = settings$years),
    "asfr" = list(year_start = settings$years,
                  age_start = settings$ages_asfr),
    "baseline" = list(year = min(settings$years),
                      sex = settings$sexes,
                      age_start = settings$ages),
    "survival" = list(year_start = settings$years,
                      sex = settings$sexes,
                      age_start = settings$ages_survival),
    "net_migration" = list(year_start = settings$years,
                           sex = settings$sexes,
                           age_start = settings$ages)
  )
  for (component in components) {
    required_cols <- component_cols[[component]]
    assertthat::assert_that(
      component %in% names(inputs),
      all(required_cols %in% names(inputs[[component]])),
      msg = paste0(component, " must be included in `inputs` with columns '",
                   paste(component_cols[[component]], collapse = "', '"), "'.")
    )
    assertable::assert_ids(
      data = inputs[[component]],
      id_vars = component_ids[[component]],
      quiet = T
    )
    assertable::assert_values(inputs[[component]], colnames = value_col,
                              test = "not_na", quiet = T)
  }

  # check values of components
  assertable::assert_values(inputs[["srb"]], colnames = value_col,
                            test = "gte", test_val = "0", quiet = T)
  assertable::assert_values(inputs[["asfr"]], colnames = value_col,
                            test = "gte", test_val = "0", quiet = T)
  assertable::assert_values(inputs[["baseline"]], colnames = value_col,
                            test = "gte", test_val = "0", quiet = T)
  assertable::assert_values(inputs[["survival"]], colnames = value_col,
                            test = "gte", test_val = "0", quiet = T)
  assertable::assert_values(inputs[["survival"]], colnames = value_col,
                            test = "lte", test_val = "1", quiet = T)
  return(invisible(inputs))
}
