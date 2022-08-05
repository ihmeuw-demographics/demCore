
#' @title Estimate child mortality from sbh information
#'
#' @description Estimate child mortality from sbh information from one source
#' using the Brass method with Trussell coefficients.
#'
#' @param dt \[`data.table()`\]\cr
#'   sbh data with `id_cols`, `n_women_col`, `ceb_col`, `ced_col`
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`. This must include
#'   'age_start' and 'age_end'.
#' @param n_women_col \[`character(1)`\]\cr
#'   Name of column storing the number of women in each maternal age group.
#' @param ceb_col \[`character(1)`\]\cr
#'   Name of column storing the number of children ever born alive in each
#'   maternal age group.
#' @param ced_col \[`character(1)`\]\cr
#'   Name of column storing the number of children ever born alive who have died
#'   in each maternal age group.
#' @param model_schedule \[`character(1)`\]\cr
#'   Name of model fertility schedule to use from `sbh_trussell_coeffs`. Default
#'   is "West". Other implemented option is "North".
#'
#' @return \[`data.table()`\] with `id_cols`, the estimated probability of
#'   dying ('nqx'), and the reference period ('tx').
#'
#' @details
#' The method reproduced in this function is described in Section B of Chapter
#' III in the UN Population Division Manual X. It estimates the probability of
#' child mortality with data classified by maternal age from one survey or
#' census.
#'
#' Summary of steps:
#'   1. Calculate average parity for each maternal age group ('P(i)').
#'   2. Calculate proportion of children who have died for each maternal age
#'   group ('D(i)').
#'   3. Use estimation equations to calculate probability of child mortality:
#'     \deqn{k(i)=a(i) + b(i) (P(1)/P(2)) + c(i) (P(2)/P(3))}
#'     \deqn{q(x) = k(i)D(i)}
#'   4. Use estimation equation to calculate reference period:
#'     \deqn{t(x)=a(i) + b(i) (P(1)/P(2)) + c(i) (P(2)/P(3))}
#'
#' @references
#' UN Population Division. 1983. Manual X: Indirect Techniques for Demographic
#' Estimation. New York: United Nations, Department of Economic and Social
#' Affairs, ST/ESA/SER.A/81.
#' http://www.un.org/esa/population/techcoop/DemEst/manual10/manual10.html
#'
#' @examples
#' calc_nqx_brass(
#'   dt = sbh_panama_1976,
#'   id_cols = c("sex", "age_start", "age_end"),
#'   model = "West"
#' )
#' calc_nqx_brass(
#'   dt = sbh_panama_1976,
#'   id_cols = c("sex", "age_start", "age_end"),
#'   model = "North"
#' )
#'
#' @export
calc_nqx_brass <- function(dt,
                           id_cols = c("age_start", "age_end"),
                           n_women_col = "n_women",
                           ceb_col = "ceb",
                           ced_col = "ced",
                           model_schedule = "West") {

  # Validate arguments ------------------------------------------------------

  model_fertility_schedule_options <- c("North", "West")
  # check `missing_dt_severity` argument
  assertthat::assert_that(
    assertthat::is.string(model_schedule),
    model_schedule %in% model_fertility_schedule_options
  )
  coeffs <- sbh_trussell_coeffs[model == model_schedule]

  assertive::assert_is_a_string(n_women_col)
  assertive::assert_is_a_string(ceb_col)
  assertive::assert_is_a_string(ced_col)

  # check `id_cols` argument
  age_cols <- c("age_start", "age_end")
  assertive::assert_is_character(id_cols)
  error_msg <- paste0("`id_cols` must include '",
                      paste(age_cols, collapse = "', '"), "'")
  assertthat::assert_that(all(age_cols %in% id_cols), msg = error_msg)

  # basic checks for `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(
    data = dt, colnames = c(id_cols, n_women_col, ceb_col, ced_col),
    only_colnames = T, quiet = T
  )

  # Calculate qx ------------------------------------------------------------

  # calculate Brass method inputs: average parity for each maternal age group
  # and proportion of children who have died for each maternal age group
  inputs <- dt[
    ,
    list(
      Di = get(ced_col) / get(ceb_col),
      Pi = get(ceb_col) / get(n_women_col)
    ),
    by = id_cols
  ]

  # use Brass estimation equations and Trussell coefficients to calculate
  # probability of child mortality for each child age group
  nqx <- merge(inputs, coeffs[type == "q(x)/D(x)"], by = c("age_start", "age_end"))
  nqx[, c("model", "type") := NULL]
  nqx <- nqx[
    , list(age_start = age_start_nqx,
           age_end = age_end_nqx,
           nqx = (a_i + (b_i * Pi[1] / Pi[2]) + (c_i * Pi[2] / Pi[3])) * Di)
    , by = setdiff(id_cols, c("age_start", "age_end"))
  ]

  # use Brass estimation equations and Trussell coefficients to calculate
  # the reference period the probability of child mortality for each child age
  # group applies to
  tx <- merge(inputs, coeffs[type == "t(x)"], by = c("age_start", "age_end"))
  tx[, c("model", "type") := NULL]
  tx <- tx[
    , list(age_start = age_start_nqx,
           age_end = age_end_nqx,
           tx = a_i + (b_i * Pi[1] / Pi[2]) + (c_i * Pi[2] / Pi[3]))
    , by = setdiff(id_cols, c("age_start", "age_end"))
  ]

  output <- merge(nqx, tx, by = id_cols)
  return(output)
}
