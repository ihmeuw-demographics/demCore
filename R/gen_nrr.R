#' @title Generate Net Reproductive Rate (NRR)
#'
#' @description Calculate Net Reproductive Rate (NRR) from age-specific
#'   fertility rate (ASFR) and sex-ratio at birth (SRB) and
#'   generate `nrr` variable in the data.table.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data with 'asfr', sex-ratio at birth ('srb'), and 'nLx'.
#' @param reproductive_age_start \[`numeric(1)`\]\cr
#'   Age in years of start of reproductive age span (default 15 years).
#' @param reproductive_age_end \[`numeric(1)`\]\cr
#'   Age in years of end of reproductive age span (default 50 years).
#' @inheritParams gen_u5_ax
#'
#' @return dt \[`data.table()`\]\cr
#'   Data table with `nrr` by `id_cols` (excluding 'age_start' and 'age_end').
#'
#' @details
#' Calculate proportion female: prop_female = (1 / srb) / (1 + (1 / srb)).
#' Then, calculate NRR: sum(asfr * prop_female * nLx) over age, by id columns.
#'
#' @seealso
#' Preston Demography textbook page 113.
#'
#' @examples
#' dt <- data.table::data.table(
#'   age_start = seq(15, 45, 5),
#'   age_end = seq(20, 50, 5),
#'   sex = "female",
#'   asfr = c(0.00002, 0.009, 0.1, 0.18, 0.19, 0.11, 0.03),
#'   srb = 1.057,
#'   nLx = 4.9
#' )
#' output <- gen_nrr(
#'   dt,
#'   id_cols = c("age_start", "age_end", "sex"),
#'   reproductive_age_start = 15,
#'   reproductive_age_end = 50
#' )
#'
#' @export
gen_nrr <- function(dt, id_cols, reproductive_age_start = 15, reproductive_age_end = 50) {

  # validate ----------------------------------------------------------------

  # check `id_cols`
  assertthat::assert_that(assertive::is_character(id_cols))
  assertthat::assert_that(
    all(c("age_start", "age_end", "sex") %in% id_cols),
    msg = "`id_cols` must have at least 'age_start', 'age_end', and 'sex'."
  )

  # check `dt`
  assertable::assert_colnames(
    dt,
    colnames = c("asfr", "srb", "nLx", id_cols),
    only_colnames = F,
    quiet = T
  )

  # check female
  assertthat::assert_that(
    unique(dt$sex) == "female",
    msg = "`sex` must be 'female'."
  )

  # check `reproductive_age_start` and `reproductive_age_end`
  hierarchyUtils::assert_no_overlapping_intervals(
    unique(dt[, list(start = age_start, end = age_end)])
  )
  assertthat::assert_that(
    assertthat::is.count(reproductive_age_start),
    assertthat::is.count(reproductive_age_end),
    min(dt$age_start) == reproductive_age_start,
    max(dt$age_end) == reproductive_age_end,
    msg = paste0("Age span in data does not match ",
                 "[`reproductive_age_start`, `reproductive_age_end`].")
  )

 # calculate ----------------------------------------------------------------

  # calculate proportion births that are female
  dt[, prop_female := (1 / srb) / (1 + (1 / srb))]

  # calculate NRR
  dt <- dt[, list(nrr = sum(asfr * prop_female * nLx)),
            by = setdiff(id_cols, c("age_start", "age_end"))]

  return(dt)

}
