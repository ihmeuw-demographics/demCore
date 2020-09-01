#' @title Generate Net Reproductive Rate (NRR)
#'
#' @description Calculate Net Reproductive Rate (NRR) from X and X and
#'   generate `nrr` variable in the data.table.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input data with asfr, sex-ratio at birth (`srb`), and nLx.
#' @param reproductive_age_start \[`numeric(1)`\]\cr
#'   Age in years of start of reproductive age span (default 15 years).
#' @param reproductive_age_end \[`numeric(1)`\]\cr
#'   Age in years of end of reproductive age span (default 49 years).
#' @inheritParams gen_u5_ax
#'
#' @return dt \[`data.table()`\]\cr
#'   Data table with `nrr` by `id_cols` (excluding 'age_start' and 'age_end').
#'
#' @examples
#'
#' @export
gen_nrr <- function(dt, id_cols, reproductive_age_start = 15, reproductive_age_end = 49) {

  # validate ----------------------------------------------------------------

  assertthat::assert_that(assertive::is_character(id_cols))
  assertthat::assert_that(
    all(c("age_start", "age_end", "sex") %in% id_cols),
    msg = "`id_cols` must have at least 'age_start', 'age_end', and 'sex'."
  )

  assertable::assert_colnames(
    dt,
    colnames = c("asfr", "srb", "nLx", id_cols),
    only_colnames = F,
    quiet = T
  )

  assertthat::assert_that(
    unique(dt$sex) == "female",
    msg = "`sex` must be 'female'."
  )

  assertthat::assert_that(
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
