#' Generate full life table from abridged life table
#'
#' Convert abridged (5-year age group) life tables to full (single-year-age)
#'   life tables using specified regression parameters or lx spline
#'
#' @description This function includes two different methods for expanding
#'   abridged life tables to full life tables. The first method is a
#'   **monotonic cubic spline** over lx. Because lx is always decreasing, the
#'   monotonic spline fits the general curve and pattern well, basically
#'   splining between the knots at each abridged-year increment. However, the
#'   spline can become wild and unreasonable in age groups with dramatic changes
#'   in mortality rate over age.
#'
#'   The second method is a **regression method** to predict log full qx from
#'   log abridged qx:
#'   \deqn{log(qx) = B0 + B1 log(qx,abridged)}
#'   Where the parameters \eqn{B0} and \eqn{B1} are fit using high
#'   quality single-year life tables (like those from Human Mortality Database),
#'   and passed into this function. The regression-based method has its own
#'   challenges, where scalloping qx patterns may arise because we do not take
#'   consecutive abridged qx values into account simultaneously.
#'
#'   The function also allows for a combination of the methods to be used, with
#'   separation on age. The default age cutoffs reflect the values we found to
#'   work best: regression method for ages <15 and >100 and spline method for
#'   ages between 15 and 100.
#'
#'   Values of ax are assumed to be 0.5 for all single-year ages, except those
#'   passed into `preserve_input_ax_ages` argument. HMD also assumes 0.5 for
#'   single-years, but we may be introducing an inconsistency between abridged
#'   and full life table mx values that are calculated from these differing ax
#'   values. Therefore, this is an area for future methods development.
#'
#' @param dt data.table with variables age, all `id_cols`, 'qx', 'ax'
#' @param id_cols character vector, variables that uniquely identify rows,
#'   must include 'age'
#' @param regression_fits data.table with variables from `id_cols`, plus
#'  'intercept', slope'
#' @param regression_id_cols character vector, variables that uniquely identify
#'   regression parameters. Must include 'age' and be contained by `id_cols`.
#' @param terminal_age integer, max age that is being computed (default: 110)
#' @param lx_spline_start_age integer, age (inclusive) to start using lx spline
#'   rather than regression fits. Use 0 to use lx spline for all ages, or
#'   integer > 110 or Inf to use regression results for all.
#' @param lx_spline_end_age integer, age (non-inclusive) to end spline and
#'   begin using regression fits.
#' @param preserve_input_ax_ages integer, ages to preserve the input ax values
#'   for. This is typically the first age group 0-1 and the terminal age group
#'   110+.
#'
#' @return data.table with columns id_cols, age, qx, ax
#'
#' @examples
#' \dontrun{
#' data("fNOR2010")
#' data("fullLTpars")
#' id_cols <- c("location", "age")
#' dt <- gen_full_lt(dt = fNOR2010, regression_fits = fullLTpars,
#'   id_cols = id_cols, regression_id_cols = c("age"), terminal_age = 95)
#' }
#' @import data.table
#' @export

gen_full_lt <- function(dt, id_cols, regression_fits, regression_id_cols,
                        terminal_age = 110, lx_spline_start_age = 15,
                        lx_spline_end_age = 100,
                        preserve_input_ax_ages = c(0, terminal_age)) {

  # validate ----------------------------------------------------------------

  # check standard inputs
  validate_param_conversion_input(dt = dt,
                                  id_cols = id_cols,
                                  param_cols = c("qx", "ax"))

  # check `regression_id_cols`
  assertive::is_character(regression_id_cols)
  assertthat::assert_that("age" %in% regression_id_cols,
                          msg = "`regression_id_cols` must include 'age'.")
  regression_id_cols_no_age <- regression_id_cols[regression_id_cols != "age"]
  if(length(setdiff(regression_id_cols, id_cols)) > 0) {
    stop("`regression_id_cols` must be contained within `id_cols`")
  }

  # check `regression_fits`
  assertable::assert_colnames(regression_fits, regression_id_cols,
                              only_colnames = F, quiet = T)
  regression_fits[, test := .N, by = c(regression_id_cols)]
  if(nrow(regression_fits[test > 1]) > 0) {
    stop("`regression_fits` must be unique on `regression_id_cols`.")
  }

  # prep --------------------------------------------------------------------

  dt <- copy(dt)

  # take out ages where we want to keep ax unchanged
  preserve_ax_dt <- dt[age %in% preserve_input_ax_ages]
  preserve_ax_dt <- preserve_ax_dt[, .SD, .SDcols = c(id_cols, "qx", "ax")]

  # get `id_cols` without 'age'
  id_cols_no_age <- id_cols[id_cols != "age"]

  # add age length
  # for now assume standard abridged ages. TODO: use demUtils function
  dt[age == 0, age_length := 1]
  dt[age == 1, age_length := 4]
  dt[is.na(age_length), age_length := 5]

  # lx spline ---------------------------------------------------------------

  # get lx
  qx_to_lx(dt, id_cols)

  # compute spline
  if (lx_spline_start_age < terminal_age) {
    full_lt_spline <- dt[age >= (lx_spline_start_age - 5),
      list(
        lx = stats::spline(age, lx, method = "hyman",
                           xout = min(age):terminal_age)$y,
        age = (min(age):terminal_age),
        abridged_age = c(
          rep(
            age[age != terminal_age],
            age_length[age != terminal_age]
          ),
          terminal_age
        )
      ),
      by = id_cols_no_age
    ]

    # convert back to qx
    lx_to_qx(full_lt_spline, id_cols, terminal_age)
    full_lt_spline[, c("lx") := NULL]
    full_lt_spline <- full_lt_spline[!age %in% preserve_input_ax_ages]

  } else {
    full_lt_spline <- data.table(age = NA, qx = NA, ax = NA)
    full_lt_spline[, (id_cols_no_age) := NA]
  }

  # expand dataset on age ---------------------------------------------------

  setnames(dt, "age", "abridged_age")
  setkeyv(dt, c(id_cols_no_age, "abridged_age"))

  dt_full <- dt[!abridged_age %in% preserve_input_ax_ages,
                list(full_age = abridged_age:(abridged_age + age_length - 1),
                  age_length, qx, px = (1 - qx)),
                by = c(id_cols_no_age, "abridged_age")]

  dt_full <- merge(dt_full, regression_fits,
                   by.x = c(regression_id_cols_no_age, "full_age"),
                   by.y = c(regression_id_cols_no_age, "age"))

  # calculate single-year qx with regression params -------------------------

  dt_full[, reg_qx := exp((log(qx) * slope) + intercept)]

  dt_full[, c("slope", "intercept") := NULL]
  setnames(dt_full, "qx", "abridged_qx")

  # rescale single-year qx --------------------------------------------------

  # calculated implied abridged px from new full lifetable
  dt_full[, pred_px := 1 - reg_qx]
  dt_full[, pred_px_abridged := prod(pred_px),
          by = c(id_cols_no_age, "abridged_age")]

  # calculate ratio to input abridged px and raise to 1/age_length
  dt_full[, adjustment_factor := (px / pred_px_abridged) ^ (1 / age_length)]

  # use this adjustment factor to get new single-year px, qx
  dt_full[, pred_px_adjusted := pred_px * adjustment_factor]
  dt_full[, pred_qx_adjusted := 1 - pred_px_adjusted]

  dt_full[, c("reg_qx", "px", "pred_px", "pred_px_adjusted",
              "adjustment_factor") := NULL]
  setnames(dt_full, "full_age", "age")
  setnames(dt_full, "pred_qx_adjusted", "qx")

  # combine regression and spline -------------------------------------------

  dt_full <- rbindlist(list(
    dt_full[age < lx_spline_start_age | age >= lx_spline_end_age, .SD,
            .SDcols = c(id_cols, "qx")],
    full_lt_spline[age >= lx_spline_start_age & age < lx_spline_end_age, .SD,
                   .SDcols = c(id_cols, "qx")]
  ), use.names = T)

  dt_full <- dt_full[, .SD, .SDcols = c(id_cols, "qx")]

  # single-year ax ----------------------------------------------------------

  # assume 0.5
  dt_full[, ax := .5]

  # add back in preserved vals and sort on age
  dt_full <- rbindlist(list(dt_full[!age %in% preserve_input_ax_ages],
                            preserve_ax_dt), use.names = T)
  setkeyv(dt_full, c(id_cols_no_age, "age"))

  # check and return --------------------------------------------------------

  # fix terminal qx at 1 in combined full life table
  dt_full[age == terminal_age, qx := 1]

  # validate outputs
  assertable::assert_values(dt_full, "qx", "gte", 0, quiet = T)
  assertable::assert_values(dt_full, "qx", "lte", 1, quiet = T)

  return(dt_full)
}
