#' @title Generate full life table from abridged life table
#'
#' @description Convert abridged (5-year age group) life tables to full
#'   (single-year-age) life tables using specified regression parameters or
#'   lx spline
#'
#' @param dt \[`data.table()`\]\cr Abridged life tables with variables
#'   'age_start', 'age_end', all `id_cols`, 'qx', 'ax'
#' @param id_cols \[`character()`\]\cr Variables that uniquely identify rows,
#'   must include 'age_start' and 'age_end'.
#' @param regression_fits \[`data.table()`\]\cr Abridged to full regression
#'   parameters. Columns include all `regression_id_cols`, 'intercept', 'slope'.
#' @param regression_id_cols \[`character()`\]\cr Variables that uniquely
#'   identify regression parameters. Must include 'age_start' and 'age_end' and
#'   be contained by `id_cols`.
#' @param lx_spline_start_age \[`integer(1)`\]\cr Age (inclusive) to start using
#'   lx spline rather than regression fits. Use 0 to use lx spline for all ages,
#'   or Inf to use regression results for all. Corresponds to 'age_start' column.
#' @param lx_spline_end_age \[`integer(1)`\]\cr Age (non-inclusive) to end spline
#'   and begin using regression fits. Corresponds to 'age_start' column.
#' @param preserve_input_ax_ages \[`integer()`\]\cr Ages to preserve the input
#'   ax values for. This is typically the first age group 0-1 and the terminal
#'   age group. Corresponds to 'age_start' column.
#'
#' @return \[`data.table()`\]\cr Full (single-year) life tables with columns
#'   `id_cols`, 'age', 'qx', 'ax'.
#'
#' @details This function includes two different methods for expanding
#'   abridged life tables to full life tables. The first method is a
#'   **monotonic cubic spline** over lx. Because lx is always decreasing, the
#'   monotonic spline fits the general curve and pattern well, basically
#'   splining between the knots at each abridged-year increment. However, the
#'   spline can become wild and unreasonable in age groups with dramatic changes
#'   in mortality rate over age. Internally, this function calls the function
#'   [stats::spline()].
#'
#'   The second method is a **regression method** to predict log full qx from
#'   log abridged qx:
#'   \deqn{log(qx) = B0 + B1 log(qx,abridged)}
#'   Where the parameters \eqn{B0} and \eqn{B1} are fit using high
#'   quality single-year life tables (like those from Human Mortality Database),
#'   and passed into this function. The regression must be fit separately by
#'   single-year age, but it may be also stratified by secondary identifying
#'   variables like 'sex'. These regression id variables must be present in the
#'   input abridged life tables you are trying to expand. The regression-based
#'   method has its own challenges, where jagged qx patterns may arise because
#'   we do not take consecutive abridged qx values into account simultaneously.
#'
#'   The function also allows for a combination of the methods to be used, with
#'   separation on age. The age cutoffs we found to work best are: regression
#'   method for ages <15 and >100 and spline method for ages between 15 and 100.
#'   This corresponds to `lx_spline_start_age = 15` and
#'   `lx_spline_end_age = 100`.
#'
#' @section Default ax:
#'   Values of ax are assumed to be 0.5 for all single-year ages, except those
#'   passed into `preserve_input_ax_ages` argument. HMD also assumes 0.5 for
#'   single-years, but we may be introducing an inconsistency between abridged
#'   and full life table mx values that are calculated from these differing ax
#'   values. Therefore, this is an area for future methods development.
#'
#' @examples
#' data("austria_1992_lt")
#' data("full_lt_pars")
#' regression_fits <- full_lt_pars[sex == "male"]
#' id_cols <- c("age_start", "age_end")
#' dt <- abridged_to_full(
#'   dt = austria_1992_lt,
#'   regression_fits = regression_fits,
#'   id_cols = id_cols,
#'   lx_spline_start_age = 15,
#'   lx_spline_end_age = 100,
#'   regression_id_cols = c("age_start", "age_end")
#' )
#'
#' @export

abridged_to_full <- function(dt, id_cols, regression_fits, regression_id_cols,
                             lx_spline_start_age,
                             lx_spline_end_age,
                             preserve_input_ax_ages = c(0, max(dt$age_start))) {

  # validate ----------------------------------------------------------------

  # check standard inputs
  validate_lifetable(dt = dt, id_cols = id_cols, param_cols = c("qx", "ax"))

  # check `regression_id_cols`
  assertive::is_character(regression_id_cols)
  assertthat::assert_that("age_start" %in% regression_id_cols &
                          "age_end" %in% regression_id_cols,
                          msg = "`regression_id_cols` must include
                          'age_start' and 'age_end'.")
  regression_id_cols_no_age <-
    regression_id_cols[!regression_id_cols %in% c("age_start", "age_end")]
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
  preserve_ax_dt <- dt[age_start %in% preserve_input_ax_ages]
  preserve_ax_dt <- preserve_ax_dt[, .SD, .SDcols = c(id_cols, "qx", "ax")]

  # get `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # add 'age_length' if not in input
  if(!"age_length" %in% names(dt)) {
    dt <- demUtils::gen_length(dt, col_stem = "age")
  }

  # capture terminal age for reference later
  terminal_age <- max(dt$age_start)

  # lx spline ---------------------------------------------------------------

  # get lx
  gen_lx_from_qx(dt, id_cols)

  # compute spline
  if (lx_spline_start_age < terminal_age) {
    full_lt_spline <- dt[age_start >= (lx_spline_start_age - 5),
      list(
        lx = stats::spline(age_start, lx, method = "hyman",
                           xout = min(age_start):terminal_age)$y,
        age_start = (min(age_start):terminal_age),
        abridged_age = c(
          rep(
            age_start[age_start != terminal_age],
            age_length[age_start != terminal_age]
          ),
          terminal_age
        )
      ),
      by = id_cols_no_age
    ]

    # add `age_end`
    full_lt_spline <- demUtils::gen_end(
      full_lt_spline,
      id_cols = c(id_cols_no_age, "age_start"),
      col_stem = "age"
    )

    # convert back to qx
    gen_qx_from_lx(full_lt_spline, id_cols)
    full_lt_spline[, c("lx") := NULL]
    full_lt_spline <- full_lt_spline[!age_start %in% preserve_input_ax_ages]

  } else {
    full_lt_spline <- data.table(age_start = NA, qx = NA, ax = NA)
    full_lt_spline[, (id_cols_no_age) := NA]
  }

  # expand dataset on age ---------------------------------------------------

  setnames(dt, "age_start", "abridged_age")
  setkeyv(dt, c(id_cols_no_age, "abridged_age"))

  dt_full <- dt[!abridged_age %in% preserve_input_ax_ages,
                list(full_age = abridged_age:(abridged_age + age_length - 1),
                     age_length,
                     qx,
                     px = (1 - qx)),
                by = c(id_cols_no_age, "abridged_age")]

  dt_full <- merge(dt_full, regression_fits,
                   by.x = c(regression_id_cols_no_age, "full_age"),
                   by.y = c(regression_id_cols_no_age, "age_start"))

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
  setnames(dt_full, "full_age", "age_start")
  setnames(dt_full, "pred_qx_adjusted", "qx")

  # combine regression and spline -------------------------------------------

  dt_full <- rbindlist(list(
    dt_full[age_start < lx_spline_start_age |
              age_start >= lx_spline_end_age,
            .SD, .SDcols = c(id_cols, "qx")],
    full_lt_spline[age_start >= lx_spline_start_age &
                     age_start < lx_spline_end_age,
                   .SD, .SDcols = c(id_cols, "qx")]
  ), use.names = T)

  dt_full <- dt_full[, .SD, .SDcols = c(id_cols, "qx")]

  # single-year ax ----------------------------------------------------------

  # assume 0.5
  dt_full[, ax := .5]

  # add back in preserved vals and sort on age
  dt_full <- rbindlist(list(dt_full[!age_start %in% preserve_input_ax_ages],
                            preserve_ax_dt), use.names = T)
  setkeyv(dt_full, c(id_cols_no_age, "age_start"))

  # check and return --------------------------------------------------------

  # fix terminal qx at 1 in combined full life table
  dt_full[age_start == terminal_age, qx := 1]

  # validate outputs
  assertable::assert_values(dt_full, "qx", "gte", 0, quiet = T)
  assertable::assert_values(dt_full, "qx", "lte", 1, quiet = T)

  return(dt_full)
}
