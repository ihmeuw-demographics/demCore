#' @title Generate a life table with all parameters
#'
#' @description Given a `data.table` with at least two of variables mx, ax, and
#'   qx, compute a complete life table. This is a helper function that combines
#'   many other functions in this package to calculate px, lx, dx, Tx, nLx,
#'   and ex.
#'
#' @param dt \[`data.table()`\]\cr
#'   Input life tables. Must include 2/3 of 'qx', 'mx', and 'ax', in addition
#'   to all `id_cols`.
#' @param id_cols \[`character()`\]\cr
#'   Columns that uniquely identify each row of `dt`. Must include 'age_start'
#'   and 'age_end'.
#' @param preserve_u5 \[`logical()`\]\cr
#'   Whether to preserve under-5 qx estimates rather than recalculating them
#'   based on mx and ax.
#' @param assert_na \[`logical()`\]\cr Whether to assert that there is no
#'   missingness.
#' @inheritParams summarize_lt
#'
#' @return \[`data.table()`\]\cr
#'   Input life table(s) with additional columns: px, lx, dx, Tx, nLx, ex.
#'   Or, if `format_long` is TRUE, additional columns 'life_table_parameter'
#'   and 'value' with data long on life table parameter.
#'
#' @details
#' Note that while it typically takes estimation techniques to arrive at mx, ax,
#' and qx from empirical data, the solution of the remaining life table
#' parameters as compiled in this function is deterministic once mx, ax, and qx
#' are specified. The steps of this solution are as follows:
#' 1. Calculate mx, ax, or qx if one of the three is missing
#' 2. Recalculate qx if all three of mx, ax, and qx are provided to confirm
#'   the three agree. See `mx_qx_ax_conversions` functions. Will not recalculate
#'   under-5 qx if `preserve_u5` is true.
#' 3. Compute px from qx
#' 4. [gen_lx_from_qx()]
#' 5. [gen_dx_from_lx()]
#' 6. [gen_nLx()]
#' 7. [gen_Tx()]
#' 8. [gen_ex()]
#' 9. Replace terminal (age_end = Inf) ax with terminal ex
#'
#' @examples
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age_start = c(0, 5, 10, 15),
#'   age_end = c(5, 10, 15, Inf),
#'   mx = c(0.1, 0.2, 0.3, 0.4),
#'   ax = c(2.5, 2.5, 2.5, 2.5)
#' )
#' dt <- lifetable(dt, id_cols = c("sex", "age_start", "age_end"))
#'
#' @export
lifetable <- function(dt,
                      id_cols,
                      preserve_u5 = FALSE,
                      assert_na = TRUE,
                      format_long = FALSE) {

  # validate and prep  ------------------------------------------------------

  # standard validations
  validate_lifetable(
    dt = dt,
    id_cols = id_cols,
    param_cols = intersect(c("mx", "ax", "qx"), names(dt)),
    assert_na = assert_na
  )

  # check `preserve_u5`
  assertive::assert_is_logical(preserve_u5)

  # check for `age_length` and add if missing
  if(!"age_length" %in% names(dt)) {
    dt <- hierarchyUtils::gen_length(dt, col_stem = "age")
  }

  # check `dt` for 2/3 of mx, ax, qx and compute missing parameter
  dt <- check_mx_ax_qx(dt)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # check `format_long`
  assertthat::assert_that(assertthat::is.flag(format_long))

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # calculate life table ----------------------------------------------------

  # recalculate qx to confirm consistency with ax and mx
  # mostly relevant if input contains all three parameters
  if (preserve_u5) {
    dt[age_start >= 5, qx := mx_ax_to_qx(mx, ax, age_length)]
  } else {
    dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
  }
  dt[age_start == max(age_start), qx := 1]
  if (nrow(dt[qx > 1]) > 0) {
    stop(paste0("Probabilities of death over 1 based on recalculation from
                mx and ax. Please re-examine input data. Max qx value is ",
                max(dt$qx)))
  }

  # px
  dt[, px := 1 - qx]

  # lx
  gen_lx_from_qx(dt, id_cols, assert_na)

  # dx
  gen_dx_from_lx(dt, id_cols, assert_na)

  # nLx
  gen_nLx(dt, id_cols, assert_na)

  # Tx
  gen_Tx(dt, id_cols, assert_na)

  # ex
  gen_ex(dt, assert_na)

  # replace terminal ax with terminal ex
  dt[age_end == Inf, ax := ex]

  # formatting -------------------------------------------------------------

  if (format_long) {
    dt <- melt(
      data = dt,
      measure.vars = c("qx", "mx", "ax", "px", "lx", "dx", "nLx", "Tx", "ex"),
      variable.name = "life_table_parameter", value.name = "value"
    )
    new_keys <- c(origi