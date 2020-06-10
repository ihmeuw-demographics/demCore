#' @title Generate a life table with all parameters
#'
#' @description Given a `data.table` with at least two of variables mx, ax, and
#'   qx, compute a complete life table. This is a helper function that combines
#'   many other functions in this package to calculate px, lx, dx, Tx, nLx,
#'   and ex.
#'
#' @param dt \[`data.table()`\]\cr Input life tables, variables: 'qx', 'mx',
#'   'ax', 'age_start', 'age_end', and all `id_cols`
#' @param id_cols \[`character()`\]\cr Columns that uniquely identify each
#'   row of `dt`
#' @param preserve_u5 \[`logical()`\]\cr Whether to preserve under-5 qx
#'   estimates rather than recalculating them based on mx and ax. Default: F.
#' @param assert_na \[`logical()`\]\cr Whether to assert that there is no
#'   missingness. Default T.
#'
#' @return \[`data.table()`\]\cr Life table(s) with additional columns: px, lx,
#'   dx, Tx, nLx, ex
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
lifetable <- function(dt, id_cols, preserve_u5 = F, assert_na = T) {

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

  # return
  setkeyv(dt, original_keys)
  return(dt)

}



#' @title Check two of mx, ax, qx
#'
#' @description Helper function to check a data.table for two of mx, ax, and qx
#'   and compute the missing parameter is one is missing.
#'
#' @param dt \[`data.table()`\]\cr Data to check for mx, ax, and/or qx. Must
#'   also have 'age_length' column.
#'
#' @return `dt` with input columns plus any of 'mx', 'ax', and 'qx' that is
#'   missing in input. Or, returns error if input has fewer than two of these
#'   parameters.
#'
#' @details Uses [mx_ax_to_qx()], [qx_ax_to_mx()], or [mx_qx_to_ax()] function
#'   to complete the set of three life table parameters.
#'
#' @example
#' dt <- data.table::data.table(
#'   age_start = c(0, 1, 5, 10),
#'   age_length = c(1, 4, 5, 5),
#'   mx = c(0.009, 0.0004, 0.00015, 0.00019),
#'   ax = c(0.068, 1.626, 2.5, 2.5)
#' )
#' dt <- check_mx_ax_qx(dt)
#'
#' @export
check_mx_ax_qx <- function(dt) {

  # check `dt` for 2/3 of mx, ax, qx
  assertthat::assert_that(
    length(intersect(c("mx", "ax", "qx"), names(dt))) >= 2,
    msg = "Need at least two of mx, ax, qx."
  )

  # qx, mx, ax if missing
  if(!"qx" %in% names(dt)) dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
  if(!"mx" %in% names(dt)) dt[, mx := qx_ax_to_mx(qx, ax, age_length)]
  if(!"ax" %in% names(dt)) dt[, ax := mx_qx_to_ax(mx, qx, age_length)]

  return(dt)

}
