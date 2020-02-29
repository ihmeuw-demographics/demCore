#' Generate a full life-table, given mx, ax, and qx
#'
#' Given a data.table with at least two of variables mx, ax, and
#'   qx, compute a full life table.
#'
#' @param dt data.table with variables: 'qx', 'mx', 'ax', 'age', 'age_int',
#'   and all `id_cols`
#' @param id_cols character vector of id columns that uniquely identify each row of `dt`
#' @param terminal_age numeric, the terminal age group for the data. Default: 110
#' @param preserve_u5 logical, whether to preserve under-5 qx estimates rather than
#'   reclaculating them based on mx and ax. Default: F.
#' @param assert_na logical, whether to assert that there is no missingness. Default T.
#'
#' @return data.frame or data.table with additional variables px, lx, dx, Tx, nLx, ex
#'
#' @export
#' @examples
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age = c(0, 5, 10, 15),
#'   age_int = c(5, 5, 5, 120),
#'   mx = c(0.1, 0.2, 0.3, 0.4),
#'   ax = c(2.5, 2.5, 2.5, 2.5)
#' )
#' dt[, qx := mx_ax_to_qx(mx, ax, age_int)]
#' dt <- lifetable(dt, id_cols = c("sex", "age"), terminal_age = 15)

lifetable <- function(dt, id_cols, terminal_age = 110, preserve_u5 = F, assert_na = T) {

  # validate and prep  ------------------------------------------------------

  # check `preserve_u5`
  assertive::assert_is_logical(preserve_u5)

  # check for `age_int` - TODO: add age_int within function if missing
  assertable::assert_colnames(dt, c("age_int"), only_colnames = F, quiet = T)

  # check `dt` for 2/3 of mx, ax, qx
  assertthat::assert_that(length(intersect(c("mx", "ax", "qx"),
                                            names(dt))) >= 2,
                          msg = "Need at least two of mx, ax, qx.")

  # create `id_cols` without age
  id_cols_no_age <- id_cols[id_cols != "age"]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age"))

  # calculate life table ----------------------------------------------------

  # qx, mx, ax if missing
  if(!"qx" %in% names(dt)) dt[, qx := as.numeric(NA)]
  if(!"mx" %in% names(dt)) dt[, mx := as.numeric(NA)]
  if(!"ax" %in% names(dt)) dt[, ax := as.numeric(NA)]
  dt[is.na(qx), qx := mx_ax_to_qx(mx, ax, age_int)]
  dt[is.na(mx), mx := qx_ax_to_mx(qx, ax, age_int)]
  dt[is.na(ax), ax := mx_qx_to_ax(mx, qx, age_int)]

  # recalculate qx to confirm consistency with ax and mx
  # mostly relevant if input contains all three parameters
  if (preserve_u5) {
    dt[age > 1, qx := mx_ax_to_qx(mx, ax, age_int)]
  } else {
    dt[, qx := mx_ax_to_qx(mx, ax, age_int)]
  }
  dt[age == terminal_age, qx := 1]
  if (nrow(dt[qx > 1]) > 0) {
    stop(paste0("Probabilities of death over 1 based on recalculation from
                mx and ax. Please re-examine input data. Max qx value is ",
                max(dt$qx)))
  }

  # px
  dt[, px := 1 - qx]

  # lx
  dt <- qx_to_lx(dt, id_cols, assert_na)

  # dx
  dt <- lx_to_dx(dt, id_cols, terminal_age, assert_na)

  # nLx
  dt <- gen_nLx(dt, id_cols, terminal_age, assert_na)

  # Tx
  dt <- gen_Tx(dt, id_cols, assert_na)

  # ex
  dt <- gen_ex(dt, assert_na)

  # return
  setkeyv(dt, original_keys)
  return(dt)

}

