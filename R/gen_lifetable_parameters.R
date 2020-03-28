#' @title Generate life table parameters
#'
#' @description
#' These functions perform life table calculations, to add selected life table
#'   parameters from others, using established demographic relationships.
#'
#' The following are required inputs for given parameters, as written in
#'   these functions:
#'   * dx requires lx
#'   * lx requires qx
#'   * qx requires lx
#'   * ex requires lx and Tx
#'   * Tx requires nLx
#'   * nLx requires lx, ax, and dx
#'
#' @param dt \[`data.table()`\]\cr variables 'lx', 'ax', 'dx', 'mx',
#'   'age_start', 'age_end'
#' @param id_cols \[`character()`\]\cr columns that uniquely identify each row
#'   of `dt`. Must include 'age_start' and 'age_end'.
#' @param assert_na \[`logical()`\]\cr whether to check for NA values in the
#'   generated variable.
#'
#' @return `dt` with column added for new life table parameter. Modifies
#'   data.tables in place.
#'
#' @details
#' **Parameter definitions:**
#' For an age interval x to x+n:
#'   * mx = mortality rate (deaths / person-years)
#'   * qx = probability of death, conditional on survival to x-th birthday
#'   * ax = average years lived of those who died in the age interval
#'   * dx = number (or proportion of cohort) who died in the age interval
#'   * lx = number (or proportion of cohort) alive at the beginning of the age
#'     interval
#'   * ex = life expectancy, expected value for years lived after age x
#'   * Tx =  total person-years lived beyond age x
#'   * nLx = total person-years lived in interval
#'
#' **qx_to_lx:** We can use probability of death (qx) to get the proportion of
#'   survivors in the beginning of an age group (lx).
#'   \deqn{At age = 0: lx = 1}
#'   \deqn{At age > 0: lx = lx_previous * (1-qx_previous)}
#'
#' **lx_to_qx:** We can use survival proportion (lx) and survival proportion at
#'   next age group (l{x+n}) to get probability of death (qx).
#'   \deqn{qx = 1 - (l{x+n}/lx)}
#'   \deqn{For terminal age group: qx = 1}
#'
#' **lx_to_dx:** Given the proportion surviving to each age group (lx), we can
#'   calculate the proportion dying in the age interval (dx).
#'  \deqn{dx = lx - l{x+n}}
#'  \deqn{For terminal age group: dx = lx}
#'
#' **gen_nLx:** To get \eqn{nLx} we calculate years lived by survivors plus years
#'   lived by those who died. For terminal ages, assume that average person years
#'   lived equals number at the start of the age group divided by the death rate.
#'   \deqn{nLx = n * l{x+n} + ax * d_x}
#'   \deqn{For terminal age group: infinityLx = lx / mx}
#'
#' **gen_Tx:** Person-years lived above age x (Tx) is based on person-years
#'   lived by people in each age interval above age x (nLx).
#'   \deqn{For terminal age group: Tx = nLx}
#'   \deqn{Otherwise: Tx = cumulative sum of nLx beyond age x}
#'
#' **gen_ex:** Life expectancy (ex) at age x is based on person-years lived
#'   above the age group (Tx) and proportion of people alive at the start of
#'   age group (lx).
#'   \deqn{ex = Tx / lx}
#'
#' @seealso lifetableUtils::mx_qx_ax_conversions, lifetableUtils::lifetable
#'
#' @examples
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age_start = c(0, 5, 10, 15),
#'   age_end = c(5, 10, 15, Inf),
#'   age_length = c(5, 5, 5, Inf),
#'   mx = c(0.1, 0.2, 0.3, 0.4),
#'   ax = c(2.5, 2.5, 2.5, 2.5)
#' )
#' dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
#' qx_to_lx(dt, id_cols = c("sex", "age_start", "age_end"))
#' lx_to_dx(dt, id_cols = c("sex", "age_start", "age_end"))
#' gen_nLx(dt, id_cols = c("sex", "age_start", "age_end"))
#' gen_Tx(dt, id_cols = c("sex", "age_start", "age_end"))
#' gen_ex(dt)
#' lx_to_qx(dt, id_cols = c("sex", "age_start", "age_end"))
#'
#' @name gen_lifetable_parameters
NULL

# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
qx_to_lx <- function(dt, id_cols, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_lifetable(dt = dt, id_cols = id_cols, param_cols = c("qx"),
                     assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # calculate lx -------------------------------------------------------------

  dt[, lx := 1]
  dt[, lx := lx[1] * cumprod(c(1, head(1 - qx, -1))), by = id_cols_no_age]

  # check and return ---------------------------------------------------------

  if (assert_na == T) assertable::assert_values(dt, "lx", "not_na", quiet = T)
  assertable::assert_values(dt, c("lx"), test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, c("lx"), test = "lte", test_val = 1, quiet = T)

  setkeyv(dt, original_keys)

}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
lx_to_qx <- function(dt, id_cols, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_lifetable(dt = dt, id_cols = id_cols, param_cols = c("lx"),
                      assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # calculate qx -------------------------------------------------------------

  dt[, qx := 1 - (shift(lx, 1, type = "lead") / lx), by = id_cols_no_age]
  dt[age_end == Inf, qx := 1]

  # check and return ---------------------------------------------------------

  if (assert_na == T) assertable::assert_values(dt, "qx", "not_na", quiet = T)
  assertable::assert_values(dt, c("qx"), test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, c("qx"), test = "lte", test_val = 1, quiet = T)

  setkeyv(dt, original_keys)

}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
lx_to_dx <- function(dt, id_cols, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_lifetable(dt = dt, id_cols = id_cols, param_cols = c("lx"),
                     assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # calculate dx -------------------------------------------------------------
  dt[, dx := lx - shift(lx, 1, type = "lead"), by = c(id_cols_no_age)]
  dt[age_end == Inf, dx := lx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) {
    assertable::assert_values(dt, "dx", "not_na", quiet = T)
  }
  setkeyv(dt, original_keys)

}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_nLx <- function(dt, id_cols, assert_na = T) {

  # prep -------------------------------------------------------------------

  # validate inputs
  validate_lifetable(dt = dt,
                     id_cols = id_cols,
                     param_cols = c("lx", "ax", "dx", "mx"),
                     assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # require terminal age group
  if(nrow(dt[age_end == Inf]) == 0) {
    stop("You do not have a terminal age group, but need one to calculate nLx.
           Designate terminal age with 'age_end' = Inf.")
  }

  # add 'age_length' if not in input
  if(!"age_length" %in% names(dt)) {
    dt <- demUtils::gen_length(dt, col_stem = "age")
  }

  # calculate nLx -----------------------------------------------------------

  # calculte nLx
  dt[, nLx := age_length * shift(lx, 1, type = "lead") + ax * dx,
       by = id_cols_no_age]
  dt[age_end == Inf, nLx := lx / mx]

  # check outputs ------------------------------------------------------------

  if (assert_na == T) {
    assertable::assert_values(dt[age_end != Inf], "nLx", "not_na", quiet = T)
  }

  setkeyv(dt, original_keys)

}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_Tx <- function(dt, id_cols, assert_na = T) {

  # prep ---------------------------------------------------------------------

  # validate inputs
  validate_lifetable(dt = dt, id_cols = id_cols, param_cols = c("nLx"),
                      assert_na = assert_na)

  # create `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # set key with 'age_start' as last variable
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols_no_age, "age_start"))

  # require terminal age group
  if(nrow(dt[age_end == Inf]) == 0) {
    stop("You do not have a terminal age group, but need one to calculate Tx.
           Designate terminal age with 'age_end' = Inf.")
  }

  # calculate Tx -------------------------------------------------------------

  # Set descending order to begin the cumulative sum at the oldest age group
  setorderv(dt, id_cols, order = -1)
  dt[, Tx := cumsum(nLx), by = id_cols_no_age]
  setorderv(dt, id_cols) # undo reverse order

  # check outputs ------------------------------------------------------------
  if (assert_na == T) assertable::assert_values(dt, "Tx", "not_na", quiet = T)
  setkeyv(dt, original_keys)

}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_ex <- function(dt, assert_na = T) {

  # validate ----------------------------------------------------------------

  validate_lifetable(dt = dt, param_cols = c("Tx", "lx"), assert_na = assert_na)

  # calculate ex -------------------------------------------------------------
  dt[, ex := Tx / lx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) assertable::assert_values(dt, "ex", "not_na", quiet = T)

}
