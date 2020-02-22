#' Generate life table parameters
#'
#' @description
#' These functions perform life table calculations, to add selected life table
#'   parameters from others, using established demographic relationships.
#'
#' The following are required inputs for given parameters, as written in
#'   these functions:
#'   * dx requires lx
#'   * lx requires qx
#'   * ex requires lx and Tx
#'   * Tx requires nLx
#'   * nLx requires lx, ax, and dx
#'
#' @param dt data.table with lx, ax, dx, mx, age, and age_int variables.
#' @param id_cols character vector of id columns that uniquely identify each row of `dt`.
#' @param terminal_age numeric, the terminal age group for the data. Default: 110.
#' @param assert_na logical, whether to check for NA values in the generated nLx variable.
#'
#' @return dt with column added for new life table parameter.
#'
#' @details
#' **Parameter definitions:**
#' For an age interval x to x+n:
#'   * mx = mortality rate (deaths / person-years)
#'   * qx = probability of death, conditional on survival to x-th birthay
#'   * ax = average years lived of those who died in the age interval
#'   * dx = number (or proportion of cohort) who died in the age interval
#'   * lx = number (or proportion of cohort) alive at the beginning of the age interval
#'   * ex = life expectancy, expected value for years lived after age x
#'   * Tx =  total person-years lived beyond age x
#'   * nLx = total person-years lived in interval
#'
#' **qx_to_lx:** We can use probability of death (qx) to get the proportion of survivors
#'   in the beginning of an age group (lx).
#'   \deqn{At age = 0: lx = 1}
#'   \deqn{At age > 0: lx = lx_previous * (1-qx_previous)}
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
#' **gen_Tx:** Person-years lived above age x (Tx) is based on person-years lived by
#'   people in each age interval above age x (nLx).
#'   \deqn{For terminal age group: Tx = nLx}
#'   \deqn{Otherwise: Tx = cumulative sum of nLx beyond age x}
#'
#' **gen_ex:** Life expectancy (ex) at age x is based on person-years lived above the
#'   age group (Tx) and proportion of people alive at the start of age group (lx).
#'   \deqn{ex = Tx / lx}
#'
#' @seealso lifetableUtils::mx_qx_ax_conversions
#'
#' @examples
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age = c(0, 5, 10, 15),
#'   age_int = c(5, 5, 5, 120),
#'   mx = c(0.1, 0.2, 0.3, 0.4),
#'   ax = c(2.5, 2.5, 2.5, 2.5)
#' )
#' dt[, qx := mx_ax_to_qx(mx, ax, age_int)]
#' dt <- qx_to_lx(dt, id_cols = c("sex"))
#' dt <- lx_to_dx(dt, id_cols = c("sex"), terminal_age = 15)
#' dt <- gen_nLx(dt, id_cols = c("sex"), terminal_age = 15)
#' dt <- gen_Tx(dt)
#' dt <- gen_ex(dt)
#'
#' @import data.table
#' @import assertable
#' @name gen_lifetable_parameters
NULL

# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
qx_to_lx2 <- function(dt, id_cols, assert_na = T) {

  # validate ----------------------------------------------------------------

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that(!"age" %in% id_cols, msg = "`id_cols` must not include 'age'.")

  # check `age` column
  assertthat::assert_that("age" %in% names(dt), msg = "`age` must be a column in `dt`")

  # check for duplicates
  dt[, test := .N, by = c(id_cols, "age")]
  assertable::assert_values(dt, c("test"), test = "equal", test_val = 1,
                            quiet = T, display_rows = F)
  dt[, test := NULL]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols, "age"))

  # calculate lx ------------------------------------------------------------

  dt[, lx := 1]
  dt[, lx := lx[1] * cumprod(c(1, head(1 - qx, -1))), by = id_cols]

  # check and return --------------------------------------------------------

  if (assert_na == T) assertable::assert_values(dt, "lx", "not_na", quiet = T)
  assertable::assert_values(dt, c("lx"), test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, c("lx"), test = "lte", test_val = 1, quiet = T)

  setkeyv(dt, original_keys)
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
lx_to_dx2 <- function(dt, id_cols, terminal_age = 110, assert_na = T) {

  # validate ----------------------------------------------------------------

  # check `id_cols`
  assertive::assert_is_character(id_cols)

  # check `terminal_age`
  assertive::assert_is_numeric(terminal_age)

  # check `dt`
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("age", "lx", id_cols), only_colnames = F, quiet = T)
  assertive::assert_is_numeric(dt[["age"]])
  assertable::assert_values(dt, c("age"), test = "lte", test_val = terminal_age, quiet = T)
  demUtils::assert_is_unique_dt(dt, id_cols = id_cols)

  # check `assert_na`
  assertive::assert_is_logical(assert_na)

  # calculate dx -------------------------------------------------------------
  dt <- dt[order(age)]
  dt[, dx := lx - shift(lx, 1, type = "lead"), by = c(id_cols[!id_cols %like% "age"])]
  dt[age == terminal_age, dx := lx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) {
    assertable::assert_values(dt, "dx", "not_na", quiet = T)
  }
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_nLx2 <- function(dt, id_cols, terminal_age = 110, assert_na = T) {

  # validate ----------------------------------------------------------------

  # check `id_cols`
  assertive::assert_is_character(id_cols)

  # check `terminal_age`
  assertive::assert_is_numeric(terminal_age)

  # check `dt`
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("age", "age_int", "lx", "ax", "dx", "mx", id_cols),
                              only_colnames = F, quiet = T)
  assertive::assert_is_numeric(dt[["age"]])
  assertable::assert_values(dt, c("age"), test = "lte", test_val = terminal_age, quiet = T)
  demUtils::assert_is_unique_dt(dt, id_cols = id_cols)

  # check `assert_na`
  assertive::assert_is_logical(assert_na)

  # calculate nLx -----------------------------------------------------------
  dt <- dt[order(age)]
  dt[, nLx := age_int * shift(lx, 1, type = "lead") + ax * dx,
     by = c(id_cols[!id_cols %like% "age"])]
  dt[age == terminal_age, nLx := lx / mx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) {
    assertable::assert_values(dt[age != terminal_age], "nLx", "not_na", quiet = T)
  }
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_Tx2 <- function(dt, id_cols, assert_na = T) {

  # validate ----------------------------------------------------------------

  # check `id_cols`
  assertive::assert_is_character(id_cols)

  # check `dt`
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("age", "nLx", id_cols), only_colnames = F, quiet = T)
  assertive::assert_is_numeric(dt[["age"]])
  demUtils::assert_is_unique_dt(dt, id_cols = id_cols)

  # check `assert_na`
  assertive::assert_is_logical(assert_na)

  # calculate Tx -------------------------------------------------------------

  id_cols_noage <- id_cols[!id_cols %in% c("age", "age_start", "age_end")]

  # Set descending order to begin the cumulative sum at the oldest age group
  setorderv(dt, id_vars, order = -1)
  dt[, Tx := cumsum(nLx), by = id_cols_noage]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) assertable::assert_values(dt, "Tx", "not_na", quiet = T)
  return(dt)
}


# ============================================================================
#' @rdname gen_lifetable_parameters
#' @export
gen_ex2 <- function(dt, assert_na = T) {

  # validate ----------------------------------------------------------------

  # check `dt`
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("Tx", "lx"), only_colnames = F, quiet = T)
  assertable::assert_values(dt, colnames = c("Tx", "lx"), test = "gte", test_val = 0)

  # check `assert_na`
  assertive::assert_is_logical(assert_na)

  # calculate ex -------------------------------------------------------------
  dt[, ex := Tx / lx]

  # check outputs ------------------------------------------------------------
  if (assert_na == T) assertable::assert_values(dt, "ex", "not_na", quiet = T)
  return(dt)
}
