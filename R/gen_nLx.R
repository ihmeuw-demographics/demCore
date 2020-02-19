#' Generate nLx
#'
#' @description
#' Calculate average person-years lived in the age interval x to x+n,
#' given a data.table with:
#'   * ax = average years lived of those who died in the age interval
#'   * dx = number who died in the age interval
#'   * lx = number at the beginning of the age interval
#'   * mx = death rate (mx)
#'
#' \eqn{_nL_x = n \cdot l_{x+n} + a_x \cdot d_x}
#' \eqn{_{\infty}L_x = l_x / m_x} for terminal age group
#'
#' In words, this formula for \eqn{_nL_x} is years lived by survivors plus years
#'   lived by those who died. For terminal ages, assume that average person years
#'   lived equals number at the start of the age group divided by the death rate.
#'
#' @param dt data.table with lx, ax, dx, mx, age, and age_int variables.
#' @param id_cols character vector of id columns that uniquely identify each row of `dt`.
#' @param terminal_age numeric, the terminal age group for the data. Default: 110.
#' @param assert_na logical, whether to check for NA values in the generated nLx variable.
#'
#' @return dt with `nLx` column added.
#' @export
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
#'
#' @import data.table
#' @import assertable
gen_nLx <- function(dt, id_cols, terminal_age = 110, assert_na = T) {

  # validate ----------------------------------------------------------------
  assertive::assert_is_numeric(terminal_age)
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("age", "age_int", "lx", "ax", "dx", "mx", id_cols),
                              only_colnames = F, quiet = T)
  assertive::assert_is_numeric(dt[["age"]])
  assertable::assert_values(dt, c("age"), test = "lte", test_val = terminal_age, quiet = T)

  # check for duplicates - TODO: replace with demUtils::assert_is_unique_dt
  dt[, test := .N, by = c(id_cols, "age")]
  assertable::assert_values(dt, c("test"), test = "equal", test_val = 1,
                            quiet = T, display_rows = F)
  dt[, test := NULL]

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
