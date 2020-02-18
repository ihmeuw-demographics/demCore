#' Calculate life expectancy (ex) from Tx and lx
#'
#' Generate life expectancy (ex) at age x based on person-years lived above the
#'   age group (Tx) and proportion of people alive at the start of age group (lx).
#'   Given a data.table with `Tx` and `lx`, \eqn{e_x = T_x / l_x}.
#'
#' @param dt data.table with `Tx` and `lx` variables
#' @param assert_na logical, default T, whether to check for NAs in generated ex
#'
#' @return None. Modifies the given data.table in-place.
#'
#' @examples
#' dt <- data.table::data.table(age = c(90),
#'                              Tx = c(3),
#'                              lx = c(0.1))
#' gen_ex(dt)
#'
#' @import data.table
#' @import assertable
#' @import assertive
#' @export
gen_ex <- function(dt, assert_na = T) {
  # validate
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("Tx", "lx"), only_colnames = F, quiet = T)

  # calculate ex
  dt[, ex := Tx / lx]

  # check results
  if (assert_na == T) {
    assertable::assert_values(dt, "ex", "not_na", quiet = T)
  }
}
