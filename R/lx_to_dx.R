#' lx to dx
#'
#' @description
#' Given a data.table with the proportion surviving to each age group (`lx`),
#' calculate the proportion dying in the age interval (`dx`).
#'
#' \eqn{d_x = l_x - l_{x+n}}
#' In terminal age group, \eqn{d_x = l_x}
#'
#' @param dt data.table columns `age`, `lx`, and all variables in id_cols.
#' @param id_cols character vector of id columns that uniquely identify each row of `dt`.
#' @param terminal_age numeric representing the last age in the dataset. Default: 110.
#' @param assert_na logical for whether to check for NA values in the qx variables.
#'
#' @return `dt` with column "dx" added.
#'
#' @examples
#' dt <- data.table::data.table(
#'   sex = rep("both", 4),
#'   age = c(0, 5, 10, 15),
#'   lx = c(1, 0.9, 0.7, 0.2)
#' )
#' dt <- lx_to_dx(dt, id_cols = c("sex"), terminal_age = 15)
#'
#' @import data.table
#' @import assertable
#' @export
lx_to_dx <- function(dt, id_cols, terminal_age = 110, assert_na = T) {

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
