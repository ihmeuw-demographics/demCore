#' Use qx to calculate lx
#'
#' Convert probability of dying (qx) to proportion of survivors in the beginning of an age group (lx).
#'   Given a data.table with a qx variable, calculate and generate lx.
#'   Starting with lx = 1 at age 0, lx = lx_previous * (1-qx_previous).
#'
#' @param dt data.table with variables: qx, age, and all id columns
#' @param id_cols character vector of column names (excluding "age") that uniquely identify rows
#' @param assert_na logical for whether to check for NA values in the generated lx variables
#'
#' @return dt, with "lx" column added
#'
#' @examples
#' dt <- data.table::data.table(
#'   sex = c(rep("male", 6), rep("female", 6)),
#'   qx = c(rep(.1, 6), rep(.2, 6)),
#'   age = rep(c(0, 1, seq(5, 20, 5)), 2)
#' )
#' dt <- qx_to_lx(dt, id_cols = c("sex"))
#'
#' @import data.table
#' @import assertable
#' @import assertive
#' @import assertthat
#' @export
qx_to_lx <- function(dt, id_cols, assert_na = T) {
  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that(!"age" %in% id_cols, msg = "`id_cols` must not include 'age'.")

  # check `age` column
  assertthat::assert_that("age" %in% names(dt), msg = "`age` must be a column in `dt`")

  # check for duplicates
  dt[, test := .N, by = c(id_cols, "age")]
  assertable::assert_values(dt, c("test"), test = "equal", test_val = 1)
  dt[, test := NULL]

  # set key
  original_keys <- key(dt)
  setkeyv(dt, c(id_cols, "age"))

  # calculate `lx`
  dt[, lx := 1]
  dt[, lx := lx[1] * cumprod(c(1, head(1 - qx, -1))), by = id_cols]

  # check results
  if (assert_na == T) assertable::assert_values(dt, "lx", "not_na", quiet = T)
  assertable::assert_values(dt, c("lx"), test = "gte", test_val = 0)
  assertable::assert_values(dt, c("lx"), test = "lte", test_val = 1)

  # return
  setkeyv(dt, original_keys)
  return(dt)
}
