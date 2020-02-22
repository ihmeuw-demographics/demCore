#' Aggregate qx values over age
#'
#' Given a data.table with a qx variable and ID variables that uniquely
#'   identify the data, compile granular ages and aggregate into combined qx values.
#'   Using probability rules, we convert qx to px (conditional survival probability),
#'   multiply these px values within a specified age range together to get the
#'   conditional probability of survival through all granular age groups, and
#'   subtract from one to get the aggregated conditional probability of death (qx).
#'
#' @param dt data.table with data to be aggregated.
#'   * must only include columns 'qx' and those specified in `id_cols`
#'   * must include 'age_start' and 'age_end' columns
#'   * `id_cols` must uniquely identify each row
#' @param id_cols character vector of column names that uniquely identify each row
#'   of `dt`
#' @param target_ages_dt data.table defining age groups to aggregate to.
#'   * must include 'age_start' and 'age_end' columns.
#'
#' @return data.table with `id_cols` and `qx` columns for aggregate age groups.
#' @details This function is a wrapper for demUtils::aggregate_age
#' @import data.table
#'
#' @examples
#' dt <- data.table::data.table(
#'   id = c(rep(1, 5), rep(2, 5)), qx = c(rep(.1, 5), rep(.2, 5)),
#'   age_start = rep(seq(15, 35, 5), 2),
#'   age_end = rep(seq(20, 40, 5), 2)
#' )
#' target_dt <- data.table::data.table(age_start = c(15),
#'                                     age_end = c(40))
#' agg_qx(dt, id_cols = c("id", "age_start", "age_end"), target_ages_dt = target_dt)
#'
#' @export
agg_qx <- function(dt, id_cols, target_ages_dt) {

  # Validate qx
  assertthat::assert_that("qx" %in% names(dt), msg = "Missing 'qx' column in input")
  assertable::assert_values(dt, c("qx"), test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, c("qx"), test = "lte", test_val = 1, quiet = T)

  # Calculate survival probability (px)
  dt[, px := 1 - qx]
  dt[, qx := NULL]

  # Aggregate over age using multiplicative aggregation
  dt <- demUtils::aggregate_age(dt,
                                id_cols,
                                value_cols = c("px"),
                                target_ages_dt = target_ages_dt,
                                value_type = "probability")

  # Convert px back to qx
  dt[, qx := 1 - px]
  dt[, px := NULL]

  return(dt)
}
