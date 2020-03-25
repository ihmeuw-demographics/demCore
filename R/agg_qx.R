#' @title Aggregate qx values over age
#'
#' @description Given a data.table with a qx variable and ID variables that
#'   uniquely identify the data, compile granular ages and aggregate into
#'   combined qx values. Using probability rules, we convert qx to px
#'   (conditional survival probability), multiply these px values within a
#'   specified age range together to get the conditional probability of survival
#'   through all granular age groups, and subtract from one to get the
#'   aggregated conditional probability of death (qx).
#'
#' @param dt \[`data.table()`\] data to be aggregated.
#'   * must only include columns 'qx' and those specified in `id_cols`
#'   * must include 'age_start' and 'age_end' columns
#'   * `id_cols` must uniquely identify each row
#' @param age_mapping \[`data.table()`\] specification of intervals to aggregate
#'   to. Required columns are 'age_start' and 'age_end'. Use "Inf" as 'age_end'
#'   for terminal age group.
#' @inheritParams demUtils::agg
#'
#' @return data.table with `id_cols` and `qx` columns for aggregate age groups.
#'
#' @details This function is a wrapper for demUtils::aggregate_age
#'
#' @examples
#' dt <- data.table::data.table(
#'   id = c(rep(1, 5), rep(2, 5)),
#'   qx = c(rep(.1, 5), rep(.2, 5)),
#'   age_start = rep(seq(15, 35, 5), 2),
#'   age_end = rep(seq(20, 40, 5), 2)
#' )
#' age_mapping <- data.table::data.table(age_start = c(15), age_end = c(40))
#' agg_qx(dt, id_cols = c("id", "age_start", "age_end"),
#'         age_mapping = age_mapping)
#' @export

agg_qx <- function(dt, id_cols, age_mapping, drop_present_aggs = F) {

  # Validate -------------------------------------------------------------

  # check `id_cols`
  assertive::assert_is_character(id_cols)
  assertthat::assert_that("age_start" %in% id_cols,
                          msg = "`id_cols` must include 'age_start'.")
  assertthat::assert_that("age_end" %in% id_cols,
                          msg = "`id_cols` must include 'age_end'.")

  # check `dt`
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("qx", id_cols), quiet = T)
  assertive::assert_is_numeric(dt[["qx"]])
  assertable::assert_values(dt, c("qx"), test = "gte", test_val = 0, quiet = T)
  assertable::assert_values(dt, c("qx"), test = "lte", test_val = 1, quiet = T)

  # other assertions completed within demUtils::aggregate_age

  # Aggregate ------------------------------------------------------------

  # copy
  dt <- copy(dt)

  # Calculate survival probability (px)
  dt[, px := 1 - qx]
  dt[, qx := NULL]

  # Aggregate over age using multiplicative aggregation
  dt <- demUtils::agg(
    dt,
    id_cols = id_cols,
    value_cols = c("px"),
    col_stem = "age",
    col_type = "interval",
    mapping = age_mapping,
    agg_function = prod,
    drop_present_aggs = drop_present_aggs
  )

  # Convert px back to qx
  dt[, qx := 1 - px]
  dt[, px := NULL]

  return(dt)
}
