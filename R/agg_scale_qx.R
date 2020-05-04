#' @title Aggregate or scale qx values over age
#'
#' @description Given a data.table with granular age interval qx values, either
#'   aggregate qx to get wider age interval qx values or scale granular qx
#'   values to be consistent with aggregate age interval qx values already
#'   present in the data.
#'
#' @param dt \[`data.table()`\]\cr Data to be aggregated.
#'   * Must only include columns 'qx' and those specified in `id_cols`
#'   * Must include 'age_start' and 'age_end' columns
#' @param age_mapping \[`data.table()`\]\cr Specification of intervals to
#'   aggregate to. Required columns are 'age_start' and 'age_end'. Use "Inf" as
#'   'age_end' for terminal age group. If scaling, this mapping is inferred
#'   from the ages included.
#' @inheritParams hierarchyUtils::agg
#'
#' @return \[`data.table()`\]\cr Aggregated or scaled qx values, has `id_cols`
#'   and 'qx' columns for aggregate age groups if aggregating or all age
#'   groups if scaling.
#'
#' @details
#' **Aggregation:** Using probability rules, we convert qx to px
#'   (conditional survival probability), multiply these px values within a
#'   specified age range together to get the conditional probability of survival
#'   through all granular age groups, and subtract from one to get the
#'   aggregated conditional probability of death (qx). `agg_qx` is a wrapper
#'   for [hierarchyUtils::agg()].
#'
#' **Scaling:** Convert to px-space, scale up age-hierarchy so that granular px
#'   values multiply to aggregate px values, convert back to qx-space.
#'   `scale_qx` is a wrapper for [hierarchyUtils::scale()].
#'
#' This function is unique to aggregating and scaling qx over age because of
#' the dependence between age and the definition of qx. Aggregation of qx
#' across sex, location, or other variables should be done by first aggregating
#' ax and mx as population-weighted means (can use [hierarchyUtils::agg()] and
#' [hierarchyUtils::scale()] directly), then recalculating qx from mx and ax
#' (see [demCore::mx_ax_to_qx()]).
#'
#' @seealso Vignette on scaling multiplicative aggregates in `hierarchyUtils`.
#'
#' @examples
#' # Example 1: aggregate qx
#' dt <- data.table::data.table(
#'   id = c(rep(1, 5), rep(2, 5)),
#'   qx = c(rep(0.1, 5), rep(0.2, 5)),
#'   age_start = rep(seq(15, 35, 5), 2),
#'   age_end = rep(seq(20, 40, 5), 2)
#' )
#' age_mapping <- data.table::data.table(age_start = c(15), age_end = c(40))
#' dt <- agg_qx(dt, id_cols = c("id", "age_start", "age_end"),
#'         age_mapping = age_mapping)
#'
#' # Example 2: scale qx up single-year to abridged u5 hierarchy
#' dt <- data.table::data.table(
#'   sex = "male",
#'   qx = c(0.03, 0.015, 0.005, 0.004, 0.001, 0.03, 0.05),
#'   age_start = c(0, 1, 2, 3, 4, 1, 0),
#'   age_end = c(1, 2, 3, 4, 5, 5, 5)
#' )
#' dt <- scale_qx(dt, id_cols = c("sex", "age_start", "age_end"))
#'
#' @name agg_scale_qx
NULL

# ============================================================================
#' @rdname agg_scale_qx
#' @export
agg_qx <- function(dt, id_cols, age_mapping, missing_dt_severity = "stop",
                   drop_present_aggs = F) {

  # Validate -------------------------------------------------------------

  # check `id_cols` and `dt`
  validate_lifetable(dt, id_cols, param_cols = c("qx"))

  # other assertions completed within hierarchyUtils::agg

  # Aggregate ------------------------------------------------------------

  # copy
  dt <- copy(dt)

  # Calculate survival probability (px)
  dt[, px := 1 - qx]
  dt[, qx := NULL]

  # Aggregate over age using multiplicative aggregation
  dt <- hierarchyUtils::agg(
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


# ============================================================================
#' @rdname agg_scale_qx
#' @export
scale_qx <- function(dt, id_cols, missing_dt_severity = "stop") {

  # Validate -------------------------------------------------------------

  # check `id_cols` and `dt`
  validate_lifetable(dt, id_cols, param_cols = c("qx"))

  # other assertions completed within hierarchyUtils::scale

  # Scale ------------------------------------------------------------

  # copy
  dt <- copy(dt)

  # Calculate survival probability (px)
  dt[, px := 1 - qx]
  dt[, qx := NULL]

  # Scale over age using multiplicative aggregation
  dt <- hierarchyUtils::scale(
    dt,
    id_cols = id_cols,
    value_cols = c("px"),
    col_stem = "age",
    col_type = "interval",
    agg_function = prod
  )

  # Convert px back to qx
  dt[, qx := 1 - px]
  dt[, px := NULL]

  return(dt)
}

