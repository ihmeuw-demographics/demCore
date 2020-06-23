#' @title Scale qx values over age
#'
#' @description Scale granular qx values to be consistent with aggregate age
#'   interval qx values already present in the data.
#'
#' @param dt \[`data.table()`\]\cr Data to be aggregated.
#'   * Must only include columns 'qx' and those specified in `id_cols`
#'   * Must include 'age_start' and 'age_end' columns
#' @inheritParams hierarchyUtils::agg
#'
#' @return \[`data.table()`\]\cr Scaled qx values, has `id_cols` and 'qx'
#'   columns for all age groups.
#'
#' @details
#' Convert to px-space, scale up age-hierarchy so that granular px
#'   values multiply to aggregate px values, convert back to qx-space.
#'   `scale_qx()` is a wrapper for
#'   \code{\link[hierarchyUtils::agg_scale]{hierarchyUtils::scale()}}.
#'
#' This function is unique to aggregating and scaling qx over age because of
#' the dependence between age and the definition of qx. Aggregation of qx
#' across sex, location, or other variables should be done by first aggregating
#' ax and mx as population-weighted means (can use
#' \code{\link[hierarchyUtils::agg_scale]{hierarchyUtils::agg()}} and
#' \code{\link[hierarchyUtils::agg_scale]{hierarchyUtils::scale()}} directly),
#' then recalculating qx from mx and ax (see [mx_ax_to_qx()]).
#'
#' @seealso Vignette on scaling multiplicative aggregates in `hierarchyUtils`.
#'
#' @examples
#' # scale qx up single-year to abridged u5 hierarchy
#' dt <- data.table::data.table(
#'   sex = "male",
#'   qx = c(0.03, 0.015, 0.005, 0.004, 0.001, 0.03, 0.05),
#'   age_start = c(0, 1, 2, 3, 4, 1, 0),
#'   age_end = c(1, 2, 3, 4, 5, 5, 5)
#' )
#' dt <- scale_qx(dt, id_cols = c("sex", "age_start", "age_end"))
#'
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

