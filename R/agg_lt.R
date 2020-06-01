#' @title Aggregate life table(s) to less granular age groups
#'
#' @description Aggregate life table(s) to less granular age groups using
#'   standard life table aggregation functions.
#'
#' @param dt \[`data.table()`\]\cr
#'   Life table  to be aggregated. Must include all columns in `id_cols`, and
#'   at least two of 'qx', 'ax', and 'mx'.
#' @param age_mapping \[`data.table()`\]\cr
#'   Specification of intervals to aggregate to. Required columns are
#'   'age_start' and 'age_end'. Use "Inf" as 'age_end' for terminal age group.
#'   The age group intervals must be contiguous and cover the entire interval
#'   specified in the input life tables `dt`.
#' @inheritParams agg_qx
#'
#' @return \[`data.table()`\]\cr Aggregated life table(s) with columns for
#'   `id_cols`, 'qx', and 'ax'. Will only include the age groups specified in
#'   `age_mapping`.
#'
#' @seealso hierarchyUtils::agg
#'
#' @examples
#' dt <- data.table::data.table(
#'   age_start = c(0:110),
#'   age_end = c(1:110, Inf),
#'   location = "Canada",
#'   qx = .2,
#'   ax = .5
#' )
#' id_cols = c("age_start", "age_end", "location")
#' dt <- agg_lt(
#'   dt = dt,
#'   id_cols = id_cols,
#'   age_mapping = data.table::data.table(
#'     age_start = seq(0, 110, 5),
#'     age_end = c(seq(5, 110, 5), Inf)
#'   )
#' )
#' @export
agg_lt <- function(dt,
                   id_cols,
                   age_mapping,
                   missing_dt_severity = "stop",
                   drop_present_aggs = F) {

  # validate -------------------------------------------------------

  # check `dt` for 2/3 of mx, ax, qx
  assertive::assert_is_data.table(dt)
  assertthat::assert_that(length(intersect(c("mx", "ax", "qx"),
                                           names(dt))) >= 2,
                          msg = "Need at least two of mx, ax, qx in 'dt'.")

  # check `dt` and `id_cols`
  param_cols <- intersect(names(dt), c("mx", "ax", "qx"))
  validate_lifetable(dt, id_cols, param_cols)

  # sort age mapping
  assertive::assert_is_data.table(age_mapping)
  assertable::assert_colnames(
    age_mapping,
    c("age_start", "age_end"),
    only_colnames = T,
    quiet = T
  )
  data.table::setkeyv(age_mapping, c("age_start", "age_end"))
  # assertion that age mapping covers same age range as input lifetable
  hierarchyUtils::assert_no_missing_intervals(
    ints_dt = age_mapping,
    expected_ints_dt = data.table(start = min(dt$age_start),
                                  end = max(dt$age_end))
  )
  hierarchyUtils::assert_no_missing_intervals(
    ints_dt = unique(dt[, .SD, .SDcols = c("age_start", "age_end")]),
    expected_ints_dt = data.table(start = min(age_mapping$age_start),
                                  end = max(age_mapping$age_end))
  )
  # other assertions completed within hierarchyUtils::agg

  # prep ------------------------------------------------------------

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  dt <- copy(dt)
  dt <- dt[, .SD, .SDcols = c(id_cols, param_cols)]

  # get `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  hierarchyUtils::gen_length(dt, "age")

  # fill in such that we have qx, ax, and dx
  if(!"qx" %in% names(dt)) dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
  if(!"ax" %in% names(dt)) dt[, ax := mx_qx_to_ax(mx, qx, age_length)]
  if(!"dx" %in% names(dt)) {
    gen_lx_from_qx(dt, id_cols, assert_na = T)
    gen_dx_from_lx(dt, id_cols, assert_na = T)
  }
  dt <- dt[, .SD, .SDcols = c(id_cols, "qx", "ax", "dx")]
  dt[, px := 1 - qx]

  # aggregate ----------------------------------------------------------

  # aggregate qx
  dt_qx <- hierarchyUtils::agg(
    dt = dt[, .SD, .SDcols = c(id_cols, "px")],
    id_cols = id_cols,
    value_cols = "px",
    col_stem = "age",
    col_type = "interval",
    mapping = age_mapping,
    agg_function = prod,
    missing_dt_severity = missing_dt_severity,
    drop_present_aggs = drop_present_aggs
  )
  dt_qx[, qx := 1 - px]
  dt_qx[, px := NULL]

  # determine the aggregate age group each granular age group belongs to
  dt[, agg_age_start := cut(
    x = age_start,
    breaks = c(age_mapping$age_start, Inf),
    labels = age_mapping$age_start,
    right = F
  )]
  dt[, agg_age_start := as.integer(as.character(agg_age_start))]

  # calculate the integer number of complete person-years lived by those who die
  # in each aggregate age group.
  dt[, ax_full_years := age_start - agg_age_start]

  # calculate total number of person-years lived by those who die in each age
  # group
  dt[, axdx_total := (ax + ax_full_years) * dx]

  dt_ax <- hierarchyUtils::agg(
    dt = dt[, .SD, .SDcols = c(id_cols, "axdx_total", "dx")],
    id_cols = id_cols,
    value_cols = c("axdx_total", "dx"),
    col_stem = "age",
    col_type = "interval",
    mapping = age_mapping,
    agg_function = sum,
    missing_dt_severity = missing_dt_severity,
    drop_present_aggs = drop_present_aggs
  )
  dt_ax[, ax := axdx_total / dx]
  dt_ax[, c("axdx_total", "dx") := NULL]

  agg_lt <- merge(dt_qx, dt_ax, all = TRUE, by = id_cols)

  # check output -----------------------------------------------------

  assertable::assert_values(agg_lt, "ax", "gte", 0, quiet = T)
  assertable::assert_values(agg_lt, "qx", "gte", 0, quiet = T)
  assertable::assert_values(agg_lt, "qx", "lte", 1, quiet = T)
  assertable::assert_values(agg_lt, c("qx", "ax"), "not_na", quiet = T)

  expected_cols <- c(id_cols, "qx", "ax")
  original_col_order <- original_col_order[original_col_order %in% expected_cols]
  data.table::setcolorder(agg_lt, original_col_order)
  data.table::setkeyv(agg_lt, original_keys)

  return(agg_lt)
}
