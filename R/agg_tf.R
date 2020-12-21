#' @title Calculate total fertility aggregates
#'
#' @description Calculate total fertility for a specified age range using
#'   age-specific fertility rates.
#'
#' @param dt \[`data.table()`\]\cr
#'   ASFR input data. Must include all columns in `id_cols`, and a column for
#'   'asfr'.
#' @param age_mapping \[`data.table()`\]\cr
#'  Specification of age interval to aggregate to. Required columns are
#'  'age_start' and 'age_end'.
#' @inheritParams hierarchyUtils::agg
#'
#' @return \[`data.table()`\]\cr Aggregated total fertility with columns for all
#'   `id_cols` and a 'tf' column. Will only return the age groups specified in
#'   `age_mapping`.
#'
#' @seealso [hierarchyUtils::agg()]
#'
#' @inheritSection hierarchyUtils::agg Severity Arguments
#'
#' @details
#' Calculate total fertility aggregate for ages within a specific age range.
#' TFR (total fertility rate) is measured over the entire reproductive age span,
#' typically defined as between age 15 and 49 (or 10 and 54). `agg_tf` also
#' allows calculation of total fertility for other age spans like total
#' fertility under 25 and total fertility over 30.
#'
#' Total fertility is calculated as the sum of ASFR multiplied by the number of
#' years in an age group. This number represents the average number of children
#' born to a woman if (1) she experiences a specific set of age specific
#' fertility rates and (2) she survives through the end of the age span.
#' Preston pg 95.
#'
#' This is different from an age-specific fertility rate (ASFR) or a crude birth
#' rate (CBR), both of which are calculated as births/population for a
#' particular age group or all reproductive ages, respectively.
#'
#' @references
#' Preston, Samuel, Patrick Heuveline, and Michel Guillot. 2001. Demography:
#' Measuring and Modeling Population. Wiley.
#'
#' @examples
#' # calculate total fertility under 25 (ages 10 to 24)
#' dt <- data.table::data.table(
#'   age_start = c(10, 15, 20, 25, 30, 35, 40, 45),
#'   age_end = c(15, 20, 25, 30, 35, 40, 45, 50),
#'   asfr = c(0.00005, 0.02, 0.07, 0.08, 0.05, 0.02, 0.004, 0.0002)
#' )
#'
#' dt <- agg_tf(
#'   dt = dt,
#'   id_cols = c("age_start", "age_end"),
#'   age_mapping = data.table::data.table(age_start = 10, age_end = 25)
#' )
#'
#' @export
agg_tf <- function(dt,
                   id_cols,
                   age_mapping,
                   missing_dt_severity = "stop",
                   overlapping_dt_severity = "stop",
                   present_agg_severity = "stop",
                   na_value_severity = "stop",
                   quiet = FALSE) {

  # Validate arguments (before `hierarchyUtils::agg`) -----------------------

  # basic checks for 'id_cols` argument
  assertthat::assert_that(
    assertive::is_character(id_cols),
    all(c("age_start", "age_end") %in% id_cols),
    msg = c("`id_cols` must be a character vector that includes 'age_start',
            'age_end', & 'asfr'")
  )

  # basic checks for `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(
    data = dt, colnames = c(id_cols, "asfr"),
    only_colnames = F, quiet = T
  )

  # prep -----------------------------------------------------------------------

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  dt <- copy(dt)
  hierarchyUtils::gen_length(dt, col_stem = 'age')

  # calculate ------------------------------------------------------------------

  dt <- dt[, tf := asfr * age_length]

  dt <- hierarchyUtils::agg(
    dt[, .SD, .SDcols = c(id_cols, 'tf')],
    id_cols = id_cols,
    value_cols = 'tf',
    col_stem = 'age',
    col_type = 'interval',
    mapping = age_mapping,
    agg_function = sum,
    missing_dt_severity = missing_dt_severity,
    overlapping_dt_severity = overlapping_dt_severity,
    present_agg_severity = present_agg_severity,
    na_value_severity = na_value_severity,
    quiet = quiet
  )

  data.table::setcolorder(dt, c(setdiff(original_col_order, "asfr"), "tf"))
  data.table::setkeyv(dt, original_keys)
  return(dt)
}
