#' @title Calculate summary statistics for life tables
#'
#' @description Calculate summary statistics for life tables when collapsing
#'   over a certain variable. For example can calculate summary statistics
#'   across a set of draws, or locations, or location-years, etc.
#'
#' @inheritParams demUtils::summarize_dt
#' @inheritParams lifetable
#' @param dt \[`data.table()`\]\cr
#'   Life tables to calculate summary statistics for. Must include all
#'   `id_cols`, `summarize_cols`, and `value_cols`.
#' @param value_cols \[`character()`\]\cr
#'   Value columns (life table parameters) that summary statistics should be
#'   calculated for. Valid choices are 'mx', 'qx', 'ax,', 'dx', 'px', 'lx',
#'   'dx', 'Tx', 'nLx', & 'ex'. Must include at least 2 of 'mx', 'qx', or 'ax'.
#' @param recalculate_stats \[`character()`\]\cr
#'   The summary statistic to recalculate all life table parameters
#'   corresponding to that statistic so that they are consistent with one
#'   another. Uses `demCore::lifetable` to recalculate all life table parameters
#'   and keeps the life table parameters specified in `value_cols`. Default is
#'   'mean'.
#' @param format_long \[`logical(1)`\]\cr
#'   Whether to format output with a 'life_table_parameter' column and a column
#'   for each summary statistic (rather than a column for each
#'   'life_table_parameter' and summary statistic pair).
#'
#' @return \[`data.table()`\] with `id_cols` (minus the `summarize_cols`) plus
#'   summary statistic columns. The summary statistic columns have the same name
#'   as each function specified in `summary_fun` and the quantiles are named
#'   like 'q_`(probs * 100)`'. Each of the summary statistic columns that are
#'   returned are prefixed with the value column name. If `format_long` then
#'   output is returned with a 'life_table_parameter' column and a column for
#'   each summary statistic.
#'
#' @details
#' One example use case for `summarize_lt` is when we have multiple draws
#' (independent simulations) of life tables to propagate uncertainty. Each of
#' the independent draws of life tables may have all life table parameters. The
#' mean, 2.5th and 97.5 percentiles across all draws for all life table
#' parameters can be calculated. But the mean 'mx', 'qx', 'ax' parameters would
#' be inconsistent with the mean 'ex' parameter for example. This is when
#' specifying `recalculate_stats = 'mean'` would recalculate the mean 'ex'
#' parameter using the mean 'mx', 'qx', and/or 'ax' as inputs to the
#' `demCore::lifetable` function.
#'
#' @seealso demUtils::summarize_dt
#' @seealso demCore::lifetable
#' @seealso demCore::validate_lifetable
#'
#' @examples
#' library(data.table)
#' data("austria_1992_lt")
#' dt <- data.table::data.table()
#' for(d in 1:100){
#'  dt_new <- copy(austria_1992_lt)
#'  dt_new[, draw := d]
#'  dt_new[, mx := mx * rnorm(1, mean = 1, sd = 0.05)]
#'  dt_new[, ax := ax * rnorm(1, mean = 1, sd = 0.05)]
#'  dt_new[, qx := NULL]
#'  dt <- rbind(dt, dt_new, fill = TRUE)
#' }
#' dt <- dt[!is.na(age_start)]
#' dt <- dt[, .(age_start, age_end, draw, mx, ax)]
#' dt <- summarize_lt(
#'   dt = dt,
#'   id_cols = c("age_start", "age_end", "draw"),
#'   summarize_cols = c("draw"),
#'   value_cols = c("mx", "ax"),
#'   recalculate_stats = "mean"
#' )
#'
#' @export
summarize_lt <- function(dt,
                         id_cols,
                         summarize_cols,
                         value_cols,
                         summary_fun = c("mean"),
                         probs = c(0.025, 0.975),
                         recalculate_stats = c("mean"),
                         preserve_u5 = FALSE,
                         assert_na = FALSE,
                         format_long = FALSE) {

  # validate and prep  ------------------------------------------------------

  # check `format_long`
  assertthat::assert_that(assertthat::is.flag(format_long))

  # check specified `value_cols`
  valid_value_cols <- c("mx", "qx", "ax", "dx", "px", "lx", "nLx", "Tx", "ex")
  required_value_cols <- c("mx", "qx", "ax")
  invalid_value_cols <- setdiff(value_cols, valid_value_cols)
  assertthat::assert_that(
    length(invalid_value_cols) == 0,
    msg = paste0("invalid `value_cols` specified: '",
                 paste(invalid_value_cols, collapse = "', '"), "'")
  )
  assertthat::assert_that(
    length(intersect(c("mx", "ax", "qx"), value_cols)) >= 2,
    msg = "`value_cols` must include at least 2 of 'mx', 'qx', or 'ax'."
  )

  # standard validations
  validate_lifetable(
    dt = dt,
    id_cols = id_cols,
    param_cols = value_cols,
    assert_na = assert_na
  )

  # summarize ---------------------------------------------------------------

  # collapse
  summary_dt <- demUtils::summarize_dt(
    dt = dt,
    id_cols = id_cols,
    summarize_cols = summarize_cols,
    value_cols = value_cols,
    summary_fun = summary_fun,
    probs = probs
  )
  original_summary_cols <- names(summary_dt)
  original_summary_keys <- key(summary_dt)

  # recalculate mean --------------------------------------------------------

  keep_params <- c("mx", "qx", "ax")
  recalculate_params <- setdiff(value_cols, keep_params)
  by_id_cols <- setdiff(id_cols, summarize_cols)

  # check if we need to recalculate lt parameters at all
  if(length(recalculate_params) > 0) {

    # check for `age_length` and add if missing
    if(!"age_length" %in% names(dt)) {
      hierarchyUtils::gen_length(dt, col_stem = "age")
    }

    keep_params <- keep_params[keep_params %in% value_cols]

    summary_dt_recalculated <- lapply(recalculate_stats, function(stat) {
      recalculate_cols <- paste0(keep_params, "_", stat)
      recalculated_cols <- paste0(value_cols, "_", stat)

      summary_dt_temp <- summary_dt[, .SD, .SDcols = c(by_id_cols, recalculate_cols)]
      setnames(summary_dt_temp, recalculate_cols, keep_params)

      summary_dt_temp <- demCore::lifetable(
        dt = summary_dt_temp,
        id_cols = by_id_cols,
        preserve_u5 = preserve_u5,
        assert_na = assert_na
      )
      summary_dt_temp <- summary_dt_temp[, .SD, .SDcols = c(by_id_cols, value_cols)]
      setnames(summary_dt_temp, value_cols, recalculated_cols)
      return(summary_dt_temp)
    })
    summary_dt_recalculated <- Reduce(f = merge, x = summary_dt_recalculated)

    # replace recalculated parameters
    all_recalculated_cols <- paste(
      rep(value_cols[!value_cols %in% keep_params], each = length(recalculate_stats)),
      recalculate_stats, sep = "_"
    )
    summary_dt_recalculated <- summary_dt_recalculated[
      , .SD,
      .SDcols = c(by_id_cols, all_recalculated_cols)
    ]
    summary_dt <- summary_dt[
      , .SD,
      .SDcols = original_summary_cols[!original_summary_cols %in% all_recalculated_cols]
    ]
    summary_dt <- merge(summary_dt, summary_dt_recalculated, by = by_id_cols)

    setcolorder(summary_dt, original_summary_cols)
    setkeyv(summary_dt, original_summary_keys)
  }

  # formatting --------------------------------------------------------------

  if (format_long) {
    summary_dt <- melt(
      data = summary_dt,
      id.vars = by_id_cols,
      measure.vars = setdiff(names(summary_dt), by_id_cols),
      variable.name = "life_table_parameter", value.name = "value"
    )

    # split into 'life_table_parameter' and 'statistic' columns
    regex <- paste0(paste(valid_value_cols, collapse = "_|"), "_")
    summary_dt[, statistic := gsub(regex, "", life_table_parameter)]
    regex <- paste0("_", paste(unique(summary_dt$statistic), collapse = "|_"))
    summary_dt[, life_table_parameter := gsub(regex, "", life_table_parameter)]
    assertable::assert_values(
      data = summary_dt, colnames = "life_table_parameter",
      test = "in", value_cols, quiet = TRUE
    )

    # cast statistics wide
    summary_dt <- dcast(
      data = summary_dt,
      formula = stats::as.formula(paste0(
        paste(c(by_id_cols, "life_table_parameter"), collapse = " + "),
        " ~ statistic")
      )
    )

    # format column order and keys
    quantile_names <- paste0("q", probs * 100)
    summary_value_cols <- c(summary_fun, if (length(probs) > 0) quantile_names)

    new_summary_cols <- original_summary_cols[original_summary_cols %in% by_id_cols]
    new_summary_cols <- c(new_summary_cols, "life_table_parameter", summary_value_cols)
    setcolorder(summary_dt, new_summary_cols)
    if (!is.null(original_summary_keys)) {
      new_summary_keys <- c(original_summary_keys, "life_table_parameter")
      setkeyv(summary_dt, new_summary_keys)
    }
  }

  return(summary_dt)
}
