#' @title Summarize life tables
#'
#' @description Create mean and uncertainty intervals for summary-level life
#'   tables based off of draws.
#'
#' @param dt \[`data.table()`\]\cr Draw-level life tables, columns include all
#'  `id_cols`, all `lt_params`, and 'draw'.
#' @param id_cols \[`character()`\]\cr Column names in 'dt' that uniquely
#'   identify all rows. Must include 'draw'.
#' @param lt_params \[`character()`\]\cr Life table parameters included in `dt`.
#'   Example: c("mx", "ax", "dx"). Must include 'mx' and 'ax'.
#'
#' @return \[`data.table()`\]\cr Summarized life table with `id_cols`,
#'   'life_table_parameter', 'mean', 'lower', and 'upper'
#'
#' @details  One draw is one independent simulation or calculation -- we
#'   typically use 100 or 1000 to bootstrap uncertainty and propagate error
#'   through multiple steps. First draws are collapsed to mean, lower
#'   (2.5th percentile), and upper (97.5th percentile). Then, we make sure
#'   mean-level life table parameters are consistent with one another by
#'   re-calculating every requested parameter other than mx and ax, using
#'   standard methods documented elsewhere.
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
#' dt <- gen_summary_lt(dt, id_cols = c("age_start", "age_end", "draw"),
#'   lt_params = c("mx", "ax"))
#'
#' @export

gen_summary_lt <- function(dt, id_cols, lt_params) {

  # validate ----------------------------------------------------------------

  # check `lt_params`
  assertthat::assert_that(
    length(setdiff(c("mx", "ax"), lt_params)) == 0,
    msg = "`lt_params` must include 'mx' and 'ax'"
  )

  # check `dt`
  validate_lifetable(dt, id_cols, param_cols = lt_params)

  # additional `id_cols` check
  assertthat::assert_that(
    "draw" %in% id_cols,
    msg = "`id_cols` must include 'draw'."
  )

  # prep --------------------------------------------------------------------

  # add 'age_length' if not in input
  if(!"age_length" %in% names(dt)) {
    dt <- hierarchyUtils::gen_length(dt, col_stem = "age")
  }

  # get `id_cols` without 'draw'
  id_cols_no_draw <- id_cols[id_cols != "draw"]

  # summarize ---------------------------------------------------------------

  # helper functions
  lower <- function(x) stats::quantile(x, probs = 0.025, na.rm = T)
  upper <- function(x) stats::quantile(x, probs = 0.975, na.rm = T)

  # melt life table parameters long
  dt <- melt(dt, id.vars = c(id_cols, "age_length"), measure.vars = lt_params,
             variable.name = "life_table_parameter", value.name = "value")

  # collapse
  dt <- dt[, list(mean = mean(value), lower = lower(value),
                  upper = upper(value)),
            by = c(id_cols_no_draw, "life_table_parameter", "age_length")]

  # recalculate mean --------------------------------------------------------

  # check if we need to recalculate lt parameters at all
  if(length(setdiff(lt_params, c("mx", "ax"))) > 0) {

    # reshape wide on life table parameter for mean values
    # only keep mx and ax
    dt_mean <- copy(dt)
    dt_mean[, c("lower", "upper") := NULL]
    dt_mean <- dt_mean[life_table_parameter %in% c("mx", "ax")]
    dt_mean <- dcast(dt_mean, ... ~ life_table_parameter, value.var = c("mean"))

    # re-calculate qx
    dt_mean[, qx := mx_ax_to_qx(mx, ax, age_length)]

    # check if we need parameters other than mx, ax, qx
    if(length(setdiff(lt_params, c("mx", "ax", "qx"))) > 0) {

      # recalculate all other life table parameters
      dt_mean <- lifetable(dt_mean, id_cols_no_draw)

    }

    # subset to only the life table parameters we want
    dt_mean <- dt_mean[, .SD, .SDcols = c(id_cols_no_draw, lt_params)]

    # melt life table parameters long
    dt_mean <- melt(
      dt_mean,
      id.vars = id_cols_no_draw,
      measure.vars = lt_params,
      variable.name = "life_table_parameter",
      value.name = "mean"
    )

    # replace mean in `dt`
    dt[, mean := NULL]
    dt <- merge(dt, dt_mean, by = c(id_cols_no_draw, "life_table_parameter"))

  }

  # return ------------------------------------------------------------------

  dt <- dt[, .SD, .SDcols = c(id_cols_no_draw, "life_table_parameter",
                              "mean", "lower", "upper")]
  return(dt)

}
