#' @title Generate abridged life table from full life table
#'
#' @description Convert full (single-year-age) life tables to abridged (5-year
#'   age group) life tables using standard life table aggregation functions.
#'
#' @param dt \[`data.table()`\] full life table(s), variables `age`, all vars
#'   in `id_cols`, and at least two of `qx`, `ax`, and `mx`. `dx` is used but if
#'   it is not provided it is calculated within.
#' @param id_cols \[`character()`\] variables that uniquely identify
#'   observations
#' @param abridged_ages \[`integer()`\] ages to break the single-year ages into.
#'   Default = c(0, 1, seq(5, 110, 5)).
#'
#' @return data.table with `id_cols`, `qx`, and `ax`, for abridged ages
#'   specified in `abridged_ages`
#'
#' @examples
#' dt <- data.table::data.table(
#'   age = c(0:110),
#'   location = "Canada",
#'   qx = .2,
#'   ax = .5
#' )
#' id_cols = c("age", "location")
#' dt <- full_to_abridged(dt, id_cols)
#' @export

full_to_abridged <- function(dt, id_cols,
                             abridged_ages = c(0, 1, seq(5, 110, 5))) {

  # validate -------------------------------------------------------

  # check `dt` for 2/3 of mx, ax, qx
  assertive::assert_is_data.table(dt)
  assertthat::assert_that(length(intersect(c("mx", "ax", "qx"),
                                           names(dt))) >= 2,
                          msg = "Need at least two of mx, ax, qx in 'dt'.")

  # check `dt` and `id_cols`
  param_cols <- intersect(names(dt), c("mx", "ax", "qx"))
  validate_lifetable(dt, id_cols, param_cols)

  # check and sort `abridged_ages`
  assertive::assert_is_numeric(abridged_ages)
  assertthat::assert_that(length(abridged_ages) ==
                            length(unique(abridged_ages)),
                          msg = "Abridged ages should be unique.")
  abridged_ages <- sort(abridged_ages)

  # check that we have all single year ages within `abridged_ages`
  expected_ages <- seq(min(abridged_ages), max(abridged_ages), 1)
  provided_ages <- unique(dt$age)
  if(!setequal(expected_ages, provided_ages)) {
    stop("dt must include all single-year ages between min(abridged_ages) and
         max(abridged_ages) and no extra ages.")
  }

  # check that data is square on age
  dt[, age_count := .N, by = c(id_cols)]
  assertthat::are_equal(1, length(unique(dt$age_count)),
                        msg = "Check that input data is square.")
  dt[, c("age_count") := NULL]

  # prep ------------------------------------------------------------

  # get `id_cols` without age
  id_cols_no_age <- id_cols[id_cols != "age"]

  # fill in such that we have qx, ax, and dx
  if(!"qx" %in% names(dt)) dt[, qx := mx_ax_to_qx(mx, ax, 1)]
  if(!"ax" %in% names(dt)) dt[, ax := mx_qx_to_ax(mx, qx, 1)]
  if(!"dx" %in% names(dt)) {
    dt <- qx_to_lx(dt, id_cols, assert_na = T)
    dt <- lx_to_dx(dt, id_cols, terminal_age = max(dt$age), assert_na = T)
  }
  dt <- dt[, .SD, .SDcols = c(id_cols, "qx", "ax", "dx")]

  # assign single year ages to abridged ages
  dt[, abridged_age := cut(age,
                           breaks = c(abridged_ages, Inf),
                           labels = abridged_ages,
                           right = F)]
  dt[, abridged_age := as.integer(as.character(abridged_age))]

  # aggregate ----------------------------------------------------------

  # aggregate qx and ax
  dt[, px := 1 - qx]
  dt[, axdx_full_years := age - abridged_age]
  agg_lt <- dt[, list(qx = (1 - prod(px)),
                   ax = (sum((ax + axdx_full_years) * dx) / sum(dx))),
                by = c(id_cols_no_age, "abridged_age")]
  setnames(agg_lt, "abridged_age", "age")

  # terminal age qx = 1
  agg_lt[age == max(abridged_ages), qx := 1]

  # check output -----------------------------------------------------

  assertable::assert_values(agg_lt, "ax", "gte", 0, quiet = T)
  assertable::assert_values(agg_lt, "qx", "gte", 0, quiet = T)
  assertable::assert_values(agg_lt, "qx", "lte", 1, quiet = T)
  assertable::assert_values(agg_lt, c("qx", "ax"), "not_na", quiet = T)

  return(agg_lt)
}
