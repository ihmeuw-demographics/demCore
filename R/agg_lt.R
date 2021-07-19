#' @title Aggregate life table(s) to less granular age groups
#'
#' @description Aggregate life table(s) to less granular age groups using
#'   standard life table aggregation functions of qx (and ax).
#'
#' @param dt \[`data.table()`\]\cr
#'   Life table  to be aggregated. Must include all columns in `id_cols`, and
#'   at least two of 'qx', 'ax', and 'mx', or just 'qx'.
#' @param age_mapping \[`data.table()`\]\cr
#'   Specification of intervals to aggregate to. Required columns are
#'   'age_start' and 'age_end'. Use "Inf" as 'age_end' for terminal age group.
#'   The age group intervals must be contiguous and cover the entire interval
#'   specified in the input life tables `dt`.
#' @param ... Other arguments to pass to [hierarchyUtils::agg()].
#' @inheritParams hierarchyUtils::agg
#'
#' @return \[`data.table()`\]\cr Aggregated life table(s) with columns for all
#'   `id_cols`. A column for 'qx' is always included, a column for 'ax' will
#'   also be returned if two of 'qx', 'ax', and 'mx' are included in the input
#'   `dt`. Will only return the age groups specified in `age_mapping`.
#'
#' @seealso [hierarchyUtils::agg()]
#'
#' @inheritSection hierarchyUtils::agg Severity Arguments
#'
#' @details
#' See the [references page](https://ihmeuw-demographics.github.io/demCore/index.html)
#' for the formatted equations below.
#'
#' This function works by aggregating the qx and ax life table parameters
#' separately. If only qx is included in `dt` then ax aggregation is not done.
#'
#' **qx aggregation:**
#'
#' To explain how qx is aggregated it is useful to define a couple of different
#' events:
#' \deqn{D = \text{death between age } x \text{ and } x + n}
#' \deqn{D' = \text{survival between age } x \text{ and } x + n}
#' \deqn{S = \text{survival to age } x}
#'
#' Now qx and px can be written in terms of events D and S.
#' \deqn{{}_{n}q_x = P(D | S)}
#' \deqn{{}_{n}p_x = P(D' | S)}
#'
#' Now say there are multiple sub age-groups that make up the overall age group
#' between \eqn{x \text{ and } x + n}. Let subscripts "1" and "2" indicate
#' values specific to the first and second sub age-groups and assume values with
#' no subscript apply to the original aggregate age group. The first sub
#' age-group could be between \eqn{x \text{ and } x + n_1} and the second
#' between \eqn{x + n_1 \text{ and } x + n_1 + n_2}, where \eqn{n_1 + n_2 = n}.
#'
#' The overall px value can be written as a function of the sub age-group's px
#' values.
#' \deqn{P(D'|S) = P(D'_1|B_1) \cap P(D'_2|B_2) = P(D'_1|B_1) * P(D'_2|B_2)}
#'
#' where:
#' \deqn{P(D'_1 | B_1) = \text{survival between age } x \text{ and } x + n_1
#'   \text{ given survival to age } x}
#' \deqn{P(D'_2 | B_2) = \text{survival between age } x + n_1 \text{ and } x + n
#'   \text{ given survival to age } x + n_1}
#'
#' More generally if there are \eqn{A} age groups between age
#' \eqn{x \text{ and } x + n}, and \eqn{i} indexes each of the sub age
#' intervals then:
#' \deqn{{}_{n}p_x = \prod_{i=1}^{A} {}_{n_i}p_{x_i} =
#'   \prod_{i=1}^{A}(1 - {}_{n_i}q_{x_i})}
#' \deqn{{}_{n}q_x = 1 - {}_{n}p_x}
#'
#' **ax aggregation:**
#'
#' \eqn{{}_{n}a_x} is aggregated across age groups by aggregating the number of
#' person-years lived in each age group by those who died in the interval.
#'
#' \deqn{{}_{n}a_x \cdot {}_{n}d_x = \text{person-years lived between age } x
#'   \text{ and } x + n \text{ by those who died in this age interval}}
#' where:
#' \deqn{{}_{n}a_x = \text{average years lived between age } x \text{ and }
#'   x + n \text{ by those who died in the age interval}}
#' \deqn{{}_{n}d_x = \text{number that died between age } x \text{ and } x + n}
#'
#' Now say there are \eqn{A} age groups between age \eqn{x \text{ and } x + n},
#' and \eqn{i} indexes each of the sub age intervals. The total number of
#' person-years lived by those who died in the aggregate age group is a simple
#' sum of the number of person-years lived in each sub age interval.
#' \deqn{{}_{n}a_x \cdot {}_{n}d_x = \sum_{i = 1}^{A}
#'   ((x_i - x) + {}_{n_i}a_{x_i}) \cdot {}_{n_i}d_{x_i}}
#' where:
#' \deqn{x_i - x = \text{ number of complete person years lived in the previous
#'   sub age intervals by someone who dies in sub age interval } i}
#'
#' The aggregate ax can then be solved for.
#' \deqn{{}_{n}a_x = \frac{\sum_{i = 1}^{A} ((x_i - x) + {}_{n_i}a_{x_i})
#'   \cdot {}_{n_i}d_{x_i}}{\sum_{i = 1}^{A} {}_{n_i}d_{x_i}}}
#'
#' @examples
#' dt <- data.table::data.table(
#'   age_start = c(0:110),
#'   age_end = c(1:110, Inf),
#'   location = "Canada",
#'   qx = c(rep(.2, 110), 1),
#'   ax = .5
#' )
#' id_cols = c("age_start", "age_end", "location")
#' dt <- agg_lt(
#'   dt = dt,
#'   id_cols = id_cols,
#'   age_mapping = data.table::data.table(
#'     age_start = seq(0, 105, 5),
#'     age_end = seq(5, 110, 5)
#'   )
#' )
#' @export
agg_lt <- function(dt,
                   id_cols,
                   age_mapping,
                   quiet = F,
                   ...) {

  # validate -------------------------------------------------------

  # check `dt` for 2/3 of mx, ax, qx
  assertive::assert_is_data.table(dt)
  param_cols <- intersect(names(dt), c("mx", "ax", "qx"))
  assertthat::assert_that(
    length(param_cols) >= 2 | "qx" %in% param_cols,
    msg = "Need at least two of 'mx', 'ax', 'qx' in 'dt' or just 'qx' in 'dt'."
  )
  only_qx <- length(param_cols) == 1

  # check `dt` and `id_cols`
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

  # prep ------------------------------------------------------------

  original_col_order <- copy(names(dt))
  original_keys <- copy(key(dt))

  dt <- copy(dt)
  dt <- dt[, .SD, .SDcols = c(id_cols, param_cols)]

  # get `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  hierarchyUtils::gen_length(dt, "age")

  if(!"qx" %in% names(dt)) dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
  dt[, px := 1 - qx]

  if (!only_qx) {
    if(!"ax" %in% names(dt)) dt[, ax := mx_qx_to_ax(mx, qx, age_length)]
    if(!"dx" %in% names(dt)) {
      gen_lx_from_qx(dt, id_cols, assert_na = T)
      gen_dx_from_lx(dt, id_cols, assert_na = T)
    }
  }

  # aggregate qx ------------------------------------------------------------

  if (!quiet) message("Aggregating px across age groups")
  dt_qx <- hierarchyUtils::agg(
    dt = dt[, .SD, .SDcols = c(id_cols, "px")],
    id_cols = id_cols,
    value_cols = "px",
    col_stem = "age",
    col_type = "interval",
    mapping = age_mapping,
    agg_function = prod,
    ...
  )
  dt_qx[, qx := 1 - px]
  dt_qx[, px := NULL]

  # aggregate ax ------------------------------------------------------------

  if (!only_qx) {

    # determine the aggregate age group each granular age group belongs to
    common_interval_mapping <- copy(age_mapping)
    data.table::setnames(
      common_interval_mapping,
      old = c("age_start", "age_end"),
      new = c("common_start", "common_end")
    )
    dt <- hierarchyUtils:::merge_common_intervals(
      dt,
      common_intervals = common_interval_mapping,
      col_stem = "age"
    )
    data.table::setnames(
      dt,
      old = c("common_start", "common_end"),
      new = c("agg_age_start", "agg_age_end")
    )

    # calculate the integer number of complete person-years lived by those who die
    # in each aggregate age group.
    dt[, ax_full_years := age_start - agg_age_start]
    dt[, c("agg_age_start", "agg_age_end") := NULL]

    # calculate total number of person-years lived by those who die in each age
    # group
    dt[, axdx_total := (ax + ax_full_years) * dx]

    if (!quiet) message("Aggregating ax across age groups")
    dt_ax <- hierarchyUtils::agg(
      dt = dt[, .SD, .SDcols = c(id_cols, "axdx_total", "dx")],
      id_cols = id_cols,
      value_cols = c("axdx_total", "dx"),
      col_stem = "age",
      col_type = "interval",
      mapping = age_mapping,
      agg_function = sum,
      ...
    )
    dt_ax[, ax := axdx_total / dx]
    dt_ax[, c("axdx_total", "dx") := NULL]

  }

  # check output -----------------------------------------------------

  if (only_qx) {
    dt <- copy(dt_qx)
  } else {
    dt <- merge(dt_qx, dt_ax, all = TRUE, by = id_cols)
  }

  if ("qx" %in% param_cols) {
    assertable::assert_values(dt, "qx", "gte", 0, quiet = T)
    assertable::assert_values(dt, "qx", "lte", 1, quiet = T)
  }

  if ("ax" %in% param_cols) {
    assertable::assert_values(dt, "ax", "gte", 0, quiet = T)
  }

  if ("mx" %in% param_cols) {
    if (!"age_length" %in% names(dt)) {
      dt <- hierarchyUtils::gen_length(dt, col_stem = "age")
    }
    dt[, mx := qx_ax_to_mx(qx, ax, age_length)]
    assertable::assert_values(dt, "mx", "gte", 0, quiet = T)
  }

  assertable::assert_values(dt, param_cols, "not_na", quiet = T)

  extra_cols_present <- setdiff(names(dt), original_col_order)
  if (length(extra_cols_present) > 0) dt[, (extra_cols_present) := NULL]
  if ("age_length" %in% original_col_order & !"age_length" %in% names(dt)) {
    dt <- hierarchyUtils::gen_length(dt, col_stem = "age")
  }

  expected_cols <- c(id_cols, "qx", "mx", if (!only_qx) "ax")
  original_col_order <- original_col_order[original_col_order %in% expected_cols]
  data.table::setcolorder(dt, original_col_order)
  data.table::setkeyv(dt, original_keys)

  return(dt)
}
