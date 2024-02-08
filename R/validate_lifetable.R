#' @title Validate life table
#'
#' @description Check life table, id columns, life table parameter columns,
#'   and other inputs to functions in this package.
#'
#' @param dt \[`data.table()`\]\cr Input life tables
#' @param id_cols \[`character()`\]\cr Columns that uniquely identify each row
#'   of `dt`.
#' @param param_cols \[`character()`\]\cr Columns containing life table
#'   parameters (qx, lx, etc.)
#' @param assert_uniform_age_length \[`logical()`\]\cr Whether to check that
#'   the age groups in the lifetable are all of the same length except for the
#'   terminal age group.
#' @param assert_uniform_terminal_age \[`logical()`\]\cr Whether to check that
#'   each terminal age group starts at the same age.
#' @param assert_age_start_0 \[`logical()`\]\cr Whether to check that the
#'   youngest age group starts at age zero.
#' @param assert_na \[`logical()`\]\cr Whether to check for NA values in the
#'   generated variable.
#'
#' @return Invisibly returns input `dt`. Fails if any assertion fails.
#'
#' @details This function performs the following checks:
#'
#'   **id_cols** is a character vector, 'age_start' and 'age_end' are included,
#'     and no other age variables are included.
#'
#'   **dt** is data.table, is unique by `id_cols`, has all `id_cols` and
#'     `param_cols`, 'age_start', 'age_end', and parameter columns are numeric,
#'     all life table parameters >= 0, parameters qx, lx, dx <= 1.
#'
#'   **assert_na** check `assert_na` is a logical.
#'
#' @export
validate_lifetable <- function(dt,
                               id_cols = c(),
                               param_cols = c(),
                               assert_uniform_age_length = FALSE,
                               assert_uniform_terminal_age = FALSE,
                               assert_age_start_0 = FALSE,
                               assert_na = NA) {

  # check `id_cols` argument -------------------------------------------------

  if(length(id_cols) > 0) {

    # character
    checkmate::assert_character(id_cols)

    # includes "age_start" and "age_end"
    assertthat::assert_that("age_start" %in% id_cols & "age_end" %in% id_cols,
                            msg = "`id_cols` must include 'age_start' and
                            'age_end'.")

    # shouldn't include other age variables
    if("age" %in% id_cols) stop("'age' cannot be in id_cols.")
    if("age_length" %in% id_cols) stop("'age_length' cannot be in id_cols.")
    if("age_group" %in% id_cols) stop("'age_group' cannot be in id_cols.")
    if("age_group_id" %in% id_cols) stop("'age_group_id' cannot be in id_cols.")

  }

  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]
  if(any(tolower(id_cols_no_age) %like% "age")) {
    warning("Confirm that no age vars other than 'age_start' and 'age_end'
              are in 'id_cols'.")
  }
  # check `dt` ---------------------------------------------------------------

  # data.table
  checkmate::assert_data_table(dt)

  # unique
  if(length(id_cols) > 0) {
    demUtils::assert_is_unique_dt(dt, id_cols = id_cols)
  }

  # has correct columns
  assertable::assert_colnames(dt, c(param_cols, id_cols),
                              only_colnames = F, quiet = T)

  # age and parameter columns numeric
  if("age_start" %in% names(dt)) checkmate::assert_numeric(dt[["age_start"]])
  if("age_end" %in% names(dt)) checkmate::assert_numeric(dt[["age_end"]])
  for(param in param_cols) {
    checkmate::assert_numeric(dt[[param]])
  }

  # check that age interval is constant in input lifetable
  if (assert_uniform_age_length) {
    age_length_column <- "age_length" %in% names(dt)
    if (!age_length_column) {
      hierarchyUtils::gen_length(dt, col_stem = "age")
    }
    age_int <- dt[age_length != Inf, unique(age_length)]
    assertthat::assert_that(
      length(age_int) == 1,
      msg = "the age intervals in `dt` must be consistent across all age groups"
    )
    if (!age_length_column) dt[, age_length := NULL]
  }

  # check that the terminal age group in `dt` is consistent
  if (assert_uniform_terminal_age) {
    dt_terminal_ages <- dt[age_end == "Inf", unique(age_start)]
    assertthat::assert_that(
      length(dt_terminal_ages) == 1,
      msg = "the terminal ages in `dt` must be consistent across `id_cols`"
    )
  }

  # check that the starting age group in `dt` is consistently zero
  if (assert_age_start_0) {
    dt_min_age_start <- dt[, min(age_start), by = id_cols_no_age]
    assertthat::assert_that(
      all(dt_min_age_start$V1 == 0),
      msg = "the starting age group in `dt` must be zero across `id_cols`"
    )
  }


  # all life table params > 0
  assertable::assert_values(dt, param_cols, test = "gte",
                            test_val = 0, quiet = T)

  # qx, lx, dx <= 1
  params_lte_1 <- intersect(param_cols, c("qx", "lx", "dx"))
  if(length(params_lte_1) > 0) {
    assertable::assert_values(dt, params_lte_1, test = "lte",
                              test_val = 1, quiet = T)
  }

  # check `assert_na` --------------------------------------------------------

  if(!is.na(assert_na)) {
    checkmate::assert_logical(assert_na)
  }
  return(invisible(dt))
}


#' @title Check two of mx, ax, qx
#'
#' @description Helper function to check a data.table for two of mx, ax, and qx
#'   and compute the missing parameter if one is missing.
#'
#' @param dt \[`data.table()`\]\cr Data to check for mx, ax, and/or qx. Must
#'   also have 'age_length' column.
#'
#' @return `dt` with input columns plus any of 'mx', 'ax', and 'qx' that is
#'   missing in input. Or, returns error if input has fewer than two of these
#'   parameters.
#'
#' @details Uses [mx_ax_to_qx()], [qx_ax_to_mx()], or [mx_qx_to_ax()] function
#'   to complete the set of three life table parameters.
#'
#' @examples
#' dt <- data.table::data.table(
#'   age_start = c(0, 1, 5, 10),
#'   age_length = c(1, 4, 5, 5),
#'   mx = c(0.009, 0.0004, 0.00015, 0.00019),
#'   ax = c(0.068, 1.626, 2.5, 2.5)
#' )
#' dt <- check_mx_ax_qx(dt)
#'
#' @export
check_mx_ax_qx <- function(dt) {

  # quick checks
  checkmate::assert_data_table(dt)
  assertable::assert_colnames(dt, c("age_length"), only_colnames = F, quiet = T)

  # check `dt` for 2/3 of mx, ax, qx
  assertthat::assert_that(
    length(intersect(c("mx", "ax", "qx"), names(dt))) >= 2,
    msg = "Need at least two of mx, ax, qx."
  )

  # qx, mx, ax if missing
  if(!"qx" %in% names(dt)) dt[, qx := mx_ax_to_qx(mx, ax, age_length)]
  if(!"mx" %in% names(dt)) dt[, mx := qx_ax_to_mx(qx, ax, age_length)]
  if(!"ax" %in% names(dt)) dt[, ax := mx_qx_to_ax(mx, qx, age_length)]

  return(dt)

}
