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

validate_lifetable <- function(dt, id_cols = c(), param_cols = c(),
                                assert_na = NA) {

  # check `id_cols` argument -------------------------------------------------

  if(length(id_cols) > 0) {

    # character
    assertive::assert_is_character(id_cols)

    # includes "age_start" and "age_end"
    assertthat::assert_that("age_start" %in% id_cols & "age_end" %in% id_cols,
                            msg = "`id_cols` must include 'age_start' and
                            'age_end'.")

    # shouldn't include other age variables
    if("age" %in% id_cols) stop("'age' cannot be in id_cols.")
    if("age_length" %in% id_cols) stop("'age_length' cannot be in id_cols.")
    if("age_group" %in% id_cols) stop("'age_group' cannot be in id_cols.")
    if("age_group_id" %in% id_cols) stop("'age_group_id' cannot be in id_cols.")
    id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]
    if(any(tolower(id_cols_no_age) %like% "age")) {
      warning("Confirm that no age vars other than 'age_start' and 'age_end'
              are in 'id_cols'.")
    }
  }

  # check `dt` ---------------------------------------------------------------

  # data.table
  assertive::assert_is_data.table(dt)

  # unique
  if(length(id_cols) > 0) {
    hierarchyUtils::assert_is_unique_dt(dt, id_cols = id_cols)
  }

  # has correct columns
  assertable::assert_colnames(dt, c(param_cols, id_cols),
                              only_colnames = F, quiet = T)

  # age and parameter columns numeric
  if("age_start" %in% names(dt)) assertive::assert_is_numeric(dt[["age_start"]])
  if("age_end" %in% names(dt)) assertive::assert_is_numeric(dt[["age_end"]])
  for(param in param_cols) {
    assertive::assert_is_numeric(dt[[param]])
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
    assertive::assert_is_logical(assert_na)
  }
  return(invisible(dt))
}

