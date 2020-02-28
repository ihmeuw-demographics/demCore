# TEMPORARY COPY OF demUtils functions until we get import working

assert_is_unique_dt <- function(dt, id_cols) {

  non_unique_dt <- identify_non_unique_dt(dt, id_cols)
  is_unique_dt <- nrow(non_unique_dt) == 0

  error_msg <- "Input data rows are not unique for each combination of the id columns. Use `identify_non_unique_dt` to see which data is problematic."
  assertthat::assert_that(is_unique_dt, msg = error_msg)
}

identify_non_unique_dt <- function(dt, id_cols) {

  # Validate arguments ------------------------------------------------------

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, id_cols, only_colnames = F, quiet = T)

  # Count number of rows in each combination of `id_cols` -------------------

  check_unique_dt <- dt[, list(check = .N), by = id_cols]
  check_unique_dt <- check_unique_dt[check > 1]

  setcolorder(check_unique_dt, c(id_cols, "check"))
  setkeyv(check_unique_dt, c(id_cols, "check"))
  return(check_unique_dt)
}

calculate_age_end <- function(dt, id_cols, terminal_age_end = 125L) {

  # Validate arguments ------------------------------------------------------

  # check `id_cols` argument
  assertive::assert_is_character(id_cols)
  assertthat::assert_that("age_start" %in% id_cols, msg = "`id_cols` must include 'age_start'.")

  # check `terminal_age_end` argument
  assertive::assert_is_numeric(terminal_age_end)

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, id_cols, only_colnames = F, quiet = T)
  assertive::assert_is_numeric(dt[["age_start"]])
  assertive::assert_all_are_not_na(dt[["age_start"]])
  assert_is_unique_dt(dt, id_cols)

  # Calculate age end column ------------------------------------------------

  setkeyv(dt, id_cols)
  by_id_cols <- id_cols[!id_cols %in% "age_start"]

  dt[, age_end := data.table::shift(age_start, type = "lead", fill = terminal_age_end),
     by = by_id_cols]

  setkeyv(dt, c(id_cols, "age_end"))
  return(invisible(NULL))
}

calculate_age_int <- function(dt) {

  # Validate arguments ------------------------------------------------------

  # check `dt` argument
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c("age_start", "age_end"), only_colnames = F, quiet = T)
  assertive::assert_is_numeric(dt[["age_start"]])
  assertive::assert_all_are_not_na(dt[["age_start"]])
  assertive::assert_is_numeric(dt[["age_end"]])

  # Calculate age interval column -------------------------------------------

  dt[, age_int := age_end - age_start]

  return(invisible(NULL))
}
