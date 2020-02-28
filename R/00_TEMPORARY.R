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
