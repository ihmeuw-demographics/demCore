#' @title Convert between demographic data stored in data.table and matrix
#'   format
#'
#' @description Convert demographic data (can be by age and/or sex) between two
#'   data formats, \[`data.table()`\] and \[`matrix()`\]. When stored in matrix
#'   form the data is required to be square (ie. same number of ages in every
#'   year).
#'
#' @param dt \[`data.table()`\]\cr
#'   Input demographic data in the data.table format described in the details
#'   section.
#' @param mdt \[`data.table()`\]\cr
#'   Input demographic data in the matrix format described in the details
#'   section.
#' @param year_right_most_endpoint \[`numeric(1)`\]\cr
#'   Assumed right most endpoint of the calendar year intervals. If `mdt` is
#'   actually data collected at a specific point in time (like census data) and
#'   not spanning intervals then assign 'NULL'.
#' @param age_right_most_endpoint \[`numeric(1)`\]\cr
#'   Assumed right most endpoint of the age group intervals. Default is Inf.
#' @inheritParams ccmpp
#' @param id_cols \[`character()`\]\cr
#'   ID columns that uniquely identify each row of `dt`. This can only contain
#'   year, sex, and age variables. 'year_start' and 'year_end' must be included,
#'   and might contain 'sex' if the data is sex-specific, and 'age_start' and
#'   'age_end' if the data is age-specific.
#' @param value_col \[`character(1)`\]\cr
#'   Name of the column containing the value of interest. Default is 'value'.
#' @param validate_arguments \[`logical(1)`\]\cr
#'   Whether to validate that the input arguments are formatted correctly.
#'   Default is 'TRUE'.
#'
#' @details
#' data.table format: When data is in data.table format then it must have a
#' column for 'year_start' and 'year_end', if the data is sex-specific it must
#' have a column for 'sex', and if the data is age-specific it must have
#' columns for 'age_start' and 'age_end.
#'
#' matrix format: When data is in matrix format, columns represent the start of
#' each calendar year interval, rows represent the start of each age interval
#' and if the data is sex specific each sex will have a separate matrix stored
#' in a named list (names corresponding to each sex).
#'
#' @return `matrix_to_dt` returns a \[`data.table()`\] in data.table format as
#' described in the details section. `dt_to_matrix` returns a matrix of list of
#' matrices as described in the details section.
#'
#' @examples
#' output_matrix <- demCore:::dt_to_matrix(thailand_initial_estimates$survival)
#' output_dt <- demCore:::matrix_to_dt(output_matrix, year_right_most_endpoint = 2000)
#'
#' @rdname dt_matrix_format
matrix_to_dt <- function(mdt,
                         year_right_most_endpoint,
                         age_right_most_endpoint = Inf,
                         gen_end_interval_col = TRUE,
                         value_col = "value",
                         validate_arguments = TRUE) {

  # Validate input arguments ------------------------------------------------

  # check `validate_arguments` argument
  assertthat::assert_that(
    assertthat::is.flag(validate_arguments),
    msg = "`validate_arguments` must be a logical flag"
  )

  if (validate_arguments) {

    # check `gen_end_interval_col` argument
    assertthat::assert_that(
      assertthat::is.flag(gen_end_interval_col),
      msg = "`gen_end_interval_col` must be a logical flag"
    )

    ## check `mdt` argument
    assertthat::assert_that(
      assertive::is_matrix(mdt) |
        (assertive::is_list(mdt) & all(mapply(assertive::is_matrix, mdt))),
      msg = "`mdt` must be a matrix or list of matrices"
    )

    # standardize to list format to make other checks easier
    check_mdt <- copy(mdt)
    sex_specific <- assertive::is_list(mdt)
    if (!sex_specific) check_mdt <- list("none" = mdt)
    assertthat::assert_that(
      all(assertive::is_not_null(unlist(mapply(rownames, check_mdt)))),
      all(assertive::is_not_null(unlist(mapply(colnames, check_mdt)))),
      msg = "rownames (age_start) and colnames (year_start) of `mdt` must exist"
    )
    assertthat::assert_that(
      all(assertive::is_numeric_string(unlist(mapply(rownames, check_mdt)))),
      all(assertive::is_numeric_string(unlist(mapply(colnames, check_mdt)))),
      msg = "rownames (age_start) and colnames (year_start) of `mdt` must be
    numeric strings"
    )

    ## check `year_right_most_endpoint` argument
    assertthat::assert_that(
      assertthat::is.number(year_right_most_endpoint) |
        is.null(year_right_most_endpoint),
      msg = "`year_right_most_endpoint` must be a length one numeric or NULL"
    )

    ## check `age_right_most_endpoint` argument
    assertthat::assert_that(
      assertthat::is.number(age_right_most_endpoint),
      msg = "`age_right_most_endpoint` must be a length one numeric"
    )

    ## check `value_col` argument
    assertthat::assert_that(assertthat::is.string(value_col))
  }

  # Convert to data.table ---------------------------------------------------

  sex_specific <- assertive::is_list(mdt)
  age_specific <- ifelse(sex_specific, nrow(mdt[[1]]) > 1, nrow(mdt) > 1)
  id_cols <- c("year_start",
               if (gen_end_interval_col) "year_end",
               if (sex_specific) "sex",
               if (age_specific) "age_start",
               if (age_specific & gen_end_interval_col) "age_end")

  melt_matrix_format <- function(m,
                                 year_right_most_endpoint,
                                 age_right_most_endpoint,
                                 gen_end_interval_col,
                                 value_col) {

    d <- data.table(m)

    # assign the age_start rownames as a new column
    age_starts <- as.numeric(rownames(m))
    d[, age_start := age_starts]

    # melt the year_start columns
    d <- melt(d, id.vars = "age_start", variable.name = "year_start",
                 variable.factor = FALSE, value.name = value_col)
    d[, year_start := as.numeric(year_start)]

    if (gen_end_interval_col) {

      # add on the year_end column
      if (!is.null(year_right_most_endpoint)) {
        hierarchyUtils::gen_end(
          dt = d,
          id_cols = c("year_start", "age_start"),
          col_stem = "year",
          right_most_endpoint = year_right_most_endpoint
        )
      } else {
        d[, year_end := year_start]
      }

      # add on the age_end column
      hierarchyUtils::gen_end(
        dt = d,
        id_cols = c("year_start", "age_start"),
        col_stem = "age",
        right_most_endpoint = age_right_most_endpoint
      )
    }

    return(d)
  }

  if (sex_specific) {
    sexes <- sort(unique(names(mdt)))
    dt <- rbindlist(
      lapply(sexes, function(s) {
        dt <- melt_matrix_format(mdt[[s]], year_right_most_endpoint,
                                 age_right_most_endpoint, gen_end_interval_col,
                                 value_col)
        dt[, sex := s]
      })
    )
  } else {
    dt <- melt_matrix_format(mdt, year_right_most_endpoint,
                             age_right_most_endpoint, gen_end_interval_col,
                             value_col)
  }

  if (!age_specific) {
    dt[, c("age_start", if (gen_end_interval_col) "age_end") := NULL]
  }

  data.table::setcolorder(dt, c(id_cols, "value"))
  data.table::setkeyv(dt, id_cols)
  return(dt)
}

#' @rdname dt_matrix_format
dt_to_matrix <- function(dt,
                         id_cols = c("year_start", "year_end", "sex",
                                     "age_start", "age_end"),
                         value_col = "value",
                         validate_arguments = TRUE) {

  # Validate arguments ------------------------------------------------------

  if (validate_arguments) {
    # check `id_cols` argument
    assertive::assert_is_character(id_cols)
    possible_id_cols <- c("year_start", "year_end", "year",
                          "sex", "age_start", "age_end")
    assertthat::assert_that(
      length(setdiff(id_cols, possible_id_cols)) == 0,
      msg = paste0("id_cols can only include '",
                   paste(possible_id_cols, collapse = "', '"), "'.")
    )

    # check `dt` argument
    assertive::assert_is_data.table(dt)
    assertable::assert_colnames(dt, c(id_cols, value_col), quiet = T)
    demUtils::assert_is_unique_dt(dt, id_cols)
  }

# Convert to matrix -------------------------------------------------------

  sex_specific <- "sex" %in% id_cols
  age_specific <- "age_start" %in% id_cols
  dt <- copy(dt)
  year_col <- ifelse("year_start" %in% id_cols, "year_start", "year")

  dcast_matrix_format <- function(d, year_col, age_specific, value_col) {
    if (age_specific) {
      age_starts <- sort(unique(d$age_start))
      form <- eval(paste0("age_start ~ ", year_col))
    } else {
      age_starts <- 0
      form <- eval(paste0(". ~ ", year_col))
    }
    m <- dcast(d,  form , value.var = value_col)
    m[, c(ifelse(age_specific, "age_start", ".")) := NULL]
    m <- as.matrix(m)
    rownames(m) <- age_starts
    return(m)
  }

  if (sex_specific) {
    sexes <- sort(unique(dt$sex))
    mdt <- lapply(sexes, function(s) {
      m <- dcast_matrix_format(dt[sex == s], year_col, age_specific, value_col)
      return(m)
    })
    names(mdt) <- sexes

  } else {
    mdt <- dcast_matrix_format(dt, year_col, age_specific, value_col)
  }
  return(mdt)
}
