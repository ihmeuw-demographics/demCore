#' @title Calculate total fertility
#'
#' @description Calculate total fertility for a specified age range based on 
#'              age-specific fertility rates
#' 
#' @param dt \[`data.table()`\]\cr Data 
#'   * Must only include columns 'asfr' and those specified in `id_cols`
#'   * Must include 'age_start' and 'age_end' columns
#' @param age_lower \[`integer(1)`\]\cr 
#'    Start age for total fertility calculation
#' @param age_upper \[`integer(1)`\]\cr
#'    End age for total fertility calculation
#' @inheritParams hierarchyUtils::agg
#'
#' @return \[`data.table()`\]\cr Total fertility values, has `id_cols` and 'tfr'
#'   columns.
#'
#' @details
#' Calculate total fertility aggregate for ages within `age_lower`` and 
#' `age_upper` such as total fertility under 25 and total fertility over 30. TFR
#'  is calculated as the sum of ASFR multiplied by the number of years in an
#'  age group.
#'  
#' @examples 
#' # calculate total fertility under 25 (ages 10 to 24)
#' dt <- data.table('asfr' = c(0.00005, 0.02, 0.07, 0.08, 0.05, 0.02, 0.004, 0.0002),
#'                  'age_start' = c(10, 15, 20, 25, 30, 35, 40, 45),
#'                  'age_end' = c(15, 20, 25, 30, 35, 40, 45, 50))
#' dt <- tfr(test, c(), 10, 25)

tfr <- function(dt, id_cols, age_lower, age_upper) {
  # validate -------------------------------------------------------------------
  
  # check for required columns
  assertable::assert_colnames(dt, c('age_start', 'age_end', 'asfr'), 
                              only_colnames = F)
  
  # check that age_lower and age_upper are length one numeric
  assertthat::is.number(age_lower)
  assertthat::is.number(age_upper)
  
  # confirm age_lower and age_upper are bounds of age groups in data
  assertthat::assert_that(age_lower %in% dt[,age_start],
                          msg = 'age_lower not found in data')
  assertthat::assert_that(age_upper %in% dt[,age_end],
                          msg = 'age_upper not found in data')
  
  # prep -----------------------------------------------------------------------
  
  dt <- copy(dt)
  hierarchyUtils::gen_length(dt, col_stem = 'age')
  
  # calculate ------------------------------------------------------------------
  
  dt <- dt[age_start >= age_lower & age_end <= age_upper]
  dt <- dt[, tfr := asfr*age_length]
  
  dt <- hierarchyUtils::agg(
    dt[,c(..id_cols, 'age_start', 'age_end', 'tfr')],
    id_cols = c(id_cols, 'age_start', 'age_end'),
    value_cols = 'tfr',
    col_stem = 'age',
    col_type = 'interval',
    mapping = data.table::data.table(
      age_start = age_lower,
      age_end = age_upper
    ),
    agg_function = sum
  )
  
  return(dt)
}


