#' @title Calculate total fertility aggregates
#'
#' @description Calculate total fertility for a specified age range based on 
#'              age-specific fertility rates
#' 
#' @param dt \[`data.table()`\]\cr Data 
#'   * Must only include columns 'asfr' and those specified in `id_cols`
#'   * Must include 'age_start' and 'age_end' columns
#' @param age_mapping \[`data.table()`\]\cr 
#'  Specification of age interval to aggregate to. Required columns are 
#'  'age_start' and 'age_end'.
#' @inheritParams hierarchyUtils::agg
#' 
#' @seealso [hierarchyUtils::agg()]
#'
#' @return \[`data.table()`\]\cr Total fertility values, has `id_cols` and 'tf'
#'   columns.
#'
#' @details
#' Calculate total fertility aggregate for ages within `age_lower` and 
#' `age_upper` such as total fertility under 25 and total fertility over 30. 
#'  Total fertility is calculated as the sum of ASFR multiplied by the number of 
#'  years in an age group. This number represents the average number of children 
#'  born to a woman if (1) she experiences a specific set of age specific 
#'  fertility rates and (2) she survives through the end of the age span. 
#'  Preston pg 95. 
#'  
#'  This is different from an age-specific fertility rate (ASFR) or a crude birth
#'  rate (CBR), both of which are calculated as births/population for a 
#'  particular age group or all reproductive ages, respectively. 
#'   `agg_tf()` is a wrapper for [hierarchyUtils::agg()].
#'  
#' @seealso Preston Demography book page 95.
#'  
#' @examples 
#' # calculate total fertility under 25 (ages 10 to 24)
#' dt <- data.table('asfr' = c(0.00005, 0.02, 0.07, 0.08, 0.05, 0.02, 0.004, 0.0002),
#'                  'age_start' = c(10, 15, 20, 25, 30, 35, 40, 45),
#'                  'age_end' = c(15, 20, 25, 30, 35, 40, 45, 50))
#' dt <- agg_tf(dt, c(), 10, 25)

agg_tf <- function(dt, id_cols, age_lower, age_upper) {
  # validate -------------------------------------------------------------------
  
  # check for required columns
  assertable::assert_colnames(
    dt, 
    c('age_start', 'age_end', 'asfr'), 
    only_colnames = F
  )
  
  # check that age_lower and age_upper are length one numeric
  # and age_lower and age_upper are bounds of age groups in data
  assertthat::assert_that(
    age_lower %in% dt[,age_start], 
    assertthat::is.number(age_lower),
    msg = 'age_lower not found in data'
  )
  
  assertthat::assert_that(
    age_upper %in% dt[,age_start], 
    assertthat::is.number(age_upper),
    msg = 'age_upper not found in data'
  )
  
  # check that age_lower < age_upper
  assertthat::assert_that(
    age_lower < age_upper,
    msg = 'age_lower is not less than age_upper'
  )
  
  # prep -----------------------------------------------------------------------
  
  dt <- copy(dt)
  hierarchyUtils::gen_length(dt, col_stem = 'age')
  
  # calculate ------------------------------------------------------------------
  
  dt <- dt[, tf := asfr*age_length]
  
  dt <- hierarchyUtils::agg(
    dt[,c(..id_cols, 'age_start', 'age_end', 'tf')],
    id_cols = c(id_cols, 'age_start', 'age_end'),
    value_cols = 'tf',
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


