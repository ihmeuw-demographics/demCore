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
#' @param 
#'
#' @return \[`data.table()`\]\cr Total fertility values, has `id_cols` and 'tfr'
#'   columns.

tfr <- function(dt, id_cols, age_lower, age_upper) {
  # validate -------------------------------------------------------------------
  
  # check for required columns
  assertable::assert_colnames(dt, c('age_start', 'age_end', 'asfr'), 
                              only_colnames = F)
  
  # confirm age_lower and age_upper are bounds of age groups in data
  assertthat::assert_that(age_lower %in% dt[,age_start],
                          msg = 'age_lower not found in data')
  assertthat::assert_that(age_upper %in% dt[,age_end],
                          msg = 'age_upper not found in data')
  
  # prep -----------------------------------------------------------------------
  
  dt <- copy(dt)
  dt[,age_group_size := age_end-age_start]
  
  # calculate ------------------------------------------------------------------
  
  dt <- dt[age_start >= age_lower & age_end <= age_upper]
  dt <- dt[,.(tfr = sum(asfr*age_group_size)), by = id_cols]
  
  return(dt)
}

