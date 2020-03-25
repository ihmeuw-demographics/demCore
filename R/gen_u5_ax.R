#' @title Generate ax for ages under 5 years
#'
#' @description Add ax onto a data.table for ages <1 and 1-4 years, with
#'   algorithm based on under-1 mx.
#'
#' @param dt \[`data.table()`\] with columns:
#'   * 'age' with values 0 (for under-1) and 1 (for 1-4). Other ages permitted
#'      but will result in NA output for ax or unchanged ax if ax in input `dt`
#'   * 'sex' where values must be 'male', 'female', or 'both'
#'   * 'mx' for mortality rate
#'   * All additional columns from 'id_cols'
#' @param id_cols \[`character()`\] columns that uniquely identify
#'   observations in `dt`. Must include 'age' and 'sex'.
#'
#' @return modifies `dt` in place with "ax" column added on.
#'
#' @details Takes a data.table with age, sex, and infant mortality rate and
#'   adds a column 'ax'. Values from:
#'
#'   Preston Samuel H, Patrick H, Michel G. Demography: measuring and modeling
#'   population processes. MA: Blackwell Publishing. 2001.
#'
#'  |                     || Males               || Females             |
#'  | ------------------- |-|--------------------|-| -------------------|
#'  | 1a0:                ||                     ||                     |
#'  | If 1m0 >= 0.107     || 0.330               || 0.350               |
#'  | If 1m0 < 0.107      || 0.045 + 2.684 * 1m0 || 0.053 + 2.800 * 1m0 |
#'  |                     ||                     ||                     |
#'  | 4a1:                ||                     ||                     |
#'  | If 1m0 >= 0.107     || 1.352               || 1.361               |
#'  | If 1m0 < 0.107      || 1.651 - 2.816 * 1m0 || 1.522 - 1.518 * 1m0 |
#'
#' @examples
#' dt <- data.table::data.table(
#'   age = c(0, 1),
#'   mx = c(0.09, 0.12),
#'   sex = c("male", "male")
#' )
#' dt <- gen_u5_ax(dt, id_cols = c("age", "sex"))
#' @export

gen_u5_ax <- function(dt, id_cols) {

  # high/low infant mx threshold from Preston book
  threshold <- 0.107

  # validate ---------------------------------------------------------------

  # check 'id_cols'
  assertive::assert_is_character(id_cols)
  cols <- c("age", "sex")
  assertthat::assert_that(all(cols %in% id_cols),
                          msg = "`id_cols` must include 'age' and 'sex'.")

  # check 'dt'
  assertive::assert_is_data.table(dt)
  assertable::assert_colnames(dt, c(id_cols, "mx"), only_colnames = F,
                              quiet = T)
  assertive::assert_is_numeric(dt[["mx"]])
  assertable::assert_values(dt, "mx", test = "gte", test_val = 0, quiet = T)

  # check age
  assertive::assert_is_numeric(dt[["age"]])
  if(!(0 %in% unique(dt$age))) stop("Age 0 must be in 'dt'.")
  if(!(1 %in% unique(dt$age))) {
    warning("'dt' does not have age 1 (1-4 years). Make sure you expect this.")
  }
  if(length(setdiff(unique(dt$age), c(0,1)) > 0)) {
    message("Rows in 'dt' with age not 0 or 1 will not be assigned ax values.")
  }

  # check sex
  assertable::assert_values(dt, colnames = "sex", test = "in",
                            test_val = c("male", "female", "both"), quiet = T)

  # merge on 1m0 -----------------------------------------------------------

  u1_mx <- dt[age == 0]
  u1_mx <- u1_mx[, .SD, .SDcols = c(id_cols, "mx")]
  setnames(u1_mx, "mx", "mx_inf")
  u1_mx[, c("age") := NULL]
  dt <- merge(dt, u1_mx, by = setdiff(id_cols, "age"))

  # calculate 1a0 ----------------------------------------------------------

  dt[sex == "male" & age == 0,
     ax := ifelse(mx_inf >= threshold, 0.330, 0.045 + 2.684 * mx_inf)]
  dt[sex == "female" & age == 0,
     ax := ifelse(mx_inf >= threshold, 0.350, 0.053 + 2.800 * mx_inf)]
  dt[sex == "both" & age == 0,
     ax := ifelse(mx_inf >= threshold, 0.340, 0.049 + 2.742 * mx_inf)]

  # calculate 4a1 ----------------------------------------------------------

  dt[sex == "male" & age == 1,
     ax := ifelse(mx_inf >= threshold, 1.352, 1.651 - 2.816 * mx_inf)]
  dt[sex == "female" & age == 1,
     ax := ifelse(mx_inf >= threshold, 1.361, 1.522 - 1.518 * mx_inf)]
  dt[sex == "both" & age == 1,
     ax := ifelse(mx_inf >= threshold, 1.3565, 1.5865 - 2.167 * mx_inf)]

  # clean up and return -----------------------------------------------------

  dt[, c("mx_inf") := NULL]

}
