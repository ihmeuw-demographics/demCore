#' @title Generate ax for ages under 5 years
#'
#' @description Add ax onto a data.table for ages <1 and 1-4 years, with
#'   algorithm based on under-1 mx.
#'
#' @param dt \[`data.table()`\]\cr columns:
#'   * 'age_start' with values 0 (for under-1) and 1 (for 1-4). Other ages
#'      permitted but will result in NA output for ax or unchanged ax if ax in
#'      input `dt`.
#'   * 'age_end' with values 1 (for under-1) and 5 (for 1-4).
#'   * 'sex' where values must be 'male', 'female', or 'both'
#'   * 'mx' for mortality rate
#'   * All additional columns from 'id_cols'
#' @param id_cols \[`character()`\]\cr columns that uniquely identify
#'   observations in `dt`. Must include 'age_start', 'age_end', and 'sex'.
#'
#' @return modifies `dt` in place with 'ax' column added on.
#'
#' @details Takes a data.table with age_start, age_end, sex, and infant
#'   mortality rate and adds a column 'ax'. Values from:
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
#'   age_start = c(0, 1),
#'   age_end = c(1, 5),
#'   mx = c(0.09, 0.12),
#'   sex = c("male", "male")
#' )
#' dt <- gen_u5_ax(dt, id_cols = c("age_start", "age_end", "sex"))
#' @export

gen_u5_ax <- function(dt, id_cols) {

  # high/low infant mx threshold from Preston book
  threshold <- 0.107

  # validate ---------------------------------------------------------------

  # standard validations
  validate_lifetable(dt, id_cols = id_cols, param_cols = c("mx"))

  # get `id_cols` without age
  id_cols_no_age <- id_cols[!id_cols %in% c("age_start", "age_end")]

  # check age
  u1 <- dt[age_start == 0 & age_end == 1]
  if(nrow(u1) == 0) {
    stop("Age 0-1 years must be in `dt`.")
  }
  u1 <- u1[, .SD, .SDcols = id_cols_no_age]
  u1[, has_1m0 := 1]
  dt <- merge(dt, u1, by = id_cols_no_age, all = T)
  if(nrow(dt[is.na(has_1m0)]) > 0) {
    stop("You do not have age 0-1 years for all `id_col` combinations.")
  }
  dt[, has_1m0 := NULL]
  if(nrow(dt[age_start == 1 & age_end == 5]) == 0) {
    warning("'dt' does not have any rows with 'age_start' 1 and 'age_end' 5
            (1-4 years). Make sure you expect this.")
  }
  if(length(setdiff(unique(dt$age_start), c(0,1)) > 0)) {
    message("Rows with age not 0-1 or 1-4 will not be assigned ax values.")
  }

  # check sex
  assertthat::assert_that("sex" %in% id_cols,
                          msg = "`id_cols` must include 'sex'.")
  assertable::assert_values(dt, colnames = "sex", test = "in",
                            test_val = c("male", "female", "both"), quiet = T)

  # merge on 1m0 -----------------------------------------------------------

  u1_mx <- dt[age_start == 0 & age_end == 1]
  u1_mx <- u1_mx[, .SD, .SDcols = c(id_cols, "mx")]
  setnames(u1_mx, "mx", "mx_inf")
  u1_mx[, c("age_start", "age_end") := NULL]
  dt <- merge(dt, u1_mx, by = id_cols_no_age, all.x = T)

  # calculate 1a0 ----------------------------------------------------------

  dt[sex == "male" & age_start == 0 & age_end == 1,
     ax := ifelse(mx_inf >= threshold, 0.330, 0.045 + 2.684 * mx_inf)]
  dt[sex == "female" & age_start == 0 & age_end == 1,
     ax := ifelse(mx_inf >= threshold, 0.350, 0.053 + 2.800 * mx_inf)]
  dt[sex == "both" & age_start == 0 & age_end == 1,
     ax := ifelse(mx_inf >= threshold, 0.340, 0.049 + 2.742 * mx_inf)]

  # calculate 4a1 ----------------------------------------------------------

  dt[sex == "male" & age_start == 1 & age_end == 5,
     ax := ifelse(mx_inf >= threshold, 1.352, 1.651 - 2.816 * mx_inf)]
  dt[sex == "female" & age_start == 1 & age_end == 5,
     ax := ifelse(mx_inf >= threshold, 1.361, 1.522 - 1.518 * mx_inf)]
  dt[sex == "both" & age_start == 1 & age_end == 5,
     ax := ifelse(mx_inf >= threshold, 1.3565, 1.5865 - 2.167 * mx_inf)]

  # clean up and return -----------------------------------------------------

  dt[, c("mx_inf") := NULL]

}
